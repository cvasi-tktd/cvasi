#' Calculate EPx values for a series of moving time window
#'
#' Calls `epx()` to calculate the EPx value (i.e. the multiplication factors of
#' an exposure profile that cause x% of effect) for moving windows with length
#' `window_length` that move timesteps defined by `window_interval`.
#'
#' @param x a [scenario]
#' @param level The target effect level of the effect, ie. the x of EPx.
#' @param factor_cutoff above which cutoff is the EPx is not relevant
#' @param window_length the length of the moving time window
#' @param window_interval the interval that the moving time window moves
#' @param ... arguments passed to `epx`
#'
#' @export
#' @return a tibble with five columns
#' \itemize{
#'  \item window.start
#'  \item window.end
#'  \item endpoint
#'  \item level
#'  \item EPx
#'  }
#'
#' @examples
#' metsulfuron %>%
#'   set_window(length=7, interval=1) %>%
#'   epx_mtw()
epx_mtw <- function(x,
                    level = c(10, 50),
                    factor_cutoff = 1000,
                    window_length = 7,
                    window_interval = 1,
                    ...) {
  x <- x %>%
    set_window(length = window_length, interval = window_interval)

  windows <- x %>% window_candidates()

  res <- lapply(windows, function(window_i) {
    scenario <- x %>%
      clip_scenario(window_i)
    epx_res <- epx(scenario,
      level = level,
      factor_cutoff = factor_cutoff,
      long_format = TRUE,
      ep_only = TRUE,
      ...
    )

    dplyr::bind_cols(t(window_i), epx_res)
  })

  return(do.call(rbind, res))
}


# Detect worst case EPx window
#
# Filters the minimal EPx per endpoint and level from the `epx_mtw` object and
# returns it as the worst case windows. If there is more than one worst case
# window, only the first window will be returned (if `only_first == TRUE`), or
# all windows will be returned (if `only_first == FALSE`).
#
# @param epx_mtw result of `epx_mtw()`
# @param only_first TRUE = first window only
# @noRd
#' @autoglobal
#
# @return a tibble with
# \itemize{
#  \item window.start
#  \item window.end
#  \item endpoint
#  \item level
#  \item EPx
#  }
epx_min_win <- function(epx_mtw, only_first = TRUE){
  wc_window <- epx_mtw %>%
    dplyr::group_by(endpoint, level) %>%
    dplyr::filter(EPx == min(EPx)) %>%
    dplyr::arrange(window.start, endpoint, level) %>%
    dplyr::ungroup()

  # check if there is more than one worst case window per endpoint
  incr_wc_window <- wc_window %>%
    dplyr::group_by(endpoint) %>%
    dplyr::summarise(size = dplyr::n()) %>%
    dplyr::filter(size > 1) %>%
    dplyr::select(endpoint)

  # remove all but the first moving time window if more than one exist
  if (only_first){
    wc_window <- wc_window %>%
      dplyr::group_by(endpoint, level) %>%
      dplyr::filter(window.start == min(window.start)) %>%
      dplyr::arrange(window.start, endpoint, level) %>%
      dplyr::ungroup()
  }

  if (incr_wc_window %>% nrow() > 0){
    warning(
      paste0("More than one worst case window found for ",
             incr_wc_window,
             ifelse(only_first, ". All later instances removed","")
      )
    )
  }

  return(wc_window)
}

#' Effect profiles (EPx values)
#'
#' Derives one or more EPx/LPx values for the supplied effect scenarios, i.e. it
#' calculates the multiplication factors of an exposure profile that cause
#' x% of effect. Scenarios are processed in parallel, if possible.
#'
#' To estimate EPx values, a *binary search* on multiplication factors is conducted.
#' The algorithm can achieve arbitrary precision in terms of effects. The
#' same approach is implemented in the `morse` package in the `MFx()` function.
#' Convergence is often achieved in less than 10 iterations per effect level and
#' endpoint.
#'
#' Internally, a knowledge base of all tried factors and resulting effect levels is
#' kept to speed up convergence if more than one endpoint or effect level was
#' requested. The algorithm will automatically sweep the range of multiplication
#' factors as needed but hard cutoff values are implemented to avoid infinite loops;
#' the algorithm will halt with an error message if tried factors are
#' smaller than `1e-30` or greater than `1e30`.
#'
#' ### Numerical precision
#' The precision of reported *EPx* values is controlled by the argument
#' `effect_tolerance` and is given as the upper absolute error threshold of
#' effects that is deemed acceptable. The default value of `0.001` ensures that
#' a derived *EPx* will result in an effect of x% ± 0.1. Decreasing the
#' `effect_tolerance` will result in additional model iterations and longer
#' runtime. Setting an extremely small tolerance value may lead to a breakdown
#' of the algorithm due to the occurrence of extremely small, quasi-random
#' numerical errors in simulation results.
#'
#' @param scenarios table or vector of `EffectScenario` objects
#' @param level effect levels in percent (%), defaults to `c(10,50)`
#' @param effect_tolerance `numeric`, minimum absolute accuracy of effect levels
#' @param factor_cutoff optional `numeric`, the search for a multiplication factor
#' will be cut short if  tried factors exceed this value; the result will report
#' the cutoff value as the final EPx value.
#' @param verbose `logic`, if `TRUE` then infos about model evaluations are displayed
#' @param ... additional arguments passed on to [effect()]
#' @param min_factor `numeric`, if tried factors fall below this threshold, the algorithm
#' will halt with an error
#' @param max_factor  `numeric`, if tried factors exceed this threshold, the algorithm
#' will halt with an error
#' @param ep_only `logical`, if `TRUE` then only EPx values are part of the output,
#'   any contextual information such as `EffectScenario` objects are left out
#' @param long_format `logical`, if `TRUE` then EPx values are returned as
#'   a table in long format, any contextual information will be duplicated
#'
#' @return
#' The original `tibble` with additional columns named after the request effect levels, e.g. `L.EP10.`
#' If no tibble was used as argument, then a new one is created. The first column `scenario` will contain
#' the supplied `EffectScenario` objects.
#' @export
#' @importFrom stats setNames
#'
#' @examples
#' minnow_sd %>% epx()
#' minnow_sd %>% epx(level=c(10,23,42))
#'
#' # displays infos about tested multiplication factors
#' minnow_sd %>% epx(verbose=TRUE)
#'
#' # return results as a table in wide format
#' minnow_sd %>% epx(long_format=TRUE)
epx <- function(scenarios, level=c(10,50), effect_tolerance=0.001, factor_cutoff=NA,
                min_factor=1e-30, max_factor=1e30, verbose=FALSE, ep_only=FALSE,
                long_format=FALSE, ...)
{
  if(any(level>100) | any(level<1))
    stop("epx: requested effect levels out of range")

  # gracious argument handling if single object or vector was supplied
  if(is(scenarios,"EffectScenario"))
    scenarios <- tibble::tibble(scenario=c(scenarios))
  else
    scenarios <- tibble::tibble(scenario=scenarios)

  # errors should not break code flow, instead catch them
  searchfun <- purrr::safely(epx_binary_search, otherwise=NA_real_)

  # sequential processing requested?
  if(getOption("cvasi.pll.off", FALSE)) {
    ret <- purrr::map(scenarios$scenario, searchfun, level=level,
             effect_tolerance=effect_tolerance, factor_cutoff=factor_cutoff,
             min_factor=min_factor, max_factor=max_factor,
             verbose=verbose, ...)
  } else {
    # disable collection of globals by furrr to increase performance
    ret <- furrr::future_map(scenarios$scenario, searchfun, level=level,
                      effect_tolerance=effect_tolerance, factor_cutoff=factor_cutoff,
                      min_factor=min_factor, max_factor=max_factor,
                      verbose=verbose, .options=furrr::furrr_options(globals=FALSE), ...)
  }

  failed <- is.na(unlist(ret))
  if(any(failed)) {
    # collect error messages, 1st from binary search
    # 2nd, any unexpected occurrences encountered by purrr/furrr
    rs <- unlist(ret)
    errors <- rs[names(rs) == "result.error" | names(rs) == "error"]
    msg <- paste0("Some scenarios have failed (n=", sum(failed), "), result will include NAs")
    if(length(errors) > 0)
      msg <- paste0(msg, "\n", paste(paste0("  ** ", errors), collapse="\n"))
    warning(msg)
    rm(rs, errors, msg)
  }
  rs <- dplyr::bind_rows(lapply(ret, `[[`, "result"))

  # convert to long format table
  if(long_format) {
    if(!ep_only)
      rs <- dplyr::bind_cols(scenarios, rs)
    return(tidyr::pivot_longer(rs, tidyr::contains("."), names_to=c("endpoint", "level"),
                        names_pattern="(.+)\\.EP(.+)", values_to="EPx"))
  }

  # extend original table by EPx columns
  if(ep_only)
    rs
  else
    dplyr::bind_cols(scenarios, rs)
}

# EPx values are estimated using a binary search on the range of multiplication
# factors. The same approach is used in the morse package. Internally, a
# knowledge base (kbs) is employed in order to avoid re-evaluation of MFs and to
# achieve a faster start-up and convergence in case of multiple endpoints or
# effect levels. The approach has several advantages:
# - it usually requires a lot less effect() calls than alternative methods
#   which is especially useful for slow running models
# - it can achieve arbitrary precision w.r.t. to effects
# - and it automatically covers the whole range of multiplication factors if necessary
#' @global ep level mf
epx_binary_search <- function(scenario, level, effect_tolerance, factor_cutoff, verbose,
                              min_factor, max_factor, ...)
{
  # init result vector before anything else can fail
  result <- expand.grid(ep=scenario@endpoints, level=level) %>%
    dplyr::mutate(name=paste0(ep, ".EP", level)) %>%
    dplyr::pull(3)
  result <- as.list(setNames(rep(NA_real_, length(result)), result))

  tryCatch({
    # control scenarios?
    if(!has_controls(scenario))
      scenario <- cache_controls(scenario, ...)

    # rounded effects in kbs need to be more precise than the requested tolerance
    e.decimals <- max(4, ceiling(abs(log10(effect_tolerance / 10))))
    # start value of the range sweep
    f.test <- 10
    if(verbose) {
      message("epx: screening multiplication factors")
      message(paste0("  start: ", f.test))
    }
    e.test <- effect(scenario, factor=f.test, ep_only=TRUE, ...)
  }, error=function(e) {
    result$error <<- e$message
  })

  if(!exists("e.test"))
    return(result)
  if(all(is.na(e.test)))
    return(result)

  # create the knowledge base
  kbs <- as.data.frame(as.list(c(mf=f.test, e.test)))

  for(ep in scenario@endpoints)
  {
    for(lv in level)
    {
      tryCatch({
        ep_name <- paste0(ep, ".EP", lv)
        # Out Of Range error conditions
        oor_error <- FALSE

        e.tgt <- lv / 100
        # if MF smaller than requested level not found yet, test smaller powers of ten
        while(min(kbs[[ep]]) > e.tgt) {
          f.test <- min(kbs[["mf"]]) / 10
          e.test <- effect(scenario, f.test, ep_only=TRUE, ...)
          kbs <- dplyr::bind_rows(kbs, c(mf=f.test, e.test))
          if(verbose)
            message(paste0("  ", ep_name, ": ", f.test, " <<<"))
          if(f.test < min_factor) {
            result$error <- paste(result$error, "multiplication factor out of range:", ep_name, "<", min_factor)
            oor_error <- TRUE
            break
          }

        }
        # if MF larger than requested level not found yet, test larger powers of ten
        while(max(kbs[[ep]]) < e.tgt) {
          f.test <- max(kbs[["mf"]]) * 10
          e.test <- effect(scenario, f.test, ep_only=TRUE, ...)
          kbs <- dplyr::bind_rows(kbs, c(mf=f.test, e.test))
          if(verbose)
            message(paste0("  ", ep_name, ": ", f.test, " >>>"))
          if(any(f.test >= factor_cutoff, na.rm=TRUE)) break
          else if(f.test > max_factor) {
            result$error <- paste(result$error, "multiplication factor out of range:", ep_name, ">", max_factor)
            oor_error <- TRUE
            break
          }
        }
        # skip this endpoint
        if(oor_error) {
          next
        }

        # sometimes effects in kbs will not increase continuously with multiplication
        # factors due to small numerical errors. these inconsistencies are irrelevant
        # if they appear e.g. in the 10th+ decimal place. therefore, effect values
        # are rounded at a suitable decimal
        kbs[[ep]] <- round(kbs[[ep]], e.decimals)
        # sort knowledge base
        dplyr::arrange(kbs, mf) -> kbs
        # check if effects are also ordered in an ascending fashion, otherwise
        # something bad has happened
        if(!all(kbs[[ep]] == sort(kbs[[ep]]))) {
          show(kbs)
          stop("effects do not fully correlate with multiplication factors, please check numerical stability", call.=FALSE)
        }
        # find lower factor boundary
        f.min <- kbs$mf[sum(kbs[[ep]] <= e.tgt)]
        # find upper factor boundary, if possible
        f.max <- rev(kbs$mf)[sum(kbs[[ep]] > e.tgt)]
        # upper boundary was not included in kbs, can only happen if we reached
        # the cutoff criterion
        if(length(f.max) == 0) f.max <- f.min

        # starting point of binary search
        f.cur <- f.min
        e.cur <- kbs[[ep]][sum(kbs[[ep]] <= e.tgt)]

        # keep halving the search interval until tolerance level is met
        while(abs(e.cur - e.tgt) > effect_tolerance) {
          if(e.cur < e.tgt) {
            f.min <- f.cur
            f.cur <- f.min + (f.max-f.min)/2
          } else if(e.cur > e.tgt) {
            f.max <- f.cur
            f.cur <- f.max - (f.max-f.min)/2
          }
          if(any(f.cur>=factor_cutoff, na.rm=TRUE)) {
            f.cur <- factor_cutoff
            break
          }
          # If max and min factor are almost identical, then search ran into a dead
          # end. This is caused by instable numerics which results in sudden leaps
          # in apparent effects and we end up with an inconsistent KBS.
          if(f.max - f.min < 10*.Machine$double.eps*f.min) # if TRUE, then values are almost equal
            stop("binary search failed, please check numerical stability", call.=FALSE)

          if(f.cur<min_factor) {
            result$error <- paste(result$error, "multiplication factor out of range:", ep_name, "<", min_factor)
            f.cur <- NA_real_
            break
          }
          if(f.cur>max_factor) {
            result$error <- paste(result$error, "multiplication factor out of range:", ep_name, ">", max_factor)
            f.cur <- NA_real_
            break
          }

          e.cur <- effect(scenario,f.cur,ep_only=TRUE,...)
          if(any(e.cur<0 | e.cur>1 | is.nan(e.cur)))
            stop("invalid effect level occurred, check numerical stability", call.=FALSE)
          kbs <- dplyr::bind_rows(kbs, c(mf=f.cur,e.cur))
          e.cur <- e.cur[[ep]]
          if(verbose) message(paste0("  ",ep_name,": ",f.cur))
        }
        result[[ep_name]] <- f.cur
      }, error=function(e) {
        result$error <- e$message
        return(result)
      })
    }
  }
  result
}
