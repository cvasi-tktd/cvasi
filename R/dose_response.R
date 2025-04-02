#' Calculate a dose response curve
#'
#' Returns a `data.frame` with points on the dose response curve for the given
#' effect scenario.
#'
#' Derives a dose response curve from a [scenario]. The result will
#' cover the requested range of effect levels. The tested multiplication factors
#' can be chosen by different strategies, i.e. a `vanilla` approach using a
#' fixed set of factors, or `decadic` and `exponential` approaches
#' employing logarithmic and exponential factor scaling, respectively.
#'
#' @param scenario used for calculation
#' @param range numeric vector specifying the required range of effect levels in
#'   percent (%), defaults to `c(1,99)`
#' @param n minimum number of points on the dose response curve
#' @param strategy controls how multiplication factors are chosen, `vanilla` uses a fixed
#' set of multiplication factors, `decadic` and `exponential` have varying step lengths.
#' @param verbose  logical, set to `TRUE` for additional status messages
#' @param ... additional arguments passed on to [effect()]
#'
#' @return `data.frame` with two columns, i.e. `mf` and `effect`
#' @export
#' @global endpoint
#'
#' @examples
#' # basic dose response curve
#' minnow_sd %>% dose_response()
#'
#' # modify the minimum number of points on the curve
#' minnow_sd %>% dose_response(n=10)
#'
#' # select a subset of the effect range
#' minnow_sd %>% dose_response(range=c(10,20))
#'
#' # use an alternative strategy for the selection of multiplication factors
#' minnow_sd %>% dose_response(strategy="decadic")
#'
#' # provide additional output how multiplication factors were selected
#' minnow_sd %>% dose_response(verbose=TRUE)
dose_response <- function(scenario, range=c(1, 99), n=20, strategy=c("exponential", "decadic", "vanilla"),
                          verbose=FALSE, ...)
{
  if(length(scenario) > 1 | is.data.frame(scenario))
    stop("multiple scenarios supplied")
  if(is.vector(scenario))
    scenario <- scenario[[1]]

  strategy <- match.arg(strategy)
  if(length(range) == 0)
    stop("effect range required")
  else if(length(unique(range)) == 1) {
    min.effect <- max(1, range[1] - 1)
    max.effect <- min(99, range[1] + 1)
  } else {
    min.effect <- max(min(range) - 1, 1)
    max.effect <- min(max(range) + 1, 99)
  }

  # sequential processing requested?
  if(getOption("cvasi.pll.off", FALSE))
    map2 <- purrr::map2_dfr
  else
    map2 <- furrr::future_map2_dfr
  # check if controls are present
  if(!has_controls(scenario))
    scenario <- cache_controls(scenario, ...)

  epx.min <- epx(scenario, level=min.effect, ...) %>%
    dplyr::rename_with(~gsub("\\.EP\\d+", "", .x))
  epx.max <- epx(scenario, level=max.effect, ...) %>%
    dplyr::rename_with(~gsub("\\.EP\\d+", "", .x))
  drc <- data.frame(endpoint=character(), mf=numeric(), effect=numeric())

  for(ep in scenario@endpoints)
  {
    f.min <- epx.min[[1, ep]]
    f.max <- epx.max[[1, ep]]
    if(is.na(f.min) | is.na(f.max)) {
      warning("Cannot determine EPx range, skipping endpoint '", ep, "'", call.=FALSE)
      next
    }

    # Decadic strategy has a constant step length in decimal multiples of powers of ten
    if(strategy=="decadic")
    {
      dec.step <- 2
      do.repeat <- T
      while(do.repeat)
      {
        dec.step <- dec.step / 2
        if(verbose)
          message("  decadic step length ", dec.step)
        if(dec.step < 1e-30)
          stop("Refinement failed, step length too small")
        do.repeat <- F
        # list of factors to calculate effects for
        mf <- c()
        for(i in seq(log10(f.min), 3))
          mf <- c(mf, seq(1, 9, dec.step) * 10^i)
        # remove factors larger than upper boundary
        if(f.max > 0)
          mf <- mf[mf <= f.max]
        # more factors requested than present?
        do.repeat <- length(mf) < n
      }
    }
    # Exponential strategy has a constant step width in the powers of ten
    else if(strategy=="exponential")
    {
      pow.step <- 0.2
      do.repeat <- T
      while(do.repeat)
      {
        pow.step <- pow.step / 2
        if(verbose)
          message(paste("  exponential step length", pow.step))
        if(pow.step < 1e-30)
          stop("Refinement failed, step length too small")
        do.repeat <- F
        # list of factors to calculate effects for
        mf <- exp(log(10) * seq(log(f.min) / log(10), 10, pow.step))
        # remove factors out of target range
        mf <- mf[mf >= f.min]
        if(f.max > 0)
          mf <- mf[mf<=f.max]
        # more factors requested than present?
        do.repeat <- length(mf)<n
      }
    }
    # Vanilla approach as implemented in an older risk assessment
    else if(strategy=="vanilla") {
      mf <- c(1:99,	1:9 * 100, 2:10 * 500, 6:10 * 1000)
      # remove factors out of target range
      mf <- mf[mf >= f.min]
      if(f.max > 0)
        mf <- mf[mf <= f.max]
      if(length(mf) < n)
        stop("Refinement not supported for vanilla strategy")
    }

    if(verbose)
      message("  calculating ", length(mf), " new effect levels")
    # calculate effects
    efx <- map2(rep(c(scenario), length(mf)), mf, effect, ep_only=TRUE, ...)
    dplyr::bind_rows(drc, tibble::tibble(mf=mf, effect=efx[[ep]], endpoint=ep)) -> drc
  }
  drc <- drc %>%
    dplyr::arrange(endpoint, mf) %>%
    as.data.frame()
  class(drc) <- c("cvasi_drc", class(drc))
  attr(drc, "scenario") <- scenario

  drc
}
