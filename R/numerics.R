# Check if simulation result contain an error raised by deSolve
#
# @param x return value of [simulate()]
# @return logical, `TRUE` means an error occurred during integration, else `FALSE`
num_error <- function(x) {
  #if(!is(x, "cvasi.simulate"))
  #  stop("Argument `x` must be a simulation result")
  status <- attr(x, "cvasi_status")
  if(is.null(status))
    return(FALSE)

  return(status == "error")
}

# Check if simulation was aborted - only for internal use
num_aborted <- function(x) {
  diagn <- attr(x, "desolve_diagn")
  if(!is.null(diagn))
    return(diagn$istate[[1]] == -1 | diagn$istate[[1]] == -2)

  return(FALSE)
}

# Helper method to convert a deSolve result type to a data.frame while also keeping
# deSolve's diagnostic info in place using attribute `desolve_diagn`
ode2df <- function(x) {
  df <- as.data.frame(x)
  attr(df, "desolve_diagn") <- attributes(x)
  df
}

#' Print information about numerical solver result
#'
#' `r lifecycle::badge("experimental")`
#'
#' Prints information on the status of a return value from [simulate()], e.g.
#' if it was successful and what, if any, issues occurred. Also provides tips
#' on solving frequently occurring issues.
#'
#' The function requires certain metadata which is created by [deSolve::ode()]
#' and is passed through to the result of `simulate()`. The metadata may be lost
#' if the `data.frame` returned by `simulate()` is converted or cast to other types.
#'
#' @param obj Return value of [simulate()]
#' @seealso [diagnostics()]
#' @export
#' @examples
#' # A simulation without any issues
#' minnow_it %>% simulate() %>% num_info()
#'
#' # A simulation which terminated early due to the solver
#' # taking too many numerical steps
#' rs <- suppressWarnings(minnow_it %>% simulate(hmax=1e-80))
#' num_info(rs)
#'
#' # Print deSolve diagnostics for additional information
#' diagnostics(rs)
num_info <- function(obj) {
  UseMethod("num_info")
}

#' @rdname num_info
#' @export
num_info.default <- function(obj) {
  stop("Type is not supported to provide numerical information.")
}

#' @rdname num_info
#' @export
num_info.cvasi_simulate <- function(obj) {
  diag <- attr(obj, "desolve_diagn")
  status <- attr(obj, "cvasi_status")

  if(status == "success") {
    cli::cli_text("Simulation status: ", cli::col_green(toupper(status)))
  } else {
    cli::cli_text("Simulation status: ", cli::col_red(toupper(status)))
  }

  # TODO do state variables contain invalid values?

  if(is.null(diag)) {
    cli::cli_text(cli::col_grey("Object does not contain solver diagnostics info."))
  }
  else
  {
    # return code
    rc <- diag$istate[[1]]

    # get issue description
    if(rc < 0) {
      tx <- tryCatch(capture.output(diagnostics.cvasi_simulate(obj)), error=function(e) "")
      idesc <- trimws(tx[which(startsWith(tx, "  return code")) + 1])
    }

    # maxsteps exceeded
    if(rc == -1)
    {
      steps <- diag$istate[[2]]

      cli::cli_h1("Numerical Steps")
      l1 <- cli::cli_ul()
      cli::cli_li(idesc)
      cli::cli_li("Simulation terminated early at t={.val {signif(diag$rstate[[1]], digits=3)}}")
      def <- ifelse(steps == 5000, " (default value)", "")
      cli::cli_li("Argument {.field maxsteps}={.val {steps}}{def}")

      cli::cli_text(" ")
      cli::cli_text(cli::col_blue("Potential Solutions"))
      cli::cli_li("Increase the number of allowed steps with argument {.field maxsteps}")
      cli::cli_li("Increase the value of arguments {.field {c('hmax','atol','rtol')}}")
      cli::cli_li("Try a different ODE solver with argument {.field method}, such as {.val {c('lsoda','lsode','ode45','rk4')}}")
      cli::cli_end(l1)
    }

    # excess accuracy requested
    if(rc == -2)
    {
      steps <- diag$istate[[2]]

      cli::cli_h1("Numerical Precision")
      l1 <- cli::cli_ul()
      cli::cli_li(idesc)
      cli::cli_li("Simulation terminated early at t={.val {signif(diag$rstate[[1]], digits=3)}}")

      cli::cli_text(" ")
      cli::cli_text(cli::col_blue("Potential Solutions"))
      cli::cli_li("Increase the value of arguments {.field {c('hmax','atol','rtol')}}")
      cli::cli_end(l1)
    }
  }

  output <- attr(obj, "desolve_output")
  if(!is.null(output))
  {
    if(length(output) > 0) {
      output <- stringr::str_trim(output)
      output <- output[output != ""]
      cli::cli_h1("deSolve Output")
      l3 <- cli::cli_ol()
      cli::cli_li(c(output))
      cli::cli_end(l3)
    }
  }

  conds <- attr(obj, "desolve_conds")
  if(!is.null(conds))
  {
    if(length(conds) > 0)
    {
      cli::cli_h1("deSolve Messages")
      l4 <- cli::cli_ol()
      for(m in conds)
      {
        if(m[[1]] == "warning")
          cli::cli_li(cli::cli_text(cli::col_red("WARNING:"), " ", stringr::str_to_sentence(m[[3]])))
        else
          cli::cli_li(cli::cli_text(cli::bg_red(cli::col_white("ERROR:")), " ", stringr::str_to_sentence(m[[3]])))
      }
      cli::cli_end(l4)
    }
  }
}

#' @rdname num_info
#' @export
num_info.cvasi_fit <- function(obj) {
  num_info.cvasi_simulate(obj)
}

#' @importFrom deSolve diagnostics diagnostics.default diagnostics.deSolve
#' @export
NULL

#' Diagnostics of solvers
#'
#' Prints several diagnostics of the simulation to the console, e.g. number of
#' steps taken, the last step size, etc. The information is provided by
#' [deSolve::diagnostics()].
#'
#' @seealso [deSolve::diagnostics()]
#' @param obj return value of a simulation
#' @param ... unused parameters
#' @rdname diagnostics
#' @export
diagnostics <- function(obj, ...) {
  UseMethod("diagnostics")
}

#' @rdname diagnostics
#' @export
diagnostics.default <- function(obj, ...) {
  deSolve::diagnostics.deSolve(obj)
}

#' @rdname diagnostics
#' @export
diagnostics.cvasi_simulate <- function(obj, ...) {
  # translate our metadata to something that is compatible with deSolve's format
  diag <- attr(obj, "desolve_diagn")
  if(is.null(diag))
    stop("Object does not contain solver diagnostics info.")
  attr(obj, "istate") <- diag$istate
  attr(obj, "rstate") <- diag$rstate
  attr(obj, "type") <- diag$type
  class(obj) <- "deSolve"
  deSolve::diagnostics.deSolve(obj)
}
