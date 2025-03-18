# Check if simulation result contain an error raised by deSolve
#
# @param x return value of [simulate()]
# @return logical, `TRUE` means an error occurred during integration, else `FALSE`
num_error <- function(x) {
  #if(!is(x, "cvasi.simulate"))
  #  cli::cli_abort("argument {.field x} must be a simulation result")
  msgs <- attr(x, "desolve_msgs")
  if(is.null(msgs))
    return(FALSE)

  return(any(sapply(msgs, function(lst) lst[[1]] == "error")))
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

#' @export
num_info <- function(obj) {
  UseMethod("num_info")
}

#' @export
num_info.default <- function(obj) {
  cli::cli_abort("Type is not supported to provide numerical information.")
}

#' @export
num_info.cvasi.simulate <- function(obj) {
  diag <- attr(obj, "desolve_diagn")
  status <- attr(obj, "cvasi_status")

  if(status == "success") {
    cli::cli_text("Simulation status: ", cli::col_green(toupper(status)))
  } else {
    cli::cli_text("Simulation status: ", cli::col_red(toupper(status)))
  }

  # TODO do state variables contain invalid values?
  # TODO if rc<0, get error message from deSolve::diagnostics()

  if(is.null(diag)) {
    cli::cli_text(cli::col_grey("Object does not contain solver diagnostics info."))
  }
  else
  {
    # return code
    rc <- diag$istate[[1]]

    # get issue description
    if(rc < 0) {
      tx <- capture.output(diagnostics.cvasi.simulate(obj))
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

#' @export
num_info.cvasi_error <- function(obj) {
  attr(obj, "cvasi_status") <- obj$cvasi_status
  attr(obj, "desolve_output") <- obj$desolve_output
  attr(obj, "desolve_conds") <- obj$desolve_conds
  num_info.cvasi.simulate(obj)
}

#' @importFrom deSolve diagnostics diagnostics.default diagnostics.deSolve
#' @export
NULL

#' @inherit deSolve::diagnostics
#' @rdname diagnostics
#' @export
diagnostics.cvasi.simulate <- function(obj, ...) {
  # translate our metadata to something that is compatible with deSolve's format
  diag <- attr(obj, "desolve_diagn")
  if(is.null(diag))
    stop("Object does not contain solver diagnostics info.")

  attr(obj, "istate") <- diag$istate
  attr(obj, "rstate") <- diag$rstate
  attr(obj, "type") <- diag$type
  deSolve::diagnostics.deSolve(obj)
}
