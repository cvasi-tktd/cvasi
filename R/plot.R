#' S3 plotting functions
#'
#' These functions overload [base::plot()] to provide simple plotting
#' routines to display various time-series and scenario objects.
#'
#' @name plot
#' @param x object to plot
#' @param y unused parameter
#' @param ... unused parameters
#' @return *ggplot2* plot object
NULL

# S3 overload to plot dose response curves
#
#' @param scale_x character, controls how the x-axis is scaled. `log10` for
#'   a log10-scaled axis, `none` for no scaling, and `auto` for automatic selection
#' @describeIn plot Plot dose response curves
#' @export
plot.cvasi_drc <- function(x, y, scale_x=c("auto", "log10", "none"), ...) {
  if(!missing(y))
    warning("Parameter `y` is unused, ignoring argument")
  if(!missing(...))
    warning("Parameter `...` is unused, ignoring additional arguments")

  scale_x <- match.arg(scale_x)
  if(scale_x == "auto") {
    exp_min <- floor(log10(min(x$mf)))
    exp_max <- ceiling(log10(max(x$mf)))
    if(is.na(exp_min) | is.na(exp_max))
      scale_x <- "none"
    else if(abs(exp_max - exp_min) > 2)
      scale_x <- "log10"
    else
      scale_x <- "none"
  }

  plot <- ggplot2::ggplot(x) +
    ggplot2::geom_line(ggplot2::aes(mf, effect, color=endpoint))
  if(scale_x == "log10") {
    plot <- plot + ggplot2::scale_x_log10()
  }

  # create a nice plot title
  subtitle <- ggplot2::element_blank()
  sc <- attr(x, "scenario")
  if(!is.null(sc)) {
    nm <- get_model_name(sc)
    tg <- get_tag(sc)
    if(is.na(tg))
      subtitle <- nm
    else
      subtitle <- paste0(nm, " #", tg)
  }

  plot +
    ggplot2::scale_color_discrete(name="Endpoint") +
    ggplot2::labs(title="Dose response curve",
                  subtitle=subtitle,
                  x=ifelse(scale_x == "log10", "Multiplication factor [log-scale] (-)", "Multiplication factor (-)"),
                  y="Effect (-)") +
    ggplot2::theme_bw()
}

#' @describeIn plot Plot return value of [simulate()]
#' @export
plot.cvasi_simulate <- function(x, y, ...) {
  if(!missing(y))
    warning("Parameter `y` is unsused, ignoring argument")
  if(!missing(...))
    warning("Parameter `...` is unused, ignoring additional arguments")

  # pivot table in wide-format to long-format. we have to assume that the
  # first column represents time. the remaining numerical columns will be
  # plotted.
  tidyr::pivot_longer(x, cols=!c(1) & dplyr::where(is.numeric)) %>%
    dplyr::rename(time=1) %>%                                  # make sure we have a well defined name
    dplyr::mutate(name=factor(name, levels=unique(name))) %>%  # enforce ordering of state variables in plot
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(time, value, color=name)) +
    ggplot2::facet_wrap(~ name, scales="free") +
    ggplot2::labs(title="Simulation result", x="Time", y="Value (?)") +
    ggplot2::guides(color="none") +
    ggplot2::theme_bw()
}
