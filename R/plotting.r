
#' Creates plot of model results (uncertainties optional)
#'
#' All parameter combinations and exposure patterns are simulated and the mean
#' of predictions is derived for a single study. The uncertainty is
#' passed to the function due to computation time. Results are displayed by
#' plotting the time series including the uncertainty interval. Observation data
#' can be optionally displayed. Data should be provided in long format. Function
#' plots the time (column 1) and the predictions (column 2, can be changed by
#' the user plot_col)
#'
#' @param model_base effect scenario object with mean parameters
#' @param treatments treatments exposure levels as data frame
#' @param rs_mean `data.frame`, model results best fit params
#' @param rs_range `data.frame`, uncertainties as data frame
#' @param obs_mean `data.frame`, observation data with means per treatment level
#' @param obs_full `data.frame`, full set including results for replicates
#' @param x_breaks optional vector of breaks of x-axis
#' @param y_lim optional vector containing limits of y-axis
#' @param grid_labels optional labels of grid headers
#' @param grid_ncol optional number of grid columns
#' @param plot_col output column which should be plotted
#' @param y_title optional title of y-axis
#' @param ... any additional parameters
#' @return a ggplot2 plot object
#' @global trial
#' @export
#'
#' @examples
#' set.seed(124)
#' exposure <- data.frame(
#'   time = 0:21,
#'   conc = rnorm(n=22, mean=0.1, sd=0.06),
#'   trial = "T1")
#' forcings <- list(temp=12, rad=15000)
#' param <- list(EC50=0.3, b=4.16, P_up=0.0054)
#' inits <- list(BM=0.0012, E=1, M_int=0)
#'
#' scenario <- Lemna_Schmitt() %>%
#'   set_forcings(forcings) %>%
#'   set_param(param) %>%
#'   set_init(inits)
#'
#' sim_result <- simulate_batch(model_base = scenario,
#'                              treatments = exposure,
#'                              param_sample = NULL)
#'
#' plot_sd(model_base = scenario,
#'         treatments = exposure,
#'         rs_mean = sim_result)
plot_sd <- function(model_base,
                    treatments,
                    rs_mean,
                    rs_range = NULL,
                    obs_mean = NULL,
                    obs_full = NULL,
                    x_breaks = NULL,
                    y_lim = NULL,
                    grid_labels = NULL,
                    grid_ncol = 2,
                    plot_col = 2,
                    y_title = NULL,...) {
  # check format
  if (!is.data.frame(treatments) && !is.null(treatments)) {
    stop("treatments not a data.frame")
  }
  if (!is.data.frame(rs_range) && !is.null(rs_range)) {
    stop("rs_range not a data.frame")
  }
  if (!is.data.frame(obs_mean) && !is.null(obs_mean)) {
    stop("obs_mean not a data.frame")
  }
  if (!is.data.frame(obs_full) && !is.null(obs_full)) {
    stop("obs_full not a data.frame")
  }

  no_trials <- length(unique(rs_mean$trial))

  # ToDo check class calibration set / set more than one exposure in an effect
  # scenario object as an alternative
  # ToDo check style guide
  # ToDo combine treatments and data (obs_mean)
  # ToDo how to handle replicates? Plot replicates? Plot uncertainties?

  exp_plot <- treatments # long format
  exp_plot$trial <- as.factor(exp_plot$trial)

  # try to guess the simulated time period
  t_max <- max(treatments[, 1]) # 1st column should contain time
  # check if replicates are present if obs_full is not null, otherwise there is
  # something funny going on with the column names. was data imported with
  # read.csv(..., check.names=FALSE) ?? read.csv should not be part of the plot
  # function. Transformation should be done before plotting? Since dplyr/tidy
  # is used, we should use always long format?
  if (!is.null(obs_full)) {
    if (length(unique(names(obs_full))) == length(names(obs_full))) {
      warning("no replicates found: check col names
              in full experimental dataset")
    }
  }

  # try to find the max number of data for setting y-axis
  obs_max <- rs_mean %>%
    dplyr::select({{plot_col}}) %>%
    max()

  if (!is.null(obs_full)) {
    obs_plot <- obs_full
    obs_max <- max(obs_max, obs_plot[,2]) #2 = data, observations
    obs_plot$trial <- as.factor(obs_plot$trial) #trial
  } else if (!is.null(obs_mean)) {
    obs_plot <- obs_mean
    obs_max <- max(obs_max, obs_plot[,2])
    obs_plot$trial <- as.factor(obs_plot$trial) #trial
  } else {
    obs_plot <- NULL
  }

  obs_max <- obs_max * 1.10 # 10 per cent for the symbols

  # determine max of x-axis and scale the x value
  exp_max <- max(exp_plot[,2]) #?? col instead of name?
  if (obs_max > exp_max * 10 | exp_max > obs_max) {
    exp_plot[,2] <- exp_plot[,2] / exp_max * obs_max
  }

  # color scheme
  col_palette <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7"
  )
  if (length(obs_mean) - 1 > length(col_palette)) {
    stop("color palette has too few elements, please amend")
  }
  # random colors instead?
  # set default breaks for x axis
  if (is.null(x_breaks)) {
    x_breaks <- grDevices::axisTicks(usr = range(treatments[, 1]),
                                     log = FALSE,
                                     axp = NULL,
                                     nint = 5)
  }
  # set default y axis limits
  if (is.null(y_lim)) {
    y_lim <- c(0, obs_max)
  }
  # set default labels
  if (is.null(grid_labels)) {
    # grid_labels is an optional function parameter
    grid_labels <- unique(exp_plot$trial)
    # create labeller for grid headers
    names(grid_labels) <- unique(exp_plot$trial)
  }
  f_lb <- ggplot2::labeller(trial = grid_labels)

  # set y title
  if (is.null(y_title)) {
    y_title <- names(rs_mean)[plot_col]
  }

  # create plot
  # one plot
  if(no_trials == 1){ #ToDo? Exposure with second y-axis
    plot <- ggplot2::ggplot() +
      # best fit
      ggplot2::geom_line(data = rs_mean, ggplot2::aes(x = rs_mean[,1], #time
                                                      y = rs_mean[, plot_col], #output selected by user
                                                      color = "black"),
                         #show.legend = TRUE
      ) +
      # exposure profiles
      ggplot2::geom_area(data = exp_plot, ggplot2::aes(x = exp_plot[,1], #time
                                                       y = exp_plot[,2], #conc
                                                       fill = "grey"),
                         alpha = 0.12,
                         position = "identity",
                         #show.legend = TRUE
      )

    # colors
    plot <- plot +
      ggplot2::scale_color_manual(values = "black", name = "Prediction") +
      ggplot2::scale_fill_manual(values = "grey", name = "Exposure") +
      ggplot2::scale_x_continuous(name = "Time (days)", breaks = x_breaks) +
      ggplot2::scale_y_continuous(name = y_title) +
      #ggplot2::scale_y_continuous(sec.axis = sec_axis(~./1,name = "Exposure")) +
      # (name of column)
      ggplot2::coord_cartesian(xlim = c(0, t_max), ylim = y_lim) + # boundaries
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")

  } else {
    plot <- ggplot2::ggplot() +

      # best fit
      ggplot2::geom_line(data = rs_mean, ggplot2::aes(x = rs_mean[,1], #time
                                                      y = rs_mean[, plot_col], #output selected by user
                                                      color = trial)
      ) +
      # exposure profiles
      ggplot2::geom_area(data = exp_plot, ggplot2::aes(x = exp_plot[,1], #time
                                                       y = exp_plot[,2], #conc
                                                       fill = "black"),
                         alpha = 0.12,
                         position = "identity"
      ) +
      # Facet by trial
      ggplot2::facet_wrap(~ trial, ncol = grid_ncol, labeller = f_lb)

    # colors
    plot <- plot +
      ggplot2::scale_color_manual(values = col_palette) +
      ggplot2::scale_fill_manual(values = col_palette) +
      ggplot2::scale_x_continuous(name = "Time (days)", breaks = x_breaks) +
      ggplot2::scale_y_continuous(name = y_title) +
      # (name of column)
      ggplot2::coord_cartesian(xlim = c(0, t_max), ylim = y_lim) + # boundaries
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")

    if (!is.null(obs_plot)) {
      plot <- plot + # observations, data
        ggplot2::geom_point(data = obs_plot, ggplot2::aes(x = obs_plot[,1], #time
                                                          y = obs_plot[,2], #observations
                                                          bg = trial),
                            size = 3, shape = 22
        ) +
        # Facet by trial
        ggplot2::facet_wrap(~ trial, ncol = grid_ncol, labeller = f_lb)
    }

    if (!is.null(rs_range)) {
      plot <- plot + # uncertainties, data
        ggplot2::geom_ribbon(data = rs_range, ggplot2::aes(x = rs_range[,1], #time
                                                           ymin = rs_range[,2], #min
                                                           ymax = rs_range[,3], #max
                                                           fill = trial),
                             alpha = 0.35
        ) +
        # Facet by trial
        ggplot2::facet_wrap(~ trial, ncol = grid_ncol, labeller = f_lb)
    }
  }
  print(plot)
}


#' Creates a PPC plot for a single dataset
#'
#' A sample of parameters representing the uncertainty within the dataset
#' is passed to the function. All parameter combinations and exposure patterns
#' are simulated and the range of predicted frond numbers is derived for a
#' single study. The uncertainty is displayed by a Posterior Predicitve Plot
#' (PPC).
#'
#' @param rs_mean `data.frame`, model results best fit params
#' @param rs_range `data.frame`, predictions (min, max from param.sample run)
#' @param obs_mean `data.frame`, observations with means per treatment level
#' @param obs_full `data.frame`, full data set including results for replicates
#' @param xy_lim optional `numeric`, limits of x and y axis for plotting
#' @param study optional `string`, name of study which can be used as key
#' @param outfile optional `string`, file path is given = save graph
#' @param ... any additional parameters
#' @export
#' @global time trial
#'
#' @return `data.frame` with fitted and observed frond numbers
plot_ppc <- function(rs_mean,
                     rs_range,
                     obs_mean = NULL,
                     obs_full = NULL,
                     xy_lim = NULL,
                     study = NULL,
                     outfile = NULL,
                     ...) {
  if (!is.data.frame(obs_mean) && !is.null(obs_mean))
    stop("obs_mean not a data.frame")
  if (!is.data.frame(obs_full) && !is.null(obs_full))
    stop("obs_full not a data.frame")

  # try to find the maximumn value of observations
  if (is.null(xy_lim)) {
    if (!is.null(obs_full)) {
      xy_lim <- max(obs_full$data)
    } else {
      xy_lim <- max(obs_mean$data)
    }
  }

  # check if replicates are present, otherwise there is something funny going
  # on with the column names. was data imported with
  # read.csv(..., check.names=FALSE) ??
  if (!is.null(obs_full)) {
    if (length(unique(names(obs_full))) == length(names(obs_full))) {
      warning("no replicates found: check col names
              in full experimental dataset")
    }
  }
  # add experimental data to table
  if (!is.null(obs_full)) {
    obs_plot <- obs_full
    obs_max <- max(obs_plot$data)
  } else if (!is.null(obs_mean)) {
    obs_plot <- obs_mean
    obs_max <- max(obs_plot$data)
  } else {
    obs_plot <- stop("No observation data are available!")
  }
  #prepare data frame for plot_ppc (pred, obs, min, max)
  rs_ppc <- data.frame(time = rs_mean$time, trial = rs_mean$trial)
  rs_ppc$pred <- rs_mean[, (ncol(rs_mean) - 1)]
  rs_ppc$obs <- obs_plot$data
  rs_ppc$min <- rs_range$min
  rs_ppc$max <- rs_range$max

  # remove start conditions and control data from dataset (#OK for GUTS, Lemna
  # what about DEB?)
  rs_ppc <- dplyr::filter(rs_ppc,
                          time > 0 & trial != unique(rs_mean$trial)[1])

  # create plot
  plot_ppc_combi(rs_ppc, xy_lim = xy_lim)
  if (!is.null(outfile)) ggplot2::ggsave(outfile, width = 8, height = 6)

  # set study key if requested
  if (!is.null(study))
    rs_ppc$study <- study #necessary when more than 1 study is available
  # return fitted and observed data
  invisible(rs_ppc)
}


#' Create PPC plot for one or more datasets
#'
#' The function expects a data.frame with four mandatory and one optional
#' column. The mandatory columns are as follows:
#' - `pred`: mean of predictions e.g. frond number for lemna
#' - `max`: maximum of predictions
#' - `min`: minimum of predictions
#' - `obs`: observations
#' The optional column is to be named `study` and contains a study identifier.
#' If more than one study identifier is present in the table, individual
#' studies will be plotted in different colors and a legend will be displayed.
#'
#' @param table `data.frame` containing return values of calls to `plot_ppc()`
#' @param xy_lim optional `numeric`, limits of x and y axis for plotting
#' @global obs min max pred study
plot_ppc_combi <- function(table, xy_lim = NULL) {
  if (!is.data.frame(table)) stop("table not a data.frame")

  if (!("obs" %in% names(table)
        && "pred" %in% names(table)
        && "min" %in% names(table)
        && "max" %in% names(table)
        )) {
    stop("Required variables (obs, pred, min, max) are missing in the dataset.")
  }

  # try to find the max number in experiments
  if (is.null(xy_lim))
    xy_lim <- max(table[, c("pred", "obs")])

  if (!("study" %in% names(table)))
    table$study <- "n/a"
  n_studies <- length(unique(table$study))

  # calculate PPC
  n <- nrow(table)
  n_fit <- nrow(dplyr::filter(table, obs <= max & obs >= min))
  ppc <- n_fit / n * 100

  # calculate NRMSE
  nrmse <- 1 / mean(table$obs) * sqrt(1 / n * sum((table$obs - table$pred)^2))

  # color scheme
  col_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                   "#0072B2", "#D55E00", "#CC79A7")
  if (n_studies == 1)
    col_palette <- c("black")
  if (n_studies > length(col_palette))
    stop("color palette has too few elements, please amend")

  # create plot
  title <- paste0("NRMSE: ",
                  round(nrmse * 100, 1),
                  "% & PPC: ",
                  round(ppc, 1),
                  "%")
  plot <- ggplot2::ggplot() +
    ggplot2::geom_area(ggplot2::aes(x = c(0, xy_lim), y = c(0, xy_lim)),
      fill = "green",
      alpha = 0.1,
      position = "identity"
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(x = c(0, xy_lim), ymin = c(0, xy_lim),ymax = c(xy_lim, xy_lim)
      ),
      fill = "red", alpha = 0.1
    ) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(obs, pred, color = study),
      data = table
    ) +
    ggplot2::geom_linerange(ggplot2::aes(obs,
                                         ymin = min,
                                         ymax = max,
                                         color = study),
      alpha = 0.7,
      data = table
    ) +
    ggplot2::coord_cartesian(xlim = c(0, xy_lim), ylim = c(0, xy_lim)) +
    ggplot2::scale_color_manual(values = col_palette, name = "Study") +
    ggplot2::labs(x = "Observed", y = "Predicted", title = title) +
    ggplot2::theme_bw()

  if (n_studies == 1) # hide legend if only one study supplied
    plot <- plot + ggplot2::theme(legend.position = "none")

  print(plot)
}

#' Plot EPx values
#'
#' @param EPx_ts the result of `epx_mtw`, ie. a tibble with window.start,
#' window.end, endpoint, level and EPx
#' @param exposure_ts an exposure time series with columns for time 't' and
#' concentration 'conc'
#' @param draw Should the whole plot be drawn? If FALSE the exposure plot and
#' the EPx plot are returned as a list for later modification
#' @param time_col the name of the time column in the exposure dataset
#' @param conc_col the name of the concentration column in the exposure dataset
#' @param epx_x_title title of the x-axis of the epx panel
#' @param conc_y_title title of the y-axis of the concentration panel
#'
#' @return a grid of ggplots
#' @export
#' @global EPx
#'
#' @examples
#' ti <- 0:21
#' expo <- abs(0.01*ti + rnorm(length(ti), 0, 0.05))
#' exposure <- data.frame(time = ti, conc = expo)
#' metsulfuron_epx_mtw <- metsulfuron %>%
#' set_exposure(exposure) %>%
#' epx_mtw(level = 10, factor_cutoff = 1000)
#' metsulfuron_epx_mtw
#' plot_epx(EPx_ts = metsulfuron_epx_mtw,
#' exposure_ts = exposure, conc_y_title = "env. concentration [µg/L]")
plot_epx <- function(EPx_ts, exposure_ts, draw = TRUE, time_col = "time", conc_col = "conc",
                     epx_x_title = "Start time", conc_y_title = "Exposure conc.") {

  plot_dat <- EPx_ts %>%
    dplyr::select(!dplyr::matches("window.end", "level"))

  # EPx plot ----
  ep_level <- unique(EPx_ts[["level"]])
  if (length(ep_level) > 1) {
    stop("only single EPx level accepted for plotting")
  }
  epx_y_title <- parse(text = paste0('EP[', ep_level, ']'))
  epx_plot_x_lim <- EPx_ts %>%
    dplyr::select(window.start, window.end) %>%
    range()
  epx_plot <- ggplot2::ggplot(plot_dat) +
    ggplot2::geom_line(ggplot2::aes(window.start, EPx),
                       color = "orange",
                       linewidth = 1.25) +
    ggplot2::ylab(epx_y_title) +
    ggplot2::xlab(epx_x_title) +
    ggplot2::ylim(0, NA) +
    ggplot2::xlim(epx_plot_x_lim) +
    ggplot2::facet_wrap(endpoint ~ ., ncol = 1, scales="free_y")

  # Concentration plot ----
  conc_plot <- ggplot2::ggplot(exposure_ts) +
    ggplot2::geom_area(ggplot2::aes(.data[[time_col]], .data[[conc_col]]),
                       alpha = 0.5,
                       fill = "blue",
                       position = "identity") +
    ggplot2::xlim(epx_plot_x_lim) +
    ggplot2::ylab(conc_y_title) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())

  # Bind and align the two plots ----
  conc_plot_ <- ggplot2::ggplotGrob(conc_plot)
  epx_plot_ <- ggplot2::ggplotGrob(epx_plot)
  plots <- rbind(conc_plot_, epx_plot_, size = "last")
  plots$widths <- grid::unit.pmax(conc_plot_$widths, epx_plot_$widths)

  if (draw) {
    grid::grid.newpage()
    grid::grid.draw(plots)
  } else {
    out <- list(conc_plot, epx_plot)
  }

}

#' Creates a prediction plot for one effect scenario
#'
#' Sometimes it is helpful if the user can plot results of one effect
#' scenario. This is for instance the case for test simulations or predictions
#' for one profile. This function runs the simulation for one effect scenario
#' and plots the results. Function plots the time (column 1) and the predictions
#' (column 2, can be changed by the user plot_col)
#'
#' @param model_base effect scenario object with mean parameters
#' @param plot_col output column which should be plotted, default = 2
#' @param trial_number name for model run (if available tag is used)
#' @return plot of the results for one `effect scenario`
#' @export
#'
#' @examples
#' plot_scenario(metsulfuron)
plot_scenario <- function(model_base, plot_col = 2, trial_number = NULL) {

  # set name of run
  if (is.null(trial_number)) {
    if (is.na(model_base@tag)) {
      trial_number <- "T1"
    } else {
      trial_number <- model_base@tag
    }
  }

  # run model
  sim_result <- model_base %>% simulate()
  sim_result$trial <- trial_number

  # prepare exposure data
  exposure <- model_base@exposure@series
  exposure$trial <- trial_number

  # plot results
  plot_sd(model_base = model_base,
    treatments = exposure,
    rs_mean = sim_result,
    obs_mean = NULL,
    plot_col = plot_col
  )

}
