#' Likelihood profiling
#'
#' @description The aim of the function is 2-fold: 1) estimate a 95% confidence
#' around each parameter of a calibrated model, and 2) see if perhaps a local minimum was found rather
#' than a global minimum. To achieve this, the likelihood profiling goes through
#' every parameter one by one (using [profile_par())]. For each parameter,
#' the model is sequentially refit with the parameter value set to
#' increasingly lower and higher values, and the likelihood of the model given the
#' data calculated (using [log_lik()]). The likelihood is then compared
#' to the likelihood of the original model (using a likelihood ratio). This leads
#' to the development of a likelihood profile, from which a plot a 95%
#' confidence interval for the parameter is derived.
#'
#' The idea of the function is a variable stepwise algorithm:
#' When the likelihood ratio changes very little (less than `l_crit_min`), the stepsize is
#' increased (up to a maximum, specified by `f_step_max`). When the lik.
#' ratio changes too much (more than `l_crit_max`), the algorithm tries again
#' with a smaller stepsize (also bound to a minimum: `f_step_min`). Note that
#' the stepsize is used as a fraction of the parameter value that is tried.
#' To prevent very small stepsizes when the value goes towards zero (as can
#' be the case for effect thresholds), an absolute minimum
#' stepsize (`f_step_abs`), which is specified as a fraction of the best
#' parameter value (`Xhat`) (unless it is zero, then algoritm takes
#' something small).
#'
#' The function was inspired by a MatLab BYOM v.6.8 procedure, created by
#' Tjalling Jager. For details, please refer to BYOM (http://debtox.info/byom.html)
#' as well as Jager (2021).
#'
#' @references
#' Jager T, 2021. Robust Likelihood-Based Optimization and Uncertainty Analysis
#' of Toxicokinetic-Toxicodynamic Models. Integrated Environmental Assessment and
#' Management 17:388-397. \doi{10.1002/ieam.4333}
#'
#' @param x either a single [scenario] or a list of [CalibrationSet] objects
#' @param par named vector - parameters (names and values) to be profiled
#' @param output character vector, name of output column of [simulate()] that
#'  is used in calibration
#' @param data only needed if `x` is an [scenario]
#' @param bounds optional list of lists (including lower and upper bound): uses defaults in `x` object, but
#'  can be overwritten here (e.g. bounds <- list(k_resp = list(0,10), k_phot_max = list(0,30)) )
#' @param refit if 'TRUE' (default), refit if a better minimum is found
#' @param type "fine" or "coarse" (default) likelihood profiling
#' @param break_prof if 'TRUE' (default), stop the profiling if a better optimum is located
#' @param ... additional parameters passed on to [stats::optim()] and [calibrate()]
#'
#' @examples
#' # Example with Lemna model - physiological params
#' library(dplyr)
#'
#' # exposure - control run
#' exp <- Schmitt2013 %>%
#'   filter(ID == "T0") %>%
#'   select(time=t, conc)
#'
#' # observations - control run
#' obs <- Schmitt2013 %>%
#'   filter(ID == "T0") %>%
#'    select(t, BM=obs)
#'
#' # update metsulfuron
#' metsulfuron2 <- metsulfuron %>%
#'   set_param(c(k_phot_fix = TRUE, k_resp = 0, Emax = 1)) %>%
#'   set_init(c(BM = 12)) %>%
#'   set_exposure(exp)
#'
#' fit <- calibrate(
#'   x = metsulfuron2,
#'   par = c(k_phot_max = 1, k_resp = 0),
#'   data = obs,
#'   output = "BM"
#' )
#'
#' # Likelihood profiling
#' \donttest{
#' res <- lik_profile(
#'   x = metsulfuron2,
#'   data = obs,
#'   output = "BM",
#'   par = fit$par,
#'   pars_bound = list(
#'     k_resp = list(0, 10),
#'     k_phot_max = list(0, 30)
#'   ),
#'   refit = FALSE,
#'   type = "fine",
#'   method = "Brent"
#' )
#' # plot
#' plot_lik_profile(res)
#' }
#'
#' @return A list containing, for each parameter profiled, the likelihood
#' profiling results as a dataframe;
#' the 95% confidence interval; the original parameter value; the likelihood plot object; and
#' the recalibrated parameter values (in case a lower optimum was found)
#' @export

# # License information related to MatLab BYOM v.6.8:
# Copyright (c) 2012-2023 Tjalling Jager
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so

lik_profile <- function(x,
                        par,
                        output,
                        data = NULL,
                        bounds = NULL,
                        refit = TRUE,
                        type = c("coarse", "fine"),
                        break_prof = FALSE, # typically we do not want to stop
                        # profiling until we have all parameters, and then make a decision based on parameter space
                        ...) {
  # check inputs
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # convert to list of calibration sets if needed
  if (length(x) == 1) {
    if (is_scenario(x)) {
      if (is.null(data)) {
        stop("EffectScenario provided, but missing data")
      } else {
        message("EffectScenario converted into Calibrationset\n")
        x <- list(CalibrationSet(x, data))
      }
    }
  }
  # check correct input parameters
  Check_inputs_lik_prof(par = par,
                        x = x,
                        output = output,
                        type = type)

  # check if type is one of the 2 options
  type <- match.arg(type)

  # update scenario of calibrationset with the provided par
  for (i in seq_along(x)) {
    x[[i]]@scenario <- x[[i]]@scenario %>%
      set_param(par)
  }

  # initialize parameter list
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # names and boundaries of free parameters
  pnames <- names(par)
  pfree <- list(
    values = par, # free param values
    bounds = get_bounds(x[[1]]@scenario)
  )

  # replace defaults if custom boundaries are given
  if(!is.null(bounds)) {
    check_bounds(bounds)
    pfree$bounds[names(bounds)] <- bounds
  }

  # check that each parameter to profile has boundaries set
  missing <- setdiff(names(par), names(pfree$bounds))
  if(length(missing) > 0) {
    cli::cli_abort("parameter boundaries missing for parameter{?s} {missing}")
  }

  # Settings for profiling
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # cutoffs for the Chi-square
  # these are the values from a standard chi-square distribution table
  # (e.g. https://www.itl.nist.gov/div898/handbook/eda/section3/eda3674.htm),
  # for df 1 to 10, at a alpha = 0.05 significance level,
  # these values are used in the chi-square test to determine if a fit is significantly different from a previous one (i.e. larger than the cutoff given here)
  chitable <- c(3.8415, 5.9915, 7.8147, 9.4877, 11.070, 12.592, 14.067, 15.507, 16.919, 18.307)
  if (length(pnames) > length(chitable)) {
    stop("more free parameters than function can handle, reduce to 10")
  } else {
    chi_crit_j <- chitable[length(pnames)]
    chi_crit_s <- chitable[1] # when profiling, we have nested models, differing
    # by 1 parameter (the profiled parameter which is fixed). The likelihood ratio
    # of such nested models follows a Chi-square distribution with 1 degree of freedom
  }


  # setting for profile type
  # values taken from BYOM
  if (type == "fine") {
    # number of iterations
    max_iter <- 50 * (length(pnames) - 1)
    # criteria for change in likelihood ratio
    l_crit_max <- 1 # maximum change in likelihood, above this value, stepsize is decreased
    l_crit_min <- 0.2 # minimum change in likelihood, below this value, stepsize is increased
    l_crit_stop <- chi_crit_s + 5 # stop when likelihood ratio reaches this value
    # settings for step size
    f_step_min <- 0.001 # min stepsize (as fraction of value that is tried)
    f_step_max <- 30 # max stepsize (as fraction of value that is tried)
  } else if (type == "coarse") {
    # number of iterations
    max_iter <- 30 * (length(pnames) - 1)
    # criteria for change in likelihood ratio
    l_crit_max <- 2
    l_crit_min <- 0.7
    l_crit_stop <- chi_crit_s + 2
    # settings for step size
    f_step_min <- 0.005
    f_step_max <- 30
  }


  # The actual profiling
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # a list to store results
  res_list <- list()

  # profile parameter by parameter
  for (par_select in pnames) {
    # profile the parameter
    res_list[[par_select]] <- suppressWarnings(profile_par(
      par_select = par_select, # par = the parameter to profile
      x = x,
      pfree = pfree,
      type = type,
      output = output,
      max_iter = max_iter,
      f_step_min = f_step_min,
      f_step_max = f_step_max,
      chi_crit_j = chi_crit_j,
      chi_crit_s = chi_crit_s,
      l_crit_max = l_crit_max,
      l_crit_min = l_crit_min,
      l_crit_stop = l_crit_stop,
      refit = refit,
      ...
    ))

    # check if better optimum was found for this parameter (if so, stop profiling)
    if (break_prof) {
      if (any(res_list[[par_select]]$likelihood_profile[, "log_lik_rat"] < 0.00)) {
        # give warning to user
        warning(paste0("Better parameter value found for ", par, ", profiling halted"))
        class(res_list) <- c(class(res_list), "lik_profile")
        return(res_list)
      }
    }
  } # end of parameter for loop

  # Return result list
  class(res_list) <- c(class(res_list), "lik_profile")
  return(res_list)
} # end of lik_profile function



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check inputs for likelihood profiling
#
# @description internal function to check if the parameters of the [lik_profile()]
# function are set correctly
#
# @param x either a single [scenario] or a list of [CalibrationSet] objects
# @param par named vector - parameters (names and values) to be profiled
# @param output character vector - the output from the [scenario] or [CalibrationSet] that is used in calibration
# @param type "fine" or "coarse" (default) likelihood profiling
#
# @return x as a list of [CalibrationSet], and objects error message when needed

Check_inputs_lik_prof<- function(par,
                      x,
                      output,
                      type){
  # check if attempt to profile more params than possible ~~~~~~~~~~~~~~~~~~~
  if (length(par) > 10) {
    stop("attempt to profile more parameters that function allows, reduce nr. of parameters")
  }

  # check model (x)~~~~~~~~~~~~~~~~~~~
  # check if Calibrationset or EffectScenario is provided, and convert EffectScenario to CalibrationSet
  if (is.list(x)) {
    if (is(x[[1]]) != "CalibrationSet") {
      warning("incorrect model specified, please enter an EffectScenario or list of 1 or more CalibrationSets\n")
      stopifnot(any(is(x[[1]]) == "CalibrationSet"))
    }
  } else { ## input is not a list
    if (is_scenario(x)) {
      # all fine
    } else { # input is not a list, and also not an EffectScenario
      warning("incorrect model specified, please enter an EffectScenario or list of 1 or more CalibrationSets\n")
      stopifnot(is_scenario(x))
    }
  }

  # check par ~~~~~~~~~~~~~~~~~~~
  # check if parameters are provided as named vector
  stopifnot(mode(par) == "numeric")
  if (is.null(names(par))) {
    warning("parameters are not provided as a named vector")
    stopifnot(!is.null(names(par)))
  }

  # check output ~~~~~~~~~~~~~~~~~~~
  stopifnot(mode(output) == "character")

}


#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Parameter profiling
#
# @description For a specific parameter, this function will sequentially refit
# a model with the parameter value set to
# increasingly lower and higher values, and the likelihood of the model given the
# data calculated (using function [log_lik()]).
#
# The function was inspired by the MatLab BYOM procedure by
# Tjalling Jager, http://debtox.info/byom.html, as described in Jager 2021: Robust Likelihood-Based Optimization and Uncertainty Analysis of Toxicokinetic-
# Toxicodynamic Models" Integrated Environmental Assessment and Management 17:388-397
# DOI: 10.1002/ieam.4333
#
# @param par `character`, the parameter to be profiled
# @param x list of [CalibrationSet] objects
# @param pfree list of parameter values and their bounds for the profiled parameter
# @param type "fine" or "coarse" (default) likelihood profiling
# @param output `character` vector - the output from the [CalibrationSet] that is used during calibration
# @param max_iter `numeric`, maximum number of profiling iterations to attempts
# @param f_step_min, `numeric`,min stepsize (as fraction of value that is tried)
# @param f_step_max, `numeric`,max stepsize (as fraction of value that is tried)
# @param chi_crit_s, `numeric`,cutoffs for the Chi-square under a 95% probability
# @param l_crit_max, `numeric`,maximum change in likelihood, above this value, stepsize is decreased
# @param l_crit_min, `numeric`,minimum change in likelihood, below this value, stepsize is increased
# @param l_crit_stop, `numeric`,stop when likelihood ratio reaches this value
# @param refit if 'TRUE' (default), refit if a better minimum is found
# @param ... additional parameters passed on to [stats::optim()] and [calibrate()]
#
# @return A list of containing the likelihood profiling results as a dataframe;
# the 95% confidence interval; the original parameter value; the likelihood plot object; and
# the recalibrated parameter values (in case a lower optimum was found)
#' @global start
profile_par <- function(par_select,
                        x, pfree, type, output,
                        max_iter, f_step_min, f_step_max,
                        chi_crit_j, chi_crit_s, l_crit_max, l_crit_min, l_crit_stop,
                        refit, ...) {
  # initialize
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # initialize empty list to store results for this param
  param_res <- list()


  # print name of parameter being profiled
  message("Profiling:", par_select, "\n")

  # get best fit value (i.e., calibrated value)
  x_hat_orig <- pfree$values[[par_select]]

  # calulate log likelihood with original model
  pred_orig <- list()
  ll_orig <- list()
  for (i in seq_along(x)) {
    pred_orig[[i]] <- x[[i]]@scenario %>%
      set_times(x[[i]]@data[, 1]) %>% # time is the 1st column, mandatory that it is the 1st column
      simulate()
    ll_orig[[i]] <- log_lik(
      npars = length(pfree$values), # Note: only the free params (i.e. ones that you did the calibration on)
      obs = x[[i]]@data[, 2], # observations are the 2nd column, mandatory that it is the 2nd column
      pred = pred_orig[[i]][, c(output)]
    )
  }

  # additional setting for profile type
  # values taken from BYOM
  if (type == "fine") {
    if (x_hat_orig == 0) { # if parameter value is zero
      f_step_abs <- 1e-4 # This is just a low value that should be ok in most situations
    } else {
      f_step_abs <- 1e-3 * abs(x_hat_orig) # smallest stepsize (in absolute sense)
    }
  } else if (type == "coarse") {
    if (x_hat_orig == 0) {
      f_step_abs <- 1e-3
    } else {
      f_step_abs <- 1e-2 * abs(x_hat_orig)
    }
  }

  message("start param value:", round(x_hat_orig, 3), "LL:", round(sum(unlist(ll_orig)), 3), "\n")

  # refit the model with the par fixed to lower values, and then higher values
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # initialize list to store results during while loop
  log_lik_rat <- c(0)
  par_value <- c(x_hat_orig)
  results <- list(
    loglik = sum(unlist(ll_orig)),
    log_lik_rat = log_lik_rat,
    par_value = par_value
  )

  # initialize while loop
  reach_limit <- FALSE
  x_try <- NA
  prof_dir <- -1 # first we go down
  change_dir <- FALSE # we are not yet ready to change profiling direction
  iter <- 2 # 2 because first one will be the original value

  while (reach_limit == FALSE) {
    # jump to a lower/upper par value ~~~~~~~~~~~~~~~~~

    if (is.na(x_try)) { # for 1st iteration (and 1st time changing direction), start from the minimum step size (as in BYOM)
      f_step <- f_step_min * abs(x_hat_orig)
      x_try <- x_hat_orig + prof_dir * f_step
    } else { # L. 619 in BYOM
      if (f_step * abs(x_try) >= f_step_abs) { # if stepsize is larger or equal to abs minimum stepsize
        x_try <- x_try + prof_dir * f_step * abs(x_try) # make new x_try value
      } else {
        if (f_step * abs(x_try) < f_step_abs) { # if stepsize is smaller that abs minimum stepsize
          x_try <- x_try + prof_dir * f_step_abs
        } # make new x_try value
        f_step <- f_step_abs / abs(x_try) # define f_step
      }
    }

    # check that values are within the specified parameter bounds
    if (prof_dir == -1) { # while going down
      if (x_try < pfree$bounds[[par_select]][1]) { # if value is below parameter minimum bound
        x_try <- pfree$bounds[[par_select]][[1]] # profile one last time, using the min bound value
        change_dir <- TRUE # then change direction
        warning("Minimum parameter bound reached")
      }
    } else {
      if (prof_dir == 1) { # while going up
        if (x_try > pfree$bounds[[par_select]][2]) { # if value is above parameter maximum bound
          x_try <- pfree$bounds[[par_select]][[2]] # profile one last time, using the max bound value
          reach_limit <- TRUE # stop profiling
          warning("Maximum parameter bound reached")
        }
      }
    }

    # save new param value, x_try
    results[["par_value"]][iter] <- x_try



    # refit model and get likelihood ~~~~~~~~~~~~~~~

    # assign name to x_try
    names(x_try) <- par_select

    # fix the parameter value to the new (lower/higher) value
    for (i in seq_along(x)) {
      x[[i]]@scenario <- x[[i]]@scenario %>% set_param(param = x_try)
    }

    # give initials for remaining free params
    prof_param_index <- which(names(pfree[["values"]]) == par_select)
    pfree_remaining <- unlist(pfree[["values"]])[-prof_param_index]

    # # recalibrate
    # fit_new <- calibrate(
    #   x = x,
    #   par = pfree_remaining,
    #   output = output
    # )


    # predict and calulate log likelihood with newly fitted model
    pred_new <- list()
    ll_new <- list()

    for (i in seq_along(x)) {
      # pred_new[[i]] <- simulate(model[[i]]@scenario)
      pred_new[[i]] <- x[[i]]@scenario %>%
        set_times(x[[i]]@data[, 1]) %>% # needs to have the same timepoint as the data
        simulate()
      ll_new[[i]] <- log_lik(
        npars = length(pfree_remaining),
        obs = x[[i]]@data[, 2],
        pred = pred_new[[i]][, output]
      )
    }

    # save new LL
    results[["loglik"]][iter] <- sum(unlist(ll_new))

    # save -2*ln(likelihood ratio) comparing the full (original) model with the nested (1 param fixed) model
    results[["log_lik_rat"]][iter] <- -2 * (results[["loglik"]][iter] - sum(unlist(ll_orig))) # matlab BYOM L. 704

    # difference with ll from previous iteration (needed to decide on step size etc.) # matlab BYOM L. 705
    if (iter == 1) {
      delta_l <- abs(results[["loglik"]][iter] - sum(unlist(ll_orig)))
    } else {
      delta_l <- abs(results[["loglik"]][iter] - results[["loglik"]][iter - 1])
    }

    # evaluate likelihood ~~~~~~~~~~~~~~~~~~~

    # X2 difference test to compare nested models
    if (results[["log_lik_rat"]][iter] > chi_crit_s) { # if the point is outside the 95% conf. region
      if (prof_dir == -1) { # while going down
        change_dir <- TRUE # then change direction
        message("hit 95% Conf Region, changing direction")
      } else {
        if (prof_dir == 1) { # while going up
          reach_limit <- TRUE # stop profiling
          message("hit 95% Conf Region, end of profiling")
        }
      }
    }

    if (delta_l > l_crit_stop) { # stop while-loop if probability is high enough
      reach_limit <- TRUE
      message("critical limit reached, end of profiling")
    } else {
      if (delta_l < l_crit_min) { # if change in likelihood is very small
        f_step <- min(2 * f_step, f_step_max) # double the stepsize, unless greater than max step
      }
    }


    # if needed, change direction
    if (change_dir == TRUE) {
      prof_dir <- 1 # change direction
      x_try <- x_hat_orig # restart from original param value
    }

    iter <- iter + 1
  } # end of while loop


  # reform results list into dataframe
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res_df <- data.frame(
    par_value = results[["par_value"]],
    log_lik_rat = results[["log_lik_rat"]],
    loglik = results[["loglik"]]
  )


  # get prof region
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  min <- min(res_df$par_value)
  max <- max(res_df$par_value)

  # get 95% CI
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # indicate the first value
  res_df$start <- "No"
  res_df[1, "start"] <- "Yes"
  res_df$start <- as.factor(res_df$start)

  # order dataset, from low to high param value
  res_df <- res_df[order(res_df$par_value), ]

  orig_ind <- which(res_df$start == "Yes")
  # lower 95
  low_df <- res_df[1:orig_ind, ]
  if (min(low_df$log_lik_rat) < 0) { # in case a lower minimum is found within the data subset
    min_ind <- which(low_df$log_lik_rat == min(low_df$log_lik_rat))
    low_df <- low_df[1:min_ind, ]
  }
  if (nrow(low_df) > 1 & !all(low_df[, "log_lik_rat"] == 0)) {
    # needs to be at least 2 values, at least one with a ll not 0
    f_inter <- stats::approxfun(
      y = low_df$par_value,
      x = low_df$log_lik_rat, rule = 2
    )
    low95 <- f_inter(chi_crit_s)
  } else {
    low95 <- low_df$par_value
    if (length(low95) > 1) {
      low95 <- min(low95)
    } # in case multiple values with ll of 0, take the min param value
  }
  # upper 95
  up_df <- res_df[orig_ind:nrow(res_df), ]
  if (min(up_df$log_lik_rat) < 0) { # in case a lower minimum is found within the data subset
    min_ind <- which(up_df$log_lik_rat == min(up_df$log_lik_rat))
    up_df <- up_df[min_ind:nrow(up_df), ]
  }
  if (nrow(up_df) > 1 & !all(up_df[, "log_lik_rat"] == 0)) {
    # needs to be at least 2 values, at least one with a ll not 0
    f_inter <- stats::approxfun(
      y = up_df$par_value,
      x = up_df$log_lik_rat, rule = 2
    )
    up95 <- f_inter(chi_crit_s)
  } else {
    up95 <- up_df$par_value
    if (length(up95) > 1) {
      up95 <- max(up95)
    } # in case multiple values with ll of 0, take the max param value
  }



  # plot
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  prof_plot <- res_df %>%
    ggplot2::ggplot() +
    # # zone of acceptabiity
    # ggplot2::geom_rect(
    #   ggplot2::aes(
    #     xmin = -Inf, xmax = Inf,
    #     ymin = min(log_lik_rat), ymax = chi_crit_s
    #   ),
    #   fill = "darkolivegreen3", alpha = 0.2
    # ) +
    ggplot2::annotate("rect",
      xmin = -Inf, xmax = Inf,
      ymin = min(res_df$log_lik_rat), ymax = chi_crit_s,
      alpha = 0.3, fill = "darkolivegreen3"
    ) +
    ggplot2::geom_hline(yintercept = chi_crit_s, color = "tomato", lwd = 1.2) +
    # points for values
    ggplot2::geom_point(ggplot2::aes(
      x = par_value,
      y = log_lik_rat,
      color = start
    ), size = 2) +
    ggplot2::scale_color_manual(values = c("black", "orange")) +
    # connect points
    ggplot2::geom_line(ggplot2::aes(
      x = par_value,
      y = log_lik_rat
    )) +
    # could add 95% CI
    ggplot2::geom_vline(
      xintercept = c(low95, up95),
      linetype = "dashed",
      color = "grey30", lwd = .5
    ) +
    ggplot2::xlab(par_select) +
    # consmetics
    ggplot2::scale_y_continuous(limits = c(min(res_df$log_lik_rat), NA), expand = c(0, 0)) +
    ggplot2::theme_classic()

  # save all outputs for the parameter
  param_res <- list(
    likelihood_profile = res_df,
    confidence_interval = c(low95, up95),
    prof_region = c(min, max),
    orig_par_value = x_hat_orig,
    plot = prof_plot,
    fit_new = NULL
  )

  # recalibrate if needed
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check in results if any log likelihood ratio is lower than the original
  if (refit == TRUE) {
    if (any(res_df[["loglik"]] > sum(unlist(ll_orig)) + 0.01)) {
      # only if the difference is at least 0.01 as in BYOM (value taken from BYOM)
      # this ignores tiny, irrelevant, improvement
      best_par_value <- res_df[which(res_df$log_lik_rat == min(res_df$log_lik_rat)), "par_value"]
      names(best_par_value) <- par_select
      # update par value in model
      for (i in seq_along(x)) {
        x[[i]]@scenario <- x[[i]]@scenario %>% set_param(param = best_par_value)
      }
      # recalibrate
      fit_new <- cvasi::calibrate(
        x = x,
        par = pfree_remaining,
        output = output
      )
      # save recalibrated result
      param_res[["fit_new"]] <- list(
        best_fit_param = c(best_par_value, fit_new$par),
        recalibration_outputs = fit_new
      )
    } else { # if no better optimum was found for this parameter
      param_res[["fit_new"]] <- "No recalibration was necessary."
    }
  }
  return(param_res)
} # end of profile_par function


#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot likelihood profiles or all profiled parameters
#'
#' @description The function provides a combined plot of the likelihood profiles
#' of all parameters profiled.
#'
#' @param x object of class lik_profile
#'
#' @return plots
#' @export
plot_lik_profile <- function(x) {
  stopifnot("lik_profile" %in% class(x))
  npars <- length(x)
  n1 <- ceiling(sqrt(npars))
  do.call(gridExtra::grid.arrange, c(lapply(x, function(x) x$plot), list(nrow = n1)))
}

#' Calculate log likelihood
#'
#' @description Calculates the sum of log likelihoods of each observation given
#' the model parameterization (considering a normal distribution around the prediction
#' for each datapoint)
#'
#' @param npars named numeric vector of parameters that the model was calibrated on
#' @param obs numeric vector of observed values
#' @param pred numeric vector of predicted values
#'
#' @return the log likelihood value
#' @export
#'
#' @examples
#'
#' # observations
#' obs <- c(12, 38, 92, 176, 176, 627, 1283, 2640)
#' # intercept, a, and slope, b, of a Poisson regression fitted through obs
#' pars <- c(a = 2, b = 0.73)
#' # predictions with the Poisson regression
#' pred <- c(15.43, 32.15, 66.99, 139.57, 290.82, 605.94, 1262.52, 2630.58)
#' # example plot
#' plot(seq(1:length(obs)), obs)
#' lines(seq(1:length(obs)), pred)
#' log_lik(
#'   npars = length(pars),
#'   obs = obs,
#'   pred = pred
#' )
#'
log_lik <- function(npars, obs, pred){
  stopifnot(length(obs) == length(pred))
  stopifnot(length(npars) == 1)
  stopifnot(is.numeric(npars))


  k <- npars
  res <- obs - pred
  n <- length(res)
  SSE <- sum(res^2)
  sigma <- sqrt(SSE / (n - k))
  sigma_unbiased <- sigma * sqrt((n - k) / n)
  # log likelihood for normal probability density
  LL <- sum(log(stats::dnorm(x = obs, mean = pred, sd = sigma_unbiased)))
  return(LL)
}
