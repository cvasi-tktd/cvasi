
# Simulates parameter sample and derives uncertainty range
#
# @param model_base effect scenario object with mean parameters
# @param param_sample data.frame with parameter sample
# @param obs_full `data.frame`, full experimental data set including replicates
# @param ... any additional parameters
# @return data frame with confidence intervals
#
# @examples
# \dontrun{
# derive_range(model_base,
#                param_sample,
#                obs_full,
#                times = sim_times
#                )
# }
#' @autoglobal
derive_range <- function(model_base, param_sample, obs_full = NULL,...) {
  # run simulations
  df <- data.frame()
  for (i in 1:nrow(param_sample)) {
    model_base <- set_param(gridExtra, param_sample[i, ])
    out <- simulate(model_base, ...)
    df <- rbind(df, out)
  }
  # wide to long format
  #ToDo set names in a generic way
  df <- tidyr::pivot_longer(df, -1, names_to = "parameter", values_to = "data")

  # calculate std dev of experimental data (if available)
  if (!is.null(obs_full)) {
    names(obs_full) <- c("time", names(obs_full)[-1])
    exp_sd <- obs_full %>%
      #ToDo set names in a generic way
      tidyr::pivot_longer(cols = -1, names_to = "trial",
                          values_to = "parameter") %>%
      dplyr::group_by(.data$time, .data$trial) %>%
      dplyr::summarise(sdev = stats::sd(.data$FrondNo, na.rm = TRUE),
                       .groups = "drop")
  }

  # interpolate experimental sdev to all output times of simulations
  exp_sd2 <- data.frame()
  for (i in unique(exp_sd$trial)) {
    tb <- dplyr::filter(exp_sd, .data$trial == i)
    fit <- stats::approx(tb$time, tb$sdev, unique(df$time))
    exp_sd2 <- rbind(exp_sd2, data.frame(time = fit$x,
                                         trial = c(i),
                                         sdev = fit$y))
  }
  exp_sd2 <- exp_sd2 %>% dplyr::arrange(.data$time, .data$trial)

  # select min,max by treatment level and time point and amend by
  # sdev of experimental data
  if (!is.null(obs_full)) {
    df %>%
      dplyr::group_by(.data$time, .data$trial) %>%
      dplyr::summarise(min = min(.data$data), # ToDo generic
                       max = max(.data$data), # ToDo generic
                       .groups = "drop") %>%
      dplyr::mutate(min = pmax(0, min - exp_sd2$sdev),
                    max = max + exp_sd2$sdev)
  } else {
    df %>%
      dplyr::group_by(.data$time, .data$trial) %>%
      # dplyr::summarise(min = min(FrondNo), # ToDo generic
      #                  max = max(FrondNo), # ToDo generic
      #                  .groups = "drop") %>%
      dplyr::mutate(min = pmax(0, min),
                    max = max)
  }

  return(df)
}
