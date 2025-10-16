#' Compare Reproduction Number Estimates from Multiple EpiEstim Outputs
#'
#' This function creates a ggplot comparing the estimated reproduction number \(R\)
#' over time from multiple EpiEstim results. Each input should be an object from
#' EpiEstim containing an `R` dataframe.
#'
#' @param ... Named EpiEstim output objects. Each object should contain a data frame
#'   `R`
#'
#' @return A ggplot object showing median \(R\) estimates with 90% credible intervals
#'   (as ribbons), colored by each result.
#'
#'
#' @examples
#' \dontrun{
#' # Assuming `fit1` and `fit2` are valid EpiEstim outputs with R data frames:
#' plot_R_fit_comparison(Fit1 = fit1, Fit2 = fit2)
#' }
#'
plot_R_fit_comparison <- function(...) {
  t_end <- `Median(R)` <- `Quantile.0.025(R)` <- `Quantile.0.975(R)` <- NULL
  window <- `Quantile.0.25(R)` <- `Quantile.0.75(R)` <- NULL
  t_start <- time <- type <- NULL
  epiestim_list <- list(...)
  plot_obj <- list()
  for (name in names(epiestim_list)) {
    epiestim_obj <- epiestim_list[[name]]
    check_is_obj(epiestim_obj, "estimate_R")
    plot_obj[[name]] <- epiestim_obj$R |>
      mutate(
        type = name
      )
  }

  g <- dplyr::bind_rows(plot_obj) |>
    dplyr::mutate(time = .5 * (t_end + t_start)) |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = `Median(R)`)) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = `Quantile.0.025(R)`, ymax = `Quantile.0.975(R)`,
      fill = type
    ), alpha = 0.2) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = `Quantile.0.25(R)`, ymax = `Quantile.0.75(R)`,
      fill = type
    ), alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(color = type)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "time", y = "R", color = "", fill = "")

  return(g)
}

#' @export
plot_forecast_comparison <- function(...) {
  date <- incidence <- sim <- type <- NULL
  quant <- val <- NULL
  quantile_df <- function(x, probs = c(0.025,0.5,0.975)) {
    tibble(
      val = quantile(x, probs, na.rm = TRUE),
      quant = probs
    )
  }

  obj_list <- list(...)
  plot_obj <- list()
  for (name in names(obj_list)) {
    obj <- obj_list[[name]]
    check_is_obj(obj, "data.frame")
    plot_obj[[name]] <- obj |>
      mutate(
        type = name
      )
  }

  g <- dplyr::bind_rows(plot_obj) |>
    reframe(quantile_df(incidence), .by = c(date,type)) |>
    tidyr::pivot_wider(id_cols = c(date,type), names_from = quant,
                values_from = val) |>
    ggplot(aes(x=date,y=val)) +
    ggplot2::geom_ribbon(ggplot2::aes(
      ymin = `0.025`, ymax = `0.975`,
      fill = type
    ), alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(y=`0.5`, color = type)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(x = "time", y = "incidence", color = "", fill = "")

  return(g)
}


#' Plot Mean Rt with time index (dates)
#' @param time_period_result output from  \code{forecast_time_period}
#' @return Mean Rt with time index plot
#' @export
plot_rt <- function(time_period_result) {
  last_time_period <- time_period_result[[length(time_period_result)]]

  # check input from validation or forecast func
  if("model_data_date" %in% names(last_time_period)){
    model_data_dates <- last_time_period$model_data_date
  }else{
    model_data_dates <- last_time_period$dates
  }

  rt_dat <- last_time_period[["R"]]
  rt_start_date <- model_data_dates[1]
  rt_date_seq <- seq(rt_start_date, by = "day", length.out = length(rt_dat$t_start))
  rt_dat$date <- rt_date_seq
  rt_dat <- rt_dat |>
    dplyr::mutate(
      weekly_date = lubridate::floor_date(date, unit = "week")
    ) |>
    dplyr::group_by(weekly_date) |>
    dplyr::summarise(
      weekly_rt = mean(`Mean(R)`), weekly_ymin = mean(`Quantile.0.025(R)`),
      weekly_ymax = mean(`Quantile.0.975(R)`)
    )
  p <- ggplot(rt_dat, aes(x = weekly_date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = weekly_ymin, ymax = weekly_ymax), fill = "#08519C", alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = weekly_rt), color = "#08519C") +
    theme_bw() +
    labs(x = "Time", y = "mean(expression(R[t]))") +
    ggplot2::geom_line(ggplot2::aes(y = weekly_rt), color = "#08519C") +
    theme_bw() +
    labs(x = "Time", y = "Mean(Rt)")
  return(p)
}


#' Internal: Check Class
#'
#' Helper function that checks if an object inherits from a given class. If not,
#' throws an error.
#'
#' @param obj The object to be checked.
#' @param class_name The class name that `obj` should inherit from.
#'
#'
#' @noRd
check_is_obj <- function(obj, class_name) {
  if (!inherits(obj, class_name)) {
    cli::cli_abort("Object needs to be of class {class_name}")
  }
}


