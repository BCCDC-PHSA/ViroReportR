#' Plot a violin plot with specific time horizon predictions against true values for validation
#'
#' @param time_period_result object of class \code{forecast_time_period_epiestim}
#' @param pred_horizon_str *string* prediction horizon time period to plot
#' @return violin validation plot for a specific time horizon
#'
#' @export
#' @examples
#' plover_data <- data.frame(
#'   epiWeek_date = as.Date(c(
#'     "2022-10-02", "2022-10-09",
#'     "2022-10-16", "2022-10-23", "2022-10-30",
#'     "2022-11-06", "2022-11-13", "2022-11-20",
#'     "2022-11-27", "2022-12-04"
#'   )),
#'   epiWeek_year = c(
#'     2022, 2022, 2022, 2022,
#'     2022, 2022, 2022, 2022, 2022, 2022
#'   ),
#'   Epiweek = c(40, 41, 42, 43, 44, 45, 46, 47, 48, 49),
#'   flu_a = c(17, 19, 32, 38, 43, 45, 73, 88, 94, 105),
#'   flu_b = c(24, 31, 39, 45, 50, 52, 68, 83, 89, 97)
#' )
#'
#' weekly_plover_data <- get_weekly_plover(plover_data)
#'
#' plover_dat_clean <- get_weekly_plover_by_date_type(
#'   weekly_plover_data,
#'   "flu_a",
#'   "2022-10-01",
#'   "2022-12-05"
#' )
#'
#' time_period_result <- forecast_time_period_epiestim(
#'   data = plover_dat_clean,
#'   start_date_str = "2022-10-02", n_days = 14, type = "flu_a"
#' )
#' validate(time_period_result, pred_horizon_str = "1 week ahead")
#'

validate <- function(time_period_result, pred_horizon_str = NULL){
  confirm <-  p50 <- point_type <- pred_horizon <- sim_draws <- weekly_date <- NULL
  aggregate_unit <-  time_period_result[[1]][["quantile_unit"]]
  if (is.null(pred_horizon_str)) {
    stop("Must specify prediction time horizon for validation plot")
  }
  if(aggregate_unit == "daily") {
    stop("Only weekly aggregated data suitable for validation plot. Please re-run forecast_time_period_epiestim with weekly_aggregate = TRUE")
  }
  forecast_dat <- create_forecast_df(time_period_result)
  if (!(pred_horizon_str %in% forecast_dat$pred_horizon)) {
    stop("Prediction horizon not found in time_period_result, please check input")
  }
  forecast_dat <- forecast_dat %>%
    dplyr::filter(pred_horizon == pred_horizon_str)
  point_data <- forecast_dat %>% dplyr::select(date = weekly_date, confirm = p50)
  point_data$point_type <- rep("Median Prediction", nrow(point_data))
  model_data <- data.frame(date = time_period_result[[length(time_period_result)]]$model_data_date,
                           confirm = time_period_result[[length(time_period_result)]]$confirm)
  model_data$point_type <- rep("Confirmed Case", nrow(model_data))
  point_data <- rbind(point_data, model_data)
  p <- ggplot2::ggplot(forecast_dat, ggplot2::aes(x= factor(weekly_date), y= sim_draws)) +
    ggplot2::geom_violin(scale = "count", colour = "gray", fill = "blue", alpha = 0.1) +
    ggplot2::ggtitle(paste0("Violin plot of ", pred_horizon_str, " prediction")) +
     ggplot2::geom_point(ggplot2::aes(x = factor(date), y = confirm, colour = point_type), data = point_data) +
    ggplot2::theme_bw() + ggplot2::labs(x = "Time", y = paste0(pred_horizon_str, " projection of confirmed cases", fill = "", colour = "")) +
    ggplot2::theme(legend.title= ggplot2::element_blank(), legend.position = "top") +
    ggplot2::scale_colour_manual(values = c("#2C728EFF", "#471164FF"))
  return(p)
}
