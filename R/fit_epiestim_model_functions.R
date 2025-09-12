#' fit_epiestim_model - Function to estimate the reproduction number of an epidemic
#'
#' @description A wrapper function for {\code{\link[EpiEstim]{estimate_R}}} from the \code{EpiEstim} library to estimate the reproduction number of epidemics to support short-term forecasts
#'
#'
#' @details \code{fit_epiestim_model} currently supports the following epidemics: Influenza, RSV and COVID-19. The default serial intervals for the estimation of R were retrieved from
#' Cowling et al., 2011, Vink et al., 2014 and Madewell et al., 2023 for Influenza A, Influenza B, RSV and COVID (BA.5 Omicron variant) respectively
#'
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases)
#' @param dt *Integer* 	Not implemented. length of temporal aggregations of the incidence data. This should be an integer or vector of integers. The default value is 7 time units (1 week).
#' @param window_size *Integer* Length of the sliding windows used for R estimates.
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "custom"
#' @param mean_si *Numeric* User specification of mean of parametric serial interval
#' @param std_si *Numeric* User specification of standard deviation of parametric serial interval
#' @param recon_opt Not implemented. One of "naive" or "match" to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page)
#' @param method One of "non_parametric_si", "parametric_si", "uncertain_si", "si_from_data" or "si_from_sample" to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page)
#' @param mean_prior *Numeric* positive number giving the mean of the common prior distribution for all reproduction numbers
#' @param std_prior *Numeric* positive number giving the standard deviation of the common prior distribution for all reproduction numbers
#'
#'
#' @return Object of class {\code{\link[EpiEstim]{estimate_R}}} (see \code{EpiEstim} help page)
#' @export
#'
#' @examples
#' fit_epiestim_model(data = weekly_transformed_plover_data, type = "flu_a")
#'
fit_epiestim_model <- function(data, dt = 1L, window_size = 7L,type = NULL, mean_si = NULL, std_si = NULL, recon_opt = "match",
                               method = "parametric_si", mean_prior = NULL, std_prior = NULL) {
  confirm <- NULL
  if (!is.data.frame(data) || !all(colnames(data) %in% c("date", "confirm"))) {
    stop("Must pass a data frame with two columns: date and confirm")
  }
  if (missing(type) || !(type %in% c("flu_a", "flu_b", "sars_cov2", "rsv", "custom"))) {
    stop("Must specify the type of epidemic (flu_a, flu_b, covid, rsv or custom)")
  }
  if (type == "custom" && any(is.null(mean_si), is.null(std_si), is.null(mean_prior), is.null(std_prior))) {
    stop("Must specify mean_si, std_si, mean_prior and std_prior for type custom")
  }
  
  if (type != "custom" && any(!is.null(mean_si), !is.null(std_si), !is.null(mean_prior), !is.null(std_prior))) {
    warning("Custom mean_si, std_s, mean_prior and std_prior can only be specified with type set to custom. Default config values were used")
  }
  
  if (dt == 7L) {
    stop("Weekly data not currently implmented. Use only daily data.")
  }
  
  data_lag <- as.numeric(difftime(data$date[2], data$date[1]))
  if (data_lag <= 7 && dt == 7L) {
    warning("Your data may not be weekly data. Please set dt to 1L for daily data")
  }
  
  # 6. Providing default values
  if (is.null(mean_si)) {
    mean_si <- switch(type,
                      "flu_a" = 4,
                      "flu_b" = 3.7,
                      "rsv" = 7.5,
                      "sars_cov2" = 4,
                      "custom" = NULL
    )
  }
  if (is.null(std_si)) {
    std_si <- switch(type,
                     "flu_a" = 2,
                     "flu_b" = 2.1,
                     "rsv" = 2.1,
                     "sars_cov2" = 4.75,
                     "custom" = NULL
    )
  }
  if (is.null(mean_prior)) {
    mean_prior <- switch(type,
                         "flu_a" = 1,
                         "flu_b" = 1,
                         "rsv" = 1,
                         "sars_cov2" = 2,
                         "custom" = NULL
    )
  }
  if (is.null(std_prior)) {
    std_prior <- switch(type,
                        "flu_a" = 1,
                        "flu_b" = 1,
                        "rsv" = 1,
                        "sars_cov2" = 1,
                        "custom" = NULL
    )
  }
  
  # 7. Configuring based on type
  if (dt == 1L) {
    incid <- data.frame(I = data$confirm, dates = data$date)
    incid <- incid %>%
      dplyr::arrange(dates)
    # this should create weekly windows for the Rt
    n_t <- nrow(incid)
    t_start <- seq(2, max(n_t-(window_size - 1),2))
    t_end <- pmin(t_start + window_size - 1,n_t)
    config <- EpiEstim::make_config(list(
      mean_si = mean_si,
      std_si = std_si,
      mean_prior = mean_prior,
      std_prior = std_prior,
      t_start = t_start,
      t_end = t_end
    ))
  } else if (dt > 1L) {
    incid <- data$confirm
    config <- EpiEstim::make_config(list(
      mean_si = mean_si,
      std_si = std_si,
      mean_prior = mean_prior,
      std_prior = std_prior
    ))
  }
  
  epiestim_estimates <- NULL
  epiestim_estimates <- suppressWarnings(EpiEstim::estimate_R(
    incid = incid,
    method = method,
    config = config
  ))
  
  
  return(epiestim_estimates)
}


#' Extract daily forecast samples
#'
#'
#' @description Function to produce short-term daily projections from objects of class {\code{\link[EpiEstim]{estimate_R}}}
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per day)
#' @param model_fit Object of class {\code{\link[EpiEstim]{estimate_R}}} generated by running \code{fit_epiestim_model}
#' @param n_days 	The number of days to run simulations for. Defaults to 14
#' @param n_sim The number of epicurves to simulate. Defaults to 1000
#'
#'
#'
#'
#' @return Data-frame of daily forecast samples from all simulations
#' \describe{
#'   \item{date}{date}
#'   \item{incidence}{projected number of daily confirmed cases}
#'   \item{sim}{simulation run number}
#' }
generate_forecasts <- function(data, model_fit, n_days = 7, n_sim = 1000) { 
  confirm <- NULL
  
  check_epiestim_format(data)
  
  # incidence expects data in linelist format
  date_list <- data |>
    tidyr::uncount(confirm) |>
    dplyr::pull(date)
  incidence_obj <- incidence::incidence(date_list)
  
  r_vals <- utils::tail(model_fit$R, n = 1)
  r_dist <- rtrunc_norm(1000, mean = r_vals$`Mean(R)`, sd = r_vals$`Std(R)`, lower_lim = 0)
  # Use the project function
  proj <- projections::project(incidence_obj,
                               R = r_dist,
                               si = model_fit$si_distr[-1],
                               n_sim = n_sim,
                               n_days = n_days,
                               R_fix_within = FALSE
  )
  
  
  data_proj <- as.data.frame(proj, long = TRUE)
  
  return(data_proj)
}


#' Main function to run the forecast model
#'
#'
forecast_epiestim <- function(
    data,
    start_date,
    window_size = 7,
    n_days = 7,
    type = NULL,
    smooth_data = FALSE,
    smoothing_cutoff = 10,
    ...
){
  
  check_epiestim_format(data)
  
  # check and filter on start date
  check_data_contains_start_date(data,start_date)
  data <- data %>%
    dplyr::filter(date > start_date)
  
  # exclude the first date if there are no confirmed cases
  non_zero_dates <- data %>%
    dplyr::filter(confirm > 0) %>%
    pull(date)
  data <- data %>%
    dplyr::filter(date >= non_zero_dates[1])
  
  # use smooth data
  if(smooth_data){
    smoothed_output <- smooth_model_data(data, smoothing_cutoff = smoothing_cutoff)
    data <- smoothed_output$data
  }
  # modelling function
  epiestim_estimates <- fit_epiestim_model(data = data,
                                           window_size = window_size,
                                           type = type,
                                           ...)
  # generating forecast data
  forecast_res <- generate_forecasts(data = data, 
                                     model_fit = epiestim_estimates, 
                                     n_days = n_days)
  
  return(forecast_res)
}



#' Smooth Model Data Using P-Spline GAM
#'
#' Applies P-spline smoothing using a Generalized Additive Model (GAM) to the input model data.
#' If the number of rows in `model_data` is greater than or equal to `smoothing_cutoff`,
#' the function fits a GAM, estimates confidence intervals, and computes uncertainty.
#'
#' @param model_data A data frame containing the model data, including a column `confirm` representing observed values.
#' @param smoothing_cutoff minimum number of rows required to smooth
#' @param n_reps Number of replicates to calculate error
#'
#' @return A list with two elements:
#'  - `original_data`: A copy of `model_data`
#' 	- `data`: A data frame with smoothed `confirm` values.
#' 	- `error`: Estimated uncertainty of the smoothing process.
#'
#' @importFrom mgcv gam predict vcov
#' @importFrom stats coef qnorm rnorm quantile
#' @importFrom dplyr mutate
#' @noRd
smooth_model_data <- function(model_data, smoothing_cutoff = 10, n_reps = 10000) {
  ##### Add in P-spline smoothing with GAM at each time-step ###################
  smoothed_model_data <- model_data
  if (nrow(model_data) >= smoothing_cutoff) {
    index <- seq_len(nrow(model_data))
    model_data$index <- index
    model_smooth <- mgcv::gam(confirm ~ s(index, bs = "ps", k = round(length(index) / 2, 0)), data = model_data)
    beta <- coef(model_smooth)
    Vb <- vcov(model_smooth)
    Cv <- chol(Vb)

    nb <- length(beta)
    br <- t(Cv) %*% matrix(rnorm(n_reps * nb), nb, n_reps) + beta
    Xp <- suppressWarnings(predict(model_smooth, newdata = data.frame(index = index), type = "lpmatrix"))
    fv <- Xp %*% br
    yr <- matrix(rnorm(nrow(fv) * ncol(fv), mean = fv, sd = model_smooth$sig2),
      nrow = nrow(fv), ncol = ncol(fv)
    )
    conf_int <- apply(yr, 1, quantile, prob = c(0.025, 0.975))
    diff <- conf_int[2, ] - conf_int[1, ]
    uncertainity_se <- diff / (qnorm(1 - 0.05 / 2) * 2)
    smoothed_estimates <- predict(model_smooth, type = "response", se.fit = TRUE)
    smoothed_model_data$confirm <- round(smoothed_estimates$fit, 0)
    smoothed_model_data <- smoothed_model_data %>%
      mutate(confirm = ifelse(confirm < 0, 0, confirm))
    smoothed_error <- data.frame(smoothed_error = smoothed_estimates$se.fit + uncertainity_se)
  } else {
    smoothed_error <- 0
  }

  return(list(original_data = model_data, data = smoothed_model_data, error = smoothed_error))
}



