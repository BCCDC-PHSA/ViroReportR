############################################## Functions from epiFilter written by Kris Parag #############################
#' fit_epifilter_model - Function to estimate the reproduction number of an epidemic
#'
#'
#' @description Model fitting function using recursive Bayesian filtering to estimate the reproduction number of epidemics to support short-term forecasts
#' From: Parag, KV, (2020) “Improved real-time estimation of reproduction numbers
# at low case incidence and between epidemic waves”
#'
#'
#' @details \code{fit_epifilter_model} currently supports the following epidemics: Influenza, RSV and SARS-CoV2. The serial intervals for the estimation of R were retrieved from
#' Cowling et al., 2011, Vink et al., 2014 and Madewell et al., 2023 for Influenza A, Influenza B, RSV and SARS-CoV2 (BA.5 Omicron variant) respectively
#'
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param dt *Integer* 	length of temporal aggregations of the incidence data. This should be an integer or vector of integers. The default value for epiFilter is 1L (daily data)
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "other"
#' @param mean_si *numeric* User specification of mean of parametric serial interval
#' @param std_si *numeric* User specification of standard deviation of parametric serial interval
#' @param grid_size *numeric* Size of uniform grid for reproduction number values (set to 200 by default)
#' @param mean_prior *numeric* positive number for the minimum grid reproduction number
#' @param std_prior *numeric* positive number for the maximum grid reproduction number
#' @param smooth *logical* set to TRUE by default to apply recursive Bayesian smoothing of estimates
#' @param confidence_est *numeric* Confidence level of Rhat estimates (set to 0.025 yielding 95% confidence intervals by default)
#' @param eta *numeric* diffusion noise (set to 0.1 by default)
#'
#' @return List storing filtered Rhat estimates and model outputs
#' \describe{
#'   \item{Rmedian}{50% quantiles of Rhat estimates}
#'   \item{Rhat_estimates}{95% quantiles of Rhat estimates}
#'   \item{Rmean}{Average of Rhat estimates}
#'   \item {smooth_filtered_posterior}{Smoothed and filtered posterior estimates}
#'
#' @export
#'
#' @examples
#' fit_epifilter_model(data = plover_data, type = "flu_a")
#'

fit_epifilter_model <- function(data, dt = 1L, type = NULL, mean_si = NULL, std_si = NULL,
                                grid_size = 200, Rmin = 0.01, Rmax = 3,
                                  smooth = TRUE, confidence_est = 0.025, eta = 0.1) {
  Iday = data$confirm
  nday = length(data$date); tday = 1:nday

  if (is.null(mean_si) && is.null(std_si)) {
    if (type == "flu_a") {
      wdist = dgamma(tday, shape = 3.1, scale = 1.6)
  } else if (type == "flu_b") {
      wdist = dgamma(tday, shape = 3.7, scale = 2.1)
    } else if (type == "rsv") {
      wdist = dgamma(tday, shape = 7.5, scale = 2.1)
    } else if (type == "sars_cov2") {
      wdist = dgamma(tday, shape = 2.75, scale = 2.53)
    }
  } else {
    wdist = dgamma(tday, shape = mean_si, scale = std_si)
  }
  if (data$confirm[1] == 0) {
    min_reliable_date <- data %>%
      dplyr::filter(confirm > 0) %>%
      dplyr::pull(date)
    stop(
      "Incidence is too low on the current start date. This will lead to NA predictions. Consider starting R estimation from ", min_reliable_date[1],
      " for an accurate estimate of the reproduction number with epiFilter"
    )
  }
  Lday = rep(0, nday)
  for(i in 2:nday){
    Lday[i] = sum(Iday[seq(i-1, 1, -1)]*wdist[1:(i-1)])
  }

# Setting up R0 grid
grid_size = grid_size; pR0 = (1/grid_size)*rep(1, grid_size)
  # Delimited grid defining space of R
Rgrid = seq(Rmin, Rmax, length.out = grid_size)

# Probability vector for R and prior
 pR = matrix(0, nday, grid_size); pRup = pR
 pR[1, ] = pR0; pRup[1, ] = pR0

 # Mean and median estimates
 Rmean = rep(0, nday); Rmed = Rmean
 # 50% and 95% (depends on a) confidence on R
 Rhat = matrix(0, 4, nday)

 # Initialise mean
 Rmean[1] = pR[1, ]%*%Rgrid
 # CDF of prior
 Rcdf0 = cumsum(pR0)
 # Initialise quartiles
 idm = which(Rcdf0 >= 0.5, 1); Rmed[1] = Rgrid[idm[1]]
 id1 = which(Rcdf0 >= confidence_est, 1); id2 = which(Rcdf0 >= 1-confidence_est, 1)
 id3 = which(Rcdf0 >= 0.25, 1); id4 = which(Rcdf0 >= 0.75, 1)
 Rhat[1, 1] = Rgrid[id1[1]]; Rhat[2, 1] = Rgrid[id2[1]]
 Rhat[3, 1] = Rgrid[id3[1]]; Rhat[4, 1] = Rgrid[id4[1]]

 # Precompute state distributions for R transitions
 pstate = matrix(0, grid_size, grid_size);
 for(j in 1:grid_size){
   pstate[j, ] = dnorm(Rgrid[j], Rgrid, sqrt(Rgrid)*eta)
 }
 # Update prior to posterior sequentially
 for(i in 2:nday){
   # Compute mean from Poisson renewal (observation model)
   rate = Lday[i]*Rgrid
   # Probabilities of observations
   pI = dpois(Iday[i], rate)

   # State predictions for R
   pRup[i, ]  = pR[i-1, ]%*%pstate
   # Update to posterior over R
   pR[i, ] = pRup[i, ]*pI
   pR[i, ] = pR[i, ]/sum(pR[i, ])

   # Posterior mean and CDF
   Rmean[i] = pR[i, ]%*%Rgrid
   Rcdf = cumsum(pR[i, ])

   # Quantiles for estimates
   idm = which(Rcdf >= 0.5, 1); Rmed[i] = Rgrid[idm[1]]
   id1 = which(Rcdf >= confidence_est, 1); id2 = which(Rcdf >= 1-confidence_est, 1)
   id3 = which(Rcdf >= 0.25, 1); id4 = which(Rcdf >= 0.75, 1)
   Rhat[1, i] = Rgrid[id1[1]]; Rhat[2, i] = Rgrid[id2[1]]
   Rhat[3, i] = Rgrid[id3[1]]; Rhat[4, i] = Rgrid[id4[1]]
 }
 # Main outputs: estimates of R and states
 #50% and 95% quantiles of estimates (Rhat),
 # causal posterior over R (pR), pre-update (pRup) and state transition matrix (pstate)
 epiFilter_output = list(Rmedian = Rmed, Rhat_estimates = Rhat, Rmean = Rmean, filtered_posterior = pR,
                  posterior_pre_filter = pRup, transition_matrix = pstate)
 return(epiFilter_output)

 if (isTRUE(smooth)) {

   # Last smoothed distribution same as filtered
   qR = matrix(0, nday, grid_size); qR[nday, ] = pR[nday, ]

   # Main smoothing equation iteratively computed
   for(i in seq(nday-1, 1)){
     # Remove zeros
     pRup[i+1, pRup[i+1, ] == 0] = 10^-8

     # Integral term in smoother
     integ = qR[i+1, ]/pRup[i+1, ]
     integ = integ%*%pstate

     # Smoothed posterior over Rgrid
     qR[i, ] = pR[i, ]*integ
     # Force a normalisation
     qR[i, ] = qR[i, ]/sum(qR[i, ]);
   }

   # Mean, median estimats of R
   Rmean = rep(0, nday); Rmed = Rmean
   # 50% and 95% (depends on a) confidence on R
   Rhat = matrix(0, 4, nday)

   # Compute at every time point
   for (i in 1:nday) {
     # Posterior mean and CDF
     Rmean[i] = qR[i, ]%*%Rgrid
     Rcdf = cumsum(qR[i, ])

     # Quantiles for estimates
     idm = which(Rcdf >= 0.5); Rmed[i] = Rgrid[idm[1]]
     id1 = which(Rcdf >= confidence_est, 1); id2 = which(Rcdf >= 1-confidence_est, 1)
     id3 = which(Rcdf >= 0.25, 1); id4 = which(Rcdf >= 0.75, 1)
     Rhat[1, i] = Rgrid[id1[1]]; Rhat[2, i] = Rgrid[id2[1]]
     Rhat[3, i] = Rgrid[id3[1]]; Rhat[4, i] = Rgrid[id4[1]]
   }

   # Main outputs: estimates of R and states

   epiSmoother_output = list(Rmedian = Rmed, Rhat_estimates = Rhat, Rmean = Rmean,
                             smooth_filtered_posterior = qR)
   return(epiSmoother_output)
 }
}

###################### Prediction function for EpiFilter (adapted from MatLab code written by Kris Parag) #############################

#' epifilter_project - Prediction function to generate forecasted predictions from epiFilter over short-term fixed horizons of time
#'
#'
#' @description Model fitting function using recursive Bayesian filtering to estimate the reproduction number of epidemics to support short-term forecasts
#' From: Parag, KV, (2020) “Improved real-time estimation of reproduction numbers
# at low case incidence and between epidemic waves”
#'
#'
#' @details \code{fit_epifilter_model} currently supports the following epidemics: Influenza, RSV and COVID-19. The serial intervals for the estimation of R were retrieved from
#' Cowling et al., 2011, Vink et al., 2014 and Madewell et al., 2023 for Influenza A, Influenza B, RSV and COVID (BA.5 Omicron variant) respectively
#'
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param dt *Integer* 	length of temporal aggregations of the incidence data. This should be an integer or vector of integers. The default value for epiFilter is 1L (daily data)
#' @param type *character* Specifies type of epidemic. Must be one of "flu_a", "flu_b", "rsv", "sars_cov2" or "other"
#' @param mean_si *numeric* User specification of mean of parametric serial interval
#' @param std_si *numeric* User specification of standard deviation of parametric serial interval
#' @param grid_size *numeric* Size of uniform grid for reproduction number values (set to 200 by default)
#' @param mean_prior *numeric* positive number for the minimum grid reproduction number
#' @param std_prior *numeric* positive number for the maximum grid reproduction number
#' @param smooth *logical* set to TRUE by default to apply recursive Bayesian smoothing of estimates
#' @param confidence_est *numeric* Confidence level of Rhat estimates (set to 0.025 yielding 95% confidence intervals by default)
#' @param eta *numeric* diffusion noise (set to 0.1 by default)
#'
#' @return List storing filtered Rhat estimates and model outputs
#' \describe{
#'   \item{Rmedian}{50% quantiles of Rhat estimates}
#'   \item{Rhat_estimates}{95% quantiles of Rhat estimates}
#'   \item{Rmean}{Average of Rhat estimates}
#'   \item {smooth_filtered_posterior}{Smoothed and filtered posterior estimes}
#'
#' @export
#'
#' @examples
#' fit_epifilter_model(data = plover_data, type = "flu_a")
#'
epifilter_project <- function(Rgrid, grid_size, eta, n_days, smooth_filtered_posterior, Lam, Iday, pR0, nday) {
  pstate = matrix(NA_real_, nrow = grid_size, ncol = grid_size)
  for (j in 1:grid_size) {
    pstate[j, ] = dnorm(Rgrid[j], Rgrid, sqrt(Rgrid)*eta)
  }
  # Mean and CIs over R
  Rmed = rep(0, n_days); Rlow = Rmed; Rhigh = Rmed
  # Distribution of R over Rgrid
  Rdist = matrix(NA_real_, nrow = n_days, ncol = grid_size)
  # Initialise with last distribution-based prediction
  Rdist[1, ] = smooth_filtered_posterior[nrow(smooth_filtered_posterior),] %*% pstate
  Rdist[1, ] = Rdist[1, ]/sum(Rdist[1, ])

  # Update this distribution over time period horizon
  for (i in 2:n_days) {
    Rdist[i, ] = Rdist[i-1, ] %*% pstate
    # Normalise over grid
    Rdist[i, ] = Rdist[i, ]/sum(Rdist[i, ])
  }
  for (i in 1:n_days) {
    # CDF and quantiles
    Rcdf = cumsum(Rdist[i, ])
    id1 = which(Rcdf >= 0.5, 1); id2 = which(Rcdf >= 0.025, 1)
    id3 = which(Rcdf >= 0.975, 1)
    Rmed[i] = Rgrid[id1[1]]
    Rlow[i] = Rgrid[id2[[1]]]; Rhigh[i] = Rgrid[id3[[1]]]
  }
  # Mean and variance of distributions
  Rmean = Rdist*Rgrid; Rvar = Rdist*Rgrid^2 - Rmean^2
  # Prior variance
  Rvar0 = pR0*t(Rgrid^2) - (pR0*t(Rgrid))^2
  # Extract R statistics
  Rstats.mean = Rmean; Rstats.var = Rvar; Rstats.var0 = Rvar0
  # Quantiles over R
  Rhoriz = c(Rlow, Rmed, Rhigh)

  # Prediction of I across time
  # Sample size for drawing incidence predictions
  nsamp = 5000; Isamp = matrix(NA_real_, nrow = n_days, ncol = nsamp)

  # First case has exact Lam and computed directly
  Rsamp = sample(Rgrid, size = nsamp, prob = Rdist[1, ], replace = TRUE)
  Isamp[1, ] = rpois(nsamp, Rsamp*Lam[length(Lam)])

  # Sample total infectiousness and hence incidence
  for (i in 2:n_days) {
    # Sample from R distribution
    Rsamp = sample(Rgrid, size = nsamp, prob = Rdist[i, ], replace = TRUE)
    # Relevant part of serial distribution
    wdist = wdist[1:(nday+i-1)]

    # Non sampled part of total infectiousness
    Lsamp = wdist*Iday[seq(i-1, 1, -1)]; Lsamp = Lsamp*rep(1, nsamp)
    wdistRem = wdist[1:i-1]

    # Sample total infectiousness
    for (j in 1:i-1) {
      Lsamp = Lsamp + wdistRem[i-j]*Isamp[j, ]
    }
    # Project to incidence distribution
    Isamp[i, ] = rpois(Rsamp, Lsamp)

  }
  # Statistics of predictions
  Istats.mean = mean(Isamp, 2); Istats.var = var(Isamp);
  # Quantiles of incidence
  Ihoriz = quantile(Isamp);
}


#' Iterate through a time-period as a sliding window to produce short-term forecasts with the EpiEstim model fit
#'
#'
#' @description Function to produce short-term forecasts from objects of class {\code{\link[EpiEstim]{estimate_R}}}
#'
#' @param data *data frame* containing two columns: date and confirm (number of cases per week)
#' @param start_date_str Initial starting time-point. Must match a timepoint in the input dataset
#' @param n_days The number of days to run simulations for. Defaults to 7
#' @param type *character* Specifies type of epidemic. Must be one of "Influenza", "RSV" and "COVID"
#' @param aggregate_week *logical* argument specifying whether to aggregate forecasts by weekly quantiles. Default is to return daily quantiles
#' @param ... Pass on optional arguments from \code{fit_epiestim_model}
#'
#'
#'
#' @return List of class \code{forecast_time_period_epiestim}
#' storing quantiles of both daily and weekly forecasts from each sliding window
#' @export
#'
#' @examples
#'
#' #  Daily forecast
#' forecast_time_period_epiestim(
#'   data = weekly_transformed_plover_data,
#'   start_date_str = "2022-10-02", n_days = 14, type = "flu_a"
#' )
#'
# weekly aggregated forecast
#' forecast_time_period_epiestim(
#'   data = weekly_transformed_plover_data,
#'   start_date_str = "2022-10-02", n_days = 14, type = "flu_a", aggregate_week = TRUE
#' )
forecast_time_period_epiestim <- function(data, start_date_str, n_days = 7, time_period = "daily",
                                          type = NULL, ...) {
  sim <- week_date <- daily_date <- NULL

  stopifnot(
    "invalid time period, available options: 'daily', 'weekly'" =
      time_period %in% c("daily", "weekly")
  )
  if (!(lubridate::ymd(start_date_str) %in% data$date)) {
    stop("Start date not present in dataset. Please check your input")
  }

  if (lubridate::ymd(start_date_str) >= max(data$date)) {
    stop("Start date greater than max date in dataset! Please check your input")
  }
  start_index <- which(data$date == lubridate::ymd(start_date_str))
  time_length <- nrow(data) - start_index
  time_period <- seq_len(time_length)
  time_period_result <- lapply(time_period, function(tp) {
    model_data <- extend_rows_model_data(
      data = data, min_model_date_str = start_date_str,
      extension_interval = tp
    )
    print(paste0("Current time period: ", tp, " ", "(", max(model_data$date), ")"))
    cur_model <- fit_epiestim_model(model_data, type = type, ...)
    cur_daily_samples <- extract_daily_samples_epiestim_fit(data = model_data, model_fit = cur_model, n_days = n_days)
    cur_daily_samples <- cur_daily_samples %>%
      rename(daily_date = date, sim = sim, daily_incidence = incidence)

    model_data <- model_data %>%
      dplyr::rename(model_data_date = date)

    if (time_period == "weekly") {
      if (isFALSE(n_days %% 7 == 0)) {
        stop("n_days must be a multiple of 7 to aggregate by week")
      }
      cur_samples <- extract_agg_samples_epiestim_fit(cur_daily_samples)
      message("Note: Weekly quantiles were calculated across simulated epicurves")
      cur_samples_agg_quantiles <- cur_samples %>%
        create_quantiles(week_date, variable = "weekly_incidence") %>%
        dplyr::rename(quantile_date = week_date)
      quantile_unit <- "weekly"
      row <- c(cur_model, tp, model_data, cur_samples, cur_samples_agg_quantiles, quantile_unit = quantile_unit)
    } else {
      message("Note: Daily quantiles were calculated across simulated epicurves")
      cur_samples_agg_quantiles <- cur_daily_samples %>%
        create_quantiles(daily_date, variable = "daily_incidence") %>%
        dplyr::rename(quantile_date = daily_date)
      quantile_unit <- "daily"
      row <- c(cur_model, tp, model_data, cur_daily_samples, cur_samples_agg_quantiles, quantile_unit = quantile_unit)
    }

    return(row)
  })
  class(time_period_result) <- c("forecast_time_period_epiestim", class(time_period_result))
  return(time_period_result)
}

