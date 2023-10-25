#' fit_epiestim_model - Function to estimate the reproduction number of an epidemic from PLOVER data
#' 
#' @description A wrapper function for {\code{\link[EpiEstim]{estimate_R}}} from the \code{EpiEstim} library to estimate the reproduction number of epidemics to support short-term forecasts
#' 
#' 
#' @details \code{fit_epiestim_model} currently supports the following epidemics: Influenza, RSV and COVID-19
#' 
#' 
#' @param data *data frame* containing two columns: date and confirm (number of cases per day)
#' @param dt *Integer* Number of days aggregated (set to 7 by default for weekly aggregate)
#' @param Type *character* Specifies type of epidemic. Must be one of "Influenza", "RSV" and "COVID-19"


fit_epiestim_model <- function(data, dt = 7L, type = NULL, ...) {
  if(!isTRUE(is.data.frame(data) && ncol(data) == 2)) {
    stop("Must pass a data frame with two columns: date and confirm")
  }
  if(is.null(type)) {
    stop("Must specify type of epidemic")
  }
  incid <- data$confirm
  if (type = "Influenza") {
  config <- make_config(list(mean_si = 3.6,
                               std_si = 1.6))
  } else if (type = "RSV") {
    # TO DO: Scan literature for serial interval values 
    config <- make_config(list(mean_si = 5.6,
                               std_si = 1.6))
  } else if (type = "COVID-19") { 
    # TO DO: Scan literature for serial interval values 
    config <- make_config(list(mean_si = 5.6,
                               std_si = 1.6))
  }
  tryCatch(
    {
  epiestim_estimates <- EpiEstim::estimate_R(incid = incid,
                                                 dt = dt,
                                                 recon_opt = "match",
                                                 method = method,
                                                 config = config)
  return(epiestim_estimates)
    },
  warning = function(warning_message)
    # TO DO: confirm number of time-points 
    message("Number of timepoints used for forecasting too low. Consider 
            increasing to atleast 14 timepoints for accurate estimate of R
            with EpiEstim")
  message(warning_message)
  return(NA)
  }
) 
 
}



# Extract Plot Data (forecast only) ---------------------------------------

#' Extract weekly samples (aggregated by date & sim#)
#'
#' @param fit EpiEstim Model output
#' @param model_data training data with 2 columns: date & confirm
#'
#' @return
#' @export
#'
#' @examples
extract_daily_samples_epiestim_fit <- function(data, fit, dt = 7L, n_days = 14, n_sim = 1000, ...){
  model_data_linelist <- 
    tibble(date = seq(min(data$date), 
                      max(data$date) + lubridate::days(dt-1), 
                      by = "1 day"),
           confirm = fit$I) %>% 
    group_by(date) %>% 
    reframe(case_index = seq(1:confirm))

 incidence_obj <- incidence::incidence(model_data_linelist$date)
 
r_vals <- tail(fit$R,n=1)
r_dist <- rnorm(100,mean = r_vals$`Mean(R)`,sd = r_vals$`Std(R)`)
  
  # Use the project function
proj <- project(incidence_obj,
                  R = r_dist, 
                  si = fit$si_distr[-1],
                  n_sim = n_sim,
                  n_days = n_days, 
                  R_fix_within = FALSE) # keep the same value of R every day (TRUE/FALSE)
  
  
data_proj <- as.data.frame(proj, long = TRUE)
  
 return (data_proj)
}


#' fit_epiestim_model - Function to estimate the reproduction number of an epidemic from PLOVER data
#' 
#' @description A wrapper function for {\code{\link[EpiEstim]{estimate_R}}} from the \code{EpiEstim} library to estimate the reproduction number of epidemics to support short-term forecasts
#' 
#' 
#' @details \code{fit_epiestim_model} currently supports the following epidemics: Influenza, RSV and COVID-19
#' 
#' 
#' 
#' 
epiestim_fit_predict <- function(start_date_str = , end_date_str = , time_period = "weekly", 
                                 ){
  n = length(time_period)
  list = vector("list", length = n)
  
  for (i in 1:n) {
    print(paste0("cur time_period : " , time_period[[i]]))
    # extract model data with the given time period of interest
    model_data <- extract_model_data_with_extension(start_date_str,
                                                    end_date_str,
                                                    time_period[[i]],
                                                    type)
    # fit model [diff]
    cur_model <- fit_epiestim(model_data)
    
    # extract current 1000 samples [diff]
    cur_daily_samples <- extract_daily_samples_epiestim_fit(cur_model, model_data,trunc_days)
    cur_samples <- extract_agg_samples_epiestim_fit(cur_daily_samples)
    
    cur_daily_samples <- cur_daily_samples %>%
      rename(daily_date = date, daily_sim = sim, daily_value = incidence)
    
    # extract samples quantiles
    cur_samples_quantiles <- cur_samples %>% 
      create_quantiles(week_date,variable = "value") %>%
      rename(quantile_week_date = week_date)
    
    model_data <- model_data %>%
      rename(model_data_date = date)
    
    # at each iteration, we will create the below output
    row <- c(cur_model, i+1, model_data, cur_daily_samples, cur_samples, cur_samples_quantiles)
    
    # append the row to the list
    list[[i]] <- row
  }
  
}