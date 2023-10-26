#' fit_epiestim_model - Function to estimate the reproduction number of an epidemic from PLOVER data
#' 
#' @description A wrapper function for {\code{\link[EpiEstim]{estimate_R}}} from the \code{EpiEstim} library to estimate the reproduction number of epidemics to support short-term forecasts
#' 
#' 
#' @details \code{fit_epiestim_model} currently supports the following epidemics: Influenza, RSV and COVID-19. The serial intervals for the estimation of R were retrieved from 
#' [Cowling et al., 2011], [Vink et al., 2014] and [Madewell et al., 2023] for Influenza, RSV and COVID (BA.5 Omicron variant) respectively
#' 
#' 
#' @param data *data frame* containing two columns: date and confirm (number of cases per day)
#' @param dt *Integer* Number of days aggregated (set to 7 by default for weekly aggregate)
#' @param Type *character* Specifies type of epidemic. Must be one of "Influenza", "RSV" and "COVID"
#' @param mean_si *Numeric* User specification of mean of parametric serial interval 
#' @param std_si *Numeric* User specification of standard deviation of parametric serial interval 
#' @param ... Optional parameters to pass on to {\code{\link[EpiEstim]{estimate_R}}} (see help page) to control estimation of reproduction number

fit_epiestim_model <- function(data, dt = 7L, type = NULL, mean_si = NULL, std_si = NULL, recon_opt = "match",
                               method = "parametric_si", ...) {
  if(isFALSE(is.data.frame(data)) | isFALSE(colnames(data) %in% c("date", "confirm")) ) {
    stop("Must pass a data frame with two columns: date and confirm")
  }
  if(missing(type)) {
    stop("Must specify type of epidemic (Influenza, RSV or COVID)")
  }
 if(!(type %in% c("Influenza", "RSV", "COVID")) ) {
   stop("Must specify type of epidemic (Influenza, RSV or COVID)")
  }
  incid <- data$confirm
  if(is.null(mean_si) && is.null(std_si)) {
    if (type == "Influenza") {
      config <- make_config(list(mean_si = 3.6,
                                 std_si = 1.6))
    } else if (type == "RSV") {
      config <- make_config(list(mean_si = 7.5,
                                 std_si = 2.1))
    } else if (type == "COVID") { 
      config <- make_config(list(mean_si = 2.3,
                                 std_si = 1.4))
    }
  } else {
    config <- make_config(list(mean_si = mean_si,
                               std_si = std_si))
  }
  a_prior <- (config$mean_prior / config$std_prior)^2
  min_nb_cases_per_time_period <- ceiling(1 / config$cv_posterior^2 - a_prior)
  tryCatch(
    {
  epiestim_estimates <- EpiEstim::estimate_R(incid = incid,
                                                 dt = dt,
                                                 recon_opt = recon_opt,
                                                 method = method,
                                                 config = config)
  return(epiestim_estimates)
    },
  warning = function(warning_message) {
    min_reliable_date <- data %>% filter(confirm >=  min_nb_cases_per_time_period) %>% pull(date)
  
    message("Incidence too low on current start date. Consider 
            starting R estimation from ", min_reliable_date[1] , " for accurate estimate of reproduction number
            with EpiEstim")
  return(epiestim_estimates)
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