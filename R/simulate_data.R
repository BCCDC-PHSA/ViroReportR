#' Simulate Daily Virus Incidence Data
#'
#' Generates simulated daily incidence data for specified respiratory viruses
#' over a defined number of days. Each virus is modeled using a Gaussian-like
#' curve, parameterized by peak day, amplitude, and scale.
#'
#' @param days Integer. Number of days to simulate (default is 365).
#' @param peaks Named numeric vector. Peak day for each virus
#' (e.g., `c("flua"=90,"rsv"=110,"covid"=160)`).
#' @param amplitudes Named numeric vector. Amplitude for each virus's peak
#' (e.g., `c("flua"=50,"rsv"=40,"covid"=20)`).
#' @param scales Named numeric vector. Scale controlling spread of the peak
#' for each virus (e.g., `c("flua"=-0.004,"rsv"=-0.005,"covid"=-0.001)`).
#'
#' @return A data frame with daily simulated incidence counts for each virus,
#' including a `date` column.
#' @examples
#' simulate_data()
#' simulate_data(days = 100, peaks = c(flua = 30), amplitudes = c(flua = 60),
#' scales = c(flua = -0.01))
#' @export
simulate_data <- function(days=365,
                          peaks = c("flua"=90,"rsv"=110,"covid"=160),
                          amplitudes=c("flua"=50,"rsv"=40,"covid"=20),
                          scales = c("flua"=-0.004,"rsv"=-0.005,"covid"=-0.001)
                          ){
  check_match_names(peaks,amplitudes,scales)
  # Define start date
  start_date <- as.Date("2024-01-01")  # Change if needed

  # Create a sequence of dates
  dates <- seq(start_date, by = "day", length.out = days + 1)
  time <- seq(0, days, by = 1)
  daily_data <- data.frame(date = dates)

  for(virus in names(peaks)){
    # Gaussian like peak
    mean_ <- amplitudes[virus] * exp( scales[virus] * (time - peaks[virus])^2)
    noise <-  rnorm(length(time), mean = 0, sd = 5)
    incidence <- floor(pmax(mean_ + noise, 0))
    daily_data[,virus] <- incidence
  }


  return(daily_data)

}

#' Check Matching Names in Named Vectors
#'
#' Internal utility function to verify that all input named vectors share the same names.
#'
#' @param ... Named numeric vectors to be compared.
#' @return Throws an error if name mismatches are found; otherwise, returns nothing.
#' @keywords internal
check_match_names <- function(...){
  vector_list <- as.list(...)
  name_list <- names(vector_list[[1]])
  for(vector in vector_list){
    missing_names <- setdiff(name_list,names(vector))
    if(length(missing_names)>0){
      stop("Missing name(s): ", paste(missing_names, collapse = ", "))
    }
  }
}


