set.seed(42)  # For reproducibility

# Generate time variable (1 year, 365 days)
days <- 365
time <- seq(0, days, by = 1)  

# Define seasonal sinusoidal patterns
peak_day_flua <- 90   # Flu peaks in winter (day 90)
peak_day_rsv <- 110   # RSV peaks slightly later (day 150)
peak_day_covid <- 160 # COVID waves peak later (day 250)

amplitude_flua <- 50  # Max cases per 100,000
amplitude_rsv <- 40
amplitude_covid <- 20

# Mean incidence modeled as a sinusoidal function
mean_flua <- amplitude_flua * exp(-0.004 * (time - peak_day_flua)^2)  # Gaussian-like peak
mean_rsv <- amplitude_rsv * exp(-0.005 * (time - peak_day_rsv)^2)
mean_covid <- amplitude_covid * exp(-0.001 * (time - peak_day_covid)^2)

# Add normally distributed noise
noise_flua <- rnorm(length(time), mean = 0, sd = 5)
noise_rsv <- rnorm(length(time), mean = 0, sd = 5)
noise_covid <- rnorm(length(time), mean = 0, sd = 5)

# Generate observed incidence
flua <- floor(pmax(mean_flua + noise_flua, 0))
rsv <- floor(pmax(mean_rsv + noise_rsv, 0))
covid <- floor(pmax(mean_covid + noise_covid, 0))

# Debugging prints
print("First few values of flua incidence:")
print(head(flua))
print("First few values of rsv incidence:")
print(head(rsv))
print("First few values of covid incidence:")
print(head(covid))
# Define start date
start_date <- as.Date("2024-01-01")  # Change if needed

# Create a sequence of dates
dates <- seq(start_date, by = "day", length.out = days + 1)

# Create a data frame
daily_data <- data.frame(date = dates, 
                            flu_a = flua,
                            rsv = rsv,
                            covid = covid)



# Plot results
library(ggplot2)
ggplot(daily_data, aes(x = date)) +
  geom_line(aes(y = flu_a, color = "FluA")) +
  geom_line(aes(y = rsv, color = "RSV")) +
  geom_line(aes(y = covid, color = "COVID")) +
  labs(title = "Simulated FluA, RSV, and COVID Incidence Over Time",
       x = "Time (Days)",
       y = "Incidence (Cases per 100,000)",
       color = "Disease") +
  theme_minimal()

# Display first few rows of the dataset
head(daily_data)

save("daily_data", file = "data/daily_data.rda")
