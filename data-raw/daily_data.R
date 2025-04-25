## code to prepare `daily_data` dataset goes here
set.seed(42)  # For reproducibility

daily_data <- simulate_data()


# Plot results
library(ggplot2)
ggplot(daily_data, aes(x = date)) +
  geom_line(aes(y = flua, color = "FluA")) +
  geom_line(aes(y = rsv, color = "RSV")) +
  geom_line(aes(y = covid, color = "COVID")) +
  labs(title = "Simulated FluA, RSV, and COVID Incidence Over Time",
       x = "Time (Days)",
       y = "Incidence (Cases per 100,000)",
       color = "Disease") +
  theme_minimal()

# Display first few rows of the dataset
head(daily_data)

usethis::use_data(daily_data, overwrite = TRUE)
