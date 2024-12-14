
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)

# Filter the data for January temperatures in the specified states
january_temps <- initial_data %>%
filter(Month == 1 & Location %in% c("Iowa", "Maine", "Oklahoma", "Washington"))

# Generate boxplot
ggplot(january_temps, aes(x = Location, y = Temp)) +
geom_boxplot() +
labs(title = "Distribution of Historical January Temperatures",
y = "Temperature (Fahrenheit)",
x = "State" )

# Calculate summary statistics for each state
summary_stats <- january_temps %>%
group_by(Location) %>%
summarize(
median_temp = median(Temp),
min_temp = min(Temp),
max_temp = max(Temp)
)

# Rank the states from coldest to warmest based on the median temperature
ranked_states <- summary_stats %>%
arrange(median_temp)

# Filter the dataset to include only August temperatures for Oklahoma
oklahoma_august_temps <- initial_data %>%
filter(Month == 8 & Location == "Oklahoma")

# Calculate temperature anomaly
oklahoma_august_temps <- oklahoma_august_temps %>%
mutate(Temperature_anomaly = Temp - Mean20C)

# Generate time series graph with smoothed trend line
ggplot(oklahoma_august_temps, aes(x = Year, y = Temperature_anomaly)) +
geom_line() + # Plot the data
geom_smooth() + # Add a smoothed trend line
labs(title = "August Temperature Anomaly Change in Oklahoma (1895 - Present)",
x = "Year",
y = "Temperature Anomaly (°F)")

# Filter the dataset to include only temperatures for Wyoming from 1980 to present
wyoming_temps <- initial_data %>%
filter(Location == "Wyoming" & Year >= 1980)

# Generate faceted time series graph
ggplot(wyoming_temps, aes(x = Year, y = Temp)) +
geom_line() + # Plot the data
facet_wrap(~ MonthName, nrow = 4) + # Facet by MonthName with 4 rows
geom_smooth() + # Add a smoothed trend line
labs(title = "Temperature Trends in Wyoming (1980 - Present)",
y = "Temperature (°F)",
x = "Year")
