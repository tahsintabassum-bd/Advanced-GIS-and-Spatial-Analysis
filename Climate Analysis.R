
install.packages(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)

okcounty <- st_read("okcounty.shp")
climate_data <- read.csv("county_climate_2016.csv")
okcounty<- within(okcounty, {
Date <- as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")
})

# Time Series Graph for Cleveland County
# Filter climate data for Cleveland County
cleveland_data <- climate_data %>%
filter(AreaCode == 40027)

# Pivot data into long format
cleveland_data_long <- cleveland_data %>%
pivot_longer(cols = c(MinAirTemperature, MaxAirTemperature, MeanAirTemperature),
names_to = "Temperature_Type",
values_to = "Temperature_Value")
cleveland_data_long <- within(cleveland_data_long, {
Date <- as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")
})

# Generate time series graph
ggplot(cleveland_data_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type))
+
geom_line() +
labs(title = "Time Series of Daily Air Temperatures in Cleveland County",
x = "Date", y = "Temperature (°C)") +
theme_minimal()

# Precipitation Maps for Oklahoma Counties
# Filter climate data for Oklahoma counties
oklahoma_data <- climate_data %>%
filter(AreaCode >= 40000 & AreaCode < 41000)
oklahoma_data<- within(oklahoma_data, {
Date <- as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d")
})

# Group and summarize data to calculate total monthly precipitation
monthly_precipitation <- oklahoma_data %>%
mutate(Date = as.Date(Date)) %>%
group_by(AreaCode, Month = format(Date, "%Y-%m-%d")) %>%
summarise(TotalPrecipitation = sum(TotalPrecipitation))

# Spatial join with Oklahoma county shapefile
oklahoma_county_data <- left_join(okcounty, monthly_precipitation,
by = c("FIPS" = "AreaCode"))

# Generate faceted precipitation maps

ggplot(oklahoma_county_data) +
geom_sf(aes(fill = TotalPrecipitation)) +
facet_wrap(~Month, nrow = 3) +
scale_fill_viridis_c(option = "magma", name = "Total Precipitation (mm)",
na.value = "grey90") +
labs(title = "Total Precipitation by County in Oklahoma (2016)",
caption = "Data source: county_climate_2016.csv",
fill = "Total Precipitation (mm)") +
theme_minimal() +
theme(axis.text = element_blank(),
axis.title = element_blank(),
plot.title = element_text(size = 12, face = "bold"),
plot.caption = element_text(size = 8))
