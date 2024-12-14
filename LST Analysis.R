library(sf)
library(raster)
library(tidyverse)
library(readr)
library (terra)
library (sp)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(dplyr)
library(lubridate)

# Read vector data and filter for Oklahoma counties
ok_counties <- st_read("cb_2018_us_county_20m.shp") %>%
filter(str_detect(GEOID, '^40'))

# Load raster data and convert LST from Kelvin to Celsius
day_lst_raster <-
terra::rast("MOD11A2.006_LST_Day_1km_doy2012177_aid0001.tif") * 0.02 - 273.15
night_lst_raster <-
terra::rast("MOD11A2.006_LST_Night_1km_doy2012177_aid0001.tif") * 0.02 - 273.15

# Transform CRS of vector data to match the CRS of raster data
vector_data_transformed <- st_transform(ok_counties, crs = terra::crs(day_lst_raster))

# Rasterize transformed vector data
cnty_ras <- terra::rasterize(vector_data_transformed, day_lst_raster, field = "GEOID")

# Calculate Zonal Statistics for Day and Night Temperatures
day_temp_stats <- terra::zonal(day_lst_raster, cnty_ras, fun = "mean", na.rm = TRUE)
night_temp_stats <- terra::zonal(night_lst_raster, cnty_ras, fun = "mean", na.rm =
TRUE)

# Rename columns for clarity
day_temp_stats <- rename(day_temp_stats, day_temp = 2)
night_temp_stats <- rename(night_temp_stats, night_temp = 2)

# Left join zonal stats back to spatial vector data
cnty_join_day <- left_join(vector_data_transformed, day_temp_stats, by = "GEOID")
cnty_join_night <- left_join(vector_data_transformed, night_temp_stats, by =
"GEOID")

# Function to create temperature plots
create_temp_plot_with_labels <- function(data, temp_column, title) {
ggplot(data) +
geom_sf(aes(fill = get(temp_column)), size = 0.05, color = 'darkblue') +
labs(title = title) +
theme_minimal()
}

# Plot Day Time Temperature
create_temp_plot_with_labels(cnty_join_day, "day_temp", "Day Temperature (C)")

# Plot Night Time Temperature
create_temp_plot_with_labels(cnty_join_night, "night_temp", "Night Temperature
(C)")

# Step 1: Load and prepare Mesonet data
meso <- read_csv("mesodata_large.csv") %>%
mutate(TMAX = na_if(TMAX, -999),
TMIN = na_if(TMIN, -999)) %>%
filter(YEAR == 2012, MONTH == 6) %>%
group_by(STID) %>%
summarize(mean_max_temp = mean(TMAX, na.rm = TRUE),
mean_min_temp = mean(TMIN, na.rm = TRUE)) %>%
drop_na()

# Step 2: Load geographic information and assign CRS
geo_coords <- read_csv("geoinfo.csv") %>%
st_as_sf(coords = c("lon", "lat"), crs = 4269)

# Filter geo_coords to include only stations present in meso_june_2012
stations <- unique(meso$STID)
geo_coords_filtered <- geo_coords %>%
filter(stid %in% stations)

# Step 3: Extract LST values for each station
day_lst_values <- extract(day_lst_raster, geo_coords_filtered)
night_lst_values <- extract(night_lst_raster, geo_coords_filtered)

# Add LST values to the geo_coords_filtered dataframe
geo_coords_filtered$day_lst <- day_lst_values[[2]]
geo_coords_filtered$night_lst <- night_lst_values[[2]]

# Merge LST values with Mesonet summaries
combined_data <- left_join(meso, geo_coords_filtered[, c("stid", "day_lst",
"night_lst")], by = c("STID" = "stid"))

# Step 4: Generate scatterplots
# Day LST vs. Max Air Temperature
ggplot(combined_data, aes(x = day_lst, y = mean_max_temp)) +
geom_point() +
geom_smooth(method = "lm", color = "blue") +
labs(title = "Day LST vs. Max Air Temperature", x = "Day LST", y = "Max Air
Temperature")

# Night LST vs. Min Air Temperature
ggplot(combined_data, aes(x = night_lst, y = mean_min_temp)) +
geom_point() +
geom_smooth(method = "lm", color = "red") +
labs(title = "Night LST vs. Min Air Temperature", x = "Night LST", y = "Min Air
Temperature")

# Step 5: Perform Linear Regression Analysis
# Max Air Temp vs. Day LST
lm_max_day <- lm(mean_max_temp ~ day_lst, data = combined_data)
summary(lm_max_day)

# Min Air Temp vs. Night LST
lm_min_night <- lm(mean_min_temp ~ night_lst, data = combined_data)
summary(lm_min_night)
