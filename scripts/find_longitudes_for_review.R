# Add missing longitudes to lab review data
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 6/19/2025

# Load libraries
library(dplyr)
library(sf)
library(rnaturalearth)
library(geosphere)


# Load review data
df <- read.csv("data/Screening & Review TRacking - Full-text Screening.csv")

# Load coastlines
coast <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")
# Bounding box for North America-ish
coast_na <- st_crop(coast, xmin = -170, xmax = -50, ymin = 5, ymax = 80)


# Define funciton to find longitude given latitude
find_lon <- function(given_lat) {
  # Create a line at given latitude
  lon_range <- seq(-180, -114, by = 0.1)
  line <- st_linestring(cbind(lon_range, rep(given_lat, length(lon_range))))
  line_sf <- st_sfc(line, crs = 4326)
  # Find nearest point on cost
  nearest <- st_nearest_points(line_sf, st_union(coast_na))
  intersection_point <- st_cast(nearest, "POINT")[2]
  lon <- st_coordinates(intersection_point)[1]
  return(lon)
}

# Apply longitude-finding code to any included datapoints missing lon data
df$lat_for_plot[df$lat_for_plot == ""] <- NA
df$lon_for_plot[df$lon_for_plot == ""] <- NA
df$lat_for_plot <- as.numeric(df$lat_for_plot)
df$lon_for_plot <- as.numeric(df$lon_for_plot)

df_t <- df %>%
  rowwise() %>%
  mutate(
    lon_for_plot2 = 
      if (Include.Exclude == 'Include' && (is.na(lon_for_plot) || lon_for_plot == "")) {
        find_lon(lat_for_plot)
      } else {
        lon_for_plot
      }
  ) %>%
  ungroup()
df_t <- select(df_t, -lon_for_plot)
df_t <- rename(df_t, lon_for_plot = lon_for_plot2)


# Write a new data sheet with the longitudes
write.csv(df_t, "processed_data/lab_review_with_longitudes.csv", row.names = FALSE)

