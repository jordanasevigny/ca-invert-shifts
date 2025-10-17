# Load in lab review data and add missing longitudes
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 6/19/2025
# Updated: 07/02/2025
# 



rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(rnaturalearth)
library(geosphere)
library(grateful)
cite_packages(out.dir = ".")            # save report to working directory

# Load review data
df <- read.csv("preliminary_data_cleaning/lab_review_clean_missing_longitudes.csv")

# Add missing longitudes for observation points ---------------------------
# Load coastlines
coast <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")
# Bounding box for North America-ish
coast_na <- st_crop(coast, xmin = -170, xmax = -50, ymin = 5, ymax = 80)


# Define function to find longitude given latitude
find_lon <- function(given_lat) {
  # Create a line at given latitude
  lon_range <- seq(-180, -114, by = 0.1)
  line <- st_linestring(cbind(lon_range, rep(given_lat, length(lon_range))))
  line_sf <- st_sfc(line, crs = 4326)
  # Find nearest point on coast
  nearest <- st_nearest_points(line_sf, st_union(coast_na))
  intersection_point <- st_cast(nearest, "POINT")[2]
  lon <- st_coordinates(intersection_point)[1]
  return(lon)
}

# Apply longitude-finding code to any included datapoints missing lon data
df$latitude[df$latitude == ""] <- NA
df$longitude[df$longitude == ""] <- NA
df$latitude <- as.numeric(df$latitude)
df$longitude <- as.numeric(df$longitude)

df_t <- df %>%
  rowwise() %>%
  mutate(
    longitude2 = 
      if (include_exclude == 'Include' && (is.na(longitude) || longitude == "")) {
        find_lon(latitude)
      } else {
        longitude
      }
  ) %>%
  mutate( 
    longitude_reference = 
      if (include_exclude == 'Include' && (is.na(longitude) || longitude == "")) {
        "R generated"
      } else {
        longitude_reference
      }
  ) %>%
  ungroup()
df_t <- dplyr::select(df_t, -longitude)
df_t <- rename(df_t, longitude = longitude2)

# Check columns
colnames(df_t)
df_t <- df_t %>% relocate(longitude, .after = latitude_reference) # Move longitude column

# Write a new data sheet with the longitudes
write.csv(df_t, "processed_data/lab_review_clean.csv", row.names = FALSE)




# rm(list = ls())
# 
# # Load libraries
# library(dplyr)
# library(sf)
# library(rnaturalearth)
# library(geosphere)
# library(grateful)
# cite_packages(out.dir = ".")            # save report to working directory
# 
# # Load review data
# df <- read.csv("data/Screening & Review TRacking - Full-text Screening.csv")
# 
# # Add missing longitudes for observation points ---------------------------
# # Load coastlines
# coast <- ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf")
# # Bounding box for North America-ish
# coast_na <- st_crop(coast, xmin = -170, xmax = -50, ymin = 5, ymax = 80)
# 
# 
# # Define function to find longitude given latitude
# find_lon <- function(given_lat) {
#   # Create a line at given latitude
#   lon_range <- seq(-180, -114, by = 0.1)
#   line <- st_linestring(cbind(lon_range, rep(given_lat, length(lon_range))))
#   line_sf <- st_sfc(line, crs = 4326)
#   # Find nearest point on coast
#   nearest <- st_nearest_points(line_sf, st_union(coast_na))
#   intersection_point <- st_cast(nearest, "POINT")[2]
#   lon <- st_coordinates(intersection_point)[1]
#   return(lon)
# }
# 
# # Apply longitude-finding code to any included datapoints missing lon data
# df$lat_for_plot[df$lat_for_plot == ""] <- NA
# df$lon_for_plot[df$lon_for_plot == ""] <- NA
# df$lat_for_plot <- as.numeric(df$lat_for_plot)
# df$lon_for_plot <- as.numeric(df$lon_for_plot)
# 
# df_t <- df %>%
#   rowwise() %>%
#   mutate(
#     lon_for_plot2 = 
#       if (Include.Exclude == 'Include' && (is.na(lon_for_plot) || lon_for_plot == "")) {
#         find_lon(lat_for_plot)
#       } else {
#         lon_for_plot
#       }
#   ) %>%
#   mutate( 
#     Lon.for.plot.origin = 
#       if (Include.Exclude == 'Include' && (is.na(lon_for_plot) || lon_for_plot == "")) {
#         "R generated"
#       } else {
#         Lon.for.plot.origin
#       }
#   ) %>%
#   ungroup()
# df_t <- dplyr::select(df_t, -lon_for_plot)
# df_t <- rename(df_t, lon_for_plot = lon_for_plot2)
# 
# # Rename columns
# colnames(df_t)
# 
# colnames(df_t) <- c(
#   "paper_id",
#   "forward_backward",
#   "screener_initials",
#   "include_exclude",
#   "inconclusive_note",
#   "exclude_t0_calcofi_report",
#   "exclude_t1_not_marine_species",
#   "exclude_t2_outdated_pre1900",
#   "exclude_t3_not_poleward",
#   "exclude_t3_not_in_california",
#   "latin_name",
#   "species_reference",
#   "common_name",
#   "taxonomic_rank",
#   "life_stage_verbatim",
#   "planktonic_larval_duration",
#   "spawn_season",
#   "observation_location_verbatim",
#   "latitude_verbatim",
#   "longitude_verbatim",
#   "latitude",
#   "latitude_location",
#   "longitude_location",
#   "year",
#   "month",
#   "day",
#   "time",
#   "historical_northern_latitude_archive",
#   "historical_latitude_origin",
#   "historical_north_range_verbatim",
#   "historical_north_range_reference",
#   "screening_notes",
#   "extension_confidence_criteria",
#   "longitude"
# )
# 
# # Write a new data sheet with the longitudes
# write.csv(df_t, "processed_data/lab_review_with_longitudes.csv", row.names = FALSE)

