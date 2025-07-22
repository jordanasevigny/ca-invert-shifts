# Merge CalCOFI and lab reviews & add in historical latitudes 
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025

# Library
library(dplyr)

# Load data
ca_rev <- read.csv("processed_data/calcofi_review_data_clean.csv")
lab_rev <- read.csv("processed_data/lab_review_with_longitudes.csv")
hist_dist <- read.csv("processed_data/historical-distributions-clean.csv")

# Filter to included data
ca_rev_inc <- filter(ca_rev, include_exclude == "Include")
lab_rev_inc  <- filter(lab_rev, include_exclude == "Include")

# Filter out the low confidence extensions from the lab review
lab_rev_inc <- filter(lab_rev_inc, extension_confidence_criteria != "Opportunistic")

# Merge like columns of the two datasets

# Identify shared columns
shared_cols <- intersect(names(ca_rev_inc), names(lab_rev_inc))

# Select only shared columns and add a source column
ca_rev_inc_common <- ca_rev_inc[, shared_cols] %>%
  mutate(source = "ca_rev")

lab_rev_inc_common <- lab_rev_inc[, shared_cols] %>%
  mutate(source = "lab_rev")

# Convert data types
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(year, as.integer))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(year, as.integer))
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(day, as.character))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(day, as.character))

# Row-bind the two
merged_df <- bind_rows(ca_rev_inc_common, lab_rev_inc_common)

# Drop Include Exclude column
merged_df <- merged_df %>%
  select(-include_exclude)

# Make latin names uppercase
merged_df$latin_name <- gsub("^(\\w)", "\\U\\1", merged_df$latin_name, perl = TRUE)

# # Find average latitude for historical range edge THIS DOESN'T WORK YET
# merged_df_histave <- merged_df %>%
#   group_by(latin_name) %>%
#   mutate(average_hist_latitude = mean(historical_northern_latitude, na.rm=TRUE))

# Make species / genus into one category if likely shared (e.g. pyrosomes, velella)
unique(merged_df$latin_name)
merged_df$latin_name_original <- merged_df$latin_name
merged_df_histedge <- merged_df %>%
  mutate(latin_name = case_when(
    str_detect(latin_name, regex("thetys", ignore_case = TRUE)) ~ "Thetys",
    TRUE ~ latin_name
  )) %>%
  mutate(latin_name = case_when(
    str_detect(latin_name, regex("velella", ignore_case = TRUE)) ~ "Velella",
    TRUE ~ latin_name
  )) %>%
  mutate(latin_name = case_when(
    str_detect(latin_name, regex("Pyrosoma", ignore_case = TRUE)) ~ "Pyrosoma",
    TRUE ~ latin_name
  ))
#unique(merged_df_histedge$latin_name)

# Merge historical distributions with extension dataset
merged_df_histedge <- merged_df_histedge %>%
  left_join(hist_dist, by="latin_name")

# Drop any dataspoints prior to 1900
merged_df_histedge <- merged_df_histedge %>%
  filter(year >= 1900)

# Add missing longitudes for historical latitudes ---------------------------
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
  # Find nearest point on cost
  nearest <- st_nearest_points(line_sf, st_union(coast_na))
  intersection_point <- st_cast(nearest, "POINT")[2]
  lon <- st_coordinates(intersection_point)[1]
  return(lon)
}

merged_df_histedge_lon <- merged_df_histedge %>%
  rowwise() %>%
  mutate(
    hist_range_lon = find_lon(hist_range_lat)) %>%
  ungroup()

# Choose the northernmost observation for each species/year combo
north_df <- merged_df_histedge_lon %>%
  group_by(year, latin_name) %>%
  slice_max(order_by = latitude, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(!is.na(latin_name))

# Make extension event ids
ext_ids <- north_df %>%
  arrange(latin_name, year) %>%
  group_by(latin_name) %>%
  mutate(
    year_diff = year - lag(year, default = first(year)),
    new_group = if_else(year_diff > 1, 1, 0),
    group_id = cumsum(new_group)
  ) %>%
  ungroup() %>%
  select(-c(year_diff, new_group)) %>%
  group_by(latin_name, group_id) %>%
  mutate(first_year = min(year)) %>%
  ungroup()

# Write dataframe
write.csv(merged_df_histedge, "processed_data/merged_calcofi_lab_review.csv")
          