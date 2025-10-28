# Merge CalCOFI and lab reviews & add in historical latitudes 
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025

# Must have already run load_lab_review.R if changes were made to the raw data
# Run this script to merge calcofi, lab review, and historical lats
# After this script is run (does not need to be rerun if no changes are made to raw data),
# you can run analysis scripts

rm(list = ls())

# Library
library(dplyr)
library(rnaturalearth)
library(sf)

# Load data
ca_rev <- read.csv("processed_data/calcofi_review_data_clean.csv")
lab_rev <- read.csv("processed_data/lab_review_with_longitudes.csv")
hist_dist <- read.csv("processed_data/historical_distributions_clean.csv")

# Filter to included data
ca_rev_inc <- filter(ca_rev, include_exclude == "Include")
lab_rev_inc  <- filter(lab_rev, include_exclude == "Include")

# Filter out the low confidence extensions from the lab review
lab_rev_inc <- filter(lab_rev_inc, extension_confidence_criteria != "Opportunistic")

# Identify shared columns
shared_cols <- intersect(names(ca_rev_inc), names(lab_rev_inc))

# Select only shared columns and add a source column
ca_rev_inc_common <- ca_rev_inc[, shared_cols] %>%
  mutate(source = "ca_rev") %>%
  mutate(calcofi_report_yr = ca_rev_inc$report_year) %>%
  mutate(paper_id = NA)
lab_rev_inc_common <- lab_rev_inc[, shared_cols] %>%
  mutate(source = "lab_rev") %>%
  mutate(calcofi_report_yr = NA) %>%
  mutate(paper_id = lab_rev_inc$paper_id)

# Convert data types
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(year, as.integer))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(year, as.integer))
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(day, as.character))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(day, as.character))
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(calcofi_report_yr, as.character))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(calcofi_report_yr, as.character))
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(paper_id, as.character))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(paper_id, as.character))

# Row-bind the two
merged_df <- bind_rows(ca_rev_inc_common, lab_rev_inc_common)

# Drop Include Exclude column
merged_df <- merged_df %>%
  dplyr::select(-include_exclude)

# Make latin names uppercase & taxa names lowercase
merged_df$latin_name <- gsub("^(\\w)", "\\U\\1", merged_df$latin_name, perl = TRUE)
merged_df$taxonomic_rank <- tolower(merged_df$taxonomic_rank)

# # Make species / genus into one category if likely shared (e.g. pyrosomes, velella)
# unique(merged_df$latin_name)
# merged_df$latin_name_original <- merged_df$latin_name
# merged_df_histedge <- merged_df %>%
#   mutate(latin_name = case_when(
#     str_detect(latin_name, regex("thetys", ignore_case = TRUE)) ~ "Thetys",
#     TRUE ~ latin_name
#   )) %>%
#   mutate(latin_name = case_when(
#     str_detect(latin_name, regex("velella", ignore_case = TRUE)) ~ "Velella",
#     TRUE ~ latin_name
#   )) %>%
#   mutate(latin_name = case_when(
#     str_detect(latin_name, regex("Pyrosoma", ignore_case = TRUE)) ~ "Pyrosoma",
#     TRUE ~ latin_name
#   ))
#unique(merged_df_histedge$latin_name)
merged_df_histedge <- merged_df
# Merge historical distributions with extension dataset
merged_df_histedge <- merged_df_histedge %>%
  left_join(hist_dist, by="latin_name")

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

# Filter to make sure observations are further north than historical range edge
north_df_filt <- north_df %>%
  filter(latitude > hist_range_lat)

# Make extension event ids
ext_ids <- north_df_filt %>%
  arrange(latin_name, year) %>%
  group_by(latin_name) %>%
  mutate(
    year_diff = year - lag(year, default = first(year)),
    new_group = if_else(year_diff > 1, 1, 0),
    group_id = cumsum(new_group) # group_id is is the extension event if per species
  ) %>%
  ungroup() %>%
  dplyr::select(-c(year_diff, new_group)) %>%
  group_by(latin_name, group_id) %>%
  mutate(first_year = min(year)) %>% # first year of each extension event
  ungroup() %>%
  rename(hist_lat_source = resource) %>%
  rename(obs_source = source)

# nrow(ext_ids %>%
#   group_by(latin_name) %>%
#   distinct(group_id))

# # Counting up where hist range edge info came from
# d <- ext_ids %>%
#   dplyr::select(latin_name, resource) %>%
#   distinct() %>%
#   count(resource)
# sum(d$n)

# Write dataframe
write.csv(ext_ids, "processed_data/merged_calcofi_lab_review.csv")
          