# Extension supplementary table
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 10/08/2025

rm(list = ls())

# Load libraries
library("ggplot2")
theme_set(theme_bw())
library(dplyr)
library(geosphere)
library(gt)


# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Calculate extension distance
# Distance function
get_distance_km <- function(lat1, lon1, lat2, lon2) {
  point1 <- c(lon1, lat1)
  point2 <- c(lon2, lat2)
  dist_meters <- distHaversine(point1, point2)
  return(dist_meters / 1000)  # convert to kilometers
}

# Apply function row-wise
ext_distance <- df %>%
  rowwise() %>%
  mutate(distance_km = get_distance_km(hist_range_lat, hist_range_lon, latitude, longitude)) %>%
  ungroup()

# Average/sd/max/mix extension distance 1) by max per event 2) by all observations
extensions <- ext_distance %>%
  group_by(latin_name, group_id, first_year) %>%
  summarise(max_dist = max(distance_km, na.rm = TRUE)) 


summary_table <- extensions %>%
  group_by(latin_name) %>%
  summarise(
    mean_extension = mean(max_dist, na.rm = TRUE),
    extension_years = paste(sort(unique(first_year)), collapse = ", ")
  ) %>%
  arrange(latin_name) %>%
  # round the mean
  mutate(mean_extension = round(mean_extension, 1))

# 
# # Make it look nice for publication
# gt_table <- summary_table %>%
#   gt() %>%
#   fmt_number(columns = mean_extension, decimals = 1) %>%
#   cols_label(
#     latin_name = "Species",
#     mean_extension = "Mean Extension Distance (km)",
#     extension_years = "Years Observed"
#   ) %>%
#   tab_header(
#     title = "Summary of Range Extension Events",
#     subtitle = "Mean northward extension distance and extension year observed per species"
#   ) %>%
#   tab_options(
#     table.font.names = "Helvetica",
#     data_row.padding = px(4),
#     table.font.size = 12,
#     heading.align = "left"
#   )
# gtsave(gt_table, "figures/extension_table.png")

# Find midpoint (or choose your own row cutoff)
n <- nrow(summary_table)
half <- ceiling(n / 2)

# Split into two data frames
summary_table1 <- summary_table[1:half, ]
summary_table2 <- summary_table[(half + 1):n, ]

# Table 1
gt_table1 <- summary_table1 %>%
  gt() %>%
  fmt_number(columns = mean_extension, decimals = 1) %>%
  cols_label(
    latin_name = "Species",
    mean_extension = "Mean Extension Distance (km)",
    extension_years = "Extension Years"
  ) %>%
  tab_header(
    title = "Summary of Range Extension Events (Part 1)",
    subtitle = "Mean northward extension distance and extension year per species"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )
gtsave(gt_table1, "figures/extension_table_part1.png")


# Table 2
gt_table2 <- summary_table2 %>%
  gt() %>%
  fmt_number(columns = mean_extension, decimals = 1) %>%
  cols_label(
    latin_name = "Species",
    mean_extension = "Mean Extension Distance (km)",
    extension_years = "Extension Years"
  ) %>%
  tab_header(
    title = "Summary of Range Extension Events (Part 2)",
    subtitle = "Mean northward extension distance and extension year per species"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )
gtsave(gt_table2, "figures/extension_table_part2.png")