# Historical Latitude Supp. Table 
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 10/28/2025

rm(list = ls())

# Libraries
library("ggplot2")
theme_set(theme_bw())
library(dplyr)
library(geosphere)
library(gt)

# Load historical data
data = read.csv("processed_data/historical_distributions_clean.csv")
unique(data$resource)
 
# # Make it look nice for publication
# gt_table <- data %>%
#   gt() %>%
#   cols_label(
#     latin_name = "Species",
#     hist_range_lat = "Historical Poleward Range Edge Latitude",
#     resource = "Source"
#   ) %>%
#   tab_header(
#     title = "Summary of Historical Poleward Range Edge Latitudes",
#   ) %>%
#   tab_options(
#     table.font.names = "Helvetica",
#     data_row.padding = px(4),
#     table.font.size = 12,
#     heading.align = "left"
#   )
# 
# gtsave(gt_table, "figures/hist_lat_supp_table.png")

# Find midpoint (or choose your own row cutoff)
n <- nrow(data)
half <- ceiling(n / 2)

# Split into two data frames
data1 <- data[1:half, ]
data2 <- data[(half + 1):n, ]

# Table 1
gt_table1 <- data1 %>%
  gt() %>%
  fmt_number(
    columns = hist_range_lat,
    decimals = 2
  ) %>%
  cols_label(
    latin_name = "Species",
    hist_range_lat = "Historical Poleward Range Edge Latitude",
    resource = "Source"
  ) %>%
  tab_header(
    title = "Summary of Historical Poleward Range Edge Latitudes (Part 1)"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )

gtsave(gt_table1, "figures/hist_lat_supp_table_part1.png")

# Table 2
gt_table2 <- data2 %>%
  gt() %>%
  fmt_number(
    columns = hist_range_lat,
    decimals = 2
  ) %>%
  cols_label(
    latin_name = "Species",
    hist_range_lat = "Historical Poleward Range Edge Latitude",
    resource = "Source"
  ) %>%
  tab_header(
    title = "Summary of Historical Poleward Range Edge Latitudes (Part 2)"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )

gtsave(gt_table2, "figures/hist_lat_supp_table_part2.png")
