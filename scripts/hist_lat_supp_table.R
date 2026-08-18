# Historical Latitude Supp. Table 
# Date created: 10/28/2025

rm(list = ls())

# Libraries
library("ggplot2")
theme_set(theme_bw())
library(dplyr)
library(geosphere)
library(gt)
library(readxl)

# Load historical data
data = read.csv("processed_data/historical_distributions_clean.csv") # Not all these species are included in analysis (some of these hist lats are further north than their observations - intersect with life history dataset)
life_history = read_xlsx("processed_data/pelagic-life-history-all-species.xlsx")
data_life_history = life_history %>%
  left_join(data, by="latin_name")
unique(data_life_history$resource)

table(data_life_history$habitat_life_history)

data_life_history <- data_life_history %>%
  mutate(
    resource = if_else(
      !is.na(source),
      paste(resource, source, sep = "; "),
      resource
    )
  ) %>%
  select(-source, -source_link)

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

# Find midpoint
n <- nrow(data_life_history)
half <- ceiling(n / 2)

# Split into two data frames
data1 <- data_life_history[1:half, ]
data2 <- data_life_history[(half + 1):n, ]

# Table 1
gt_table1 <- data1 %>%
  gt() %>%
  fmt_number(
    columns = hist_range_lat,
    decimals = 2
  ) %>%
  cols_label(
    latin_name = "Species",
    phylum = "Phylum",
    hist_range_lat = "Historical Poleward Range Edge Latitude",
    habitat_life_history = "Habitat/Life History",
    resource = "Source"
  ) %>%
  cols_align(
    align = "left",
    columns = hist_range_lat
  ) %>%
  cols_width(
    latin_name ~ px(140),
    phylum ~ px(100),
    hist_range_lat ~ px(110),
    habitat_life_history ~ px(150),
    resource ~ px(250)
  ) %>%
  tab_header(
    title = "Summary of Historical Poleward Range Edge Latitudes, Habitat, and Life History (Part 1)"
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
    phylum = "Phylum",
    hist_range_lat = "Historical Poleward Range Edge Latitude",
    habitat_life_history = "Habitat/Life History",
    resource = "Source"
  ) %>%
  cols_align(
    align = "left",
    columns = hist_range_lat
  ) %>%
  cols_width(
    latin_name ~ px(140),
    phylum ~ px(100),
    hist_range_lat ~ px(110),
    habitat_life_history ~ px(150),
    resource ~ px(250)
  ) %>%
  tab_header(
    title = "Summary of Historical Poleward Range Edge Latitudes, Habitat, and Life History (Part 2)"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )

gtsave(gt_table2, "figures/hist_lat_supp_table_part2.png")
