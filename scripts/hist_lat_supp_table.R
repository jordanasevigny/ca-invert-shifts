# Historical Latitude Supp. Table 
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 10/28/2025


# Libraries
library("ggplot2")
theme_set(theme_bw())
library(dplyr)
library(geosphere)
library(gt)

# Load historical data
data = read.csv("processed_data/historical_distributions_clean.csv")


# Make it look nice for publication
gt_table <- data %>%
  gt() %>%
  cols_label(
    latin_name = "Species",
    hist_range_lat = "Historical Poleward Range Edge Latitude",
    resource = "Source"
  ) %>%
  tab_header(
    title = "Summary of Historical Poleward Range Edge Latitudes",
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )

gtsave(gt_table, "figures/hist_lat_supp_table.png")
