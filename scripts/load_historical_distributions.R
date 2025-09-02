# Load historical range edges and clean data
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/21/2025

rm(list = ls())

library(readxl)
library(dplyr)

hi <- read_excel("data/historical-distributions.xlsx")

hi_c <- hi %>%
  filter(!is.na(hist_range_lat)) %>%
  dplyr::select(c(latin_name, hist_range_lat))

write.csv(hi_c, "processed_data/historical-distributions-clean.csv", row.names = FALSE)

hi_c %>%
  count(resource)
