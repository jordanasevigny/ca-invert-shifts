# Save the furthest north point for 3+ ext species
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025

# Load libraries
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
library(rsoi)
library(lubridate)
library(zoo)
library(gganimate)
library(forcats)
#install.packages("gifski")
#install.packages("av")  

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Identify the species with X+ events and filter for those species
species_with_group2plus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 2)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

# Filter full dataset for those species
ext_Xplus <- df %>%
  filter(latin_name %in% species_with_group2plus)

furthest_noth <- ext_Xplus %>%
  group_by(latin_name) %>%
  slice_max(latitude, n = 1, with_ties = FALSE)

# Save the dataset of the furthest north sightings of the 3+ ext species
write.csv(furthest_noth, "processed_data/threeplus_ext_sp_furthest_north.csv", row.names = FALSE)
