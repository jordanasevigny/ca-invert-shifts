# Save the furthest north point for 3+ ext species
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025

# Load libraries
library("ggplot2")
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
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

# Color palette
# #2B5275FF = la nina ; #D16647FF = el nino ; gray60 = enso outline / oni ; black at alpha=0.6 = extension event tallies ; #A69F55FF = free variable (green) ; #FFFBDDFF = blob (white fill)
# theme_minimal(base_size = 16) for all ggplot


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

# # Save the dataset of the furthest north sightings of the 3+ ext species
# write.csv(furthest_noth, "processed_data/threeplus_ext_sp_furthest_north.csv", row.names = FALSE)

#Load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_curve(data = furthest_noth,
             aes(x = hist_range_lon, y = hist_range_lat, xend = longitude, yend = latitude, color = latin_name),
             curvature = -0.2,
             arrow = arrow(length = unit(0.4, "cm"))
  ) +
  coord_sf(xlim = c(-127, -114), ylim = c(30, 50), expand = FALSE) +
  scale_x_continuous(breaks = c(-126, -122, -118, -114)) +
  theme_minimal(base_size = 16) +
  labs(title = "Furthest Species Range Extensions\n(3+ extensions required)", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")

# Zoom out
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_curve(data = furthest_noth,
             aes(x = hist_range_lon, y = hist_range_lat, xend = longitude, yend = latitude, color = latin_name),
             curvature = -0.2,
             arrow = arrow(length = unit(0.4, "cm"))
  ) +
  coord_sf(xlim = c(-160, -114), ylim = c(30, 60), expand = FALSE) +
  scale_x_continuous(breaks = c(-152, -146, -138, -130, -122, -114)) +
  theme_minimal(base_size = 16) +
  labs(title = "Furthest Species Range Extensions\n(3+ extensions required)", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")
