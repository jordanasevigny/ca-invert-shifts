# Save the furthest north point for 3+ ext species
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025

rm(list = ls())

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




set.seed(47) # 47 is pretty good
eps <- 1  # degrees; tune smaller/larger as needed
palette_18_alt <- c(
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#E5C494",
  "#843C39", "#7F7F7F", "#BCBD22", "#17BECF", "#393B79", "#637939",
  "#8C6D31", "#E377C2", "#7B4173", "#FFD92F", "#E6550D", "#A6D854"
)

# need to change yellow, orange, turquoise and pink duplicates

furthest_noth_j <- furthest_noth %>%
  mutate(
    x0 = hist_range_lon + runif(n(), -eps, 0),
    y0 = hist_range_lat + runif(n(), 0, 0),
    x1 = longitude      + runif(n(), -eps, 0),
    y1 = latitude       + runif(n(), -0, 0)
  ) %>%
  mutate(len = sqrt((x1-x0)^2 + (y1-y0)^2)) %>%
  arrange(desc(len))

# Manually fix the gulf of ca coordinate
furthest_noth_j$x0[which.max(furthest_noth_j$x0)] <- furthest_noth_j$hist_range_lon[which.max(furthest_noth_j$x0)]
map <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_curve(
    data = furthest_noth_j,
    aes(x = x0, y = y0, xend = x1, yend = y1, color = latin_name),
    curvature = -0.2,
    arrow = arrow(length = unit(0.6, "cm")),
    size=1.2,
    alpha=0.8
  ) +
  scale_color_manual(values = palette_18_alt) +
  coord_sf(xlim = c(-127, -114), ylim = c(30, 50), expand = FALSE) +
  scale_x_continuous(breaks = c(-126, -122, -118, -114)) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Species") + 
  theme(
         legend.text = element_text(size=10, face = "italic"),
         legend.position = c(0.999, 0.999),   # (x, y) inside plot coordinates
         legend.justification = c("right", "top"), # anchor legend box at that point
         legend.box.margin = margin(0,0,0,0),
         legend.margin = margin(0,0,0,0)
       )
map
ggsave("figures/ext_map.png", plot = map, width = 6, height = 8, units = "in", dpi = 600)
ggsave("figures/ext_map.pdf", plot = map, width = 6, height = 8, units = "in", dpi = 600)


# Zoom out
map_supp <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_curve(data = furthest_noth_j,
             aes(x = x0, y = y0, xend = x1, yend = y1, color = latin_name),
             curvature = -0.2,
             arrow = arrow(length = unit(0.4, "cm"))
  ) +
  scale_color_manual(values = palette_18_alt) +
  coord_sf(xlim = c(-170, -114), ylim = c(30, 60), expand = FALSE) +
  scale_x_continuous(breaks = c(-158, -152, -146, -138, -130, -122, -114)) +
  theme_minimal(base_size = 16) +
  labs(
       x = "Longitude", 
       y = "Latitude", 
       color = "Species") + 
  theme(
    legend.text = element_text(size=10, face = "italic"),
    legend.position = c(0.001, 0.001),   # (x, y) inside plot coordinates
    legend.justification = c("left", "bottom") # anchor legend box at that point
  )
map_supp
ggsave("figures/ext_map_supp.png", plot = map_supp, width = 8, height = 8, units = "in", dpi = 600)
ggsave("figures/ext_map_supp.pdf", plot = map_supp, width = 8, height = 8, units = "in", dpi = 600)



####
# No species list on map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_curve(
    data = furthest_noth_j,
    aes(x = x0, y = y0, xend = x1, yend = y1, color = latin_name),
    curvature = -0.2,
    arrow = arrow(length = unit(0.6, "cm")),
    size=1.2,
    alpha=0.8
  ) +
  scale_color_manual(values = palette_18_alt) +
  coord_sf(xlim = c(-127, -114), ylim = c(30, 50), expand = FALSE) +
  scale_x_continuous(breaks = c(-126, -122, -118, -114)) +
  theme_minimal(base_size = 16) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Species") 
