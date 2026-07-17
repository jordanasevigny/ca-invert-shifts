# Save the furthest north point for 3+ ext species
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
  "#8C6D31", "#E377C2", "#7B4173", "#FFD92F", "#E6550D", "#A6D854", "#1F97B8", "#49DB79"
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
  coord_sf(xlim = c(-127, -110), ylim = c(20, 50), expand = FALSE) +
  scale_x_continuous(breaks = c(-127, -122, -117, -112)) +
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
  coord_sf(xlim = c(-170, -110), ylim = c(20, 60), expand = FALSE) +
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

# Blank map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  # geom_curve(
  #   data = furthest_noth_j,
  #   aes(x = x0, y = y0, xend = x1, yend = y1, color = latin_name),
  #   curvature = -0.2,
  #   arrow = arrow(length = unit(0.6, "cm")),
  #   size=1.2,
  #   alpha=0.8
  # ) +
  scale_color_manual(values = palette_18_alt) +
  coord_sf(xlim = c(-130, -114), ylim = c(30, 50), expand = FALSE) +
  scale_x_continuous(breaks = c(-130, -122, -118, -114)) +
  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  labs(x = "Longitude", 
       y = "Latitude", 
       color = "Species") 



# Map with extension destination clusters
single_ext <- df %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
single_ext_nc <- df %>%
  filter(obs_source != "ca_rev") %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()

ext_counts <- single_ext %>%
  group_by(latitude, longitude) %>%
  summarise(n_extensions = n(), .groups = "drop")

ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_point(
    data = ext_counts,
    aes(x = longitude, y = latitude, size = n_extensions),
    alpha = 0.7
  ) +
  coord_sf(xlim = c(-160, -114), ylim = c(30, 60), expand = FALSE) +
  scale_x_continuous(breaks = c(-158, -152, -146, -138, -130, -122, -114)) +
  scale_size_continuous(name = "Number of extensions") +
  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

# # Ext vs latitude
ggplot(ext_counts, aes(x=latitude, y=n_extensions)) +
  geom_point() +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
  ggtitle("Number of extensions by latitude")
ggplot(single_ext, aes(x = latitude)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Histogram of number of extensions by latitude: all data")
ggplot(single_ext_nc, aes(x = latitude)) +
  geom_histogram(binwidth = 1) +
  ggtitle("Histogram of number of extensions by latitude: lit review only (no CalCOFI)")

single_ext$dataset <- "All data"
single_ext_nc$dataset <- "Lit review only"

plot_df <- bind_rows(single_ext, single_ext_nc)
ggplot(plot_df, aes(x = latitude, fill = dataset)) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.5
  ) +
  labs(
    title = "Histogram of extensions by latitude",
    x = "Latitude",
    y = "Count"
  )
ggplot(plot_df, aes(x = first_year, fill = dataset)) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.5
  ) +
  labs(
    title = "Histogram of extensions by extension year",
    x = "Extension Year",
    y = "Count"
  )
# Extensions frm south of PC to north of PC
single_ext_pc <- df %>%
  filter(hist_range_lat <=34.5 & latitude > 34.5) %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
# 73 of 149 extensions
single_ext_pc_sp <- single_ext_pc %>%
  select(latin_name) %>%
  distinct()
# 33 species - all ones that are always in the water or have PLDs
single_ext_cm <- df %>%
  filter(hist_range_lat <=40.4 & latitude > 40.4) %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
# 45 of 149 extensions
single_ext_cm_sp <- single_ext_cm %>%
  select(latin_name) %>%
  distinct()
# 25 species
# knowing whether these are intertidal, subtidal, benthic, or open ocean creatures would be helpful

# species with extensions to near PC but not past it:
single_ext_almost_pc <- df %>%
  select(latin_name, hist_range_lat, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(hist_range_lat <= 32.5 & latitude > 33.5 & latitude < 34.5)
# 2 of 148 extensions

# s_pc <- df %>%
#   filter(hist_range_lat <=34.5) %>%
#   select(latin_name, first_year, latitude, longitude) %>%
#   distinct() %>%
#   group_by(latin_name, first_year) %>%
#   filter(latitude == max(latitude, na.rm = TRUE)) %>%
#   ungroup()
# 70 extensions went around pc of 90 with hist lat < 34.5

ext_counts_pc <- single_ext_pc %>%
  group_by(latitude, longitude) %>%
  summarise(n_extensions = n(), .groups = "drop")

ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_point(
    data = ext_counts_pc,
    aes(x = longitude, y = latitude, size = n_extensions),
    alpha = 0.7
  ) +
  coord_sf(xlim = c(-160, -114), ylim = c(30, 60), expand = FALSE) +
  scale_x_continuous(breaks = c(-158, -152, -146, -138, -130, -122, -114)) +
  scale_size_continuous(name = "Number of extensions\ncrossing PC") +
  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )


# Map with historical range edge clusters
single_hist <- df %>%
  select(latin_name, hist_range_lat, hist_range_lon) %>%
  distinct()

hist_counts <- single_hist %>%
  group_by(hist_range_lat, hist_range_lon) %>%
  summarise(n_extensions = n(), .groups = "drop")

ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray80") +
  geom_sf(data = states, fill = NA, color = "gray80", size = 0.3) +
  geom_point(
    data = hist_counts,
    aes(x = hist_range_lon, y = hist_range_lat, size = n_extensions),
    alpha = 0.7
  ) +
  coord_sf(xlim = c(-160, -113), ylim = c(25, 60), expand = FALSE) +
  scale_x_continuous(breaks = c(-158, -152, -146, -138, -130, -122, -113)) +
  scale_size_continuous(name = "Number of historical range edges") +
  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  labs(
    x = "Longitude",
    y = "Latitude"
  )

# # Hist vs latitude
ggplot(hist_counts, aes(x=hist_range_lat, y=n_extensions)) +
  geom_point() +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
  ggtitle("Number of historical range edges by latitude")
ggplot(single_hist, aes(x = hist_range_lat)) +
  geom_histogram(binwidth = 1)



# lats of extensions by episodic extenders -------------------------------------------

ext_Xplus_events <- ext_Xplus %>%
  select(latin_name, latitude, longitude, first_year) %>%
  distinct()

# coastline/world layer
world <- ne_countries(scale = "medium", returnclass = "sf")

# unique species list
species_list <- unique(ext_Xplus_events$latin_name)

for(sp in species_list){
  
  message("Making map for: ", sp)
  
  sp_df <- ext_Xplus_events %>%
    filter(latin_name == sp)
  
  # skip if no coordinates
  if(nrow(sp_df) == 0) next
  
  p <- ggplot() +
    
    geom_sf(
      data = world,
      fill = "gray90",
      color = "gray50",
      linewidth = 0.3
    ) +
    
    geom_point(
      data = sp_df,
      aes(
        x = longitude,
        y = latitude,
        color = first_year
      ),
      size = 3
    ) +
    
    scale_color_viridis_c() +
    
    coord_sf(
      xlim = range(sp_df$longitude, na.rm = TRUE) + c(-2, 2),
      ylim = range(sp_df$latitude, na.rm = TRUE) + c(-2, 2),
      default_crs = sf::st_crs(4326),
      expand = FALSE
    ) +
    
    labs(
      title = sp,
      color = "First year"
    ) +
    
    theme_minimal()
  print(p)
}
