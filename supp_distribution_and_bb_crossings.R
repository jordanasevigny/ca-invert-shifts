
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
library(cowplot)
library(gt)

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Identify the species with X+ events and filter for those species
species_with_group2plus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 0)) %>% # 2 would be three events (0, 1, 2)
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

single_ext$dataset <- "Lit review & CalCOFI"
single_ext_nc$dataset <- "Lit review only"

plot_df <- bind_rows(single_ext, single_ext_nc)


# Supplementary panel of spatial and temporal resolution

B <- ggplot(plot_df, aes(x = latitude, fill = dataset)) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.65
  ) + 
  scale_fill_manual(values = c(
    "Lit review & CalCOFI" = "#2CA02C",
    "Lit review only" = "#E377C2"
  )) +
  labs(
    x = "Latitude",
    y = "Number extensions",
    fill = "Source"
  ) + theme_minimal(base_size = 20) +
  theme(
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )
A <- ggplot(plot_df, aes(x = first_year, fill = dataset)) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.65
  ) + 
  scale_fill_manual(values = c(
    "Lit review & CalCOFI" = "#2CA02C",
    "Lit review only" = "#E377C2"
  )) +
  labs(
    x = "Extension Year",
    y = "Number extensions",
    fill = "Source"
  ) + theme_minimal(base_size = 20) +
  theme(
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  )

# Plot panel
AB <- plot_grid(A, B, labels = c('a', 'b'), label_size = 18, ncol=1)
ggsave("figures/supp-spatial-temp-panel.png", plot = AB, width = 14, height = 8, unit = "in", dpi = 600)
ggsave("figures/supp-spatial-temp-panel.pdf", plot = AB, width = 14, height = 8, unit = "in", dpi = 600)


# Extensions frm south of PC to north of PC
single_ext_pc <- df %>%
  filter(hist_range_lat <=34.5 & latitude > 34.5) %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
# 73 of 153 extensions

single_ext_pc_sp <- single_ext_pc %>%
  select(latin_name) %>%
  distinct()
# 33 species - all ones that are always in the water or have PLDs

# Extensions frm south of CM to north of CM
single_ext_cm <- df %>%
  filter(hist_range_lat <=40.4 & latitude > 40.4) %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
# 45 of 153 extensions

single_ext_cm_sp <- single_ext_cm %>%
  select(latin_name) %>%
  distinct()
# 25 species
# knowing whether these are intertidal, subtidal, benthic, or open ocean creatures would be helpful



# Extensions frm south of CF to north of CF
single_ext_cf <- df %>%
  filter(hist_range_lat <=48.385 & latitude > 48.385) %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
# 17 of 153 extensions

single_ext_cf_sp <- single_ext_cf %>%
  select(latin_name) %>%
  distinct()
# 7 species

# Extensions frm south of PE to north of PE
single_ext_pe <- df %>%
  filter(hist_range_lat <=27.84 & latitude > 27.84) %>%
  select(latin_name, first_year, latitude, longitude) %>%
  distinct() %>%
  group_by(latin_name, first_year) %>%
  filter(latitude == max(latitude, na.rm = TRUE)) %>%
  ungroup()
# 18 of 153 extensions

single_ext_pe_sp <- single_ext_pe %>%
  select(latin_name) %>%
  distinct()
# 9 species



# Combine extension events across boundaries
boundary_extensions <- bind_rows(
  "Punta Eugenia"   = single_ext_pe,
  "Point Conception" = single_ext_pc,
  "Cape Mendocino"   = single_ext_cm,
  "Cape Flattery"    = single_ext_cf,
  .id = "boundary"
)

# Summarize extensions and species for each boundary
boundary_summary <- boundary_extensions %>%
  group_by(boundary) %>%
  summarise(
    n_extensions = n(),
    n_species = n_distinct(latin_name),
    species = paste(
      sort(unique(latin_name)),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  # Put boundaries in geographic order, south to north
  mutate(
    boundary = factor(
      boundary,
      levels = c(
        "Punta Eugenia",
        "Point Conception",
        "Cape Mendocino",
        "Cape Flattery"
      )
    )
  ) %>%
  arrange(boundary)

gt_boundary_table <- boundary_summary %>%
  gt() %>%
  cols_label(
    boundary = "Biogeographic Boundary",
    n_extensions = "Number of Extension Events",
    n_species = "Number of Species",
    species = "Species"
  ) %>%
  tab_header(
    title = "Poleward Extension Events Crossing Major Biogeographic Boundaries"
  ) %>%
  cols_align(
    align = "center",
    columns = c(n_extensions, n_species)
  ) %>%
  cols_align(
    align = "left",
    columns = c(boundary, species)
  ) %>%
  cols_width(
    boundary ~ px(170),
    n_extensions ~ px(120),
    n_species ~ px(100),
    species ~ px(600)
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    table.font.size = px(18),      # was 12
    heading.title.font.size = px(22),
    column_labels.font.size = px(18),
    data_row.padding = px(16),     # more vertical spacing
    heading.align = "left"
  )

gt_boundary_table

gtsave(
  gt_boundary_table,
  "figures/biogeographic_boundary_extensions_table.png",
  vwidth = 2200,
  vheight = 1200
)
