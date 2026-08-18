# Life History vs Extension Distance
# Date created: 08/2026

rm(list = ls())

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
library(geosphere)
library(moments)
library(mgcv)
library(lme4)
library(gt)
library(pscl)
library(topmodels)
library(tibble)
library(mgcv)
library(ggplot2)
library(emmeans)
citation("emmeans")
# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")
life_history = read_xlsx("processed_data/pelagic-life-history-all-species.xlsx")


# Identify the species with X+ events and filter for those species
species_with_groupXplus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 2)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

# Filter full dataset for those species
ext_Xplus <- df %>%
  filter(latin_name %in% species_with_groupXplus)
# unique(ext_Xplus$latin_name)

# Calculate extension distance
# Distance function
get_distance_km <- function(lat1, lon1, lat2, lon2) {
  point1 <- c(lon1, lat1)
  point2 <- c(lon2, lat2)
  dist_meters <- distHaversine(point1, point2)
  return(dist_meters / 1000)  # convert to kilometers
}

# Apply extension distance function row-wise
ext_distance <- ext_Xplus %>%
  rowwise() %>%
  mutate(distance_km = get_distance_km(hist_range_lat, hist_range_lon, latitude, longitude)) %>%
  ungroup()

ext_dist_life <- ext_distance %>%
  left_join(life_history, by="latin_name")

ext_dist_life_events <- ext_dist_life %>%
  group_by(latin_name, group_id) %>%
  mutate(max_ext_dist = max(distance_km)) %>%
  ungroup() %>%
  dplyr::select(latin_name, group_id, max_ext_dist, habitat_life_history) %>%
  distinct()

boxplot(max_ext_dist ~ habitat_life_history, data=ext_dist_life_events)


# Do extension distances differ among habitat/life-history groups, while accounting for repeated observations of the same species??

dat_gam <- ext_dist_life_events %>%
  filter(
    !is.na(max_ext_dist),
    !is.na(habitat_life_history),
    !is.na(latin_name),
    max_ext_dist > 0
  ) %>%
  mutate(
    latin_name = factor(latin_name),
    habitat_life_history = factor(habitat_life_history)
  ) %>%
  droplevels()

m <- gam(
  log(max_ext_dist) ~ habitat_life_history +
    s(latin_name, bs = "re"),
  data = dat_gam,
  method = "REML"
)

summary(m)
exp(c(0.5138, 1.6865))
# conditional on the model assumptions, fully pelagic species have estimated extension distances about 5.4 times greater and pelagic larval dispersing benthic species extension distances about 1.7 times greater than than limited-dispersal benthic species.

emm <- emmeans(m, ~ habitat_life_history)

pairs(emm, adjust = "tukey")

# pairwise.t.test(
#   log(ext_dist_life_events$max_ext_dist),
#   ext_dist_life_events$habitat_life_history,
#   p.adjust.method = "BH"
# )

box <- ggplot(
  ext_dist_life_events,
  aes(x = habitat_life_history, y = max_ext_dist, fill = habitat_life_history)
) +
  geom_boxplot(
    width = 0.6,
    fill = "grey75",
    color = "black",
    alpha = 0.7,
    outlier.shape = NA,
    linewidth = 0.6
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 2,
    alpha = 0.6
  ) +
  labs(
    x = "Habitat / Life History",
    y = "Extension Event Distance (km)"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    )
  )
ggsave("figures/life_hist_dist_boxplot.png", plot = box, width = 10, height = 8, unit = "in", dpi = 600)
