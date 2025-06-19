# Map observation points for lab review
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 6/19/2025

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

# AS OF 6/19/2025, THIS DATA STILL NEEDS TO BE DEDUPLICATED

# load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Load review data
df <- read.csv("processed_data/lab_review_with_longitudes.csv")

# Load enso data
enso <- download_enso(climate_idx = "oni", create_csv = FALSE)
enso_yr <- enso %>%
  select(c(Year, phase)) %>%
  filter(phase == 'Warm Phase/El Nino') %>%
  distinct()

# Filter for included data only
df_inc <- df %>%
  filter(Include.Exclude == "Include")
  

# --- Plotting data ---
# Plot freq of species extensions by year

# ggplot(df_inc, aes(x = Year, y = Latin.name)) +
#   geom_point() +
#   scale_x_continuous(breaks = seq(min(df_inc$Year), max(df_inc$Year), by = 5)) + 
#   labs(
#     x = "Year",
#     y = "Latin Name",
#     title = "Species-Year Dot Plot"
#   ) +
#   theme_minimal()

ggplot(df_inc, aes(x = Year, y = Latin.name)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Map extensions
ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = df_inc,
    aes(
      x = lon_for_plot,
      y = lat_for_plot,
      color = Latin.name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location") +
  coord_sf(xlim = c(-180, -115), ylim = c(30, 63), expand = FALSE)



# Add enso to time series
enso_bands <- data.frame(
  xmin = enso_yr$Year - 0.5,
  xmax = enso_yr$Year + 0.5
)

# Need to fig out what to do with year ranges - removing them for now
df_clean <- df_inc %>%
  filter(!grepl("-", Year)) %>%     # Drop year ranges like "2015-2016"
  mutate(Year = as.numeric(Year))   # Convert to numeric

ggplot(df_clean, aes(x = Year, y = Latin.name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(1950, NA))

