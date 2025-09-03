# Figure 3 Panel
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 08/2025

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

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load enso data
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)

# Classify enso by start, peak, end --------------------------------------------
# For each event, get the years and count months per year
# Create event groupings from monthly phase info
enso_events <- enso_df %>%
  filter(phase == "Warm Phase/El Nino") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = ymd(paste(Year, Month, "15")),
    yearmon = as.yearmon(Date)
  ) %>%
  arrange(Date) %>%
  mutate(
    month_diff = c(0, diff(yearmon)),
    new_event = month_diff >  3/12,
    group_id = cumsum(new_event)
  )

# Count El Niño months per year per event
yearly_counts <- enso_events %>%
  group_by(group_id, Year) %>%
  summarise(n_months = n(), .groups = "drop")

# Get start, peak, end for each event
event_year_labels <- yearly_counts %>%
  group_by(group_id) %>%
  summarise(
    start = min(Year),
    end = max(Year),
    peak = Year[which.max(n_months)]
  ) %>%
  pivot_longer(cols = c(start, peak, end), names_to = "phase", values_to = "Year") %>%
  arrange(Year)

# Combine phase roles per year (across all events)
year_phases <- event_year_labels %>%
  group_by(Year) %>%
  summarise(enso_phase = paste(sort(phase), collapse = "-")) %>%
  ungroup() 

# Averages oni by year from complete enso dataset (without filtering for el nino)
oni_ave_by_yr <- enso_df %>%
  dplyr::select(Year, ONI) %>%
  group_by(Year) %>%
  summarise(oni_ave = mean(ONI, na.rm = TRUE))


# Prep review data for analysis --------------------------------------------

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
# library(clipr)
# write_clip(unique(ext_Xplus$latin_name))

# Drop any 'extensions' that did not get past the historical range edge we have documented - this is already done in the merge script and should not make a difference here
ext_Xplus <- ext_Xplus %>%
  filter(latitude > hist_range_lat)

# Dataframe of all extension events for species with X events combined with el nino phase data (start, peak, end)
ext_year_phase <- left_join(ext_Xplus, year_phases, by = c("first_year" = "Year"))

# Calculate extension distance
# Distance function
get_distance_km <- function(lat1, lon1, lat2, lon2) {
  point1 <- c(lon1, lat1)
  point2 <- c(lon2, lat2)
  dist_meters <- distHaversine(point1, point2)
  return(dist_meters / 1000)  # convert to kilometers
}

# Apply function row-wise
ext_distance <- ext_year_phase %>%
  rowwise() %>%
  mutate(distance_km = get_distance_km(hist_range_lat, hist_range_lon, latitude, longitude)) %>%
  ungroup()

#===================================
# Max ONI (all shifted to year prior) - Max extension distance per event

# Make a new column of the year prior to the year of observation and find the ave ONI for those years
ext_distance_oni <- ext_distance %>%
  mutate(year_prior = year-1) %>%
  left_join(oni_ave_by_yr, by = c("year_prior" = "Year")) %>%
  rename(oni_prior_yr = oni_ave)

# Select the Max of the average ONI for each event and the max extension distance for each event
max_ext_oni_yr_prior <- ext_distance_oni %>%
  group_by(latin_name, group_id) %>%
  mutate(max_oni = max(oni_prior_yr)) %>%
  mutate(max_ext_dist = max(distance_km)) %>%
  ungroup() %>%
  dplyr::select(latin_name, group_id, max_ext_dist, max_oni) %>%
  distinct()

# no ONI pre 1950 (drops any extensions pre 1950)
B <- ggplot(max_ext_oni_yr_prior, aes(max_oni, max_ext_dist)) +
  geom_point(color="black", size=2) +
  geom_smooth(method = "lm", se = TRUE, color="#E9C46A", fill="#E9C46A") +  # se = FALSE to hide confidence interval
  labs(x = "Extension Event ONI", y = "Extension Event\nDistance (km)", ) +
  theme_minimal(base_size = 16)

# Density Plot Low vs High ONI
plot3a_data <- max_ext_oni_yr_prior %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na() 
 
A <- ggplot(plot3a_data, aes(x = max_ext_dist, fill = oni_cat)) +
  geom_density(alpha = 0.6) +
  labs(x = "Extension Event\nDistance (km)", y= "Density", fill = "ONI Level") +
  scale_fill_manual(
    values = c("High ONI (>=0.5)" = "#D62828",   # red
               "Low ONI" = "#003049")           # blue
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.position = c(0.999, 0.999),   # (x, y) inside plot coordinates
    legend.justification = c("right", "top"), # anchor legend box at that point
    axis.title.y.right = element_blank()
  )



# Plot panel
AB <- plot_grid(A, B, labels = c('A', 'B'), label_size = 18, rel_widths = c(1, 2))
ggsave("figures/figure3panel.png", plot = AB, width = 14, height = 4, unit = "in", dpi = 600)
