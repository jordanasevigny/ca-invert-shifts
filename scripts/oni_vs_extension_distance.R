# ONI vs extension length
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/24/2025

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

#install.packages("gifski")
#install.packages("av")  

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

#Lload a world map
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
species_with_group2plus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 2)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

# Filter full dataset for those species
ext_Xplus <- df %>%
  filter(latin_name %in% species_with_group2plus)
# unique(ext_Xplus$latin_name)
# library(clipr)
# write_clip(unique(ext_Xplus$latin_name))

# Drop any 'extensions' that did not get past the historical range edge we have documented
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

# Join both year and first_year ONI values to main_df
ext_distance_oni <- ext_distance %>%
  left_join(oni_ave_by_yr, by = c("year" = "Year")) %>%
  rename(oni_year = oni_ave) %>%
  left_join(oni_ave_by_yr, by = c("first_year" = "Year")) %>%
  rename(oni_first_year = oni_ave)

# # Now choose which ONI value to use based on whether `enso phase` is NA
# ext_distance <- ext_distance_oni %>%
#   mutate(oni_final = if_else(!is.na(enso_phase), oni_first_year, oni_year))

#===================================
# Max ONI - Max extension distance per event
max_ext_oni <- ext_distance_oni %>%
  group_by(latin_name, group_id) %>%
  mutate(max_oni = max(oni_year)) %>%
  mutate(max_ext_dist = max(distance_km)) %>%
  ungroup() %>%
  dplyr::select(latin_name, group_id, max_ext_dist, max_oni) %>%
  distinct()

ggplot(max_ext_oni, aes(max_oni, max_ext_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # se = FALSE to hide confidence interval
  labs(x = "Max ONI of Extension Event", y = "Max Extension Distance (km) per Extension Event", title = "Max Extension vs. Max ONI by Extension Event") +
  theme_minimal()

# Fit the linear model
model <- lm(max_ext_dist ~ max_oni, data = max_ext_oni)

# View the model summary
summary(model)

# Density Plot Low vs High ONI
max_ext_oni %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na() %>%
  ggplot(aes(x = max_ext_dist, fill = oni_cat)) +
  geom_density(alpha = 0.4) +
  labs(x = "Max Extension Distance (km)", y= "Density", fill = "ONI Category")

# Create category and drop NA
oni_test_data <- max_ext_oni %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na(max_ext_dist, oni_cat)

# Run t-test
t.test(max_ext_dist ~ oni_cat, data = oni_test_data)

#===================================
# Max ONI - Max extension distance per event with addition of if first year = end phase, do oni for year prior
ext_distance_oni <- ext_distance %>%
  mutate(year_adj = if_else(enso_phase=="end",year-1, year, missing = year)) %>%
  left_join(oni_ave_by_yr, by = c("year" = "Year")) %>%
  rename(oni_year = oni_ave) %>%
  left_join(oni_ave_by_yr, by = c("first_year" = "Year")) %>%
  rename(oni_first_year = oni_ave) %>%
  left_join(oni_ave_by_yr, by = c("year_adj" = "Year")) %>%
  rename(oni_adj_yr = oni_ave)

max_ext_oni <- ext_distance_oni %>%
  group_by(latin_name, group_id) %>%
  mutate(max_oni = max(oni_adj_yr)) %>%
  mutate(max_ext_dist = max(distance_km)) %>%
  ungroup() %>%
  dplyr::select(latin_name, group_id, max_ext_dist, max_oni) %>%
  distinct()

ggplot(max_ext_oni, aes(max_oni, max_ext_dist)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +  # se = FALSE to hide confidence interval
  labs(x = "Max ONI of Extension Event", y = "Max Extension Distance (km) per Extension Event", title = "Max Extension vs. Max ONI by Extension Event With End Years Shifted to One Year Prior") +
  theme_minimal()

# Fit the linear model
model <- lm(max_ext_dist ~ max_oni, data = max_ext_oni)

# View the model summary
summary(model)

# Density Plot Low vs High ONI
max_ext_oni %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na() %>%
  ggplot(aes(x = max_ext_dist, fill = oni_cat)) +
  geom_density(alpha = 0.4) +
  labs(x = "Max Extension Distance (km)", y= "Density", fill = "ONI Category")


# Create category and drop NA
oni_test_data <- max_ext_oni %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na(max_ext_dist, oni_cat)

# Run t-test
t.test(max_ext_dist ~ oni_cat, data = oni_test_data)