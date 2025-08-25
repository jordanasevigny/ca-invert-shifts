# Figure 2 Panel


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
library(ggh4x)
library(paletteer)
library(cowplot)


# Panel A

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Prep review data for analysis --------------------------------------------

# Identify the species with X+ events and filter for those species
species_with_groupXplus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 0)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

# Filter full dataset for those species
ext_Xplus <- df %>%
  filter(latin_name %in% species_with_groupXplus)

# Filter to just keep the first extension of each event (double check that there aren't duplicates)
first_ext <- ext_Xplus %>%
  filter(year == first_year)

# Tally the number of extensions events for each year
extension_counts <- first_ext %>%
  count(year, name = "n_extensions")

# Add Date (middle of the year) column to align with ONI
extension_counts$Date <- as.Date(paste0(extension_counts$year, "-06-15"))

# Average number events/year
sum(extension_counts$n)/(2020-1903)


# ENSO Data ---------------------------------------------------------------

# Load enso data
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)

# Plot Data ---------------------------------------------------------------

# Define scale factor for ONI so the line fits visually with extension counts
scale_factor <- max(extension_counts$n_extensions, na.rm = TRUE) / max(abs(enso_df$ONI), na.rm = TRUE)


A <- ggplot() +
  # Ribbon for ONI > 0 (red)
  geom_ribbon(data = enso_df,
              aes(x = Date, ymin = 0, ymax = ifelse(ONI>0, ONI * scale_factor, 0)),
              fill = "#E63946", color = "gray60", alpha = 0.7) +
  
  # Ribbon for ONI < 0 (blue)
  geom_ribbon(data = enso_df,
              aes(x = Date, ymin = ifelse(ONI < 0, ONI * scale_factor, 0), ymax = 0),
              fill = "#457B9D", color = "gray60", alpha = 0.7) +
  
  # ONI line
  # geom_line(data = enso_df,
  #           aes(x = Date, y = ONI * scale_factor),
  #           color = "gray60", size = 0.5) +
  
  # Extensions as dots at y = 0, with size by count
  geom_point(data = extension_counts,
             aes(x = Date, y = 0, size = n_extensions),
             shape=21, fill = "white", color="black", stroke = 1.2, alpha = 0.7) +
  
  
  
  # Y axis scaled for ONI only
  scale_y_continuous(
    name = "ONI",
    breaks = seq(-2, 2, 1) * scale_factor,
    labels = seq(-2, 2, 1)
  ) +
  
  scale_size_continuous(
    name = "Number of\nextension events",
    range = c(1, 14),
    c(2, 6, 10, 14)
  ) +
  # scale_size_continuous(
  #   name = "Number of extensions",
  #   range = c(1, 6),
  #   c(1, 3, 6)
  # ) +
  
  scale_x_date(
    limits = as.Date(c("1954-01-01", "2021-06-01")),
    date_breaks = "5 years",
    date_labels = "%Y"
  ) +
  
  theme_minimal(base_size = 22) +
  labs(x = " ", ) +
  theme(
    legend.title = element_text(size=12),
    legend.text = element_text(size=14),
    legend.position = c(0.00001, 0.9999),   # (x, y) inside plot coordinates
    legend.justification = c("left", "top"), # anchor legend box at that point
    axis.title.y.right = element_blank()
  )



A_supp <- ggplot() +
  # Ribbon for ONI > 0 (red)
  geom_ribbon(data = enso_df,
              aes(x = Date, ymin = 0, ymax = ifelse(ONI>0, ONI * scale_factor, 0)),
              fill = "#E63946", color = "gray60", alpha = 0.7) +
  
  # Ribbon for ONI < 0 (blue)
  geom_ribbon(data = enso_df,
              aes(x = Date, ymin = ifelse(ONI < 0, ONI * scale_factor, 0), ymax = 0),
              fill = "#457B9D", color = "gray60", alpha = 0.7) +
  
  # ONI line
  # geom_line(data = enso_df,
  #           aes(x = Date, y = ONI * scale_factor),
  #           color = "gray60", size = 0.5) +
  
  # Extensions as dots at y = 0, with size by count
  geom_point(data = extension_counts,
             aes(x = Date, y = 0, size = n_extensions),
             shape=21, fill = "white", color="black", stroke = 1.2, alpha = 0.7) +
  
  
  
  # Y axis scaled for ONI only
  scale_y_continuous(
    name = "ONI",
    breaks = seq(-2, 2, 1) * scale_factor,
    labels = seq(-2, 2, 1)
  ) +
  
  scale_size_continuous(
    name = "Number of\nextension events",
    range = c(1, 14),
    c(2, 6, 10, 14)
  ) +
  # scale_size_continuous(
  #   name = "Number of extensions",
  #   range = c(1, 6),
  #   c(1, 3, 6)
  # ) +
  
  scale_x_date(
    limits = as.Date(c("1900-01-01", "2021-06-01")),
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  
  theme_minimal(base_size = 22) +
  labs(x = " ", ) +
  theme(
    legend.title = element_text(size=12),
    legend.text = element_text(size=14),
    legend.position = c(0.00001, 0.9999),   # (x, y) inside plot coordinates
    legend.justification = c("left", "top"), # anchor legend box at that point
    axis.title.y.right = element_blank()
  )


# Panel B
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
ext_year_phase <- filter(ext_year_phase, year >= 1950) # 1950 is the cutoff for ONI

# El Nino frequency
## Monthly res
en_count <- enso_df %>%
  filter(str_detect(phase, "Warm")) %>%
  tally()
en_freq_month <- en_count$n / nrow(enso_df)

ext_summary <- ext_year_phase %>%
  filter(year == first_year) %>%
  mutate(
    is_peak_or_end = str_detect(enso_phase, "peak|end")
  ) %>%
  group_by(latin_name) %>%
  summarise(
    total_extensions = n(),
    peak_or_end_extensions = sum(is_peak_or_end, na.rm = TRUE),
    proportion_peak_or_end = peak_or_end_extensions / total_extensions
  ) %>%
  arrange(desc(proportion_peak_or_end))

B <- ggplot(ext_summary, aes(x = proportion_peak_or_end)) +
  geom_dotplot(binwidth = 0.1, fill = "gray70", color = "black", stroke=1) +
  geom_vline(xintercept = en_freq_month, linetype = "dashed", color = "#E63946", size=1.3) +
  labs(
    x = "Proportion of Extensions Events\nin El Niño Peak/End Years",
    y = "Number of Species"
  ) +
  theme_minimal(base_size = 18) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())




# Panel C


# Max ONI (year prior) vs extension count --------------------------------------------------

#===================================
# Max ONI (all shifted to year prior) - Max extension distance per event


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


ext_distance_oni <- ext_distance %>%
  mutate(year_prior = year-1) %>%
  left_join(oni_ave_by_yr, by = c("year" = "Year")) %>%
  rename(oni_year = oni_ave) %>%
  left_join(oni_ave_by_yr, by = c("first_year" = "Year")) %>%
  rename(oni_first_year = oni_ave) %>%
  left_join(oni_ave_by_yr, by = c("year_prior" = "Year")) %>%
  rename(oni_prior_yr = oni_ave)

max_ext_oni_yr_prior <- ext_distance_oni %>%
  group_by(latin_name, group_id) %>%
  mutate(max_oni = max(oni_prior_yr)) %>%
  mutate(max_ext_dist = max(distance_km)) %>%
  ungroup() %>%
  dplyr::select(latin_name, group_id, max_ext_dist, max_oni) %>%
  distinct()

# Linear regression
oni_freq <- max_ext_oni_yr_prior %>%
  count(max_oni) %>%
  drop_na()

C <- ggplot(oni_freq, aes(x=max_oni, y=n)) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se = TRUE, color="#E9C46A", fill="#E9C46A") + 
  labs(x = "Extension Event ONI", y = "Number of\nExtension Events") +
  theme_minimal(base_size = 18)

# Plot panel
BC <- plot_grid(B, C, labels = c('B', 'C'), label_size = 18, rel_widths = c(1, 2))


panel <- plot_grid(A, BC, labels = c('A', ''), label_size = 18, ncol = 1)

ggsave("figures/figure2panel.png", plot = panel, width = 14, height = 8, unit = "in", dpi = 600)


# A_supp plot
ggsave("figures/figure2a_supp.png", plot = A_supp, width = 14, height = 4, unit = "in", dpi = 600)

