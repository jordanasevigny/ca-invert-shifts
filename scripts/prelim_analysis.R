# Maps of range extensions and historical edges
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

#Lload a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load enso data
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)
#write.csv(enso_df, "enso_data.csv")

# Make El Niño data plottable ---------------------------------------------
# I TOGGLE BETWEEN USING phase == 'Warm Phase/El Nino' and dSST3.4 > 0 HERE
enso <- enso_df %>%
  select(c(Year, Month, phase)) %>%
  filter(phase == 'Warm Phase/El Nino') %>%
  distinct() %>%
  select(Year, Month)

oni_years <- enso %>%
  select(Year) %>%
  distinct()

# Manually add historical el nino events from Quinn et al. 1987 for 1850-1950
# https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1029/JC092iC13p14449
# Their data extends back to 1525!
man_hist_enso <- c(1864, 1871, 1877, 1878, 1884, 1891, 1899, 1900, 1911, 1912, 1917, 1925, 1926, 1932, 1940, 1941)
man_hist_df <- data.frame(Year = man_hist_enso)

enso_bands_old <- data.frame(
  xmin = man_hist_df$Year - 0.5,
  xmax = man_hist_df$Year + 0.5
)

# Make bands for modern enso data
# Convert to date
enso <- enso %>%
  mutate(
    Month = match(Month, month.abb),  # convert "Aug" -> 8, etc.
    Date = ymd(paste(Year, Month, 15, sep = "-"))  # use 15th as mid-month
  ) %>%
  arrange(Date)

# Calculate difference in months
enso <- enso %>%
  arrange(Date) %>%
  mutate(
    month_diff = c(0, diff(as.yearmon(Date))),
    new_event = month_diff > 3/12,  # if jump > 3 month, new group
    group_id = cumsum(new_event)
  )

# Make x bands
enso_bands_oni <- enso %>%
  group_by(group_id) %>%
  summarise(
    xmin = min(as.numeric(as.yearmon(Date))),
    xmax = max(as.numeric(as.yearmon(Date))) + 1/12  # to cover full last month
  ) %>%
  select(c(xmin, xmax))

# Merge historical and modern datasets for El Nino bands for mapping
enso_bands <- rbind(enso_bands_oni, enso_bands_old)

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

# El Nino frequency
## Monthly res
en_count <- enso_df %>%
  filter(str_detect(phase, "Warm")) %>%
  tally()
en_freq_month <- en_count$n / nrow(enso_df)

# Prep review data for mapping --------------------------------------------

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

# This is an odd dataset. For each extensions per species, this has the northernmost latitude of all observations of that possibly multi-year period paired to the first year of the extension episode
northern_most_per_ext <- ext_Xplus %>% 
  select(c(latin_name, latitude, longitude, first_year, hist_range_lat, group_id)) %>%
  group_by(latin_name, group_id) %>%
  slice_max(latitude, with_ties = FALSE)

# Plots of historical range edge vs extension position --------------------------------------------
# Arrow map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = northern_most_per_ext, 
               aes(x = longitude, y = hist_range_lat, 
                   xend = longitude, yend = latitude, color = latin_name),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-150, -110), ylim = c(30, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions (3+ extensions required)", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species")

nrow(northern_most_per_ext) # number of extensions
length(unique(northern_most_per_ext$latin_name)) # number of species

# Animated Map
# p <- ggplot() +
#   geom_sf(data = world, fill = "gray90", color = "black") +
#   geom_segment(data = ext_Xplus, 
#                aes(x = longitude, y = hist_range_lat, 
#                    xend = longitude, yend = latitude, color = latin_name),
#                arrow = arrow(length = unit(0.2, "cm")),
#                linewidth = 0.7) +
#   transition_states(year, transition_length = 1, state_length = 1) +
#   labs(title = "Year: {closest_state}") +
#   coord_sf(xlim = c(-150, -110), ylim = c(20, 60), expand = FALSE)
# p + theme(legend.position = "bottom")
# anim <- animate(p, renderer = gifski_renderer(), width = 1000, height = 750, fps = 3)
# anim
# anim_save("preliminary_plots/range_extensions.gif", animation = anim)
# dev.off()



# Plot time series ordered by group id
ext_Xplus <- ext_Xplus %>%
  mutate(latin_name = fct_reorder(latin_name, group_id, .desc = TRUE))

ggplot(ext_Xplus, aes(x = year, y = latin_name)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot (3+ extensions required)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(1850, 2025),
                     breaks = seq(1850, 2025, by = 5))

ggplot(ext_Xplus, aes(x = year, y = latin_name)) +
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot (3+ extensions required) - El Niño in red"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(1850, 2025),
                     breaks = seq(1850, 2025, by = 5))



# Histograms of el nino-related extensions -------------------------------------------------------

# Dataframe of all extension events for species with X events combined with el nino phase data (start, peak, end)
ext_year_phase <- left_join(ext_Xplus, year_phases, by = c("first_year" = "Year"))

# Limit Dataframe to just the initial extensions
lim_ext_year_phase <- ext_year_phase %>%
  group_by(latin_name, group_id) %>% 
  slice_min(year) %>%
  mutate(enso_phase = ifelse(is.na(enso_phase), "non-El Niño", as.character(enso_phase)))

# Number of extensions by ENSO phase classification
ggplot(lim_ext_year_phase, aes(x = enso_phase)) +
  geom_bar(fill = "cyan4") +
  labs(
    title = "Total Range Extensions by ENSO Phase (3+ extensions required)",
    x = "ENSO Phase Classification",
    y = "Number of Extensions"
  ) +
  theme_minimal()

# Faceted by species
ggplot(lim_ext_year_phase, aes(x = enso_phase)) +
  geom_bar(fill = "cyan4") +
  facet_wrap(~ latin_name, scales = "free_y") +
  labs(
    title = "Range Extensions by ENSO Phase per Species (3+ extensions required)",
    x = "ENSO Phase Classification",
    y = "Extensions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tally number of extensions per species occurring in peak or end el nino phase
ext_summary <- lim_ext_year_phase %>%
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

# Proportion of extensions in el nino by species
ggplot(ext_summary, aes(x = reorder(latin_name, -proportion_peak_or_end), y = proportion_peak_or_end)) +
  geom_col(fill = "cyan4") +
  coord_flip() +
  labs(
    title = "Proportion of Extensions in El Niño Peak/End Years (3+ extensions required)",
    x = "Species",
    y = "Proportion of Extensions in El Niño Peak/End Years"
  ) +
  theme_minimal()

# Proportion of extensions histogram
ggplot(ext_summary, aes(x = proportion_peak_or_end)) +
  geom_histogram(binwidth = 0.1, fill = "cyan4", color = "white", boundary = 0) +
  geom_vline(xintercept = en_freq_month, linetype = "dashed", color = "red") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of Species by Proportion of Extensions During El Niño Peak/End Years\n(El Niño frequency in red dotted line)",
    x = "Proportion of Extensions in El Niño Peak/End Years",
    y = "Number of Species"
  ) +
  theme_minimal()



# STATS
## El Nino wo blob - el nino month resolution
# Observed counts
observed <- c(sum(ext_summary$peak_or_end_extensions), sum(ext_summary$total_extensions)-sum(ext_summary$peak_or_end_extensions))  # 35 El Niño, 65 Not El Niño
# Expected proportions
expected_proportions_month <- c(en_freq_month, (1-en_freq_month))
# Run chi-squared goodness-of-fit test
chisq.test(x = observed, p = expected_proportions_month)

# Plotting The Blob 2014-2016 -------------------------------------------------------

# Identify the species with X+ events and filter for those species
species_with_group2plus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 0)) %>% # 2 would be three events (0, 1, 2)
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

# The blob df (extension events 2014-2016)
blob_df <- ext_Xplus %>%
  filter(first_year>=2014 & first_year<=2016)

# Arrow map colored by species the species
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = blob_df, 
               aes(x = longitude, y = hist_range_lat, 
                   xend = longitude, yend = latitude, color = latin_name),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-130, -110), ylim = c(30, 50), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions 2014-2016 (1+ extension required)", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Year")
# Arrow map colored by extension year
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = blob_df, 
               aes(x = longitude, y = hist_range_lat, 
                   xend = longitude, yend = latitude, color = factor(first_year)),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-130, -110), ylim = c(30, 50), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions 2014-2016 (1+ extension required)", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Year")
# Arrow may colored by species and faceted by the extension year
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = blob_df, 
               aes(x = longitude, y = hist_range_lat, 
                   xend = longitude, yend = latitude, 
                   color = latin_name),   # Color remains by species
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-130, -110), ylim = c(30, 50), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions 2014-2016 (1+ extension required)", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Species") +                # Change legend title to 'Species'
  facet_wrap(~ first_year)

# Time series
ggplot(blob_df, aes(x = year, y = latin_name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot - Plot the Blob"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(2013, 2017),
                     breaks = seq(2013, 2017, by = 1))
# Blob Stats
lim_first_ext <- ext_Xplus %>%
  group_by(latin_name, group_id) %>%
  slice_min(year)
nrow(lim_first_ext) # number of extensions
lim_first_ext_blob <- ext_Xplus %>%
  group_by(latin_name, group_id) %>%
  slice_min(year) %>%
  filter(first_year>=2014 & first_year<=2016)
nrow(lim_first_ext_blob) # number of blob extensions
