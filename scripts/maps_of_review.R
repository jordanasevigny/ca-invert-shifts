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
#install.packages("gifski")
#install.packages("av")  

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

#Lload a world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# Load enso data
# I TOGGLE BETWEEN USING phase == 'Warm Phase/El Nino' and dSST3.4 > 0 HERE
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)
#write.csv(enso_df, "enso_data.csv")


# Make El Niño data plottable ---------------------------------------------

enso <- enso_df %>%
  select(c(Year, Month, phase)) %>%
  filter(phase == 'Warm Phase/El Nino') %>%
  distinct() %>%
  select(Year, Month)
# enso_yr <- enso_df %>%
#   select(c(Year, Month, dSST3.4)) %>%
#   filter(dSST3.4 > 0) %>%
#   distinct() %>%
#   select(Year, Month)

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
    new_event = month_diff > 3/12,  # if jump > 1 month, new group
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



# Prep review data for mapping --------------------------------------------

# Choose the northernmost observation for each species/year combo
north_df <- df %>%
  group_by(year, latin_name) %>%
  slice_max(order_by = latitude, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(!is.na(latin_name))

ext_3plus <- north_df %>%
  arrange(latin_name, year) %>%
  group_by(latin_name) %>%
  mutate(
    year_diff = year - lag(year, default = first(year)),
    new_group = if_else(year_diff > 1, 1, 0),
    group_id = cumsum(new_group)
  ) %>%
  ungroup() %>%
  select(-c(year_diff, new_group)) %>%
  group_by(latin_name, group_id) %>%
  mutate(first_year = min(year)) %>%
  ungroup() %>%
  filter(group_id >= 2) # group ids start at 0 so 2 is group 3

# Plots of historical range edge vs extension position --------------------------------------------
# Arrow map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = ext_3plus, 
               aes(x = longitude, y = average_hist_latitude, 
                   xend = longitude, yend = latitude, color = latin_name),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-150, -110), ylim = c(20, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Year")


# Animated Map
p <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = ext_3plus, 
               aes(x = longitude, y = average_hist_latitude, 
                   xend = longitude, yend = latitude, color = latin_name),
               arrow = arrow(length = unit(0.2, "cm")),
               linewidth = 0.7) +
  transition_states(year, transition_length = 1, state_length = 1) +
  labs(title = "Year: {closest_state}") +
  coord_sf(xlim = c(-150, -110), ylim = c(20, 60), expand = FALSE)
p + theme(legend.position = "bottom")
anim <- animate(p, renderer = gifski_renderer(), width = 1000, height = 750, fps = 3)
anim
anim_save("preliminary_plots/range_extensions.gif", animation = anim)
dev.off()

# Need to figure out what to do with these ones. Maybe look more closely at "historical range edge"
north_df_lower <- ext_3plus %>%
  filter(latitude < average_hist_latitude)


# Plotting The Blob and 2015-2016 El Nino -------------------------------------------------------

# The blob df (extension events beginning 2013 and 2014)
blob_df <- ext_3plus %>%
  filter(first_year==2013 | first_year==2014)

# Arrow map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = blob_df, 
               aes(x = longitude, y = average_hist_latitude, 
                   xend = longitude, yend = latitude, color = latin_name),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-150, -110), ylim = c(20, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions Beginning 2013 & 2014", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Year")

# The blob and 2015 El Nino df (extension events beginning 2013-2016)
blob_en_df <- ext_3plus %>%
  filter(first_year>=2013 & first_year<=2016)

# Arrow map
ggplot() +
  geom_sf(data = world, fill = "gray90", color = "black") +
  geom_segment(data = blob_en_df, 
               aes(x = longitude, y = average_hist_latitude, 
                   xend = longitude, yend = latitude, color = latin_name),
               arrow = arrow(length = unit(0.2, "cm")),
               size = 0.7) +
  coord_sf(xlim = c(-150, -110), ylim = c(20, 60), expand = FALSE) +
  theme_minimal() +
  labs(title = "Species Range Extensions Beginning 2013-2016", 
       x = "Longitude", 
       y = "Latitude", 
       color = "Year")

# Plotting El Nino vs not El Nino Extensions -------------------------------------------------------


# El Nino frequency
## Monthly res
en_count <- enso_df %>%
  filter(str_detect(phase, "Warm")) %>%
  tally()
en_freq_month <- en_count$n / nrow(enso_df)

## Yearly res
oni_years_all <- enso_df %>%
  select(Year) %>%
  distinct()

en_freq_year <- nrow(oni_years) / nrow(oni_years_all)

## SST > 0 res
sst_count <- enso_df %>%
  filter(dSST3.4 > 0) %>%
  tally()
en_freq_sst <- sst_count$n / nrow(enso_df)

# Label Extensions during El Nino
group_flags <- ext_3plus %>%
  group_by(latin_name, group_id) %>%
  summarise(first_year = min(year), .groups = "drop") %>%
  mutate(started_during_enso = if_else(first_year %in% enso_years$Year, "y", NA_character_))
enso_ext <- group_flags %>%
  filter(started_during_enso == "y")

