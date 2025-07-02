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

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

#Lload a world map
world <- ne_countries(scale = "medium", returnclass = "sf")


# Load enso data
# I TOGGLE BETWEEN USING phase == 'Warm Phase/El Nino' and dSST3.4 > 0 HERE
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)
#write.csv(enso_df, "enso_data.csv")


# Make El Niño data plottable ---------------------------------------------

enso <- enso %>%
  select(c(enso_df, Month, phase)) %>%
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
  ungroup()




