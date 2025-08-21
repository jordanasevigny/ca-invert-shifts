# Extension bubble vs ENSO Time Series
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/24/2025

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
paletteer_d("lisa::Pierre_AugusteRenoir")

# Color palette
# #2B5275FF = la nina ; #D16647FF = el nino ; gray60 = enso outline / oni ; black at alpha=0.6 = extension event tallies ; #A69F55FF = free variable (green) ; #FFFBDDFF = blob (white fill)
# theme_minimal(base_size = 16) for all ggplot

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


ggplot() +
  # Ribbon for ONI > 0 (red)
  geom_ribbon(data = enso_df,
              aes(x = Date, ymin = 0, ymax = ifelse(ONI>0, ONI * scale_factor, 0)),
              fill = "#D16647FF", alpha = 0.4) +
  
  # Ribbon for ONI < 0 (blue)
  geom_ribbon(data = enso_df,
              aes(x = Date, ymin = ifelse(ONI < 0, ONI * scale_factor, 0), ymax = 0),
              fill = "#1A3146", alpha = 0.4) +
  
  # ONI line
  # geom_line(data = enso_df,
  #           aes(x = Date, y = ONI * scale_factor),
  #           color = "gray60", size = 0.5) +
  
  # Extensions as dots at y = 0, with size by count
  geom_point(data = extension_counts,
             aes(x = Date, y = 0, size = n_extensions),
             color = "black", alpha = 0.3) +
  
  # Horizontal zero line
  geom_hline(yintercept = 0, color = "gray60", linetype = "dashed") +
  
  # Y axis scaled for ONI only
  scale_y_continuous(
    name = "ONI (Oceanic Niño Index)",
    breaks = seq(-2, 2, 1) * scale_factor,
    labels = seq(-2, 2, 1)
  ) +
  
  scale_size_continuous(
    name = "Number of Extensions",
    range = c(1, 14),
    c(2, 6, 10, 14)
  ) +
  # scale_size_continuous(
  #   name = "Number of Extensions",
  #   range = c(1, 6),
  #   c(1, 3, 6)
  # ) +
  
  scale_x_date(
    limits = as.Date(c("1953-01-01", "2020-06-01")),
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  
  theme_minimal(base_size = 16) +
  labs(x = "Year", title = "Species Range Extensions vs. ONI (1+ extensions required)") +
  theme(
    legend.position = "right",
    axis.title.y.right = element_blank()
  )
sum(extension_counts$n_extensions) # total number of extension events
length(unique(first_ext$latin_name)) # Number of species with extension events


# CalCOFI vs Lab Review Extensions ----------------------------------------
# Tally the number of extensions events for each year
extension_counts_source <- first_ext %>%
  count(year, source, name = "n_extensions")
extension_counts_source$Date <- as.Date(paste0(extension_counts_source$year, "-06-15"))

#scale_factor <- 3 
ggplot() +
  geom_col(
    data = extension_counts_source,
    aes(x = Date, y = n_extensions, fill = source), color = "gray30"
  ) +
  scale_fill_manual(values = c("ca_rev" = "#7570B3", "lab_rev" = "#66A61E"), labels = c("ca_rev" = "CalCOFI",
                                                                                   "lab_rev" = "Lit. Review")) +
  
  # # Plot rescaled dSST3.4 (to align with n_extensions)
  # geom_line(
  #   data = enso_df,
  #   aes(x = Date, y = ONI * scale_factor),
  #   linetype = "twodash",
  #   color = "black"
  # ) +
  # 
  # 
  # # Add the secondary axis
  # scale_y_continuous(
  #   name = "Number of Extensions",
  #   sec.axis = sec_axis(
  #     ~ . / scale_factor,
  #     name = "ONI"
  #   )
  # ) +
  
  scale_x_date(,
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  labs(fill=" ", title = "Number of Extensions by CalCOFI and Literature Reveiw", y="Number of Extension Events", x="Year") +
  theme_minimal(base_size = 16)
