# The Blob Analysis
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/30/2025


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

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Prep review data for analysis --------------------------------------------

# Identify the species with X+ events and filter for those species
species_with_group2plus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 0)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

# Filter full dataset for those species
ext_Xplus <- df %>%
  filter(latin_name %in% species_with_group2plus)

# Filter to just keep the first extension of each event (double check that there aren't duplicates)
first_ext <- ext_Xplus %>%
  filter(year == first_year)

# Tally the number of extensions events for each year
extension_counts <- first_ext %>%
  count(year, name = "n_extensions")

# Add Date (middle of the year) column to align with ONI
extension_counts$Date <- as.Date(paste0(extension_counts$year, "-06-15"))


# Bar graph of 2011-13; 2014-16; 2017-18 extensions -----------------------

pre_11_13 <- extension_counts %>% filter(year >= 2011 & year <= 2013)
pre_11_13_c <- sum(pre_11_13$n_extensions)
blob_14_16 <- extension_counts %>% filter(year >= 2014 & year <= 2016)
blob_14_16_c <- sum(blob_14_16$n_extensions)
post_17_19 <- extension_counts %>% filter(year >= 2017 & year <= 2019)
post_17_19_c <- sum(post_17_19$n_extensions)

dataset_c <- tibble(
  period = c("2011–2013", "2014–2016", "2017–2019"),
  n_extensions = c(pre_11_13_c, blob_14_16_c, post_17_19_c)
)

ggplot(dataset_c, aes(x = period, y = n_extensions)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Time Period",
    y = "Number of Extension Events",
    title = "Species Range Extension Events by Time Period"
  ) +
  theme_minimal()



# Blob vs not blob extension analysis -------------------------------------

extension_counts <- extension_counts %>%
  mutate(period = case_when(
    year >= 2014 & year <= 2016 ~ "blob",
    TRUE ~ "non_blob"
  ))

ggplot(extension_counts, aes(x = year, y = n_extensions, fill = period)) +
  geom_col() +
  scale_fill_manual(values = c("non_blob" = "gray70", "blob" = "firebrick")) +
  theme_minimal()

mean(extension_counts$n_extensions)
var(extension_counts$n_extensions)
# var >> mean -> can't use poisson

library(MASS)
model_nb <- glm.nb(n_extensions ~ period, data = extension_counts)
summary(model_nb)

# ENSO - Blob Analysis ---------------------------------------------------------------

# Load enso data
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)

# Find a reasonable scaling factor
# Example: if max n_extensions ≈ 100 and max dSST3.4 ≈ 2.5, then:
scale_factor <- 3  # Adjust this based on your data

ggplot() +
  geom_col(
    data = extension_counts,
    aes(x = Date, y = n_extensions, fill = period)
  ) +
  scale_fill_manual(values = c("non_blob" = "gray70", "blob" = "firebrick")) +
  
  # Plot rescaled dSST3.4 (to align with n_extensions)
  geom_line(
    data = enso_df,
    aes(x = Date, y = ONI * scale_factor),
    linetype = "twodash",
    color = "black"
  ) +
  
  # Add the secondary axis
  scale_y_continuous(
    name = "Number of Extensions",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "ONI"
    )
  ) +
  
  scale_x_date(
    limits = as.Date(c("1950-01-01", "2020-01-01")),
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  
  theme_minimal()
