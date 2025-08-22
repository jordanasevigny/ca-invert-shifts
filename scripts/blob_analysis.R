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

# #2B5275FF = la nina ; #D16647FF = el nino ; gray60 = enso outline / oni ; black at alpha=0.6 = extension event tallies ; #A69F55FF = extension stats ; #FFFBDDFF = blob (white fill)
# theme_minimal(base_size = 16) for all ggplot

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
  geom_col(fill = "black", alpha=0.6) +
  labs(
    x = "Time Period",
    y = "Number of Extension Events",
    title = "Species Range Extension Events by Time Period"
  ) +
  theme_minimal(base_size = 16)



# Blob vs not blob extension analysis -------------------------------------

extension_counts <- extension_counts %>%
  mutate(period = case_when(
    year >= 2014 & year <= 2016 ~ "blob",
    TRUE ~ "non_blob"
  ))

ggplot(extension_counts, aes(x = year, y = n_extensions, fill = period)) +
  geom_col(color = "black") +  # Adds black outline to all bars
  scale_fill_manual(values = c("non_blob" = "gray70", "blob" = "white")) +
  theme_minimal()

mean(extension_counts$n_extensions)
var(extension_counts$n_extensions)
# var >> mean -> can't use poisson


# ENSO - Blob Analysis ---------------------------------------------------------------

# Load enso data
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)

# Find a reasonable scaling factor
# Example: if max n_extensions ≈ 100 and max dSST3.4 ≈ 2.5, then:
scale_factor <- 3  # Adjust this based on your data

blob <- ggplot() +
  geom_col(
    data = extension_counts,
    aes(x = Date, y = n_extensions, fill = period), color = "gray40", alpha=0.6
  ) +
  scale_fill_manual(values = c("non_blob" = "white", "blob" = "black"), labels = c("blob" = "2014-2016",
                                                                                   "non_blob" = "Other Years")) +
  
  # Plot rescaled dSST3.4 (to align with n_extensions)
  geom_line(
    data = enso_df,
    aes(x = Date, y = ONI * scale_factor),
    linetype = "twodash",
    color = "black",
    size=0.5
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
    limits = as.Date(c("1995-01-01", "2020-01-01")),
    date_breaks = "3 years",
    date_labels = "%Y"
  ) +
  labs(x = "") +
  theme_minimal(base_size = 22) +
  theme(legend.position = "none"
      #  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      )
ggsave("figures/Figure4Horz.png", plot = blob, width = 14, height = 4, unit = "in", dpi = 600)

sum(extension_counts$n_extensions[extension_counts$period == "blob"]) # num blob extensions
sum(extension_counts$n_extensions) # num total extensions
sum(extension_counts$n_extensions[extension_counts$period == "blob"]) / sum(extension_counts$n_extensions) # fraction extensions during blob


# STATS - chi sq test
# Observed counts
yrs_dif <- as.numeric(max(extension_counts$Date) - min(extension_counts$Date)) / 365
blob_prop <- (2017-2014)/yrs_dif
observed <- c(sum(extension_counts$n_extensions[extension_counts$period == "blob"]), sum(extension_counts$n_extensions)-sum(extension_counts$n_extensions[extension_counts$period == "blob"]))  # 35 El Niño, 65 Not El Niño
# Expected proportions
expected_proportions <- c(blob_prop, (1-blob_prop))
# Run chi-squared goodness-of-fit test
chisq.test(x = observed, p = expected_proportions)

# What species had blob extensions?
first_ext_periods <- first_ext %>%
  mutate(period = case_when(
    year >= 2014 & year <= 2016 ~ "blob",
    TRUE ~ "non_blob"
  ))
# species in blob years
species_blob <- unique(first_ext_periods$latin_name[first_ext_periods$period == "blob"])

# species in non-blob years
species_nonblob <- unique(first_ext_periods$latin_name[first_ext_periods$period == "non_blob"])

# species that ONLY appear in blob years
species_only_blob <- setdiff(species_blob, species_nonblob)

length(species_only_blob)
species_only_blob
