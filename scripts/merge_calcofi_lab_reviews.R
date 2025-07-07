# Merge CalCOFI and lab reviews
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025

library(dplyr)

# Load data
ca_rev <- read.csv("processed_data/calcofi_review_data_clean.csv")
lab_rev <- read.csv("processed_data/lab_review_with_longitudes.csv")

# Filter to included data
ca_rev_inc <- filter(ca_rev, include_exclude == "Include")
lab_rev_inc  <- filter(lab_rev, include_exclude == "Include")

# Merge like columns of the two datasets

# Identify shared columns
shared_cols <- intersect(names(ca_rev_inc), names(lab_rev_inc))

# Select only shared columns and add a source column
ca_rev_inc_common <- ca_rev_inc[, shared_cols] %>%
  mutate(source = "ca_rev")

lab_rev_inc_common <- lab_rev_inc[, shared_cols] %>%
  mutate(source = "lab_rev")

# Convert data types
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(year, as.integer))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(year, as.integer))
ca_rev_inc_common <- ca_rev_inc_common %>%
  mutate(across(day, as.character))
lab_rev_inc_common <- lab_rev_inc_common %>%
  mutate(across(day, as.character))

# Row-bind the two
merged_df <- bind_rows(ca_rev_inc_common, lab_rev_inc_common)

# Drop Include Exclude column
merged_df <- merged_df %>%
  select(-include_exclude)

# Make latin names uppercase
merged_df$latin_name <- gsub("^(\\w)", "\\U\\1", merged_df$latin_name, perl = TRUE)

# Find average latitude for historical range edge THIS DOESN'T WORK YET
merged_df_histave <- merged_df %>%
  group_by(latin_name) %>%
  mutate(average_hist_latitude = mean(historical_northern_latitude, na.rm=TRUE))

# Write dataframe
write.csv(merged_df_histave, "processed_data/merged_calcofi_lab_review.csv")
          