# Load in CalCOFI review data and rename columns
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/02/2025
rm(list = ls())
library(readxl)
library(tidyr)
library(stringr)
library(purrr)
library(dplyr)
library(grateful)
cite_packages(out.dir = ".")            # save report to working directory

# load in data
ca <- read_excel("data/CalCOFI Coding Form (Responses).xlsx")


# Identify the metadata columns by name or position
metadata_cols <- c("Timestamp", "Email Address", "Editor Name", "Excerpt ID", "Should this excerpt be included or excluded?", "If excluded, why?", "Notes: Please enter info on anything unusual in how this information was coded.")  

# Extract question columns (e.g., 1.1 ..., 2.1 ..., etc.)
question_blocks <- ca %>%
  dplyr::select(-all_of(metadata_cols)) %>%
  names() %>%
  str_extract("^\\d+(?=\\.)") %>%
  unique()

# For each block (1, 2, ..., 10), extract those columns and rename them to just .X
ca_blocks <- map_dfr(question_blocks, function(block_num) {
  # Get the columns from that block
  block_cols <- names(ca)[str_detect(names(ca), paste0("^", block_num, "\\."))]
  
  # Extract that block, and rename the questions to drop the prefix (e.g., '1.1 qA' → 'qA')
  ca_block <- ca %>%
    select(all_of(metadata_cols), all_of(block_cols)) %>%
    rename_with(
      .fn = ~ str_replace(.x, paste0("^", block_num, "\\.\\s*"), ""),
      .cols = all_of(block_cols)
    ) %>%
    mutate(block = block_num)
  
  return(ca_block)
})

# --- Cleaning data ---
# Drop repeat column lacking data
ca_blocks <- select(ca_blocks, -`23Is Day exact or approximate?`)

# Identify question columns 2–24 (adjust the names as needed)
question_cols <- as.character(2:26)

# If your columns are named like "2 Question text", "3 Question text", etc.
# and you want to match those that start with "2 ", "3 ", ..., "26 "
question_cols_pattern <- paste0("^(", paste(question_cols, collapse = "|"), ")\\b")

# Select those column names
cols_to_check <- names(ca_blocks)[grepl(question_cols_pattern, names(ca_blocks))]

# Filter rows where NOT all of those columns are NA and filter out a replicate Jordana did to compare to Bella's work
ca_clean <- ca_blocks %>%
  filter(!if_all(all_of(cols_to_check), is.na)) %>%
  filter(is.na(`Notes: Please enter info on anything unusual in how this information was coded.`) |
           `Notes: Please enter info on anything unusual in how this information was coded.` != "test input to compare to Bella's")

colnames(ca_clean)

# Rename columns
colnames(ca_clean) <- c(
  "timestamp",
  "email",
  "editor",
  "excerpt_id",
  "include_exclude",
  "exclude_reason",
  "coding_notes",
  "species_verbatim",
  "latin_name",
  "latin_name_source",
  "latin_name_location",
  "taxonomic_rank",
  "common_name",
  "common_name_source",
  "common_name_location",
  "location_verbatim",
  "latitude",
  "latitude_certainty",
  "latitude_location",
  "longitude",
  "longitude_certainty",
  "longitude_location",
  "observation_time_verbatim",
  "year",
  "year_certainty",
  "month",
  "month_certainty",
  "day",
  "day_certainty",
  "trait_info",
  "historical_northern_latitude",
  "historical_latitude_source",
  "block"
)

write.csv(ca_clean, "processed_data/calcofi_review_data_clean.csv")
