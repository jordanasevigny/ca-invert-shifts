# Script to make the list of classes and phylum to use for review
# By: Jordana Sevigny, jordana.sevigny@gmail.com

rm(list = ls())

# libraries
library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(taxize)
library(purrr)

# load processed WoRMS data
# SWITCHED TO V2, WHICH INCLUDES SMALL CRITTERS
data <- read_excel(here("processed_data", "WoRMS_taxlist_20250211_processed_V2.xlsx"))

# Filter for class and drop anything with a reason for exclusion
classes <- data %>%
  filter(!is.na(Class)) %>%
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  dplyr::select(Class) %>%
  unique() %>%
  rename(taxa = Class)

# Isolate the alpha ids for the classes
classes_ID <- data %>%
  filter(!is.na(Class)) %>%
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  dplyr::select(AphiaID) %>%
  unique()

# Filter for phylums & their associated scientific names (sub phylums / super classes)
# First just look at the phylums with associated classes to drop phylums that majoratively are not applicable
# Identify unwanted phyla (> 50% classes excluded)
# This is just a lot of code to systematically remove phylum chordata

phylums_class_to_excl <- data %>%
  filter(taxonRank=='Class') %>%
  dplyr::select(c('Phylum', 'JKS Notes for Exclusion')) %>%
  group_by(Phylum) %>%
  summarise(
    total_count = n(),
    excl_count = sum(`JKS Notes for Exclusion` != "" & !is.na(`JKS Notes for Exclusion`)),
    frac = excl_count/total_count) %>%
  filter(frac > 0.5 ) %>%
  dplyr::select(Phylum) %>%
  rename(taxa = Phylum)

# # Get a list of the relevant phylums but filter out unwanted phyla - not necessary but here if helpful
# phylums <- data %>%
#   filter(is.na(`JKS Notes for Exclusion`)) %>%
#   dplyr::select(Phylum) %>%
#   unique() %>%
#   rename(taxa = Phylum) %>%
#   anti_join(phylums_class_to_excl)

# Get all the class through phylum names and drop unwanted phyla
inter_taxa = data %>% 
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  dplyr::select(ScientificName) %>%
  rename(taxa = ScientificName) %>%
  anti_join(phylums_class_to_excl)

# Get just the unique names in case there are overlap between taxa levels
class_phylums <- inter_taxa %>%
  unique()

########################
# Get orders for desired classes

# Obtain everything down to the desired orders from WoRMs database
ds <- lapply(classes_ID$AphiaID, function(ID) downstream(ID, downto = "order", db = "worms"))
# 
# # Adds class name
# ds_df <- mapply(function(lst, name) {
#   if (is.list(lst) && is.data.frame(lst[[1]]) && nrow(lst[[1]]) > 0) { 
#     lst[[1]]$class <- name  # Add 'class' column only if it has rows
#   }
#   return(lst)
# }, ds, classes$taxa, SIMPLIFY = FALSE)
# 
# # Adds class id
# ds_df <- mapply(function(lst, name) {
#   if (is.list(lst) && is.data.frame(lst[[1]]) && nrow(lst[[1]]) > 0) { 
#     lst[[1]]$class_id <- name  # Add 'class' column only if it has rows
#   }
#   return(lst)
# }, ds_df, classes_ID$AphiaID, SIMPLIFY = FALSE)

# Extract all data frames from the list and combine them into a dataframe
combined_df <- bind_rows(lapply(ds, function(lst) {
  if (is.list(lst) && is.data.frame(lst[[1]])) {
    return(lst[[1]])  # Extract the inner data frame
  } else {
    return(NULL)  # Ignore non-data-frame elements
  }
}), .id = "source")  # Add a column to track which list element each row came from

# Clean to just orders names
orders <- combined_df %>%
  dplyr::select(name) %>%
  mutate(taxa = str_extract(name, "[A-Z][a-z]+")) %>%
  dplyr::select(taxa)

# Join phylums and classes to keep
ocp <- rbind(class_phylums, orders) %>%
  mutate(taxa = str_extract(taxa, "[A-Z][a-z]+")) %>%
  unique()
# make sure there are no NAs

# Transform to be WOS format - includes * before and after word; drops the last letter (e.g. allows decapoda to be decapod)
final_string_ocp <- ocp %>%
  mutate(taxa = str_sub(taxa, 1, -2)) %>%
  mutate(words = str_c('"*', taxa, '*"')) %>% 
  pull(words) %>%
  str_c(collapse = " OR ")

# This prints the string of all order/class/phylum names in the WOS format
cat(final_string_ocp)

# next move is to copy and paste the printed output into the species name section of the whole WOS search terms in OneNote
