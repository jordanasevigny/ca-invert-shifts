# script to make the list of classes and phylum to use for review

# libraries
library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(taxize)
library(purrr)

# load processed WoRMS data
data <- read_excel(here("processed_data", "WoRMS_taxlist_20250211_processed.xlsx"))

# screen classes
classes <- data %>%
  filter(!is.na(Class)) %>%
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  select(Class) %>%
  unique() %>%
  rename(taxa = Class)
classes_ID <- data %>%
  filter(!is.na(Class)) %>%
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  select(AphiaID) %>%
  unique()



# screen phylums & their associated scientific names (sub phylums / super classes)
# first just look at the phylums with associated classes to screen phylums that majoratively are not applicable
# identify unwanted phyla (> 50% classes excluded)
phylums_class_to_excl <- data %>%
  filter(taxonRank=='Class') %>%
  select(c('Phylum', 'JKS Notes for Exclusion')) %>%
  group_by(Phylum) %>%
  summarise(
    total_count = n(),
    excl_count = sum(`JKS Notes for Exclusion` != "" & !is.na(`JKS Notes for Exclusion`)),
    frac = excl_count/total_count) %>%
  filter(frac > 0.5 ) %>%
  select(Phylum)

# filter out unwanted phyla
phylums <- data %>%
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  select(Phylum) %>%
  unique() %>%
  anti_join(phylums_class_to_excl) %>%
  rename(taxa = Phylum)

inter_taxa = data %>% 
  filter(is.na(`JKS Notes for Exclusion`)) %>%
  select(ScientificName) %>%
  rename(taxa = ScientificName)

# join phylums and classes to keep
class_phylums <- rbind(classes, inter_taxa, phylums) %>%
  unique()



########################
# add orders for desired classes


ds <- lapply(classes_ID$AphiaID, function(ID) downstream(ID, downto = "order", db = "worms"))

ds_df <- mapply(function(lst, name) {
  if (is.list(lst) && is.data.frame(lst[[1]]) && nrow(lst[[1]]) > 0) { 
    lst[[1]]$class <- name  # Add 'class' column only if it has rows
  }
  return(lst)
}, ds, classes$taxa, SIMPLIFY = FALSE)

ds_df <- mapply(function(lst, name) {
  if (is.list(lst) && is.data.frame(lst[[1]]) && nrow(lst[[1]]) > 0) { 
    lst[[1]]$class_id <- name  # Add 'class' column only if it has rows
  }
  return(lst)
}, ds_df, classes_ID$AphiaID, SIMPLIFY = FALSE)

ds_df[[2]][[1]]

# Extract all data frames from the list and combine them
combined_df <- bind_rows(lapply(ds_df, function(lst) {
  if (is.list(lst) && is.data.frame(lst[[1]])) {
    return(lst[[1]])  # Extract the inner data frame
  } else {
    return(NULL)  # Ignore non-data-frame elements
  }
}), .id = "source")  # Add a column to track which list element each row came from

# clean to just orders names
orders <- combined_df %>%
  select(name) %>%
  mutate(taxa = str_extract(name, "[A-Z][a-z]+")) %>%
  select(taxa)

# join phylums and classes to keep
ocp <- rbind(class_phylums, orders) %>%
  mutate(taxa = str_extract(taxa, "[A-Z][a-z]+")) %>%
  unique()
# make sure there are no NAs

# transform to be WOS format - includes * before and after word; drops the last letter (e.g. allows decapoda to be decapod)
final_string_ocp <- ocp %>%
  mutate(taxa = str_sub(taxa, 1, -2)) %>%
  mutate(words = str_c('"*', taxa, '*"')) %>% 
  pull(words) %>%
  str_c(collapse = " OR ")

cat(final_string_ocp)
