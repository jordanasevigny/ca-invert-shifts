# Find different taxa levels of our species
# Author: Jordana Sevigny
# Date: 8/17/2025

rm(list = ls())

# Load libraries
library(dplyr)
library(taxize)
library(ggplot2)

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

sp_list <- unique(df$latin_name)

# Query classification using ITIS (default) or other databases
out <- classification(sp_list, db = "worms")

# Extract phylum for each species
taxa <- sapply(out, function(x) {
  if (!is.data.frame(x)) return(NA)              # handle errors
  phyl_row <- x$name[x$rank == "Order"]         # pick the name where rank == "Phylum" or "Class" etc.
  if (length(phyl_row) == 0) return(NA)          # if not found
  phyl_row[1]
})

# Turn into a data frame with species name
taxa_df <- data.frame(
  species = names(taxa),
  taxon  = unname(taxa),
  stringsAsFactors = FALSE
)

taxa_df


# Summarize counts
taxa_counts <- taxa_df %>%
  drop_na() %>%
  group_by(taxon) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)                   # proportion
         )


ggplot(taxa_counts, aes(x = "", y = prop, fill = taxon)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Species by Order (1+ Extension Required)") +
#  scale_fill_brewer(palette = "Dark2") +
  geom_text(aes(label = n), position=position_stack(vjust = 0.5), color = "white", size = 4)



# 3+ Extension  -----------------------------------------------------------
species_with_groupXplus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 2)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

taxa_Xplus <- taxa_df %>%
  filter(species %in% species_with_groupXplus) %>%
  drop_na() %>%
  group_by(taxon) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)                   # proportion
         )
dark2_10 <- c(
  "#1B9E77", # teal green
  "#D95F02", # burnt orange
  "#7570B3", # muted purple
  "#E7298A", # magenta-pink
  "#66A61E", # olive green
  "#E6AB02", # goldenrod
  "#A6761D", # brown
  "#666666", # dark gray (added)
  "#1F78B4", # deep blue (added)
  "#B2DF8A"  # light green (added)
)
ggplot(taxa_Xplus, aes(x = "", y = prop, fill = taxon)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Species by Order (3+ Extension Required)") +
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values = dark2_10) +
  geom_text(aes(label = n), position=position_stack(vjust = 0.5), color = "white", size = 4)

