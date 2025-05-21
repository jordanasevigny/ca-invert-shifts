# preliminary data map
# August 13, 2024
# Updated May 16, 2025
# Jordana Sevigny
# jsevigny@ucsc.edu
# resources
## https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
## https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html


# libraries

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

# load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# load in data
# poor naming scheme but the file with the similar name but excel has two sheets with the other having the original excerpt
df <- read_excel("data/20250519_CalCOFI Coding Form (Responses).xlsx")


# Identify the metadata columns by name or position
metadata_cols <- c("Timestamp", "Email Address", "Editor Name", "Excerpt ID", "Should this excerpt be included or excluded?", "If excluded, why?", "Notes: Please enter info on anything unusual in how this information was coded.")  

# Extract question columns (e.g., 1.1 ..., 2.1 ..., etc.)
question_blocks <- df %>%
  select(-all_of(metadata_cols)) %>%
  names() %>%
  str_extract("^\\d+(?=\\.)") %>%
  unique()

# For each block (1, 2, ..., 10), extract those columns and rename them to just .X
df_blocks <- map_dfr(question_blocks, function(block_num) {
  # Get the columns from that block
  block_cols <- names(df)[str_detect(names(df), paste0("^", block_num, "\\."))]
  
  # Extract that block, and rename the questions to drop the prefix (e.g., '1.1 qA' → 'qA')
  df_block <- df %>%
    select(all_of(metadata_cols), all_of(block_cols)) %>%
    rename_with(
      .fn = ~ str_replace(.x, paste0("^", block_num, "\\.\\s*"), ""),
      .cols = all_of(block_cols)
    ) %>%
    mutate(block = block_num)
  
  return(df_block)
})

# --- Cleaning data ---
# Drop repeat column lacking data
df_blocks <- select(df_blocks, -`23Is Day exact or approximate?`)

# Identify question columns 2–24 (adjust the names as needed)
question_cols <- as.character(2:24)

# If your columns are named like "2 Question text", "3 Question text", etc.
# and you want to match those that start with "2 ", "3 ", ..., "24 "
question_cols_pattern <- paste0("^(", paste(question_cols, collapse = "|"), ")\\b")

# Select those column names
cols_to_check <- names(df_blocks)[grepl(question_cols_pattern, names(df_blocks))]

# Filter rows where NOT all of those columns are NA and filter out a replicate Jordana did to compare to Bella's work
df_clean <- df_blocks %>%
  filter(!if_all(all_of(cols_to_check), is.na)) %>%
  filter(is.na(`Notes: Please enter info on anything unusual in how this information was coded.`) |
           `Notes: Please enter info on anything unusual in how this information was coded.` != "test input to compare to Bella's")

colnames(df_clean)


# --- Inspecting data ---
df_inc <- df_clean %>%
  filter(`Should this excerpt be included or excluded?` == "Include")
unique_sp_latin <- unique(df_inc$`3 Latin name (e.g., pleuroncodes planipes)`)
print(unique_sp_latin)
years <- unique(df_inc$`18 Year (if range like 1957-1958, put most recent like 1958)`)
print(years)


# --- Plotting data ---
# Plot freq of species extensions by year

df_sp_yr <- df_inc %>%
  filter(!is.na(`18 Year (if range like 1957-1958, put most recent like 1958)`), !is.na(`3 Latin name (e.g., pleuroncodes planipes)`)) %>%
  distinct(`18 Year (if range like 1957-1958, put most recent like 1958)`, `3 Latin name (e.g., pleuroncodes planipes)`)

ggplot(df_sp_yr, aes(x = `18 Year (if range like 1957-1958, put most recent like 1958)`, y = `3 Latin name (e.g., pleuroncodes planipes)`)) +
  geom_point() +
  scale_x_continuous(breaks = seq(min(df_sp_yr$`18 Year (if range like 1957-1958, put most recent like 1958)`), max(df_sp_yr$`18 Year (if range like 1957-1958, put most recent like 1958)`), by = 5)) + 
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot"
  ) +
  theme_minimal()


# Map extensions
northmost_points_perspyr <- df_inc %>%
  filter(!is.na(`18 Year (if range like 1957-1958, put most recent like 1958)`), !is.na(`3 Latin name (e.g., pleuroncodes planipes)`), !is.na(`11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`)) %>%
  group_by(`18 Year (if range like 1957-1958, put most recent like 1958)`, `3 Latin name (e.g., pleuroncodes planipes)`) %>%
  slice_max(order_by = `11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`, n = 1, with_ties = FALSE) %>%
  ungroup()

ggplot(data = world) +
  geom_sf() +
  geom_point(data = northmost_points_perspyr, aes(x = `14 Longitude (e.g., -127.123, if longitude is a range, put the farthest east/towards the coast)`, y = `11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`, color = `3 Latin name (e.g., pleuroncodes planipes)`), size = 2.5) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color="Unusual Observation Location") +
  coord_sf(xlim = c(-115, -135), ylim = c(30, 56), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = northmost_points_perspyr,
    aes(
      x = `14 Longitude (e.g., -127.123, if longitude is a range, put the farthest east/towards the coast)`,
      y = `11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`,
      color = `3 Latin name (e.g., pleuroncodes planipes)`
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location") +
  coord_sf(xlim = c(-135, -115), ylim = c(30, 56), expand = FALSE)

# --- Ignore the following ---
################################

# Old preliminary code from belore update in May 2025
# drop unused columns, select distinct rows only (when a single species has multiple recorded shifts, their origin is repeated, we don't need that)
df <- data.frame(raw) %>%
  select(-c(Report_Volume, Observation_Date, Notes)) %>%
  distinct()

# need to add some longitude jitter into the points so they all show up
# Initialize a counter for each word
loc_count <- list()
# longitude error base amount
er <- -1.25

# Loop through each row of the data frame
for (i in 1:nrow(df)) {
  location <- df$Observation_Location[i]
  
  # Update the counter for the word
  if (is.null(loc_count[[location]])) {
    loc_count[[location]] <- 0
  } else {
    loc_count[[location]] <- loc_count[[location]] + 1
  }

  # Add the error times the iteration to the longitude 
  df$Longitude[i] <- df$Longitude[i] + er*loc_count[[location]]
}  


# map with species points
ggplot(data = world) +
  geom_sf() +
  geom_point(data = df, aes(x = Longitude, y = Latitude, shape = Species, color = Old_or_New), size = 2.5) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color="Range Edge") +
  coord_sf(xlim = c(-110, -150), ylim = c(26, 63), expand = FALSE)


# map with ENSO/species points
ggplot(data = world) +
  geom_sf() +
  geom_point(data = df, aes(x = Longitude, y = Latitude, shape = ENSO, color = Species), size = 2.5) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color="Range Edge") +
  coord_sf(xlim = c(-110, -150), ylim = c(26, 63), expand = FALSE)


# same data as above, just going back to original coordinates

# Identify duplicated locations per species

df <- data.frame(raw) %>%
  group_by(Species, Longitude, Latitude) %>%
  mutate(n = n(),  # Count occurrences of each (Longitude, Latitude)
         rank = row_number() - (n() + 1) / 2) %>%  # Assign a centered rank (-1, 0, 1, etc.)
  ungroup()

# Apply uniform vertical spacing (e.g., shift by 0.1 degrees per duplicate)
spacing_factor <- 2  # Adjust as needed
df <- df %>%
  mutate(Longitude = ifelse(n > 1, Longitude + rank*spacing_factor, Longitude))

# Now plot with modified latitude
ggplot(data = world) +
  geom_sf() +
  geom_point(data = df, aes(x = Longitude, y = Latitude, shape = ENSO, color = Old_or_New), 
             size = 2.5) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Range Edge", shape = "ENSO Phase") +
  coord_sf(xlim = c(-150, -110), ylim = c(26, 63), expand = FALSE) +
  facet_wrap(~Species) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=8),  # Rotate x-axis labels
    plot.margin = margin(10, 10, 40, 10),  # Increase bottom margin
    panel.spacing = unit(1.25, "lines")
  )


