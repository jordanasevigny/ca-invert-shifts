# Map observation points for lab review
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 6/19/2025

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

# AS OF 6/19/2025, THIS DATA STILL NEEDS TO BE DEDUPLICATED

# load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Load review data
df <- read.csv("processed_data/lab_review_with_longitudes.csv")

# Load enso data
enso <- download_enso(climate_idx = "oni", create_csv = FALSE)
enso_yr <- enso %>%
  select(c(Year, phase)) %>%
  filter(phase == 'Warm Phase/El Nino') %>%
  distinct() %>%
  select(Year)

# Manually add historical el nino events from Quinn et al. 1987 for 1850-1950
# https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1029/JC092iC13p14449
# Their data extends back to 1525!
man_hist_enso <- c(1864, 1871, 1877, 1878, 1884, 1891, 1899, 1900, 1911, 1912, 1917, 1925, 1926, 1932, 1940, 1941)
man_hist_df <- data.frame(Year = man_hist_enso)

# Append manual data to ONI
enso_yr <- rbind(enso_yr, man_hist_df)

# Add enso to time series
enso_bands <- data.frame(
  xmin = enso_yr$Year - 0.5,
  xmax = enso_yr$Year + 0.5
)



# Filter for included data only
df_inc <- df %>%
  filter(Include.Exclude == "Include")
  

# Need to fig out what to do with year ranges - removing them for now
df_clean <- df_inc %>%
  filter(!grepl("-", Year)) %>%     # Drop year ranges like "2015-2016"
  mutate(Year = as.numeric(Year))   # Convert to numeric

# Filter for northernmost points in a year - lab review
northmost_points_perspyr <- df_clean %>%
  filter(!is.na(Year), !is.na(Latin.name), !is.na(lat_for_plot)) %>%
  group_by(Year, Latin.name) %>%
  slice_max(order_by = lat_for_plot, n = 1, with_ties = FALSE) %>%
  ungroup()

# --- Plotting data ---
# Plot freq of species extensions by year

# ggplot(df_inc, aes(x = Year, y = Latin.name)) +
#   geom_point() +
#   scale_x_continuous(breaks = seq(min(df_inc$Year), max(df_inc$Year), by = 5)) + 
#   labs(
#     x = "Year",
#     y = "Latin Name",
#     title = "Species-Year Dot Plot"
#   ) +
#   theme_minimal()

ggplot(df_inc, aes(x = Year, y = Latin.name)) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

# Map extensions
ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = df_inc,
    aes(
      x = lon_for_plot,
      y = lat_for_plot,
      color = Latin.name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location") +
  coord_sf(xlim = c(-180, -115), ylim = c(30, 63), expand = FALSE)



ggplot(northmost_points_perspyr, aes(x = Year, y = Latin.name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(1950, 2025),
                     breaks = seq(1950, 2025, by = 5))

ggplot(northmost_points_perspyr, aes(x = Year, y = Latin.name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(
                     breaks = seq(1850, 2025, by = 5))


# Combine CalCOFI & Lab Review -------------------------------------------- (Cmd + Shift + R)

# load in data
# poor naming scheme but the file with the similar name but excel has two sheets with the other having the original excerpt
ca <- read_excel("data/20250519_CalCOFI Coding Form (Responses).xlsx")


# Identify the metadata columns by name or position
metadata_cols <- c("Timestamp", "Email Address", "Editor Name", "Excerpt ID", "Should this excerpt be included or excluded?", "If excluded, why?", "Notes: Please enter info on anything unusual in how this information was coded.")  

# Extract question columns (e.g., 1.1 ..., 2.1 ..., etc.)
question_blocks <- ca %>%
  select(-all_of(metadata_cols)) %>%
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
question_cols <- as.character(2:24)

# If your columns are named like "2 Question text", "3 Question text", etc.
# and you want to match those that start with "2 ", "3 ", ..., "24 "
question_cols_pattern <- paste0("^(", paste(question_cols, collapse = "|"), ")\\b")

# Select those column names
cols_to_check <- names(ca_blocks)[grepl(question_cols_pattern, names(ca_blocks))]

# Filter rows where NOT all of those columns are NA and filter out a replicate Jordana did to compare to Bella's work
ca_clean <- ca_blocks %>%
  filter(!if_all(all_of(cols_to_check), is.na)) %>%
  filter(is.na(`Notes: Please enter info on anything unusual in how this information was coded.`) |
           `Notes: Please enter info on anything unusual in how this information was coded.` != "test input to compare to Bella's")

colnames(ca_clean)


# --- Inspecting data ---
ca_inc <- ca_clean %>%
  filter(`Should this excerpt be included or excluded?` == "Include")
unique_sp_latin <- unique(df_inc$`3 Latin name (e.g., pleuroncodes planipes)`)
print(unique_sp_latin)
years <- unique(df_inc$`18 Year (if range like 1957-1958, put most recent like 1958)`)
print(years)


ca_sp_yr <- ca_inc %>%
  filter(!is.na(`18 Year (if range like 1957-1958, put most recent like 1958)`), !is.na(`3 Latin name (e.g., pleuroncodes planipes)`)) %>%
  distinct(`18 Year (if range like 1957-1958, put most recent like 1958)`, `3 Latin name (e.g., pleuroncodes planipes)`)

# Filter for northern-most points in a year
ca_northmost_points_perspyr <- ca_inc %>%
  filter(!is.na(`18 Year (if range like 1957-1958, put most recent like 1958)`), !is.na(`3 Latin name (e.g., pleuroncodes planipes)`), !is.na(`11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`)) %>%
  group_by(`18 Year (if range like 1957-1958, put most recent like 1958)`, `3 Latin name (e.g., pleuroncodes planipes)`) %>%
  slice_max(order_by = `11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`, n = 1, with_ties = FALSE) %>%
  ungroup()

# GO THROUGH ABOVE CODE THEN FORMAT calCofi and lab rev to be mergable 
lab_df_for_merge <- northmost_points_perspyr %>%
  select(c(Latin.name, Year, lat_for_plot, lon_for_plot)) %>%
  rename(latin_name = Latin.name,
         year = Year,
         lat = lat_for_plot,
         lon = lon_for_plot) %>%
  drop_na()
ca_df_for_merge <- ca_northmost_points_perspyr %>%
  rename(latin_name = `3 Latin name (e.g., pleuroncodes planipes)`,
         year = `18 Year (if range like 1957-1958, put most recent like 1958)`,
         lat = `11 Latitude (e.g., 35.732; if the northward latitude is a range, put the farthest south)`,
         lon = `14 Longitude (e.g., -127.123, if longitude is a range, put the farthest east/towards the coast)`) %>%
  select(c(latin_name, year, lat, lon)) %>%
  drop_na()
# Make latin names uppercase
ca_df_for_merge$latin_name <- gsub("^(\\w)", "\\U\\1", ca_df_for_merge$latin_name, perl = TRUE)
  
# Merge CalCOFI and Lab data
combined_df <- bind_rows(lab_df_for_merge, ca_df_for_merge)
combined_df <- combined_df %>%
  group_by(year, latin_name) %>%
  slice_max(order_by = lat, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot combined data
# Map extensions
ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = combined_df,
    aes(
      x = lon,
      y = lat,
      color = latin_name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location") +
  coord_sf(xlim = c(-180, -115), ylim = c(30, 63), expand = FALSE)

ggplot(combined_df, aes(x = year, y = latin_name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot Combined Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2025, by = 5))


# Order data by rank (e.g. sp with most years)
rank_df <- combined_df %>%
  add_count(latin_name, name = "n_years")  %>%
  mutate(latin_name = forcats::fct_reorder(latin_name, n_years))
ggplot(rank_df, aes(x = year, y = latin_name)) +
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot Combined Data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 5))

# Filter to the top benthic macro zoo plankton
macro_benthic <- rank_df %>%
  filter(latin_name %in% c("Pleuroncodes planipes", 
                           "Emerita analoga",
                         "Panulirus interruptus", 
                         "Tetraclita rubescens",
                         "Pachythyone rubra",
                         "Ophiothrix spiculata",
                         "Megabalanus californicus",
                         "Mexacanthina lugubris"))
# Plot macro 
# Map extensions
ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = macro_benthic,
    aes(
      x = lon,
      y = lat,
      color = latin_name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location") +
  coord_sf(xlim = c(-130, -115), ylim = c(30, 50), expand = FALSE)

ggplot(macro_benthic, aes(x = year, y = latin_name)) +
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Benthic Macro Inverts with >=5 data points"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 5))
ggplot(macro_benthic, aes(x = year, y = latin_name)) +
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Benthic Macro Inverts with >=5 data points"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(1935, 2025),
                     breaks = seq(1935, 2025, by = 5))

