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
library(lubridate)
library(zoo)


# AS OF 6/19/2025, THIS DATA STILL NEEDS TO BE DEDUPLICATED

# load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Load review data
df <- read.csv("processed_data/lab_review_with_longitudes.csv")

# Load enso data
# I TOGGLE BETWEEN USING phase == 'Warm Phase/El Nino' and dSST3.4 > 0 HERE
enso <- download_enso(climate_idx = "oni", create_csv = FALSE)
#write.csv(enso, "enso_data.csv")
enso_yr <- enso %>%
  select(c(Year, Month, phase)) %>%
  filter(phase == 'Warm Phase/El Nino') %>%
  distinct() %>%
  select(Year, Month)
# enso_yr <- enso %>%
#   select(c(Year, Month, dSST3.4)) %>%
#   filter(dSST3.4 > 0) %>%
#   distinct() %>%
#   select(Year, Month)
oni_years <- enso_yr %>%
  select(Year) %>%
  distinct()
# Manually add historical el nino events from Quinn et al. 1987 for 1850-1950
# https://agupubs.onlinelibrary.wiley.com/doi/epdf/10.1029/JC092iC13p14449
# Their data extends back to 1525!
man_hist_enso <- c(1864, 1871, 1877, 1878, 1884, 1891, 1899, 1900, 1911, 1912, 1917, 1925, 1926, 1932, 1940, 1941)
man_hist_df <- data.frame(Year = man_hist_enso)

enso_bands_old <- data.frame(
  xmin = man_hist_df$Year - 0.5,
  xmax = man_hist_df$Year + 0.5
)

# Convert to date
enso_yr <- enso_yr %>%
  mutate(
    Month = match(Month, month.abb),  # convert "Aug" -> 8, etc.
    Date = ymd(paste(Year, Month, 15, sep = "-"))  # use 15th as mid-month
  ) %>%
  arrange(Date)
# Calculate difference in months
enso_yr <- enso_yr %>%
  arrange(Date) %>%
  mutate(
    month_diff = c(0, diff(as.yearmon(Date))),
    new_event = month_diff > 3/12,  # if jump > 1 month, new group
    group_id = cumsum(new_event)
  )

enso_bands_oni <- enso_yr %>%
  group_by(group_id) %>%
  summarise(
    xmin = min(as.numeric(as.yearmon(Date))),
    xmax = max(as.numeric(as.yearmon(Date))) + 1/12  # to cover full last month
  ) %>%
  select(c(xmin, xmax))

enso_bands <- rbind(enso_bands_oni, enso_bands_old)


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
ca <- read_excel("data/CalCOFI Coding Form (Responses).xlsx")


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
# Filter for included excerpts
ca_inc <- ca_clean %>%
  filter(`Should this excerpt be included or excluded?` == "Include")

unique_sp_latin <- unique(df_inc$`3 Latin name (e.g., pleuroncodes planipes)`)
print(unique_sp_latin)
years <- unique(df_inc$`18 Year (if range like 1957-1958, put most recent like 1958)`)
print(years)

# Filter out NAs and look at distinct species/years
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
# Choose the northermost observation for each species/year combo
combined_df <- combined_df %>%
  group_by(year, latin_name) %>%
  slice_max(order_by = lat, n = 1, with_ties = FALSE) %>%
  ungroup()
unique(combined_df$latin_name)
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
    title = "Species-Year Dot Plot Combined Data - ONI El Niño"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 5))

# Plot timeseries for each independent species
# Create output folder if it doesn't exist
output_dir <- "species_plots"
dir.create(output_dir, showWarnings = FALSE)

# Get unique species names
species_list <- unique(rank_df$latin_name)

# Copy the rank_df table and manually record the extensions events during and not during el nino
#write.table(rank_df, pipe("pbcopy"), sep = "\t", row.names = FALSE)

# Loop through each species and save a plot
for (sp in species_list) {
  
  df_sp <- rank_df %>% filter(latin_name == sp)

  p <- ggplot(df_sp, aes(x = year, y = latin_name)) +
    geom_rect(data = enso_bands, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "red", alpha = 0.2) +
    geom_point() +
    labs(
      x = "Year",
      y = "Latin Name",
      title = paste("Time Series for", sp)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    scale_x_continuous(limits = c(1850, 2025),
                       breaks = seq(1850, 2025, by = 5))
  print(p)
  # Clean filename (remove illegal characters)
  filename <- paste0(gsub("[^a-zA-Z0-9]", "_", sp), ".png")
  
  # Save plot
  ggsave(filename = file.path(output_dir, filename), plot = p, width = 8, height = 4, dpi = 300)
}


# Top zoo inverts ---------------------------------------------------------

# Filter to the top benthic macro zoo inverts
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





# enso - extension frequency ----------------------------------------------

# El Nino frequency
## Monthly res
en_count <- enso %>%
  filter(str_detect(phase, "Warm")) %>%
  tally()
en_freq_month <- en_count$n / nrow(enso)

## Yearly res
oni_years_all <- enso %>%
  select(Year) %>%
  distinct()

en_freq_year <- nrow(oni_years) / nrow(oni_years_all)

## SST > 0 res
sst_count <- enso %>%
  filter(dSST3.4 > 0) %>%
  tally()
en_freq_sst <- sst_count$n / nrow(enso)

# Extension frequency - wo blob 
rank_df <- rank_df %>%
  arrange(latin_name, year) %>%
  group_by(latin_name) %>%
  mutate(
    year_diff = year - lag(year, default = first(year)),
    new_group = if_else(year_diff > 1, 1, 0),
    group_id = cumsum(new_group)
  ) %>%
  ungroup() %>%
  select(-c(year_diff, new_group))
enso_years <- rbind(oni_years, man_hist_df)


# The below goes only by El Nino and does not include the onset of MHW prior to El Nino
# Identify whether first year in each group is an El Niño year
group_flags <- rank_df %>%
  group_by(latin_name, group_id) %>%
  summarise(first_year = min(year), .groups = "drop") %>%
  mutate(started_during_enso = if_else(first_year %in% enso_years$Year, "y", NA_character_))

# Join back to rank_df to label all rows in those groups
rank_df_enso <- rank_df %>%
  left_join(group_flags, by = c("latin_name", "group_id"))

# Tally the fraction of extensions occurring during El Nino for each species
# Total groups per species

enso_group_counts <- rank_df_enso %>%
  filter(started_during_enso == "y") %>%
  distinct(latin_name, group_id) %>%  # avoid duplicate rows within the same group
  count(latin_name, name = "n_enso_groups")

total_group_counts <- rank_df_enso %>%
  distinct(latin_name, group_id) %>%
  count(latin_name, name = "n_total_groups")

# Merge with enso counts
enso_summary <- total_group_counts %>%
  left_join(enso_group_counts, by = "latin_name") %>%
  mutate(
    n_enso_groups = replace_na(n_enso_groups, 0),
    prop_enso = n_enso_groups / n_total_groups
  )

# Filter for species with 3+ extensions
enso_summary_3 <- enso_summary %>%
  filter(n_total_groups >= 3)

hist(enso_summary_3$prop_enso,
     main = "Proportion of Extensions Starting During an El Niño year\n Red line is El Niño frequency at monthly resolution | Blue line is El Niño frequency at yearly resolution\n Green line is dSST3.4 > 0 frequency at monthly resolution",
     xlab = "Proportion of extensions during El Niño",
     ylab = "Frequency")
abline(v = en_freq_month, col = "red", lty = 2, lwd = 2)
abline(v = en_freq_sst, col = "green", lty = 2, lwd = 2)
abline(v = en_freq_year, col = "blue", lty = 2, lwd = 2)

# STATS
## El Nino wo blob - el nino month resolution
# Observed counts
observed <- c(sum(enso_summary_3$n_enso_groups), sum(enso_summary_3$n_total_groups)-sum(enso_summary_3$n_enso_groups))  # 35 El Niño, 65 Not El Niño
# Expected proportions
expected_proportions_month <- c(en_freq_month, (1-en_freq_month))
expected_proportions_sst <- c(en_freq_sst, (1-en_freq_sst))
expected_proportions_year <- c(en_freq_year, (1-en_freq_year))
# Run chi-squared goodness-of-fit test
chisq.test(x = observed, p = expected_proportions_month)
chisq.test(x = observed, p = expected_proportions_sst)
chisq.test(x = observed, p = expected_proportions_year)


# Extension frequency - with blob 

# The below goes only by El Nino and DOES include the onset of MHW prior to El Nino
# Identify whether first year in each group is an El Niño year
group_flags <- rank_df %>%
  group_by(latin_name, group_id) %>%
  summarise(first_year = min(year), .groups = "drop") %>%
  mutate(
    started_during_enso = if_else(
      first_year %in% c(enso_years$Year, 2013, 2014),
      "y",
      NA_character_
    )
  )

# Join back to rank_df to label all rows in those groups
rank_df_enso <- rank_df %>%
  left_join(group_flags, by = c("latin_name", "group_id"))

# Tally the fraction of extensions occurring during El Nino for each species
# Total groups per species

enso_group_counts <- rank_df_enso %>%
  filter(started_during_enso == "y") %>%
  distinct(latin_name, group_id) %>%  # avoid duplicate rows within the same group
  count(latin_name, name = "n_enso_groups")

total_group_counts <- rank_df_enso %>%
  distinct(latin_name, group_id) %>%
  count(latin_name, name = "n_total_groups")

# Merge with enso counts
enso_summary <- total_group_counts %>%
  left_join(enso_group_counts, by = "latin_name") %>%
  mutate(
    n_enso_groups = replace_na(n_enso_groups, 0),
    prop_enso = n_enso_groups / n_total_groups
  )

# Filter for species with 3+ extensions
enso_summary_3 <- enso_summary %>%
  filter(n_total_groups >= 3)

hist(enso_summary_3$prop_enso,
     xlim=c(0,1),
     main = "Proportion of Extensions Starting During an El Niño year or the blob\n Red line is El Niño frequency at monthly resolution | Blue line is El Niño frequency at yearly resolution\n Green line is dSST3.4 > 0 frequency at monthly resolution",
     xlab = "Proportion of extensions during El Niño",
     ylab = "Frequency")
abline(v = en_freq_month, col = "red", lty = 2, lwd = 2)
abline(v = en_freq_sst, col = "green", lty = 2, lwd = 2)
abline(v = en_freq_year, col = "blue", lty = 2, lwd = 2)

# STATS
## El Nino w blob - el nino month resolution
# Observed counts
observed <- c(sum(enso_summary_3$n_enso_groups), sum(enso_summary_3$n_total_groups)-sum(enso_summary_3$n_enso_groups))  # 35 El Niño, 65 Not El Niño
# Expected proportions
expected_proportions_month <- c(en_freq_month, (1-en_freq_month))
expected_proportions_sst <- c(en_freq_sst, (1-en_freq_sst))
expected_proportions_year <- c(en_freq_year, (1-en_freq_year))
# Run chi-squared goodness-of-fit test
chisq.test(x = observed, p = expected_proportions_month)
chisq.test(x = observed, p = expected_proportions_sst)
chisq.test(x = observed, p = expected_proportions_year)



# The Blob ----------------------------------------------------------------

rank_df_enso
blob <- rank_df_enso %>%
  filter(first_year==2013 | first_year==2014)

# Map extensions
ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = blob,
    aes(
      x = lon,
      y = lat,
      color = latin_name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location\n the Blob edition") +
  coord_sf(xlim = c(-180, -115), ylim = c(30, 63), expand = FALSE)

# Time series
ggplot(blob, aes(x = year, y = latin_name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot - Plot the Blob"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(2012, 2017),
    breaks = seq(2012, 2017, by = 1))

# 2015 El Nino -------------------------------

rank_df_enso
en2015 <- rank_df_enso %>%
  filter(first_year==2015 | first_year==2016)

# Map extensions
ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = en2015,
    aes(
      x = lon,
      y = lat,
      color = latin_name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location\n the 2015 El Niño edition") +
  coord_sf(xlim = c(-180, -115), ylim = c(30, 63), expand = FALSE)

# Time series
ggplot(en2015, aes(x = year, y = latin_name)) +
  # El Niño bands
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +  # Transparent red
  # Your points
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot -  2015 El Niño "
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(limits = c(2012, 2017),
                     breaks = seq(2012, 2017, by = 1))

blob_sp <- unique(blob$latin_name)
en2015_sp <- unique(en2015$latin_name)
shared_species <- intersect(blob_sp, en2015_sp)
only_in_blob <- setdiff(blob_sp, en2015_sp)
only_in_en2015 <- setdiff(en2015_sp, blob_sp)
list(
  shared = shared_species,
  only_blob = only_in_blob,
  only_en2015 = only_in_en2015
)

# Number of species with extensions in the blob / total # extensions
rank_df_enso
num_ext_blob <- rank_df_enso %>%
  group_by(latin_name, group_id) %>%
  summarise(first_year = min(year), .groups = "drop") %>%
  mutate(blob = first_year %in% c(2013, 2014))

# Count how many groups started in 2013 or 2014
num_blob_groups <- sum(num_ext_blob$blob, na.rm = TRUE)

# Count total number of groups
total_groups <- nrow(num_ext_blob)

# Calculate proportion
prop_blob_groups <- num_blob_groups / total_groups

# 3+ events map and time series -----------------------------------------------------------
ext_3andup <- rank_df %>%
  group_by(latin_name) %>%
  arrange(year) %>%  # assuming you want to order within each species by year
  mutate(num_ext = max(group_id)+1) %>%
  ungroup() %>%
  filter(num_ext >= 3)

ggplot(ext_3andup, aes(x = year, y = latin_name)) +
  geom_rect(data = enso_bands, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "red", alpha = 0.2) +
  geom_point() +
  labs(
    x = "Year",
    y = "Latin Name",
    title = "Species-Year Dot Plot Combined Data - Species with 3+ events"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  scale_x_continuous(breaks = seq(1850, 2025, by = 5))

ggplot(data = world) +
  geom_sf() +
  geom_point(
    data = ext_3andup,
    aes(
      x = lon,
      y = lat,
      color = latin_name
    ),
    size = 2.5,
    position = position_jitter(width = 1, height = 0)  # jitter x only
  ) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Unusual Observation Location\n Species with 3+ events") +
  coord_sf(xlim = c(-160, -115), ylim = c(30, 60), expand = FALSE)

