# preliminary data map
# August 13, 2024
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

# load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# load in data
# poor naming scheme but the file with the similar name but excel has two sheets with the other having the original excerpt
raw <- read.csv("processed_data/Trial_CalCOFI_Report_Data_R_format_2-2025.csv")

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
  coord_sf(xlim = c(-110, -150), ylim = c(30, 63), expand = FALSE)


# map with ENSO/species points
ggplot(data = world) +
  geom_sf() +
  geom_point(data = df, aes(x = Longitude, y = Latitude, shape = ENSO, color = Species), size = 2.5) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color="Range Edge") +
  coord_sf(xlim = c(-110, -150), ylim = c(30, 63), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = df, aes(x = Longitude, y = Latitude, shape = ENSO, color = Old_or_New), size = 2.5) +
  xlab("Longitude") + ylab("Latitude") +
  labs(color = "Range Edge", shape = "ENSO Phase") +
  coord_sf(xlim = c(-150, -110), ylim = c(30, 63), expand = FALSE) +
  facet_wrap(~Species)  # Facet by species
