# ONI vs extension length
# By: Jordana Sevigny, jordana.sevigny@gmail.com
# Date created: 07/24/2025

rm(list = ls())

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
library(geosphere)
library(moments)
library(mgcv)
library(lme4)
library(gt)

# Load review data
df <- read.csv("processed_data/merged_calcofi_lab_review.csv")

# Load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load enso data
enso_df <- download_enso(climate_idx = "oni", create_csv = FALSE)

# Classify enso by start, peak, end --------------------------------------------
# For each event, get the years and count months per year
# Create event groupings from monthly phase info
enso_events <- enso_df %>%
  filter(phase == "Warm Phase/El Nino") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = ymd(paste(Year, Month, "15")),
    yearmon = as.yearmon(Date)
  ) %>%
  arrange(Date) %>%
  mutate(
    month_diff = c(0, diff(yearmon)),
    new_event = month_diff >  1.5/12, # Requires more than a month (1 month is the min between any phase classification) of non-el nino to have passed for an el nino event to be distinct from the last
    group_id = cumsum(new_event)
  )

# Count El Niño months per year per event
yearly_counts <- enso_events %>%
  group_by(group_id, Year) %>%
  summarise(n_months = n(), .groups = "drop")

# Get start, peak, end for each event
event_year_labels <- yearly_counts %>%
  group_by(group_id) %>%
  summarise(
    start = min(Year),
    end = max(Year),
    peak = Year[which.max(n_months)]
  ) %>%
  pivot_longer(cols = c(start, peak, end), names_to = "phase", values_to = "Year") %>%
  arrange(Year)

# Combine phase roles per year (across all events)
year_phases <- event_year_labels %>%
  group_by(Year) %>%
  summarise(enso_phase = paste(sort(phase), collapse = "-")) %>%
  ungroup() 

# Averages oni by year from complete enso dataset (without filtering for el nino)
oni_ave_by_yr <- enso_df %>%
  dplyr::select(Year, ONI) %>%
  group_by(Year) %>%
  summarise(oni_ave = mean(ONI, na.rm = TRUE))


# Prep review data for analysis --------------------------------------------

# How many species had just 1 extension?
df %>% group_by(latin_name) %>% filter(!any(group_id > 0)) %>% pull(latin_name) %>% unique()

# What species had the most extensions?
max_group_id = max(df$group_id)
df %>% group_by(latin_name) %>% filter(group_id == max_group_id) %>% pull(latin_name) %>% unique()

 # Identify the species with X+ events and filter for those species
species_with_groupXplus <- df %>%
  group_by(latin_name) %>%
  filter(any(group_id >= 2)) %>% # 2 would be three events (0, 1, 2)
  pull(latin_name) %>%
  unique()

# Filter full dataset for those species
ext_Xplus <- df %>%
  filter(latin_name %in% species_with_groupXplus)
# unique(ext_Xplus$latin_name)

# Dataframe of all extension events for species with X+ events combined with el nino phase data (start, peak, end)
ext_year_phase <- left_join(ext_Xplus, year_phases, by = c("first_year" = "Year"))
ext_year_phase <- filter(ext_year_phase, year >= 1950) # 1950 is the cutoff for ONI

# Calculate extension distance
# Distance function
get_distance_km <- function(lat1, lon1, lat2, lon2) {
  point1 <- c(lon1, lat1)
  point2 <- c(lon2, lat2)
  dist_meters <- distHaversine(point1, point2)
  return(dist_meters / 1000)  # convert to kilometers
}


# Apply extension distance function row-wise
ext_distance <- ext_year_phase %>%
  rowwise() %>%
  mutate(distance_km = get_distance_km(hist_range_lat, hist_range_lon, latitude, longitude)) %>%
  ungroup()

# Average/sd/max/mix extension distance 1) by max per event 2) by all observations
ext_distance %>%
  group_by(latin_name, group_id) %>%
  summarise(max_dist = max(distance_km, na.rm = TRUE)) %>%
  pull(max_dist) %>%
  max(na.rm = TRUE) # switch what want to find here


#===================================
# Max ONI (all shifted to year prior) - Max extension distance per event

# Make a new column of the year prior to the year of observation and find the ave ONI for those years
ext_distance_oni <- ext_distance %>%
  mutate(year_prior = first_year-1) %>%
  left_join(oni_ave_by_yr, by = c("year_prior" = "Year")) %>%
  rename(oni_prior_yr = oni_ave) %>%
  left_join(oni_ave_by_yr, by = c("first_year" = "Year")) %>%
  rename(oni_first_yr = oni_ave)

# Select the Max of the average ONI for each event and the max extension distance for each event
max_ext_oni_yr_prior <- ext_distance_oni %>%
  group_by(latin_name, group_id) %>%
  mutate(
    max_oni = max(oni_prior_yr, oni_first_yr)) %>%
  mutate(max_ext_dist = max(distance_km)) %>%
  ungroup() %>%
  dplyr::select(latin_name, group_id, max_ext_dist, max_oni) %>%
  distinct()

# Distribution curve for distance data
hist(max_ext_oni_yr_prior$max_ext_dist, breaks = 20, freq = FALSE)

curve(dlnorm(x,
             meanlog = mean(log(max_ext_oni_yr_prior$max_ext_dist)),
             sdlog   = sd(log(max_ext_oni_yr_prior$max_ext_dist))),
      add = TRUE, col = "blue", lwd = 2)
# Empirical CDF
fx = (1:length(max_ext_oni_yr_prior$max_ext_dist))/(length(max_ext_oni_yr_prior$max_ext_dist)+1)
plot(sort(max_ext_oni_yr_prior$max_ext_dist), fx, type="l")
# Add log normal distribution
x1 = seq(0,3500,0.1) 
y = log(max_ext_oni_yr_prior$max_ext_dist)
Fx2 = plnorm(x1,mean(y),sd(y))
lines(x1,Fx2,col="gold")
# Add normal distribution
x2 = seq(0,3500,0.1) 
y = max_ext_oni_yr_prior$max_ext_dist
Fx3 = pnorm(x2,mean(y),sd(y))
lines(x2,Fx3,col="green")
# Log normal is much better fit
boxplot(max_ext_oni_yr_prior$max_ext_dist) # boxplot



# no ONI pre 1950 (drops any extensions pre 1950)
ggplot(max_ext_oni_yr_prior, aes(max_oni, log(max_ext_dist))) +
  geom_point(color="black") +
  geom_smooth(method = "lm", se = TRUE, color="#E9C46A", fill="#E9C46A") +  # se = FALSE to hide confidence interval
  labs(x = "ONI", y = "Log Extension Distance (km)", ) +
  theme_minimal(base_size = 16)

# Wondering if I need to log the distance data
# Fit the linear model
lin_model <- lm(log(max_ext_dist) ~ max_oni, data = max_ext_oni_yr_prior)
# View the model summary
summary(lin_model)

# Quadratic (2nd degree polynomial)
poly_model <- lm(log(max_ext_dist) ~ poly(max_oni, 2, raw = TRUE), 
                 data = max_ext_oni_yr_prior)
summary(poly_model)
# Compare to linear
anova(lin_model, poly_model)

# Fit GAM with smooth term for ONI
gam_model <- gam(log(max_ext_dist) ~ s(max_oni), data = max_ext_oni_yr_prior)
summary(gam_model)
# Plot smooth
plot(gam_model, shade = TRUE, main = "GAM fit: max_ext_dist ~ s(max_oni)")

# Mixed effects model with random intercept
m_mixed <- lmer(log(max_ext_dist) ~ max_oni + (1 | latin_name), 
                data = max_ext_oni_yr_prior)

summary(m_mixed)
ranef(m_mixed)$latin_name

# Mixed effects model with random intercept and random slope
m_mixed_slopes <- lmer(log(max_ext_dist) ~ max_oni + (max_oni | latin_name), data = max_ext_oni_yr_prior)
summary(m_mixed_slopes)
ranef(m_mixed_slopes)$latin_name

# AIC
AIC(lin_model, poly_model, gam_model, m_mixed, m_mixed_slopes)


# Make a table of the intercepts for supplementary ------------------------
# Extract random effects
ran <- ranef(m_mixed)$latin_name %>%
  tibble::rownames_to_column("latin_name") %>%
  rename(random_intercept = `(Intercept)`)

# Extract fixed intercept (needed if back-transforming)
fix_int <- fixef(m_mixed)[["(Intercept)"]]
# Revert from log back to km
ran <- ran %>%
  mutate(
    intercept_log = fix_int + random_intercept,
    intercept_km = exp(intercept_log)  # assumes natural log
  )
species_intercept_table <- ran %>%
  select(latin_name, intercept_km) %>%
  arrange(desc(intercept_km)) %>%  # optional: sort largest to smallest
  gt() %>%
  fmt_number(columns = c(intercept_km), decimals = 1) %>%
  cols_label(
    latin_name = "Species",
    intercept_km = "Estimated Intercept (km)"
  ) %>%
  tab_header(
    title = "Species-Specific Random Intercepts for Mixed-Effects Model"
  ) %>%
  tab_options(
    table.font.names = "Helvetica",
    data_row.padding = px(4),
    table.font.size = 12,
    heading.align = "left"
  )
gtsave(species_intercept_table, "figures/species-intercept-table.png")


# Density Plot Low vs High ONI
max_ext_oni_yr_prior %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na() %>%
  ggplot(aes(x = log(max_ext_dist), fill = oni_cat)) +
  geom_density(alpha = 0.6) +
  labs(x = "Log Extension Event Distance (km)", y= "Density", fill = "ONI Level") +
  scale_fill_manual(
    values = c("High ONI (>=0.5)" = "#D62828",   # red
               "Low ONI" = "#003049")           # blue
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.position = c(0.999, 0.999),   # (x, y) inside plot coordinates
    legend.justification = c("right", "top"), # anchor legend box at that point
    axis.title.y.right = element_blank()
  )

# Create category and drop NA
oni_test_data <- max_ext_oni_yr_prior %>%
  mutate(oni_cat = ifelse(max_oni >= 0.5, "High ONI (>=0.5)", "Low ONI")) %>%
  drop_na(max_ext_dist, oni_cat)


# Run t-test
t.test(log(max_ext_dist) ~ oni_cat, data = oni_test_data) # two-tailed


# Max ONI (year prior) vs extension count --------------------------------------------------


# extension counts
oni_freq <- max_ext_oni_yr_prior %>%
  count(max_oni)

# Leave one species out correlation checks
for (sp in unique(max_ext_oni_yr_prior$latin_name)) {
  loso_freq <- max_ext_oni_yr_prior %>%
    filter(latin_name != sp) %>%
    count(max_oni)
  print(cor(loso_freq$max_oni, log(loso_freq$n)))
}

# Extension counts distribution curve
hist(oni_freq$n, breaks = 5, freq = FALSE)

curve(dlnorm(x,
             meanlog = mean(log(oni_freq$n)),
             sdlog   = sd(log(oni_freq$n))),
      add = TRUE, col = "blue", lwd = 2)
# Empirical CDF
fx = (1:length(oni_freq$n))/(length(oni_freq$n)+1)
plot(sort(oni_freq$n), fx, type="l")
# Add log normal distribution
x1 = seq(0,3500,0.1) 
y = log(oni_freq$n)
Fx2 = plnorm(x1,mean(y),sd(y))
lines(x1,Fx2,col="gold")
# Add normal distribution
x2 = seq(0,3500,0.1) 
y = oni_freq$n
Fx3 = pnorm(x2,mean(y),sd(y))
lines(x2,Fx3,col="green")
# Log normal is better fit
boxplot(oni_freq$n) # boxplot

# Linear regression
ggplot(oni_freq, aes(x=max_oni, y=log(n))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color="#E9C46A", fill="#E9C46A") + 
  labs(x = "ONI", y = "Log Number of Extension Events") +
  theme_minimal(base_size = 16)

# Fit the linear model
lin_model <- lm(log(n) ~ max_oni, data = oni_freq)
# View the model summary
summary(lin_model)

# Quadratic (2nd degree polynomial)
poly_model <- lm(log(n) ~ poly(max_oni, 2, raw = TRUE), 
                 data = oni_freq)
summary(poly_model)
# Compare to linear
anova(lin_model, poly_model)


# Fit GAM with smooth term for ONI
gam_model <- gam(log(n) ~ s(max_oni), data = oni_freq)
summary(gam_model)

# # Mixed effects model
# # Make oni_freq by species
# oni_freq_sp <- max_ext_oni_yr_prior %>%
#   group_by(latin_name) %>%
#   count(max_oni) %>%
#   drop_na()
# 
# m_mixed <- lmer(n ~ max_oni + (1 | latin_name),
#                 data = oni_freq_sp)
# 
# summary(m_mixed) # Does not work because all sp have same # of oni observations

# AIC
AIC(lin_model, poly_model, gam_model)



# Do more extensions occur during el nino than expected by chance? --------------------------------------------------
# 3+ EXTENSIONS

# El Nino frequency
## Monthly res
en_count <- enso_df %>%
  filter(str_detect(phase, "Warm")) %>%
  tally()
en_freq_month <- en_count$n / nrow(enso_df)

ext_summary <- ext_year_phase %>%
  filter(year == first_year) %>%
  mutate(
    is_peak_or_end = str_detect(enso_phase, "peak|end")
  ) %>%
  group_by(latin_name) %>%
  summarise(
    total_extensions = n(),
    peak_or_end_extensions = sum(is_peak_or_end, na.rm = TRUE),
    proportion_peak_or_end = peak_or_end_extensions / total_extensions
  ) %>%
  arrange(desc(proportion_peak_or_end))

# Proportion of extensions histogram
ggplot(ext_summary, aes(x = proportion_peak_or_end)) +
  geom_histogram(binwidth = 0.1, fill = "white", color = "black", size=1.1, boundary=0) +
  geom_vline(xintercept = en_freq_month, linetype = "dashed", color = "#D62828", size=1.3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Proportion of Extension Events in El Niño Peak/End Years",
    y = "Number of Species"
  ) +
  theme_minimal()

ggplot(ext_summary, aes(x = proportion_peak_or_end)) +
  geom_dotplot(binwidth = 0.04, fill = "white", color = "black", stroke=2) +
  geom_vline(xintercept = en_freq_month, linetype = "dashed", color = "#D62828", size=1.3) +
  labs(
    x = "Proportion of Extensions Events in El Niño Peak/End Years",
    y = "Number of Species"
  ) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())

# Color dots by total number extensions made by that species
# change legend label & make color scale black and white gradient
# Horizontal 
supp3 <- ggplot(ext_summary, aes(x = proportion_peak_or_end, fill = factor(total_extensions))) +
  geom_dotplot(    binaxis = "x",
                   method = "histodot",      # use fixed histogram bins (not density)
                   binpositions = "all",     # all groups share bin boundaries
                   binwidth = 0.16,          # pick your bin width
                   stackgroups = TRUE,       # groups stack within the same bin
                   stackdir = "up",
                   colour = "black",
                   stroke = 1) +
  geom_vline(xintercept = en_freq_month, linetype = "dashed", color = "#D62828", size=1.3) +
  labs(
    fill = "Total number of\nextensions",
    x = "Proportion of Extensions Events in El Niño Peak/End Years",
    y = "Number of Species"
  ) +
  theme_minimal(base_size = 18) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=14),
        legend.position = c(0.00001, 0.9999),   # (x, y) inside plot coordinates
        legend.justification = c("left", "top"), # anchor legend box at that point
        axis.title.y.right = element_blank()
  )

ggsave("figures/figure3_supp.png", plot = supp3, width = 10, height = 7, unit = "in", dpi = 600)
ggsave("figures/figure3_supp.pdf", plot = supp3, width = 10, height = 7, unit = "in", dpi = 600)

# Vertical
ggplot(ext_summary, aes(x=1, y = proportion_peak_or_end, fill = factor(total_extensions))) +
  geom_dotplot(binaxis = "y", stackgroups = TRUE, binwidth = 0.1, stroke=1) +
  geom_hline(yintercept = en_freq_month, linetype = "dashed", color = "#D62828", size=1.3) +
  labs(
    fill = "Total number of\nextension",
    y = "Proportion of Extensions Events in El Niño Peak/End Years",
    x = "Number of Species"
  ) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x  = element_blank())



# STATS - chisq for el nino extensions

# Observed counts
observed <- c(sum(ext_summary$peak_or_end_extensions), sum(ext_summary$total_extensions)-sum(ext_summary$peak_or_end_extensions))  # 35 El Niño, 65 Not El Niño
# Expected proportions
expected_proportions_month <- c(en_freq_month, (1-en_freq_month))
# Run chi-squared goodness-of-fit test
chisq.test(x = observed, p = expected_proportions_month)

