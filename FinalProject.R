library(dplyr)

health_data <- read.csv("North Carolina_CensusZipCodeTabulationArea.csv")

walkability_data <- read.csv("EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")


head(walkability_data)

#filter by the state‐FIPS code for North Carolina (which is 37)


walkability_nc <- walkability_data %>%
  filter(STATEFP == 37)

write.csv(walkability_nc, "walkability_nc.csv", row.names = FALSE)

library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

#Read walkability CSV and rebuild the 12-digit GEOID
walk <- read_csv(
  "walkability_nc.csv",
  col_types = cols(
    STATEFP  = col_character(),
    COUNTYFP = col_character(),
    TRACTCE  = col_character(),
    BLKGRPCE = col_character(),
    NatWalkInd = col_double()
  )
) %>%
  mutate(
    STATEFP  = str_pad(STATEFP,  2, pad = "0"),
    COUNTYFP = str_pad(COUNTYFP, 3, pad = "0"),
    TRACTCE  = str_pad(TRACTCE,  6, pad = "0"),
    GEOID10  = paste0(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)
  ) %>%
  select(GEOID10, NatWalkInd)

# Download NC block-group geometries (they already have a GEOID column)
blocks_nc <- block_groups(state = "37", year = 2020, cb = TRUE) |>
  st_transform(4326)

# Join on the true 12-digit ID and drop missing
walk_sf <- blocks_nc |>
  left_join(walk, by = c("GEOID" = "GEOID10")) |>
  filter(!is.na(NatWalkInd))
#nrow(walk_sf)
# 4. 2020 ZCTA polygons, keep only NC prefixes 27*–28*
zctas_raw <- tigris::zctas(year = 2020, cb = TRUE) |>
  st_transform(4326)

# figure out which field is the 5-digit ZCTA code
zip_candidates <- c("ZCTA5CE10","ZCTA5CE20","GEOID10","GEOID20")
zip_col <- zip_candidates[zip_candidates %in% names(zctas_raw)][1]
if (is.na(zip_col)) stop("No ZIP-code column found in zctas_raw!")

zctas <- zctas_raw |>
  mutate(zip = .data[[zip_col]]) |>                 # bring it out as `zip`
  filter(substr(zip, 1, 2) %in% c("27", "28")) |>
  select(zip, geometry)                             # keep only zip + shape

# Centroids of block groups with walkability
centroids <- st_centroid(walk_sf)

#  Spatial‐join centroids into ZCTAs, allowing boundary cases
walk_zcta <- st_join(
  centroids,
  zctas,
  join = st_intersects,   # “within or touching”
  left = FALSE
)

# now this should yield actual rows
zcta_walk <- walk_zcta |>
  st_drop_geometry() |>
  group_by(zip) |>
  summarise(mean_walk = mean(NatWalkInd), .groups = "drop")

# Aggregate mean walkability by ZCTA
#zcta_walk <- walk_zcta |>
 # st_drop_geometry() |>
  #group_by(zip) |>
  #summarise(mean_walk = mean(NatWalkInd), .groups = "drop")

# Health outcomes (CDC Places)  -------------------
health <- read_csv("North_Carolina_CensusZipCodeTabulationArea.csv",
                   col_types = cols(placeName = col_character())) |>
  select(placeName,
         Value.Percent_Person_Obesity,
         Value.Percent_Person_WithDiabetes) |>
  rename(zip      = placeName,
         obesity  = Value.Percent_Person_Obesity,
         diabetes = Value.Percent_Person_WithDiabetes)

# Final analysis dataframe
df <- zcta_walk |>
  left_join(health, by = "zip") |>
  filter(!is.na(obesity) & !is.na(diabetes))

library(tidyverse)

# Scatterplot: Walkability vs Obesity
ggplot(df, aes(mean_walk, obesity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Walkability vs. Obesity (NC ZCTAs)",
       x = "Mean Walkability Index",
       y = "Percent Obesity") +
  theme_minimal()

# Scatterplot: Walkability vs Diabetes
ggplot(df, aes(mean_walk, diabetes)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Walkability vs. Diabetes (NC ZCTAs)",
       x = "Mean Walkability Index",
       y = "Percent Diabetes") +
  theme_minimal()

# 03_more_figures.R

# Assumes `zcta_walk` (with columns `zip`, `mean_walk`)
#       and `health` (zip, obesity, diabetes) 

library(tidyverse)
library(readr)

# Extra health variables: physical inactivity & high blood pressure
health_extra <- read_csv(
  "North_Carolina_CensusZipCodeTabulationArea.csv",
  col_types = cols(placeName = col_character())
) |>
  select(
    placeName,
    Value.Percent_Person_PhysicalInactivity,
    Value.Percent_Person_WithHighBloodPressure
  ) |>
  rename(
    zip        = placeName,
    inactivity = Value.Percent_Person_PhysicalInactivity,
    hbp        = Value.Percent_Person_WithHighBloodPressure
  )

# Combine walkability + full set of health metrics
df2 <- zcta_walk |>
  left_join(health,       by = "zip") |>
  left_join(health_extra, by = "zip") |>
  filter(!is.na(inactivity) & !is.na(hbp))

# Plot: Walkability vs Physical Inactivity
ggplot(df2, aes(mean_walk, inactivity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Walkability vs. Physical Inactivity (NC ZCTAs)",
    x     = "Mean Walkability Index",
    y     = "Percent Physical Inactivity"
  ) +
  theme_minimal()

# Plot: Walkability vs High Blood Pressure
ggplot(df2, aes(mean_walk, hbp)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Walkability vs. High Blood Pressure (NC ZCTAs)",
    x     = "Mean Walkability Index",
    y     = "Percent High Blood Pressure"
  ) +
  theme_minimal()

# Plot: Obesity vs Physical Inactivity
ggplot(df2, aes(obesity, inactivity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Obesity vs. Physical Inactivity (NC ZCTAs)",
    x     = "Percent Obesity",
    y     = "Percent Physical Inactivity"
  ) +
  theme_minimal()

# 04_alt_figures.R
# --------------------------------------------------
# Uses objects created in 01_data_preparation.R
#   zctas (sf with column `zip`)
#   zcta_walk (zip, mean_walk)
#   health       (zip, obesity, diabetes)
#   health_extra (zip, inactivity, hbp) – created in 03_more_figures.R
# If `health_extra` not in memory, recreate quickly:
if (!exists("health_extra")) {
  library(readr)
  health_extra <- read_csv(
    "North_Carolina_CensusZipCodeTabulationArea.csv",
    col_types = cols(placeName = col_character())
  ) |>
    select(placeName,
           Value.Percent_Person_PhysicalInactivity,
           Value.Percent_Person_WithHighBloodPressure) |>
    rename(
      zip        = placeName,
      inactivity = Value.Percent_Person_PhysicalInactivity,
      hbp        = Value.Percent_Person_WithHighBloodPressure
    )
}

library(tidyverse)
library(sf)
library(viridis)


# Combine everything into one dataframe
df_full <- zcta_walk |>
  left_join(health,       by = "zip") |>
  left_join(health_extra, by = "zip") |>
  filter(!is.na(obesity) & !is.na(inactivity) & !is.na(hbp))


# Boxplot – Obesity by Walkability Quartile
df_full <- df_full |>
  mutate(walk_q = ntile(mean_walk, 4))               # quartiles 1–4

ggplot(df_full, aes(x = factor(walk_q), y = obesity)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(title = "Obesity Distribution by Walkability Quartile",
       x     = "Walkability Quartile (1 = Lowest, 4 = Highest)",
       y     = "Percent Obesity") +
  theme_minimal()


# Grouped Bar – Mean Health Metrics by Walkability Quartile
metric_means <- df_full |>
  group_by(walk_q) |>
  summarise(
    obesity     = mean(obesity),
    inactivity  = mean(inactivity),
    hbp         = mean(hbp),
    .groups     = "drop"
  ) |>
  pivot_longer(-walk_q, names_to = "metric", values_to = "value")

ggplot(metric_means,
       aes(x = factor(walk_q), y = value, fill = metric)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d(name = NULL,
                       labels = c("High Blood Pressure",
                                  "Inactivity",
                                  "Obesity")) +
  labs(title = "Average Health Metrics by Walkability Quartile",
       x     = "Walkability Quartile",
       y     = "Percent") +
  theme_minimal()


# Choropleth Map – Mean Walkability by ZIP Code
walk_map <- zctas |>
  left_join(zcta_walk, by = "zip")

ggplot(walk_map) +
  geom_sf(aes(fill = mean_walk), colour = NA) +
  scale_fill_viridis_c(name = "Walkability",
                       option = "C") +
  labs(title = "Mean Walkability Index by ZIP Code (North Carolina)") +
  theme_void()




