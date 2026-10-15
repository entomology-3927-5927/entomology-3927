library(tidyverse)
library(googledrive)
library(googlesheets4)

set.seed(3927)

plant_data <- tibble(
  plot_id = sprintf("P%03d", 1:500),

  site = sample(
    sprintf("Site_%02d", 1:20),
    500,
    replace = TRUE
  ),

  habitat = sample(
    c("remnant_prairie",
      "restored_prairie",
      "savanna",
      "crop_edge"),
    500,
    replace = TRUE
  ),

  species = sample(
    c("Andropogon_gerardii",
      "Schizachyrium_scoparium",
      "Sorghastrum_nutans",
      "Panicum_virgatum",
      "Solidago_canadensis",
      "Echinacea_purpurea",
      "Monarda_fistulosa",
      "Asclepias_tuberosa"),
    500,
    replace = TRUE
  ),

  precipitation_mm = round(
    rnorm(500, mean = 750, sd = 100),
    1
  ),

  temperature_C = round(
    rnorm(500, mean = 17, sd = 3),
    1
  ),

  soil_moisture = round(
    runif(500, 10, 45),
    1
  ),

  canopy_cover = round(
    runif(500, 0, 80),
    1
  ),

  plant_height_cm = round(
    rnorm(500, mean = 80, sd = 25),
    1
  ),

  percent_cover = round(
    runif(500, 0, 100),
    1
  ),

  biomass_g = round(
    rgamma(500, shape = 3, rate = 0.04),
    1
  )
)

gs4_deauth()
drive_deauth()
gs4_auth(
   scopes = c(
     "https://www.googleapis.com/auth/spreadsheets",
     "https://www.googleapis.com/auth/drive"
   )
)

drive_auth(
   scopes = c(
     "https://www.googleapis.com/auth/spreadsheets",
     "https://www.googleapis.com/auth/drive"
   )
)

# Create sheet
ss <- gs4_create("w5-workshop-data")
folder <- drive_find(type = "folder", pattern = "example-data", n_max = 1)

# Move it to the desired Google Drive folder
drive_mv(
   file = ss,
   path = folder
)

sheet_write(
  data = plant_data,
  ss = ss
)

write_csv(
  plant_data,
  "workshop/workshop_data/w5-workshop-data.csv"
)


# messy plant data

library(tidyverse)

set.seed(3927)

n <- 500

plant_data_messy <- tibble(
  `Plot ID` = sprintf("P%03d", sample(1:250, n, replace = TRUE)),

  Year = sample(
    c(2025, 2026),
    n,
    replace = TRUE
  ),

  `site name` = sample(
    sprintf("Site_%02d", 1:20),
    n,
    replace = TRUE
  ),

  `Habitat-Type` = sample(
    c("remnant_prairie",
      "restored_prairie",
      "savanna",
      "crop_edge"),
    n,
    replace = TRUE
  ),

  `Species Name` = sample(
    c("Andropogon_gerardii",
      "Schizachyrium_scoparium",
      "Sorghastrum_nutans",
      "Panicum_virgatum",
      "Solidago_canadensis",
      "Echinacea_purpurea",
      "Monarda_fistulosa",
      "Asclepias_tuberosa"),
    n,
    replace = TRUE
  ),

  `Annual Precipitation (mm)` = round(
    rnorm(n, mean = 750, sd = 100),
    1
  ),

  `Temperature C` = round(
    rnorm(n, mean = 17, sd = 3),
    1
  ),

  `Soil Moisture (%)` = round(
    runif(n, 10, 45),
    1
  ),

  `Canopy Cover (%)` = round(
    runif(n, 0, 80),
    1
  ),

  `Plant Height (cm)` = round(
    rnorm(n, mean = 80, sd = 25),
    1
  ),

  `Percent Cover` = round(
    runif(n, 0, 100),
    1
  ),

  `Biomass (g)` = round(
    rgamma(n, shape = 3, rate = 0.04),
    1
  )
)

# Introduce messy categorical values
plant_data_messy$`Habitat-Type`[
  sample(1:n, 20)
] <- sample(
  c("Remnant Prairie", "remnant prairie",
    "RESTORED_PRAIRIE", "Crop Edge"),
  20,
  replace = TRUE
)

plant_data_messy$`Species Name`[
  sample(1:n, 15)
] <- NA_character_

# Introduce missing values
plant_data_messy$`Soil Moisture (%)`[
  sample(1:n, 12)
] <- NA_real_

plant_data_messy$`Canopy Cover (%)`[
  sample(1:n, 10)
] <- NA_real_

# Introduce text into numeric columns
plant_data_messy$`Annual Precipitation (mm)` <- as.character(
  plant_data_messy$`Annual Precipitation (mm)`
)

plant_data_messy$`Annual Precipitation (mm)`[
  sample(1:n, 5)
] <- c("unknown", "750 mm", "missing", "NA", "not recorded")

plant_data_messy$`Plant Height (cm)` <- as.character(
  plant_data_messy$`Plant Height (cm)`
)

plant_data_messy$`Plant Height (cm)`[
  sample(1:n, 5)
] <- c("tall", "85 cm", "unknown", "NA", "not measured")

# Add inconsistent missing-value codes
plant_data_messy$`Biomass (g)` <- as.character(
  plant_data_messy$`Biomass (g)`
)

plant_data_messy$`Biomass (g)`[
  sample(1:n, 8)
] <- sample(
  c("", "NA", "n/a", "not collected"),
  8,
  replace = TRUE
)

# Introduce suspicious but technically numeric values
plant_data_messy$`Temperature C`[
  sample(1:n, 3)
] <- c(-99, 999, 0)

# Pivot into wide messy format
plant_data_messy |>
  mutate(
    observation_id = row_number()
  ) |>
  select(
    observation_id,
    `Plot ID`,
    `site name`,
    `Habitat-Type`,
    `Species Name`,
    Year,
    `Plant Height (cm)`,
    `Percent Cover`,
    `Biomass (g)`,
    `Soil Moisture (%)`,
    `Canopy Cover (%)`,
    `Temperature C`,
    `Annual Precipitation (mm)`
  ) |>
  pivot_wider(
    id_cols = c(
      observation_id,
      `Plot ID`,
      `site name`,
      `Habitat-Type`,
      `Species Name`
    ),
    names_from = Year,
    values_from = c(
      `Plant Height (cm)`,
      `Percent Cover`,
      `Biomass (g)`,
      `Soil Moisture (%)`,
      `Canopy Cover (%)`,
      `Temperature C`,
      `Annual Precipitation (mm)`
    ),
    names_glue = "{.value}_{Year}"
  )

# Save the messy dataset
write_csv(plant_data_messy, "workshop/workshop_data/w5-workshop-data-messy.csv")

# Messy, but simpler
set.seed(3927)

n <- 500

# Generate plant height data for two years
plant_data_messy <- tibble(
  `Plot ID` = sprintf("P%03d", sample(1:250, n, replace = TRUE)),

  `site name` = sample(
    sprintf("Site_%02d", 1:20),
    n,
    replace = TRUE
  ),

  `Habitat-Type` = sample(
    c(
      "remnant_prairie",
      "restored_prairie",
      "savanna",
      "crop_edge"
    ),
    n,
    replace = TRUE
  ),

  `Species Name` = sample(
    c(
      "Andropogon_gerardii",
      "Schizachyrium_scoparium",
      "Sorghastrum_nutans",
      "Panicum_virgatum",
      "Solidago_canadensis",
      "Echinacea_purpurea",
      "Monarda_fistulosa",
      "Asclepias_tuberosa"
    ),
    n,
    replace = TRUE
  ),

  Year = sample(
    c(2025, 2026),
    n,
    replace = TRUE
  ),

  `Plant Height (cm)` = round(
    rnorm(n, mean = 80, sd = 25),
    1
  )
)

# Introduce inconsistent habitat names
plant_data_messy$`Habitat-Type`[
  sample(1:n, 20)
] <- sample(
  c(
    "Remnant Prairie",
    "remnant prairie",
    "RESTORED_PRAIRIE",
    "Crop Edge"
  ),
  20,
  replace = TRUE
)

# Introduce missing species names
plant_data_messy$`Species Name`[
  sample(1:n, 15)
] <- NA_character_

# Introduce text into the plant height column
plant_data_messy$`Plant Height (cm)` <- as.character(
  plant_data_messy$`Plant Height (cm)`
)

plant_data_messy$`Plant Height (cm)`[
  sample(1:n, 5)
] <- c(
  "tall",
  "85 cm",
  "unknown",
  "NA",
  "not measured"
)

# Introduce suspicious but technically numeric heights
plant_data_messy$`Plant Height (cm)`[
  sample(1:n, 3)
] <- c("-99", "999", "0")

# Widen the dataset so each year gets its own height column
plant_data_messy_wide <- plant_data_messy |>
  pivot_wider(
    id_cols = c(
      `Plot ID`,
      `site name`,
      `Habitat-Type`,
      `Species Name`
    ),
    names_from = Year,
    values_from = `Plant Height (cm)`,
    names_prefix = "height_"
  )
# Export the messy dataset
write_csv(plant_data_messy_wide, "workshop/workshop_data/w5-workshop-data-messy.csv")

plant_data_clean <- plant_data_messy_wide %>%
  janitor::clean_names() %>%
  pivot_longer(
    cols = num_range("height_", 2025:2026),
    names_to = "year",
    values_to = "height_cm", 
    names_prefix = "height_"
  ) %>%
  mutate(
    year = parse_number(year),
    height_cm = parse_number(height_cm)
)
