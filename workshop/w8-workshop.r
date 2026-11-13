
library(tidyverse)
library(lubridate)

set.seed(5126)

# ---------------------------
# 1. Sampling sites
# ---------------------------

sites <- tibble(
  site_id = paste0("SITE_", sprintf("%02d", 1:12)),
  habitat = rep(
    c("Forest", "Grassland", "Wetland"),
    each = 4
  ),
  site_effect = rnorm(12, 0, 0.25)
)

# ---------------------------
# 2. Sampling dates
# ---------------------------

sampling_dates <- tibble(
  date = as.Date(c(
    "2026-05-15",
    "2026-06-15",
    "2026-07-15",
    "2026-08-15",
    "2026-09-15",
    "2026-10-15"
  ))
) |>
  mutate(
    month = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date),
    day_of_year = yday(date)
  )

# ---------------------------
# 3. Bird species information
# ---------------------------

species_info <- tribble(
  ~species,                  ~guild,             ~preferred_habitat, ~base_abundance,
  "Black-capped Chickadee",  "Insectivore",       "Forest",            8,
  "Red-eyed Vireo",          "Insectivore",       "Forest",            7,
  "Downy Woodpecker",        "Insectivore",       "Forest",            5,
  "Ovenbird",                "Ground nester",     "Forest",            6,
  "Eastern Meadowlark",      "Grassland bird",    "Grassland",         7,
  "Bobolink",                "Grassland bird",    "Grassland",         8,
  "Savannah Sparrow",        "Grassland bird",    "Grassland",         6,
  "Marsh Wren",              "Wetland bird",      "Wetland",           6,
  "Red-winged Blackbird",    "Wetland bird",      "Wetland",          10,
  "Great Blue Heron",        "Wading bird",       "Wetland",           3
)

# ---------------------------
# 4. Create site-level visits
# ---------------------------

visits <- crossing(
  site_id = sites$site_id,
  date = sampling_dates$date
) |>
  left_join(sites, by = "site_id") |>
  left_join(sampling_dates, by = "date") |>
  mutate(
    # Seasonal temperature pattern
    temperature = 17 +
      10 * sin(pi * (month_num - 5) / 5) +
      rnorm(n(), 0, 2),

    # Wind conditions
    wind_speed = pmax(
      0,
      rnorm(n(), 9, 3.5)
    )
  )

# ---------------------------
# 5. Expand to species observations
# ---------------------------

bird_data <- crossing(
  visits,
  species = species_info$species
) |>
  left_join(species_info, by = "species") |>
  mutate(
    # Strong habitat associations
    habitat_effect = if_else(
      habitat == preferred_habitat,
      2.8,
      0.18
    ),

    # Species-specific seasonal patterns
    seasonal_effect = case_when(

      # Relatively consistent throughout summer
      species == "Black-capped Chickadee" ~
        1.0,

      # Peaks in midsummer
      species == "Red-eyed Vireo" ~
        0.5 + 1.0 * exp(-((month_num - 7.2)^2) / 1.8),

      # Relatively consistent
      species == "Downy Woodpecker" ~
        0.9,

      # Strong early-season peak
      species == "Ovenbird" ~
        0.35 + 1.2 * exp(-((month_num - 6)^2) / 1.5),

      # Peaks in early summer
      species == "Eastern Meadowlark" ~
        0.4 + 1.1 * exp(-((month_num - 6.5)^2) / 1.8),

      # Peaks in midsummer, declines in fall
      species == "Bobolink" ~
        0.2 + 1.5 * exp(-((month_num - 7)^2) / 1.5),

      # Declines toward fall
      species == "Savannah Sparrow" ~
        0.4 + 0.9 * exp(-((month_num - 7)^2) / 3),

      # Peaks during breeding season
      species == "Marsh Wren" ~
        0.3 + 1.1 * exp(-((month_num - 7)^2) / 2),

      # High abundance in early/midsummer
      species == "Red-winged Blackbird" ~
        0.4 + 1.2 * exp(-((month_num - 6.5)^2) / 2),

      # More consistent, but lower abundance
      species == "Great Blue Heron" ~
        0.8
    ),

    # Expected abundance
    expected_abundance = base_abundance *
      habitat_effect *
      seasonal_effect *
      exp(
        0.035 * (temperature - 20) -
          0.045 * wind_speed +
          site_effect
      ),

    # Simulate overdispersed count data
    abundance = rnbinom(
      n(),
      mu = expected_abundance,
      size = 2.5
    )
  ) |>
  select(
    site_id,
    habitat,
    date,
    # month,
    # month_num,
    # day_of_year,
    temperature,
    wind_speed,
    species,
    guild,
    abundance
  ) |>
  arrange(site_id, date, species)

# ---------------------------
# 6. Inspect and export
# ---------------------------

glimpse(bird_data)

write_csv(
  bird_data,
  "./workshop/workshop_data/w8/w8-workshop-burds.csv"
)


library(tidyverse)
library(lubridate)

set.seed(5126)

# ---------------------------
# 1. Create sampling sites
# ---------------------------

sites <- tibble(
  site_id = paste0("SITE_", sprintf("%02d", 1:12)),
  habitat = rep(
    c("Prairie", "Field margin", "Urban garden"),
    each = 4
  ),
  site_effect = rnorm(12, mean = 0, sd = 0.20)
)

# ---------------------------
# 2. Create sampling dates
# ---------------------------

sampling_dates <- tibble(
  date = seq(
    as.Date("2026-05-15"),
    as.Date("2026-08-30"),
    length.out = 8
  )
) |>
  mutate(
    day_of_year = yday(date),
    visit = row_number()
  )

# ---------------------------
# 3. Create site-level visits
# ---------------------------

visits <- crossing(
  site_id = sites$site_id,
  date = sampling_dates$date
) |>
  left_join(sites, by = "site_id") |>
  left_join(sampling_dates, by = "date") |>
  mutate(
    # Seasonal temperature pattern
    temperature = 19 +
      7 * sin(pi * (day_of_year - 135) / 130) +
      rnorm(n(), 0, 2.5),

    # Wind conditions
    wind_speed = pmax(
      0,
      rnorm(n(), mean = 10, sd = 4)
    ),

    # Floral richness varies by habitat and season
    floral_richness = round(
      case_when(
        habitat == "Prairie" ~ 12,
        habitat == "Field margin" ~ 7,
        habitat == "Urban garden" ~ 10
      ) +
        5 * sin(pi * (day_of_year - 125) / 130) +
        rnorm(n(), 0, 2)
    ),

    floral_richness = pmax(1, floral_richness),

    # Flower abundance
    flower_abundance = round(
      floral_richness * runif(n(), 12, 25) +
        rnorm(n(), 0, 15)
    ),

    flower_abundance = pmax(1, flower_abundance)
  )

# ---------------------------
# 4. Expand to pollinator groups
# ---------------------------

pollinator_data <- crossing(
  visits,
  pollinator_group = c(
    "Bees",
    "Butterflies",
    "Flies",
    "Beetles"
  )
) |>
  mutate(
    # Baseline abundance differs among groups
    group_effect = case_when(
      pollinator_group == "Bees" ~ 1.5,
      pollinator_group == "Butterflies" ~ 0.8,
      pollinator_group == "Flies" ~ 1.2,
      pollinator_group == "Beetles" ~ 0.6
    ),

    # Habitat effects
    habitat_effect = case_when(
      habitat == "Prairie" ~ 1.5,
      habitat == "Field margin" ~ 1.0,
      habitat == "Urban garden" ~ 1.2
    ),

    # Different seasonal activity patterns
    seasonal_effect = case_when(
      pollinator_group == "Bees" ~
        0.8 + 0.5 * sin(pi * (day_of_year - 130) / 125),

      pollinator_group == "Butterflies" ~
        0.6 + 0.9 * sin(pi * (day_of_year - 145) / 125),

      pollinator_group == "Flies" ~
        0.9 + 0.3 * sin(pi * (day_of_year - 120) / 125),

      pollinator_group == "Beetles" ~
        0.8 + 0.4 * sin(pi * (day_of_year - 155) / 125)
    ),

    # Expected abundance
    expected_abundance = exp(
      log(5) +
        log(group_effect) +
        log(habitat_effect) +
        log(seasonal_effect) +
        0.025 * (temperature - 20) -
        0.035 * wind_speed +
        0.025 * (floral_richness - 10) +
        site_effect
    ),

    # Simulate overdispersed count data
    abundance = rnbinom(
      n(),
      mu = expected_abundance,
      size = 3
    )
  ) |>
  select(
    site_id,
    habitat,
    date,
    # day_of_year,
    # visit,
    temperature,
    wind_speed,
    floral_richness,
    flower_abundance,
    pollinator_group,
    abundance
  ) |>
  arrange(site_id, date, pollinator_group)

# ---------------------------
# 5. Inspect and export
# ---------------------------

glimpse(pollinator_data)

write_csv(
  pollinator_data,
  "./workshop/workshop_data/w8/w8-workshop-beez.csv"
)

bird_data
pollinator_data

