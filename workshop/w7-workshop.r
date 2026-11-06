library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Define site characteristics
sites <- tibble(
  site_id = paste0("SITE_", sprintf("%02d", 1:12)),
  habitat = rep(
    c("Forest", "Grassland", "Agriculture", "Wetland"),
    each = 3
  ),
  treatment = rep(c("Control", "Restoration", "Control"), 4)
)

# Define insect species and average body lengths
species_data <- tibble(
  species = c(
    "Bombus impatiens",
    "Bombus griseocollis",
    "Apis mellifera",
    "Osmia lignaria",
    "Halictus rubicundus",
    "Agapostemon virescens"
  ),
  mean_body_length = c(14, 12, 11, 9, 7, 8)
)

# Generate sampling dates
sampling_dates <- tibble(
  date = seq(
    as.Date("2025-05-01"),
    as.Date("2025-09-30"),
    by = "2 weeks"
  )
)

# Create dataset
insect_data <- expand_grid(
  site_id = sites$site_id,
  date = sampling_dates$date,
  species = species_data$species
) %>%
  left_join(sites, by = "site_id") %>%
  left_join(species_data, by = "species") %>%
  mutate(
    # Seasonal temperature pattern
    day_of_year = yday(date),

    temperature = 18 +
      7 * sin((day_of_year - 100) * 2 * pi / 365) +
      case_when(
        habitat == "Forest" ~ -2,
        habitat == "Wetland" ~ -1,
        habitat == "Grassland" ~ 1,
        habitat == "Agriculture" ~ 2
      ) +
      rnorm(n(), 0, 2),

    # Expected abundance varies by habitat, treatment, and season
    abundance_mean = case_when(
      habitat == "Forest" ~ 5,
      habitat == "Grassland" ~ 9,
      habitat == "Agriculture" ~ 6,
      habitat == "Wetland" ~ 7
    ) *
      if_else(treatment == "Restoration", 1.4, 1) *
      exp(-((day_of_year - 200) / 100)^2) *
      case_when(
        species == "Bombus impatiens" ~ 1.2,
        species == "Bombus griseocollis" ~ 1,
        species == "Apis mellifera" ~ 1.5,
        species == "Osmia lignaria" ~ 0.7,
        species == "Halictus rubicundus" ~ 1.1,
        species == "Agapostemon virescens" ~ 0.9
      ),

    # Generate insect counts
    abundance = rpois(n(), lambda = abundance_mean),

    # Generate individual/species body-length measurements
    body_length = round(
      rnorm(n(), mean = mean_body_length, sd = 1.5),
      1
    ),

    # Remove intermediate variables
    day_of_year = NULL,
    mean_body_length = NULL,
    abundance_mean = NULL
  ) %>%
  select(
    date,
    site_id,
    habitat,
    treatment,
    species,
    abundance,
    body_length,
    temperature
  )

# Inspect dataset
glimpse(insect_data)

# Export dataset
write_csv(insect_data, "./workshop/workshop_data/w7-exercise1.csv")


# Exercise 2
# Set seed for reproducibility
set.seed(42)

# Define habitat characteristics
habitats <- tibble(
  habitat_type = c(
    "Forest",
    "Grassland",
    "Wetland",
    "Agriculture",
    "Urban"
  ),

  mean_temp = c(17, 20, 18, 19, 22),

  temp_amplitude = c(3, 8, 4, 7, 6)
)

# Generate temperature measurements
temperature_data <- expand_grid(
  habitat_type = habitats$habitat_type,
  hour = 0:23,
  sensor = 1:3
) %>%
  left_join(habitats, by = "habitat_type") %>%
  mutate(
    # Daily temperature cycle
    temperature = mean_temp +
      temp_amplitude * cos((hour - 15) * 2 * pi / 24) +
      rnorm(n(), mean = 0, sd = 1.2),

    temperature = round(temperature, 1),

    sensor = paste0("SENSOR_", sensor)
  ) %>%
  select(
    habitat_type,
    sensor,
    hour,
    temperature
  )

# Summarize temperature by habitat and hour
temperature_summary <- temperature_data %>%
  group_by(habitat_type, hour) %>%
  summarize(
    mean_temperature = mean(temperature),
    sd_temperature = sd(temperature),
    .groups = "drop"
  )

# Inspect datasets
glimpse(temperature_data)
glimpse(temperature_summary)

# Export datasets
write_csv(temperature_data, "./workshop/workshop_data/w7-exercise2.csv")
# write_csv(temperature_summary, "./workshop/workshop_data/w7-exercise1.csv")
