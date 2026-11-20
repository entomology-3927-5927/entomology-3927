# ============================================================
# Answer key: Writing functions workshop
# Packages: tidyverse (includes dplyr, ggplot2, purrr, readr)
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# Exercise 1. Vector function: insects per trap-day
# Data: data/exercise1_sampling_rates/trap_catches.csv
# ------------------------------------------------------------

trap_data <- read_csv(
  "data/exercise1_sampling_rates/trap_catches.csv",
  show_col_types = FALSE
)

calculate_rate <- function(count, effort) {
  # Return NA when effort is missing or zero.
  # pmax() is not needed because negative effort should be treated
  # as invalid; this version explicitly handles it.
  rate <- count / effort
  rate[is.na(effort) | effort <= 0] <- NA_real_
  rate
}

trap_data <- trap_data |>
  mutate(
    insects_per_trap_day = calculate_rate(
      count = count,
      effort = trap_days
    )
  )

trap_data

# Optional input checks for a more defensive function:
calculate_rate_checked <- function(count, effort) {
  if (!is.numeric(count) || !is.numeric(effort)) {
    stop("`count` and `effort` must both be numeric.")
  }
  if (length(count) != length(effort)) {
    stop("`count` and `effort` must have the same length.")
  }

  rate <- count / effort
  rate[is.na(effort) | effort <= 0] <- NA_real_
  rate
}


# ------------------------------------------------------------
# Exercise 2. Data-frame function: summarize by habitat
# Data: data/exercise2_habitat_summary/bird_observations.csv
# ------------------------------------------------------------

bird_data <- read_csv(
  "data/exercise2_habitat_summary/bird_observations.csv",
  show_col_types = FALSE
)

summarize_habitat <- function(data, group, abundance, taxon) {
  data |>
    group_by({{ group }}) |>
    summarise(
      total_abundance = sum({{ abundance }}, na.rm = TRUE),
      species_richness = n_distinct({{ taxon }}),
      .groups = "drop"
    )
}

habitat_summary <- summarize_habitat(
  data = bird_data,
  group = habitat,
  abundance = count,
  taxon = species
)

habitat_summary

# Another useful application: summarize by site instead.
site_summary <- summarize_habitat(
  data = bird_data,
  group = site,
  abundance = count,
  taxon = species
)

# Optional extension: count distinct sampling events separately.
# This calculation assumes that `site` and `visit` identify an event.
bird_data |>
  distinct(habitat, site, visit) |>
  count(habitat, name = "sampling_events")


# ------------------------------------------------------------
# Exercise 3. Plot function: relationship between two variables
# Data: data/exercise3_plotting/insect_environment.csv
# ------------------------------------------------------------

insect_environment <- read_csv(
  "data/exercise3_plotting/insect_environment.csv",
  show_col_types = FALSE
)

# Here `group` is an optional CHARACTER column name, e.g. "habitat".
# This makes the optional argument straightforward to handle.
plot_relationship <- function(data, x, y, group = NULL, title = NULL) {
  p <- ggplot(
    data,
    aes(x = .data[[x]], y = .data[[y]])
  )

  if (is.null(group)) {
    p <- p +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)
  } else {
    p <- p +
      geom_point(aes(color = .data[[group]])) +
      geom_smooth(
        aes(color = .data[[group]]),
        method = "lm",
        se = FALSE
      )
  }

  p +
    labs(
      x = x,
      y = y,
      color = group,
      title = title
    ) +
    theme_minimal()
}

# Without grouping
plot_relationship(
  data = insect_environment,
  x = "temperature_c",
  y = "abundance",
  title = "Insect abundance across a temperature gradient"
)

# With habitat-specific colors and fitted lines
plot_relationship(
  data = insect_environment,
  x = "temperature_c",
  y = "abundance",
  group = "habitat",
  title = "Temperature and insect abundance by habitat"
)

# Alternative tidy-evaluation version for students who want to pass
# bare column names (e.g. x = temperature_c). This version uses
# enquo()/!! to handle the optional grouping argument.
plot_relationship_tidy <- function(data, x, y, group = NULL, title = NULL) {
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)

  p <- ggplot(data, aes(x = !!x, y = !!y))

  if (quo_is_null(group)) {
    p <- p +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE)
  } else {
    p <- p +
      geom_point(aes(color = !!group)) +
      geom_smooth(aes(color = !!group), method = "lm", se = FALSE)
  }

  p +
    labs(title = title) +
    theme_minimal()
}

# Example tidy-evaluation call:
plot_relationship_tidy(
  insect_environment,
  temperature_c,
  abundance,
  habitat,
  title = "Temperature and abundance by habitat"
)


# ------------------------------------------------------------
# Exercise 4. File-processing function + iteration
# Data: five CSVs in data/exercise4_multiple_files/
# ------------------------------------------------------------

survey_files <- list.files(
  "data/exercise4_multiple_files/",
  pattern = "_survey\\.csv$",
  full.names = TRUE
)

read_survey <- function(file) {
  site_name <- basename(file) |>
    str_remove("_survey\\.csv$")

  read_csv(file, show_col_types = FALSE) |>
    mutate(site = site_name, .before = 1)
}

# Read each file and combine into one data frame
survey_data <- map_dfr(survey_files, read_survey)

survey_data

# A quick check that all files were included:
survey_data |>
  count(site)

# Optional: save the combined dataset
write_csv(survey_data, "data/combined_insect_surveys.csv")


# ------------------------------------------------------------
# Bonus: a compact alternative using map() + list_rbind()
# (available in modern purrr versions)
# ------------------------------------------------------------

survey_data_alt <- map(survey_files, read_survey) |>
  list_rbind()
