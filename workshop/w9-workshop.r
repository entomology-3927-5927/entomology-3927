z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

insect_data |>
  mutate(abun_z_score = z_score(abundance))


# Exercise 1

# Exercise 2

# Exercise 3
plot_relationship <- function(data, x, y, group = NULL) {
  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()
}

# Relationship between temperature and abundance
plot_relationship(
  data = insect_data,
  x = temperature,
  y = abundance
)
