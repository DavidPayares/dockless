# -------------------------------------------------------------------
# This script reproduces the descriptive statistics of the clusters'
# distance data, as displayed in Table 5.1 of the thesis.

# NOTE: first, the analysis script should be ran!
# -------------------------------------------------------------------

require(dockless)
require(dplyr)
require(tibble)
require(tsfeatures)

## ---------------------------- N -----------------------------------

# Total number of time series
n_total = length(distancedata_centroids)

# Number of time series per cluster
n_cluster = gridcentroids %>%
  group_by(cluster) %>%
  summarise(length = n())

## --------------------------- mu -----------------------------------

# Calculate mean of each time series
mu_vector = sapply(
  distancedata_centroids,
  function(x) mean(x$distance, na.rm = TRUE)
)

# Calculate average mean of all time series
mu_total = mean(mu_vector, na.rm = TRUE)

# Calculate average mean per cluster
mu_cluster = mu_vector %>%
  enframe() %>%
  mutate(cluster = gridcentroids$cluster) %>%
  group_by(cluster) %>%
  summarise(mu = mean(value, na.rm = TRUE))

## ------------------------- range ----------------------------------

# Calculate range of each time series
range_vector = sapply(
  distancedata_centroids,
  function(x) max(x$distance, na.rm = TRUE) - min(x$distance, na.rm = TRUE)
)

# Calculate average range of all time series
range_total = mean(range_vector, na.rm = TRUE)

# Calculate average range per cluster
range_cluster = range_vector %>%
  enframe() %>%
  mutate(cluster = gridcentroids$cluster) %>%
  group_by(cluster) %>%
  summarise(range = mean(value, na.rm = TRUE))

## ------------------------- sigma ----------------------------------

# Calculate variance of each time series
sigma_squared_vector = sapply(
  distancedata_centroids,
  function(x) var(x$distance, na.rm = TRUE)
)

# Calculate average variance of all time series
# Take square root to get standard deviation
sigma_total = sqrt(mean(sigma_squared_vector, na.rm = TRUE))

# Calculate average variances per cluster
# Take square roots to get standard deviations
sigma_cluster = sigma_squared_vector %>%
  enframe() %>%
  mutate(cluster = gridcentroids$cluster) %>%
  group_by(cluster) %>%
  summarise(sigma = sqrt(mean(value, na.rm = TRUE)))

## --------------------------- rho ----------------------------------

# Calculate first-order autocorrelation of each time series
rho_vector = sapply(
  distancedata_centroids,
  function(x) tsfeatures::acf_features(x$distance)[[1]]
)

# Calculate average first-order autocorrelation of all time series
rho_total = mean(rho_vector, na.rm = TRUE)

# Calculate average first-order autocorrelation per cluster
rho_cluster = rho_vector %>%
  enframe() %>%
  mutate(cluster = gridcentroids$cluster) %>%
  group_by(cluster) %>%
  summarise(rho = mean(value, na.rm = TRUE))

## ---------------------------- H -----------------------------------

# Calculate spectral entropy of each time series
h_vector = sapply(
  distancedata_centroids,
  function(x) tsfeatures::entropy(x$distance)
)

# Calculate average first-order autocorrelation of all time series
h_total = mean(h_vector, na.rm = TRUE)

# Calculate average first-order autocorrelation per cluster
h_cluster = h_vector %>%
  enframe() %>%
  mutate(cluster = gridcentroids$cluster) %>%
  group_by(cluster) %>%
  summarise(h = mean(value, na.rm = TRUE))
