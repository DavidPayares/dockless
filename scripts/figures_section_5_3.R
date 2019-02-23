# -------------------------------------------------------------------
# This script reproduces all figures in section 5.3 of the thesis.

# NOTE: first, the analysis script should be ran!
# -------------------------------------------------------------------

require(dockless)
require(ggplot2)
require(dplyr)
require(tibble)
require(tidyr)
require(sf)
require(rosm)
require(ggspatial)
require(lubridate)

## -------------------- test points locations -----------------------

testpoints_locations = ggplot() +
  ggspatial::annotation_map_tile(
    type = 'cartolight',
    zoom = 13
  ) +
  ggspatial::layer_spatial(
    data = clusters$outlines,
    col = 'grey',
    lwd = 1,
    alpha = 0.4
  ) +
  ggspatial::layer_spatial(
    data = testpoints,
    col = '#fc8c01',
    alpha = 0.7
  ) +
  theme(
    text = element_text(family = 'serif'),
    plot.title = element_text(hjust = 0.5)
  )

## ------------------- test points timestamps -----------------------

# Round timestamps to the nearest hour
testpoints_rounded = testpoints %>%
  mutate(time = lubridate::round_date(time, 'hour'))

# Plot
testpoints_time = ggplot(
  data = testpoints_rounded,
  mapping = aes(x = time)
) +
  geom_bar(
    fill = '#fc8c01'
  ) +
  scale_x_datetime(
    date_breaks = '1 days',
    date_labels = c('Nov 5', 'Oct 29', 'Oct 30', 'Oct 31', 'Nov 1', 'Nov 2', 'Nov 3', 'Nov 4')
  ) +
  theme(
    text = element_text(family = 'sans'),
    axis.title.x = element_blank()
  )

## ------------------------- rmse per hour --------------------------

# Calculate DBAFS forecasts RMSE's per hour of the day
rmse_dbafs = dockless::error_hourofday(
  forecasts_dbafs,
  type = 'RMSE'
)
rmse_dbafs[25] = rmse_dbafs[1]

# Calculate Naive forecasts RMSE's per hour of the day
rmse_nfs = dockless::error_hourofday(
  forecasts_nfs,
  type = 'RMSE'
)
rmse_nfs[25] = rmse_nfs[1]

# Combine
rmse_hourofday = data.frame(
  rmse = c(rmse_dbafs, rmse_nfs),
  hour = c(rep(seq(0, 24, 1), 2)),
  method = c(rep('DBAFS', 25), rep('NFS', 25))
)

# Plot
hourofday = ggplot(
  data = rmse_hourofday,
  mapping = aes(x = hour, y = rmse)
) +
  geom_line(
    mapping = aes(col = method),
    lwd = 2
  ) +
  scale_color_manual(
    values = c('#fc8c01', 'tan')
  ) +
  scale_x_continuous(
    breaks = c(0, 4, 8, 12, 16, 20, 24),
    labels = c("0:00", "4:00", "8:00", "12:00", "16:00", "20:00", "24:00")
  ) +
  labs(
    x = 'Hour of the day',
    y = 'Average RMSE'
  ) +
  theme(
    text = element_text(family = 'sans'),
    legend.position = 'none'
  )

## -------------------------- rmse per lag --------------------------

# Calculate DBAFS forecasts RMSE's per forecasting lag
rmse_dbafs = dockless::error_lag(
  forecasts_dbafs,
  type = 'RMSE'
)

# Calculate Naive forecasts RMSE's per forecasting lag
rmse_nfs = dockless::error_lag(
  forecasts_nfs,
  type = 'RMSE'
)

# Combine
rmse_lag = data.frame(
  rmse = c(rmse_dbafs, rmse_nfs),
  lag = c(rep(seq(1, 96, 1), 2)),
  method = c(rep('DBAFS', 96), rep('NFS', 96))
)

# Plot
lag = ggplot(
  data = rmse_lag,
  mapping = aes(x = lag, y = rmse)
) +
  geom_line(
    mapping = aes(col = method),
    lwd = 2
  ) +
  scale_color_manual(
    values = c('#fc8c01', 'tan')
  ) +
  scale_x_continuous(
    breaks = c(0, 16, 32, 48, 64, 80, 96),
    labels = c('0 hours', '4 hours', '8 hours', '12 hours', '16 hours', '20 hours', '24 hours')
  ) +
  labs(
    x = 'Forecast lag',
    y = 'Average RMSE'
  ) +
  theme(
    text = element_text(family = 'sans'),
    legend.position = c(0.9, 0.9),
    legend.background = element_blank()
  )

## ------------------- model point forecasts ------------------------

# Forecast the whole week
forecasts_modelpoints_week = dockless::forecast_multiple(
  data = distancedata_modelpoints_test,
  method = 'DBAFS',
  perspective = 'operator',
  points = modelpoints,
  models = models
)

# Add cluster information
f = function(x, y) {
  x$cluster = y
  return(x)
}

cluster_vector = as.factor(c(1,2,3,4))
data = mapply(
  f,
  forecasts_modelpoints_week,
  cluster_vector,
  SIMPLIFY = FALSE
)

# Bind all data frames together
newdata = do.call(rbind, data)

# Data frame for observations
obs_newdata   = newdata

# Add color columns
newdata$color = '#fc8c01'
obs_newdata$color = 'darkgrey'

# Plot
forecastplot = ggplot() +
  geom_line(
    data = obs_newdata,
    mapping = aes(x = time, y = observation, col = color)
  ) +
  geom_line(
    data = newdata,
    mapping = aes(x = time, y = forecast, col = color),
    size = 1
  ) +
  scale_x_datetime(
    date_breaks = '1 days',
    date_labels = c('Nov 5', 'Oct 29', 'Oct 30', 'Oct 31', 'Nov 1', 'Nov 2', 'Nov 3', 'Nov 4')
  ) +
  scale_color_manual(
    name = " ",
    values = c('#fc8c01', 'darkgrey'),
    labels = c('forecast', 'observation')
  ) +
  labs(
    x = 'Time',
    y = 'Distance to the nearest available bike (m)'
  ) +
  theme(
    text = element_text(family = 'sans'),
    axis.title.x = element_blank(),
    legend.position = 'bottom'
  ) +
  facet_wrap(
    ~ cluster,
    ncol = 1,
    scales = 'free',
    strip.position = 'right',
    labeller = as_labeller(
      c(
        '1' = 'Bayview',
        '2' = 'Downtown',
        '3' = 'Residential',
        '4' = 'Presidio'
      )
    )
  )

# Color the facet backgrounds
# Code retrieved from https://github.com/tidyverse/ggplot2/issues/2096
forecastgrid = ggplot_gtable(ggplot_build(forecastplot))
stripr = which(grepl('strip-', forecastgrid$layout$name))
colors = dockless_colors(categorical = TRUE)
k = 1
for (i in stripr) {
  j = which(grepl('rect', forecastgrid$grobs[[i]]$grobs[[1]]$childrenOrder))
  forecastgrid$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill = colors[k]
  k = k + 1
}
