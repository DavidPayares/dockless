# -------------------------------------------------------------------
# This script reproduces all figures in appendix B of the thesis.

# NOTE: first, the analysis script should be ran!
# -------------------------------------------------------------------

require(dockless)
require(forecast)
require(ggplot2)
require(dplyr)
require(tibble)
require(tidyr)

## ------------------------- downtown -------------------------------

stl = models[[2]]$stl %>%
  as_tibble() %>%
  gather() %>%
  mutate(key = factor(.$key, levels = c('Data', 'Trend', 'Seasonal96', 'Remainder'))) %>%
  mutate(time = rep(distancedata_modelpoints[[2]]$time, length(unique(.$key))))

stlplot_2 = ggplot() +
  geom_line(
    data = stl,
    mapping = aes(x = time, y = value)
  ) +
  labs(
    x = 'Time',
    y = 'Log transformed distance to the nearest bike'
  ) +
  scale_x_datetime(
    date_breaks = '1 weeks',
    date_labels = c('Oct 15', 'Sep 17', 'Sep 24', 'Oct 1', 'Oct 8')
  ) +
  theme(
    text = element_text(family = 'sans'),
    strip.background = element_rect(fill = dockless_colors(categorical = TRUE)[2])
  ) +
  facet_grid(
    key ~ .,
    scale = 'free_y'
  )

## ------------------------ residential -----------------------------

stl = models[[3]]$stl %>%
  as_tibble() %>%
  gather() %>%
  mutate(key = factor(.$key, levels = c('Data', 'Trend', 'Seasonal96', 'Remainder'))) %>%
  mutate(time = rep(distancedata_modelpoints[[3]]$time, length(unique(.$key))))

stlplot_3 = ggplot() +
  geom_line(
    data = stl,
    mapping = aes(x = time, y = value)
  ) +
  labs(
    x = 'Time',
    y = 'Log transformed distance to the nearest bike'
  ) +
  scale_x_datetime(
    date_breaks = '1 weeks',
    date_labels = c('Oct 15', 'Sep 17', 'Sep 24', 'Oct 1', 'Oct 8')
  ) +
  theme(
    text = element_text(family = 'sans'),
    strip.background = element_rect(fill = dockless_colors(categorical = TRUE)[3])
  ) +
  facet_grid(
    key ~ .,
    scale = 'free_y'
  )

## ------------------------- presidio -------------------------------

stl = models[[4]]$stl %>%
  as_tibble() %>%
  gather() %>%
  mutate(key = factor(.$key, levels = c('Data', 'Trend', 'Seasonal672', 'Remainder'))) %>%
  mutate(time = rep(distancedata_modelpoints[[3]]$time, length(unique(.$key))))

stlplot_4 = ggplot() +
  geom_line(
    data = stl,
    mapping = aes(x = time, y = value)
  ) +
  labs(
    x = 'Time',
    y = 'Log transformed distance to the nearest bike'
  ) +
  scale_x_datetime(
    date_breaks = '1 weeks',
    date_labels = c('Oct 15', 'Sep 17', 'Sep 24', 'Oct 1', 'Oct 8')
  ) +
  theme(
    text = element_text(family = 'sans'),
    strip.background = element_rect(fill = dockless_colors(categorical = TRUE)[4])
  ) +
  facet_grid(
    key ~ .,
    scale = 'free_y'
  )
