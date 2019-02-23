# -------------------------------------------------------------------
# This script reproduces all figures in section 5.1 of the thesis.

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
require(ggsci)
require(lubridate)

## ------------------------- grid map -------------------------------

grid_map = ggplot() +
  ggspatial::annotation_map_tile(
    type = 'cartolight',
    zoom = 13
  ) +
  ggspatial::layer_spatial(
    data = systemarea,
    col = 'black',
    lwd = 1,
    fill = NA
  ) +
  ggspatial::layer_spatial(
    data = gridcells,
    col = '#fc8c01',
    lwd = 1,
    alpha = 0.7
  ) +
  ggspatial::layer_spatial(
    data = gridcentroids,
    col = 'black',
    size = 0.5
  ) +
  theme(
    text = element_text(family = 'sans'),
    plot.title = element_text(hjust = 0.5)
  )

## ------------------------ pick-ups map ----------------------------

pickups_map = ggplot() +
  ggspatial::annotation_map_tile(
    type = 'cartolight',
    zoom = 13
  ) +
  ggspatial::layer_spatial(
    data = systemarea,
    col = 'black',
    lwd = 1,
    fill = NA
  ) +
  ggspatial::layer_spatial(
    data = gridcells,
    mapping = aes(fill = intensity),
    lwd = NA,
    alpha = 0.7
  ) +
  scale_fill_material(
    name = 'pick-ups',
    palette = 'orange'
  ) +
  theme(
    text = element_text(family = 'sans'),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.1, 0.2),
    legend.background = element_blank()
  )

## -------------------- pick-ups per day of week --------------------

# Pre process data
usagedata_train$day = lubridate::wday(
  usagedata_train$time,
  label = TRUE,
  abbr = TRUE,
  week_start = 1,
  locale = Sys.setlocale('LC_TIME', 'English')
)

usagedata_dayofweek = as.data.frame(table(usagedata_train$day)/4)

# Plot
usage_dayofweek = ggplot(
  data = usagedata_dayofweek,
  mapping = aes(x = Var1, y = Freq)
) +
  geom_bar(
    fill = '#fc8c01',
    stat = 'identity'
  ) +
  labs(
    x = 'Day of the week',
    y = 'Average number of pick-ups'
  ) +
  theme(
    text = element_text(family = 'sans')
  )

## -------------------- pick-ups per hour of day --------------------

# Pre process data
usagedata_train$hour = lubridate::hour(usagedata_train$time)
usagedata_hourofday = as.data.frame(table(usagedata_train$hour)/28)

# Plot
usage_hourofday = ggplot(
  data = usagedata_hourofday,
  mapping = aes(x = Var1, y = Freq)
) +
  geom_bar(
    fill = '#fc8c01',
    stat = 'identity'
  ) +
  labs(
    x = 'Hour of the day',
    y = 'Average number of pick-ups'
  ) +
  theme(
    text = element_text(family = 'sans')
  )

## ------------------------- clusters map ---------------------------

clusters_map = ggplot() +
  ggspatial::annotation_map_tile(
    type = 'cartolight',
    zoom = 13
  ) +
  ggspatial::layer_spatial(
    data = clusters$outlines,
    mapping = aes(fill = cluster),
    col = 'black',
    lwd = 1,
    alpha = 0.7
  ) +
  scale_fill_manual(
    name = 'Cluster',
    values = dockless_colors(categorical = TRUE),
    labels = c('Bayview', 'Downtown', 'Residential', 'Presidio')
  ) +
  theme(
    text = element_text(family = 'sans'),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.1, 0.2),
    legend.background = element_blank()
  )

## ---------------------- model points map --------------------------

modelpoints_map = ggplot() +
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
    data = modelpoints,
    mapping = aes(fill = cluster),
    color = 'black',
    shape = 21,
    stroke = 2,
    size = 4
  ) +
  scale_fill_manual(
    name = 'Model point',
    values = dockless_colors(categorical = TRUE),
    labels = c('Bayiew', 'Downtown', 'Residential', 'Presidio')
  ) +
  theme(
    text = element_text(family = 'sans'),
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.1, 0.2),
    legend.background = element_blank()
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = 21))
  )

## ----------------------- clusters patterns ------------------------

# Aggregate all data frames by weekhour
data_aggregated = dockless::aggregate_by_weekhour(distancedata_centroids)

# Normalize the distance column of each aggregated data frame
f = function(x) {
  x$distance_scaled = dockless::scale_minmax(x$distance)
  return(x)
}

data_scaled = lapply(data_aggregated, f)

# Get the cluster information
clusters = gridcells$cluster

# Add cluster information to each data frame
f = function(x, y) {
  x$cluster = y
  return(x)
}

data_clustered = mapply(f, data_scaled, clusters, SIMPLIFY = FALSE)

# Bind all data frames together
data_combined = do.call(rbind, data_clustered)

# Aggregate per cluster per weekhour
newdata = data_combined %>%
  group_by(cluster, weekhour) %>%
  summarise(distance = mean(distance, na.rm = TRUE),
            distance_scaled = mean(distance_scaled, na.rm = TRUE))

# Plot
clusterplot = ggplot(
  data = newdata,
  mapping = aes(x = weekhour, y = distance_scaled)
) +
  geom_line(
    lwd = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 168, 24)
  ) +
  labs(
    x = 'Hour of the week',
    y = 'Average normalized distance to nearest bike'
  ) +
  theme(
    text = element_text(family = 'sans')
  ) +
  facet_grid(
    cluster ~ .,
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
clustergrid = ggplot_gtable(ggplot_build(clusterplot))
stripr = which(grepl('strip-r', clustergrid$layout$name))
colors = dockless_colors(categorical = TRUE)
k = 1
for (i in stripr) {
  j = which(grepl('rect', clustergrid$grobs[[i]]$grobs[[1]]$childrenOrder))
  clustergrid$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill = colors[k]
  k = k + 1
}
