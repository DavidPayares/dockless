# -------------------------------------------------------------------
# This script reproduces the prediction interval check, as displayed
# in Table 5.4 of the thesis.

# NOTE: first, the analysis script should be ran!
# -------------------------------------------------------------------

require(dockless)
require(dplyr)

# Determine if observations fall within 95% interval
f = function(x) {

  inside = (x$observation <= x$upper95 & x$observation >= x$lower95)
  count_inside = sum(inside, na.rm = TRUE)
  count_total  = length(inside) - length(inside[is.na(inside)])

  data.frame(inside = count_inside, total = count_total)

}

data = lapply(forecasts_dbafs, f) %>%
  bind_rows() %>%
  mutate(cluster = testpoints$cluster)

# Total percentage of observations inside 95% interval
percentage_total = sum(data$inside) / sum(data$total) * 100

# Percentage of observations inside 95% interval, per cluster
percentage_cluster = data %>%
  group_by(cluster) %>%
  summarise(percentage = sum(inside) / sum(total) * 100)
