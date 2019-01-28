#' Color schemes for the \code{dockless} package
#'
#' Creates either sequential or categorical color schemes in the dockless style.
#'
#' @param n the number of colors to be in the palette. Ignored if
#' \code{categorical} is set to \code{TRUE}.
#' @param categorical logical. If \code{TRUE}, a categorical color scheme will be
#' produced. If \code{FALSE}, a sequential color scheme will be produced.
#' @return Returns an vector of colors.
#' @export
dockless_colors = function(n = 10, categorical = FALSE) {

  if (categorical) {
    c('orange', 'deepskyblue', 'magenta', 'lawngreen' )
  } else {
    color_function = grDevices::colorRampPalette(
      c('#fbd38c', '#fac56a', '#f9b847', '#f9ab24', '#f39c06', '#d08605',
        '#ad6f04', '#8a5903', '#684302', '#452c01')
    )

    color_function(n)
  }

}


#' Project \code{sf} object to State Plane California Zone III
#'
#' Transforms the CRS of an object of class \code{sf} to the projection
#' optimized for San Francisco with units in meters, i.e. State Plane
#' California Zone III, EPSG:26943.
#'
#' @param data object of class \code{sf}.
#' @return Returns an object of class \code{sf}.
#' @export
project_sf = function(data) {

  # Calculcate centroid of area
  # Ignore warning about longitude/latitude data
  centroid = suppressWarnings(
    sf::st_centroid(sf::st_convex_hull(data))
  )

  # Seperate coordinates of the centroid
  centroid_coord = sf::st_coordinates(centroid)

  # If area is in SF, use State Plane California Zone III (EPSG:26943)
  if (ceiling(centroid_coord[1,1]) == -122 & floor(centroid_coord[1,2]) == 37) {
    projection = 26943
  } else {
    stop('The provided area is not in San Francisco')
  }

  # Project the coordinates of the area
  sf::st_transform(data, crs = projection)

}

#' Min/max normalization
#'
#' Normalizes a numeric vector with min/max normalization. Missing values are
#' allowed.
#'
#' @param x numeric vector.
#' @return Returns a numeric vector.
#' @export
scale_minmax = function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#' Extract hour of the week
#'
#' Extracts the hour of the week from a timestamp of class \code{POSIXct}.
#'
#' @param x timestamp of class \code{POSIXct}.
#' @param week_start day on which week starts following ISO conventions -
#' 1 means Monday, 7 means Sunday.
#' @return Returns a numeric value between 0 and 167.
#' @export
weekhour = function(x, week_start = 1) {
  weekday = lubridate::wday(x, week_start = week_start)
  dayhour = lubridate::hour(x)
  weekhour = dayhour + (weekday - 1) * 24

  return(weekhour)
}

#' Aggregate by hour of the week
#'
#' Aggregates the distance column in each \code{dockless_df} data frame of a
#' \code{dockless_dfc} by the hour of the week.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param week_start day on which week starts following ISO conventions -
#' 1 means Monday, 7 means Sunday.
#' @return Returns a numeric value between 0 and 167.
#' @export
aggregate_by_weekhour = function(data, week_start = 1) {

  # Add a weekhour column to the data
  f = function(x) {
    x$weekhour = weekhour(x$time, week_start = week_start)
    return(x)
  }

  data = lapply(data, f)

  # For each data frame, aggregate distance per weekhour
  f = function(x) {
    stats::aggregate(
      x['distance'],
      by = list(weekhour = x$weekhour),
      FUN = function(x) mean(x, na.rm = TRUE)
    )
  }

  lapply(data, f)
}

#' Dissimilarity matrix of a \code{dockless_dfc} object
#'
#' Creates a dissimilarity matrix based on the euclidean distances between all
#' \code{dockless_df} objects in a \code{dockless_dfc}.
#'
#' @param data object of class \code{dockless_dfc}.
#' @return Returns an object of class \code{dist}.
#' @export
dissimilarity_data = function(data) {

  # Aggregate all data frames by weekhour
  data_aggregated = dockless::aggregate_by_weekhour(data)

  # Normalize the distance column of each aggregated data frame
  f = function(x) {
    dockless::scale_minmax(x$distance)
  }

  distance_scaled_list = lapply(data_aggregated, f)

  # Store the distance_scaled columns of all data frames in one matrix
  distance_scaled_matrix = do.call(rbind, distance_scaled_list)

  # Create euclidean distance matrix
  stats::dist(distance_scaled_matrix, method = 'euclidean')

}

#' Dissimilarity matrix of a \code{sf} object
#'
#' Creates a dissimilarity matrix based on the adjacency of all polygons in a
#' \code{sf} object with polygon geometry.
#'
#' @param polygons object of class \code{sf} with polygon geometry.
#' @return Returns an object of class \code{dist}.
#' @export
dissimilarity_spatial = function(polygons) {

  # Project the polygons to State Plane California Zone III (EPSG:26943)
  polygons_projected = dockless::project_sf(polygons)

  # Create logical adjacency matrix
  adjacency = as.matrix(
    sf::st_relate(polygons_projected, polygons_projected, pattern = "F***T****")
  )

  # Give a value of 1 to FALSE and 0 to TRUE, to give neighbors..
  # ..a lower dissimilarity value
  adjacency_inverse = 1 - adjacency

  # Convert to a 'dist' object
  stats::as.dist(adjacency_inverse)

}

#' Weighted centroid
#'
#' Calculates the weighted centroid of an object of class \code{sf} with
#' point geometry.
#'
#' @param points object of class \code{sf} with point geometry.
#' @param weights vector specifying the weight of each point.
#' @return Returns an object of class \code{sfc_POINT}.
#' @export
weighted_centroid = function(points, weights) {
  lat = stats::weighted.mean(sf::st_coordinates(points)[,1], weights)
  lon = stats::weighted.mean(sf::st_coordinates(points)[,2], weights)

  sf::st_sfc(sf::st_point(c(lat, lon)), crs = sf::st_crs(points))
}
