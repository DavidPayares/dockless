#' Usage intensity in grid cells
#'
#' Calculates the number of pick-ups in each polygon of an overlaying grid.
#'
#' @param points individual pick-ups, as an object of class \code{sf} with point geometry.
#' @param polygons grid, as an object of class \code{sf} with polygon geometry.
#' @return Returns an numeric vector with each element specifying the number 
#' of pick-ups in the grid cell with corresponding index.
#' @export
usage_intensity = function(points, polygons) {
  
  # Project the points to State Plane California Zone III (EPSG:26943)
  points_projected = project_sf(points)
  
  # Project the polygons to State Plane California Zone III (EPSG:26943)
  polygons_projected = project_sf(polygons)
  
  # Calculate the number of points in each polygon
  f = function(x) {
    nrow(sf::st_intersection(x, points_projected))
  }
  
  sapply(split(polygons, 1:nrow(polygons)), f)
  
}