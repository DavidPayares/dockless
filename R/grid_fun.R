#' Square grid over service area
#'
#' Creates a square grid with given cell size over the service area 
#' of the dockless bike sharing system.
#'
#' @param area object of class \code{sf} representing the service area.
#' @param type either \code{polygons} or \code{centers}.
#' @param ... further arguments passed to \code{st_make_grid}.
#' @return If \code{type} is set to \code{polygons}, it returns an object 
#' of class \code{sf} containing the grid cells as square polygons.
#' If \code{type} is set to \code{centers} it returns an object of 
#' class \code{sf} containing the grid cell centroids as points.
#' @export
make_grid = function(area, type, ...) {
  
  # Project the area
  area_projected = project_sf(area)
  
  # Create a grid over the area, with the given cell size
  geometry = sf::st_make_grid(area_projected, ...)
  grid = sf::st_sf(geometry)
  
  # Clip the grid with the area
  grid$intersects = as.vector(
    sf::st_intersects(grid, area_projected, sparse = FALSE)
  )
  grid_clipped = grid[grid$intersects,]
  grid_clipped$intersects = NULL
  
  # If type is 'centers', calculate grid centers and then transform back to WGS84
  # If type is 'cells', transform to WGS84 directly
  if (type == 'centers') {
    # Calculate centroids of the grid cells
    centers = suppressWarnings(
      sf::st_centroid(grid_clipped) 
    )
    sf::st_transform(centers, crs = 4326)
  } else if (type == 'cells') {
    sf::st_transform(grid_clipped, crs = 4326)
  } else {
    stop("Variable 'type' should be either 'cells' or 'centers'")
  }
  
}