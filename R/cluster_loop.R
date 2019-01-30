#' Square grid over service area
#'
#' Creates a square grid with given cell size over the service area
#' of the dockless bike sharing system.
#'
#' @param area object of class \code{sf} representing the service area.
#' @param ... further arguments passed to \code{st_make_grid}.
#' @return If \code{type} is set to \code{polygons}, it returns an object
#' of class \code{sf} containing the grid cells as square polygons.
#' If \code{type} is set to \code{centers} it returns an object of
#' class \code{sf} containing the grid cell centroids as points.
#' @export
create_grid = function(area, ...) {

  # Project the area
  area_projected = dockless::project_sf(area)

  # Create a grid over the area, with the given cell size
  geometry = sf::st_make_grid(area_projected, ...)
  grid = sf::st_sf(geometry)

  # Clip the grid with the area
  grid$intersects = as.vector(
    sf::st_intersects(grid, area_projected, sparse = FALSE)
  )
  grid_clipped = grid[grid$intersects,]
  grid_clipped$intersects = NULL

  sf::st_transform(grid_clipped, crs = 4326)

}


#' Usage intensity in grid cells
#'
#' Calculates the number of pick-ups in each polygon of an overlaying grid.
#'
#' @param usage pick-ups, as an object of class \code{sf} with point geometry.
#' @param grid grid cells as an object of class \code{sf} with polygon geometry.
#' @return Returns an numeric vector with each element specifying the number
#' of pick-ups in the grid cell with corresponding index.
#' @export
usage_intensity = function(usage, grid) {

  # Project the points to State Plane California Zone III (EPSG:26943)
  points_projected = dockless::project_sf(usage)

  # Project the polygons to State Plane California Zone III (EPSG:26943)
  polygons_projected = dockless::project_sf(grid)

  # Calculate the number of points in each polygon
  f = function(x) {
    nrow(sf::st_intersection(x, points_projected))
  }

  sapply(split(polygons_projected, 1:nrow(polygons_projected)), f)

}

#' Spatially constrained hierarchical clustering of a \code{dockless_dfc} object
#'
#' Clusters the \code{dockless_df} objects in a \code{dockless_dfc} with
#' spatially constrained hierarchical clustering.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param grid grid cells as object of class \code{sf} with polygon geometry, in which
#' each cell refers to the spatial location of each \code{dockless_df} object in
#' the \code{dockless_dfc}.
#' @param K vector of integers specifying which values of k, the number of
#' clusters, should be tested.
#' @param omega vector of values specifying which values of alpha, the mixing
#' parameter, should be tested.
#' @return Returns a list of 2 with one element being a numeric vector of cluster
#' indices that specifies for each of the given \code{dockless_df} objects to which
#' cluster it belongs, and the second element being the geographical outines of each
#' cluster, bundled in an object of class \code{sf} with polygon geometry.
#' @export
spatial_cluster = function(data, grid, K, omega = seq(0, 1, 0.1)) {

  # Create a dissimilarity matrix from the data
  data_dis = dockless::dissimilarity_data(data)

  # Create a spatial dissimilarity matrix
  spatial_dis = dockless::dissimilarity_spatial(grid)

  # Calculate Dunn Index for all k in K
  validation = suppressWarnings(
    clValid::clValid(
      obj = as.matrix(data_dis),
      nClust = K,
      clMethods = 'hierarchical',
      validation = 'internal',
      method = 'ward'
    )
  )

  dunn_indices = validation@measures[2, , ]

  # Choose the optimal value of k
  k_star = K[which.max(dunn_indices)]

  # Spatially constrained hierarchical clusterings for all alpha in omega
  information_criteria = ClustGeo::choicealpha(
    data_dis,
    spatial_dis,
    range.alpha = omega,
    K = k_star,
    graph = FALSE
  )

  # Choose the optimal value of alpha
  alpha_star = omega[which.max(information_criteria$Qnorm[which(information_criteria$Qnorm[, 1] >= 0.9), 2])]

  # Cluster with spatially constrained hierarchical clustering
  sch_clust = ClustGeo::hclustgeo(
    D0 = data_dis,
    D1 = spatial_dis,
    alpha = alpha_star
  )

  # Cut the tree based on the provided number of clusters k
  cluster_indices = stats::cutree(sch_clust, k = k_star)

  # Add cluster information to grid cells
  grid$cluster = cluster_indices

  # Split by cluster
  cells_per_cluster = split(
    x = grid,
    f = grid$cluster
  )

  # Dissolve grid cells per cluster
  cells_dissolved = lapply(
    cells_per_cluster,
    function(x) sf::st_union(x)
  )

  # If a cluster is a multipolygon, split it into seperate polygons
  # Return one sf data frame
  f = function(x) {
    if (methods::is(x, 'sfc_MULTIPOLYGON')) {

      cluster = sf::st_sf(sf::st_cast(x, 'POLYGON'))
      names(cluster)[1]= 'geometry'
      sf::st_geometry(cluster) = 'geometry'

      return(cluster)

    } else {
      cluster = sf::st_sf(x)
      names(cluster)[1]= 'geometry'
      sf::st_geometry(cluster) = 'geometry'

      return(cluster)
    }
  }

  cluster_outlines = do.call('rbind', lapply(cells_dissolved, f))

  # Sort based on Y coordinate of centroid
  cluster_centroids = sf::st_coordinates(
    sf::st_centroid(dockless::project_sf(cluster_outlines))
  )

  cluster_outlines = cluster_outlines[order(cluster_centroids[,"Y"]), ]

  # Add cluster index
  cluster_outlines$cluster = as.factor(c(1:nrow(cluster_outlines)))

  # Update cluster information of grid cells
  grid$cluster = NULL
  grid_updated = sf::st_join(
    dockless::project_sf(grid),
    dockless::project_sf(cluster_outlines),
    join = sf::st_covers
  )
  grid_updated = sf::st_transform(grid_updated, crs = 4326)

  # Retrieve cluster indices
  cluster_indices_updated = grid_updated$cluster

  # Return list of indices and outlines
  list(indices = cluster_indices_updated, outlines = cluster_outlines)

}


#' Create model points
#'
#' Creates an object of class \code{sf} containing the geographical locations
#' for which the forecasting models will be build. The locations are calculated
#' by taking, per cluster, the centroid of the grid cell centers , weighted by
#' the usage intensity of the grid cell polygons.
#'
#' @param centroids all grid cell centroids as an \code{sf} object with point geometry,
#' containing at least the attributes \code{cluster}, specifying to which cluster each
#' grid cell centroid belongs, and \code{intensity}, specifying the number of pick-ups
#' in the grid cell that corresponds to the grid cell centroid.
#' @return Returns an object of class \code{sf} with point geometry.
#' @export
create_modelpoints = function(centroids) {

  # Split the centroids object by cluster
  centroids_per_cluster = split(
    x = centroids,
    f = centroids$cluster
  )

  # Calculate weighted centroid per cluster
  # Output as sf data frame instead of only sfc geometry
  f = function(x) {
    geometry = dockless::weighted_centroid(
      points = x,
      weights = x$intensity
    )

    sf::st_sf(geometry)
  }

  do.call('rbind', lapply(centroids_per_cluster, f))

}
