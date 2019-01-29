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
#' @param return one of 'vector' or 'polygons'.
#' @return If \code{return} is set to 'vector', the output is a numeric vector of cluster
#' indices that specifies for each of the given \code{dockless_df} objects to which
#' cluster it belongs. If \code{return} is set to 'polygons', the output contains the
#' geographical outines of each cluster, bundled in an object of class \code{sf} with
#' polygon geometry.
#' @export
spatial_cluster = function(data, grid, K, omega = seq(0, 1, 0.1),
                           return = 'polygons') {

  # Create a dissimilarity matrix from the data
  data_dis = dockless::dissimilarity_data(data)

  # Create a spatial dissimilarity matrix
  spatial_dis = dockless::dissimilarity_spatial(grid)

  # Calculate Dunn Index for all k in K
  f = function(x) {

    h_clust = ClustGeo::hclustgeo(
      D0 = data_dis,
      alpha = 0
    )

    clusters = stats::cutree(h_clust, k = x)

    clValid::dunn(
      distance = data_dis,
      clusters = clusters
    )

  }

  dunn_indices = sapply(K, f)

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

  if (return == 'vector') {

    return(cluster_indices)

  } else {

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
      function(x) sf::st_sf(sf::st_union(x))
    )

    # Bind together
    cluster_outlines = do.call('rbind', cells_dissolved)

    return(cluster_outlines)

  }

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
