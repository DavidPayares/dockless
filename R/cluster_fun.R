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
  data_aggregated = aggregate_by_weekhour(data)

  # Normalize the distance column of each aggregated data frame
  f = function(x) {
    scale_minmax(x$distance)
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
  polygons_projected = project_sf(polygons)

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

#' Dendrogram of non-spatial hierarchical clustering of a \code{dockless_dfc} object
#'
#' The non-spatial dendrogram is meant to choose the number of clusters k used in
#' a spatially-constrained hierarchical clustering.
#'
#' @param data object of class \code{dockless_dfc}.
#' @return Returns a plot.
#' @export
choose_k_plot = function(data) {

  # Non-spatial clustering
  cluster = spatial_cluster(data = data, alpha = 0)

  # Plot the dendrogram
  graphics::plot(cluster$hclust)

}

#' Homogeneity plot of spatially constrained hierarchical clustering of
#' a \code{dockless_dfc} object
#'
#' The homogeneity plot is meant to choose the optimal value of the mixing
#' parameter alpha, which sets the strength of the spatial constraint in
#' a spatially-constrained hierarchical clustering.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param polygons object of class \code{sf} with polygon geometry, in which each
#' feature refers to the spatial location of each \code{dockless_df} object in
#' the \code{dockless_dfc}.
#' @param k number of clusters.
#' @param plot logical; defining if the output should be a plot.
#' @param range_alpha the values of alpha to be tested for. Should be a vector
#' of real values between 0 and 1.
#' @return If \code{plot} is \code{TRUE}, it returns a plot. Otherwise, it
#' returns an object of class \code{choicealpha} from the package \code{ClustGeo}.
#' @export
choose_alpha_plot = function(data, polygons, k, plot = TRUE,
                             range_alpha = seq(0,1,0.1)) {

  # Create dissimilarity matrix from the data
  data_dis = dissimilarity_data(data)

  # Create a spatial dissimilarity matrix
  spatial_dis = dissimilarity_spatial(polygons)

  # Plot the homogeneity plot
  choicealpha = ClustGeo::choicealpha(
    data_dis,
    spatial_dis,
    range.alpha = range_alpha,
    K = k,
    graph = FALSE
  )

  # If plot is TRUE (default), return the plot
  if (plot) {
    graphics::plot(choicealpha, norm = TRUE)
  } else {
    return(choicealpha)
  }

}


#' Spatially constrained hierarchical clustering of a \code{dockless_dfc} object
#'
#' Clusters the \code{dockless_df} objects in a \code{dockless_dfc} with
#' spatially constrained hierarchical clustering.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param polygons object of class \code{sf} with polygon geometry, in which each
#' feature refers to the spatial location of each \code{dockless_df} object in
#' the \code{dockless_dfc}.
#' @param k number of clusters.
#' @param alpha value of the mixing parameter alpha, which sets the strength
#' of the spatial constraint.
#' @return Returns an object of class \code{dockless_clust}, which is a list
#' containing two elements: 1 - a vector that specifies for each of the given
#' \code{dockless_df} objects to which cluster it belongs and 2 - object of class
#' \code{hclust} which describes the tree produced by the clustering process.
#' @export
spatial_cluster = function(data, polygons = NULL, k = NULL, alpha = 0) {

  # Create a dissimilarity matrix from the data
  data_dis = dissimilarity_data(data)

  # Create a spatial dissimilarity matrix
  if (is.null(polygons)) {
    spatial_dis = NULL
  } else {
    spatial_dis = dissimilarity_spatial(polygons)
  }

  # Cluster with spatially constrained hierarchical clustering
  hclust = ClustGeo::hclustgeo(
    D0 = data_dis,
    D1 = spatial_dis,
    alpha = alpha
  )

  # Cut the tree based on the provided number of clusters k
  if (is.null(k)) {
    clusters = NULL
  } else {
    clusters = stats::cutree(hclust, k = k)
  }

  # Combine clusters and hclust into a list
  # Return as object of class dockless_clust
  structure(
    list(clusters = clusters, hclust = hclust),
    class = c("dockless_clust", "list")
  )

}


#' Create model points
#'
#' Creates an object of class \code{sf} containing the geographical locations
#' for which the forecasting models will be build. The locations are calculated
#' by taking, per cluster, the centroid of the grid cell centers , weighted by
#' the usage intensity of the grid cell polygons.
#'
#' @param points all grid cell centers as an \code{sf} object with point geometry.
#' @param clusters vector specifying to which cluster each grid cell center belongs.
#' @param weights vector specifying the usage intensity of each grid cell
#' corresponding to a grid center.
#' @return Returns an object of class \code{sf} with point geometry.
#' @export
create_modelpoints = function(points, clusters, weights) {

  # Split the points object by cluster
  points_per_cluster = split(
    x = points,
    f = unclass(points[clusters])[[1]]
  )

  # Calculate weighted centroid per cluster
  # Output as sf data frame instead of only sfc geometry
  f = function(x) {
    geometry = weighted_centroid(
      points = x,
      weights = unclass(x[weights])[[1]]
    )

    sf::st_sf(geometry)
  }

  modelpoints_list = lapply(points_per_cluster, f)

  # Bind together into one sf data frame
  modelpoints_df = do.call(rbind, modelpoints_list)

  # Add cluster information
  modelpoints_df$cluster = unique(unclass(points[clusters])[[1]])

  return(modelpoints_df)

}
