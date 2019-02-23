#' Distance data for the grid centroids
#'
#' The distance data for the centroids of the grid cells, during the
#' training period. Used for clustering.
#'
#' @format An object of class dockless_dfc, containing 249 data frames of
#' subclass dockless_df, with 2688 rows.
#' \describe{
#'   \item{bike_id}{unique identification number of the bike}
#'   \item{time}{timestamp of the observation}
#'   \item{distance}{distance to the neareast available bike, in meters}
#' }
"distancedata_centroids"
