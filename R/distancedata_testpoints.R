#' Distance data for the test points
#'
#' The distance data for the test points, both for decomposition, as for
#' the validation of the forecast results.
#'
#' @format An object of class dockless_dfc, containing 500 data frames of
#' subclass dockless_df, with 2117 rows.
#' \describe{
#'   \item{bike_id}{unique identification number of the bike}
#'   \item{time}{timestamp of the observation}
#'   \item{distance}{distance to the neareast available bike, in meters}
#' }
"distancedata_testpoints"
