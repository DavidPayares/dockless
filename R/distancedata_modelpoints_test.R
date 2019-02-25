#' Distance data for the model points during the test period
#'
#' The distance data for the model points, during the
#' test period. Used to create the plot in section 5.4.1.
#'
#' @format An object of class dockless_dfc, containing 4 data frames of
#' subclass dockless_df, with 2117 rows.
#' \describe{
#'   \item{bike_id}{unique identification number of the bike}
#'   \item{time}{timestamp of the observation}
#'   \item{distance}{distance to the neareast available bike, in meters}
#' }
"distancedata_modelpoints_test"
