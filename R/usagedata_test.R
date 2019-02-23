#' Usage data for the test period
#'
#' The usage data, i.e. all pick-ups, during the test week.
#'
#' @format An object of class sf with POINT geometry, with 20737 features and 4 columns.
#' \describe{
#'   \item{bike_id}{unique identification number of the bike}
#'   \item{time}{timestamp of the pick-up}
#'   \item{battery_level}{battery level at time of pick-up, in %}
#'   \item{location}{location of the pick-up}
#' }
"usagedata_test"
