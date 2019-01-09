#' Mean absolute error
#'
#' Calculates the mean absolute error of one or more forecasts.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}.
#' @return Returns a numeric value.
#' @export
mae = function(x) UseMethod("mae")


#' @name mae
#' @export
mae.dockless_fc = function(x) {

  # Calculate mean absolute error
  mean(abs(x$error), na.rm = TRUE)

}


#' @name mae
#' @export
mae.dockless_fcc = function(x) {

  # Calculate mae per dockless_fc
  errors = sapply(x, function(x) mae(x))

  # Average
  mean(errors, na.rm = TRUE)

}


#' Mean absolute error per hour of the day
#'
#' Calculates the mean absolute error per hour of the day.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}..
#' @return Returns a vector of numeric values.
#' @export
mae_hourofday = function(x) UseMethod("mae_hourofday")


#' @name mae_hourofday
#' @export
mae_hourofday.dockless_fc = function(x) {

  # Convert error into absolute error
  x$error = abs(x$error)

  # Add a column specifying the hour of the day
  x$hour = lubridate::hour(x$time)

  # Aggregate by hour and calculate mae
  agg = stats::aggregate(x$error, by = list(x$hour), FUN = mean)

  # Return the mae's as named vector
  stats::setNames(agg[,2], nm = agg[,1])

}

#' @name mae_hourofday
#' @export
mae_hourofday.dockless_fcc = function(x) {

  # Calculate mae per hour of day for each dockless_fc
  errors = sapply(x, function(x) mae_hourofday(x))

  # Average per hour of day
  rowMeans(errors, na.rm = TRUE)

}


#' Mean absolute error per forecast lag
#'
#' Calculates the mean absolute error per forecast lag.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}..
#' @return Returns a vector of numeric values.
#' @export
mae_lag = function(x) UseMethod("mae_lag")


#' @name mae_lag
#' @export
mae_lag.dockless_fc = function(x) {

  # Convert error into absolute error
  x$error = abs(x$error)

  # Return as named vector
  stats::setNames(x$error, nm = seq(1, nrow(x), 1))

}

#' @name mae_lag
#' @export
mae_lag.dockless_fcc = function(x) {

  # Calculate mae per forecast lag for each dockless_fc
  errors = sapply(x, function(x) mae_lag(x))

  # Average per hour of day
  rowMeans(errors, na.rm = TRUE)

}
