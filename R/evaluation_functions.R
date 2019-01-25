#' Forecast error
#'
#' Calculates either the root mean squared error or the mean absolute
#' error of a set of forecasts.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}.
#' @param type one of 'RMSE' or 'MAE'.
#' @return Returns a numeric value.
#' @export
error = function(x, type) UseMethod("error")


#' @name error
#' @export
error.dockless_fc = function(x, type) {

  if (type = 'RMSE') {

    # Calculate root mean squared error
    sqrt(mean((x$error) * (x$error), na.rm = TRUE))

  } else if (type = 'MAE') {

    # Calculate mean absolute error
    mean(abs(x$error), na.rm = TRUE)

  } else {

    # Stop the function
    stop("Type should be either 'RMSE' or 'MAE'")

  }

}


#' @name error
#' @export
error.dockless_fcc = function(x, type) {

  # Calculate error per dockless_fc
  errors = sapply(x, function(x) error(x, type = type))

  # Average
  mean(errors, na.rm = TRUE)

}


#' Forecast error per hour of the day
#'
#' Calculates eiter the root mean squared error or the mean absolute
#' error, per hour of the day.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}..
#' @param type one of 'RMSE' or 'MAE'.
#' @return Returns a vector of numeric values.
#' @export
error_hourofday = function(x, type) UseMethod("error_hourofday")


#' @name error_hourofday
#' @export
error_hourofday.dockless_fc = function(x, type) {

  # Add a column specifying the hour of the day
  x$hour = lubridate::hour(x$time)

  if (type = 'RMSE') {

    # Convert error into squared error
    x$error = (x$error)^2

    # Aggregate by hour and calculate mse
    agg = stats::aggregate(x$error, by = list(x$hour), FUN = mean)

    # Take square root
    agg[, 2] = sqrt(agg[, 2])

  } else if (type = 'MAE') {

    # Convert error into absolute error
    x$error = abs(x$error)

    # Aggregate by hour and calculate mae
    agg = stats::aggregate(x$error, by = list(x$hour), FUN = mean)

  } else {

    # Stop the function
    stop("Type should be either 'RMSE' or 'MAE'")

  }

  # Return the errors as named vector
  stats::setNames(agg[,2], nm = agg[,1])

}

#' @name error_hourofday
#' @export
error_hourofday.dockless_fcc = function(x, type) {

  # Calculate error per hour of day for each dockless_fc
  errors = sapply(x, function(x) error_hourofday(x, type = type))

  # Average per hour of day
  rowMeans(errors, na.rm = TRUE)

}


#' Forecast error per forecast lag
#'
#' Calculates either the root mean squared error or the mean absolute
#' error, per forecast lag.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}..
#' @param type one of 'RMSE' or 'MAE'.
#' @return Returns a vector of numeric values.
#' @export
error_lag = function(x, type) UseMethod("error_lag")


#' @name error_lag
#' @export
error_lag.dockless_fc = function(x, type) {

  if (type = 'RMSE') {

    # Convert error into squared error
    x$error = (x$error)^2

    # Take square root
    agg[, 2] = sqrt(agg[, 2])

    # Return as named vector
    stats::setNames(x$error, nm = seq(1, nrow(x), 1))

  } else if (type = 'MAE') {

    # Convert error into absolute error
    x$error = abs(x$error)

    # Return as named vector
    stats::setNames(x$error, nm = seq(1, nrow(x), 1))

  } else {

    # Stop the function
    stop("Type should be either 'RMSE' or 'MAE'")

  }

}

#' @name error_lag
#' @export
error_lag.dockless_fcc = function(x, type) {

  # Calculate error per forecast lag for each dockless_fc
  errors = sapply(x, function(x) error_lag(x, type = type))

  # Average per hour of day
  rowMeans(errors, na.rm = TRUE)

}
