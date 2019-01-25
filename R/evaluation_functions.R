#' Forecast error
#'
#' Calculates either the root mean squared error or the mean absolute
#' error of a set of forecasts.
#'
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}.
#' @param type one of 'RMSE' or 'MAE'.
#' @param return one of 'average', 'min', 'max' or 'all'. If 'average',
#' the average error of all \code{dockless_fc} objects in a
#' \code{dockless_fcc} will be returned. If 'min', this will be the
#' minimum error, and if 'max', this will be the maximum error. If 'all'
#' errors for all \code{dockless_fc} objects in a \code{dockless_fcc}
#' are given seperately, in a numeric vector. If \code{x} is an
#' object of class \code{dockless_fc}, 'all' is the only option.
#' @return Returns either a numeric value or a vector of numeric values.
#' @export
error = function(x, type, return) UseMethod("error")


#' @name error
#' @export
error.dockless_fc = function(x, type, return = 'all') {

  if (return == 'all') {

    if (type == 'RMSE') {

      # Calculate root mean squared error
      sqrt(mean((x$error) * (x$error), na.rm = TRUE))

    } else if (type == 'MAE') {

      # Calculate mean absolute error
      mean(abs(x$error), na.rm = TRUE)

    } else {

      # Stop the function
      stop("Type should be either 'RMSE' or 'MAE'")

    }

  } else {

    # Stop the function
    stop("For a dockless_fc object, return can only be set to 'all'")

  }

}


#' @name error
#' @export
error.dockless_fcc = function(x, type, return = 'average') {

  # Calculate error per dockless_fc
  errors = sapply(x, function(x) error(x, type = type))

  if (return == 'average') {

    # Return average
    mean(errors, na.rm = TRUE)

  } else if (return == 'min') {

    # Return minimum
    min(errors, na.rm = TRUE)

  } else if (return == 'max') {

    # Return maximum
    max(errors, na.rm = TRUE)

  } else if (return == 'all') {

    # Return named vector
    stats::setNames(errors, nm = seq(1, length(errors), 1))

  } else {

    # Stop the function
    stop("Return should be either 'average', 'min', 'max' or 'all'")

  }

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

  if (type == 'RMSE') {

    # Convert error into squared error
    x$error = (x$error)^2

    # Aggregate by hour and calculate mse
    agg = stats::aggregate(
      x$error,
      by = list(x$hour),
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    # Take square root
    agg[, 2] = sqrt(agg[, 2])

  } else if (type == 'MAE') {

    # Convert error into absolute error
    x$error = abs(x$error)

    # Aggregate by hour and calculate mae
    agg = stats::aggregate(
      x$error,
      by = list(x$hour),
      FUN = function(x) mean(x, na.rm = TRUE)
    )

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
#' @param x object either of class \code{dockless_fc} or \code{dockless_fcc}.
#' @param type one of 'RMSE' or 'MAE'.
#' @return Returns a vector of numeric values.
#' @export
error_lag = function(x, type) UseMethod("error_lag")


#' @name error_lag
#' @export
error_lag.dockless_fc = function(x, type) {

  if (type == 'RMSE') {

    # Convert error into root squared error
    x$error = sqrt((x$error)^2)

    # Return as named vector
    stats::setNames(x$error, nm = seq(1, nrow(x), 1))

  } else if (type == 'MAE') {

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

#' Forecast error per cluster
#'
#' Calculates either the root mean squared error or the mean absolute
#' error, per cluster.
#'
#' @param x object of class \code{dockless_fcc}.
#' @param clusters vector specifying to which cluster each test point belongs.
#' @param type one of 'RMSE' or 'MAE'.
#' @param return one of 'average', 'min' or 'max'. If 'average',
#' the average error per cluster will be returned. If 'min', this will be
#' the minimum error, and if 'max', this will be the maximum error.
#' @return Returns a vector of numeric values.
#' @export
error_cluster = function(x, clusters, type, return) {

  # Calculate forecast error per dockless_fc
  errors_vec = error(x, type = type, return = 'all')

  # Add cluster information
  errors_df = data.frame(
    error = errors_vec,
    cluster = clusters
  )

  if (return == 'average') {

    # Calculate average error per cluster
    agg = stats::aggregate(
      errors_df$error,
      by = list(errors_df$cluster),
      FUN = function(x) mean(x, na.rm = TRUE)
    )

  } else if (return == 'min') {

    # Calculate minimum error per cluster
    agg = stats::aggregate(
      errors_df$error,
      by = list(errors_df$cluster),
      FUN = function(x) min(x, na.rm = TRUE)
    )

  } else if (return == 'max') {

    # Calculate maximum error per cluster
    agg = stats::aggregate(
      errors_df$error,
      by = list(errors_df$cluster),
      FUN = function(x) max(x, na.rm = TRUE)
    )

  } else {

    # Stop the function
    stop("Return should be either 'average', 'min' or 'max'")

  }

  # Return named vector
  stats::setNames(agg[, 2], nm = agg[, 1])

}


#' Evaluate a \code{dockless_fcc} object
#'
#' Calculates the average, maximum and minimum root mean squared error or
#' mean absolute of a \code{dockless_fcc} object, both for the total area
#' as per cluster.
#'
#' @param x object either of class \code{dockless_fcc}.
#' @param clusters vector specifying to which cluster each test point belongs.
#' @param type one of 'RMSE' or 'MAE'.
#' @return Returns a matrix.
#' @export
evaluate = function(x, clusters, type) {

  # List the options
  metrics = c('average', 'min', 'max')

  # Calculate errors for the total area
  errors_area = mapply(
    function(x, y) error(x, type = type, return = y),
    list(x, x, x),
    metrics
  )

  # Calculate errors per cluster
  errors_cluster = mapply(
    function(x, y) error_cluster(x, clusters = clusters, type = type, return = y),
    list(x, x, x),
    metrics
  )

  # Combine in a matrix
  errors = do.call('rbind', list(errors_area, errors_cluster))
  rownames(errors) = c('total', as.character(seq(1, nrow(errors_cluster), 1)))
  colnames(errors) = metrics

  return(errors)

}
