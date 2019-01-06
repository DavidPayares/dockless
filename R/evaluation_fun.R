#' Mean absolute error
#'
#' Calculates the mean absolute error of one or more forecasts.
#'
#' @param x object either of class \code{dockless_fc_df} or \code{dockless_fc_dfc}.
#' @return Returns a numeric value.
#' @export
mae = function(x) UseMethod("mae")


#' @name mae
#' @export
mae.dockless_fc_df = function(x) {

  # Calculate error
  error = x$observation - x$forecast

  # Calculate mean absolute error
  mean(abs(error), na.rm = TRUE)
}


#' @name mae
#' @export
mae.dockless_fc_dfc = function(x) {

  # Calculate average mae per matrix
  f = function(x) {
    # Calculate mae's for each unique (point, day) combination in a matrix
    maes = apply(x, (1:2), function(y) mae(y[[1]]))

    # Average
    mean(maes, na.rm = TRUE)
  }

  mae_matrix = sapply(x, f)

  # Average over all matrices
  mean(mae_matrix, na.rm = TRUE)

}


#' Mean absolute error per forecasting point
#'
#' Calculates the mean absolute error of each of the forecasted point in a
#' \code{dockless_fc_dfc} object.
#'
#' @param x object of class \code{dockless_fc_dfc}.
#' @return Returns a vector of numeric values.
#' @export
mae_points = function(x) {

  # Calculate average mae per point per matrix
  f = function(x) {
    # Calculate mae's for each unique (point, day) combination in a matrix
    maes = apply(x, (1:2), function(y) mae(y[[1]]))

    # Average per column
    apply(maes, 2, function(x) mean(x, na.rm = TRUE))
  }

  mae_matrix = lapply(x, f)

  # Bind together
  mae_matrices = do.call(rbind, mae_matrix)

  # Calculate mae per point for all periods together
  apply(mae_matrices, 2, function(x) mean(x, na.rm = TRUE))

}


#' Mean absolute error per time of the week
#'
#' Calculates the mean absolute error of each timestamp in a
#' \code{dockless_fc_dfc} object.
#'
#' @param x object of class \code{dockless_fc_dfc}.
#' @return Returns a vector of numeric values.
#' @export
mae_timeofday = function(x) {

  # Calculate average mae per time-of-day per matrix
  f = function(x) {
    maes = c()
    for(i in c(1:96)) {
      g = function(y) mae((y[[1]])[i,])
      maes[i] = mean(apply(x, c(1,2), g))
    }
    return(maes)
  }

  mae_matrix = lapply(x, f)

  # Bind together
  mae_matrices = do.call(rbind, mae_matrix)

  # Calculate mae per point for all periods together
  apply(mae_matrices, 2, function(x) mean(x, na.rm = TRUE))

}


#' Mean absolute error per day of the week
#'
#' Calculates the mean absolute error of each of day-of-the-weeks in a
#' \code{dockless_fc_dfc} object.
#'
#' @param x object of class \code{dockless_fc_dfc}.
#' @return Returns a vector of numeric values.
#' @export
mae_dayofweek = function(x) {

  # Calculate average mae per day-of-week per matrix
  f = function(x) {
    # Calculate mae's for each unique (point, day) combination in a matrix
    maes = apply(x, (1:2), function(y) mae(y[[1]]))

    # Average per row
    apply(maes, 1, function(x) mean(x, na.rm = TRUE))
  }

  mae_matrix = lapply(x, f)

  # Bind together
  mae_matrices = do.call(rbind, mae_matrix)

  # Calculate mae per day-of-week for all matrices together
  apply(mae_matrices, 2, function(x) mean(x, na.rm = TRUE))

}


#' Evaluate a \code{dockless_fc_dfc} object
#'
#' Calculates the mean absolute error of a \code{dockless_fc_dfc} object, the
#' mean absolute error of each of the forecasted points in that object and the
#' mean absolute error of each day-of-the-week in that object.
#'
#' @param x object of class \code{dockless_fc_dfc}.
#' @return Returns a list.
#' @export
forecast_evaluation = function(x) {

  list(
    total = mae(x),
    per_point = mae_points(x),
    per_dayofweek = mae_dayofweek(x)
  )

}
