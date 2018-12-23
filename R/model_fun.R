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


#' Forecasting model for \code{dockless_df} object
#'
#' Fits a forecasting model on a \code{dockless_df} time series. If the time
#' series is non-seasonal, an ARIMA model will be fitted on the log transformed
#' data. If the time series is seasonal, the log transformed data will be
#' decomposed with STL decomposition, and an ARIMA model will be fitted on the
#' seasonally adjusted data.
#'
#' @param data object of class \code{dockless_df}.
#' @param auto_seasonality logical. If \code{TRUE}, the seasonal periods of the
#' time series will be automatically determined with time series cross-validation.
#' @param seasons if \code{auto_seasonality} is set to \code{TRUE}: a list of
#' different seasonal period lengths - or, in the case of multiple seasonality,
#' combinations of seasonal periods lenghts - in time lags to be tested for in
#' the time series cross-validation. No seasonality should be represented by
#' \code{NULL}. If \code{auto_seasonality} is set to \code{FALSE}: the seasonal
#' period length - or, in the case of multiple seasonality, combination of
#' seasonal period lenghts - in time lags. No seasonality should be represented
#' by \code{NULL}.
#' @param window rolling window for time series cross validation, in weeks.
#' Ignored when \code{auto_seasonality} is set to \code{FALSE}.
#' @return Returns an object of class \code{ARIMA} for non-seasonal data and an
#' object of class \code{stlm} for seasonal data. Both classes come from the
#' \code{forecast} package.
#' @export
build_single_model = function(data, auto_seasonality = TRUE,
                              seasons = list(NULL, 96, 672, c(96, 672)),
                              window = 2) {

  # If auto_seasonality is TRUE, automatically define the seasonal periods
  if (auto_seasonality) {
    seasons = test_seasonality(
      data = data,
      seasons = seasons,
      window = window
    )
  }

  # If seasons is NULL, fit an ARIMA model directly
  # If seasons is not NULL, decompose with STL and then..
  # ..fit an ARIMA model to the trend + remainder
  if (is.null(seasons)) {
    # Convert the distance column of the data into a ts object
    distances = stats::ts(data$distance)

    # Define the model function for the non-seasonal part of the data
    model_function = function(x) {
      forecast::auto.arima(
        y = x,
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE,
        lambda = 0,
        biasadj = TRUE
      )
    }

    # Fit ARIMA model to that data
    model = model_function(distances)
  } else {
    # Convert the distance column of the data into a ts object
    distances = forecast::msts(
      data$distance,
      seasonal.periods = seasons
    )

    # Define the model function for the non-seasonal part of the data
    model_function = function(x) {
      forecast::auto.arima(
        y = x,
        seasonal = FALSE,
        stepwise = FALSE,
        approximation = FALSE
      )
    }

    # Decompose and fit ARIMA model to trend+remainder
    model = forecast::stlm(
      y = distances,
      s.window = 13,
      robust = TRUE,
      lambda = 0,
      biasadj = TRUE,
      modelfunction = model_function
    )
  }

  return(model)

}

#' Automatic seasonality testing
#'
#' Finds the seasonal period - or, in the case of multiple seasonality,
#' combination of seasonal periods - of a \code{dockless_df} time series that
#' leads to the highest forecast accuracy. Does this with time series
#' cross-validation.
#'
#' @param data object of class \code{dockless_df}.
#' @param seasons a list of different seasonal period lengths - or, in the case
#' of multiple seasonality, combinations of seasonal period lenghts - in time
#' lags to be tested for. No seasonality should be represented by \code{NULL}.
#' @param window rolling window for time series cross validation, in weeks.
#' @return Returns a value specifying the length in time lags of the seasonal
#' period. In the case of multiple seasonality, it returns a vector. In the case
#' of no seasonality, it returns \code{NULL}.
#' @export
test_seasonality = function(data, seasons, window = 2) {

  # Function to find the error of one of the provided (set of) seasonal periods
  error_seasons = function(data, seasons, window) {

    # Retrieve the last timestamp of the complete data frame
    last_timestamp = data[nrow(data), 'time']

    # Define the 'last timestamp' of the first period for model building
    # This is always one month after the start of the data
    t = (data[1, 'time']) + ((60 * 60 * 24 * 7 * 4))

    # List the 'last timestamps' of all periods for model building
    period_ends = c()

    while(t < (last_timestamp - (60 * 60 * 24 * 7 * window))) {
      period_ends[length(period_ends) + 1] = t
      t = t + (60 * 60 * 24 * 7 * window)
    }

    # Build models and forecast (window) weeks ahead for each period
    f = function(x) {
      # Select model data
      model_period = data[data$time <= x,]

      # Build model
      model = build_single_model(
        data = model_period,
        auto_seasonality = FALSE,
        seasons = seasons
      )

      # Select forecast period
      forecast_period = data[data$time <= x + (60 * 60 * 24 * 7),]

      # Forecast
      forecast = forecast_lastweek(
        data = forecast_period,
        model = model,
        weeks_of_data = 8
      )

      # Return mae of forecast
      mae(do.call(rbind, forecast))

    }

    errors = sapply(period_ends, f)

    mean(errors, na.rm = TRUE)

  }

  # Calculate the forecast errors for all given (sets of) seasonal periods
  f = function(x) {
    error_seasons(
      data = data,
      seasons = x,
      window = window
    )
  }

  all_errors = sapply(seasons, f)

  # Return the (set of) seasonal period(s) with the lowest forecast error
  seasons[[which.min(all_errors)]]

}

#' Forecasting models for \code{dockless_dfc} object
#'
#' Fits a forecasting model on each \code{dockless_df} time series in a
#' \code{dockless_dfc}. For non-seasonal time series, an ARIMA model will
#' be fitted on the log transformed data. For seasonal time series, the log
#' transformed data will be decomposed with STL decomposition, and an ARIMA
#' model will be fitted on the seasonally adjusted data.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param weeks_of_data how many weeks of historical data to use for model building.
#' @param last_day character specifying the last day of the model building period.
#' Should be in the format 'YYYY-mm-dd'.
#' @param auto_seasonality logical. If \code{TRUE}, the seasonal period of each
#' time series will be automatically determined with time series cross-validation.
#' @param seasons if \code{auto_seasonality} is set to \code{TRUE}: a list of
#' different seasonal period lengths - or, in the case of multiple seasonality,
#' combinations of seasonal period lenghts - in time lags to be tested for in
#' the time series cross-validation. No seasonality should be represented by
#' \code{NULL}. If \code{auto_seasonality} is set to \code{FALSE}: a list of
#' seasonal period lengths - or, in the case of multiple seasonality,
#' combinations of seasonal period lenghts - in time lags to be used for each
#' of the time series. No seasonality should be represented by \code{NULL}. The
#' list should be of the same length as the number of time series in the
#' \code{dockless_dfc} object.
#' @param window rolling window for time series cross validation, in weeks.
#' Ignored when \code{auto_seasonality} is set to \code{FALSE}.
#' @return Returns a list of models, one for each \code{dockless_df} time series.
#' Each model is an object of class \code{ARIMA} for non-seasonal data and an
#' object of class \code{stlm} for seasonal data. Both classes come from the
#' \code{forecast} package.
#' @export
build_models = function(data, weeks_of_data = 12, last_day = NULL,
                        auto_seasonality = TRUE,
                        seasons = list(NULL, 96, 672, c(96, 672)),
                        window = 2) {

  # Define the last timestamp of the data that needs to be used to build the model
  if (is.null(last_day)) {
    last_timestamp = (data[[1]])[nrow(data[[1]]), 'time']
  } else {
    last_timestamp = as.POSIXct(
      paste(as.character(last_day), '23:45:00'),
      format = '%Y-%m-%d %H:%M:%S',
      tz = 'America/Los_Angeles'
    )
  }

  # Define the first timestamp of the data that needs to be used to build the model
  first_timestamp = last_timestamp - (60 * 60 * 24 * 7 * weeks_of_data)

  # Select rows from the data that lie within the model building period
  f = function(x) {
    x[x$time > first_timestamp & x$time <= last_timestamp,]
  }

  selected_data = lapply(data, f)

  # Run the build single model function for all the given data frames
  # Test automatically for the best seasonality
  if (auto_seasonality) {
    f = function(x) {
      build_single_model(
        data = x,
        auto_seasonality = TRUE,
        seasons = seasons,
        window = window
      )
    }

    all_models = lapply(selected_data, f)
  } else {
    g = function(x, y) {
      build_single_model(
        data = x,
        auto_seasonality = FALSE,
        seasons = y,
        window = window
      )
    }

    all_models = mapply(selected_data, seasons, g)
  }

  return(all_models)

}
