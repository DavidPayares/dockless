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
#' @return Returns an object of class \code{ARIMA} for non-seasonal data and an
#' object of class \code{stlm} for seasonal data. Both classes come from the
#' \code{forecast} package.
#' @export
build_single_model = function(data, auto_seasonality = TRUE,
                              seasons = list(NULL, 96, 672, c(96, 672))) {

  # If auto_seasonality is TRUE, automatically define the seasonal periods
  if (auto_seasonality) {

    seasons = dockless::test_seasonality(
      data = data,
      seasons = seasons
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

    # Convert the distance column of the data into a msts object
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
#' @return Returns a value specifying the length in time lags of the seasonal
#' period. In the case of multiple seasonality, it returns a vector. In the case
#' of no seasonality, it returns \code{NULL}.
#' @export
test_seasonality = function(data, seasons) {

  # Retrieve the last timestamp of the complete data frame
  last_timestamp = data[nrow(data), 'time']

  # Define the last timestamp of the first period for model building
  # This is always two weeks after the start of the data
  t = (data[1, 'time']) + ((60 * 60 * 24 * 7 * 2))

  # List the last timestamps of all model building periods
  period_ends = c()

  while(t < (last_timestamp - (60 * 60 * 24 * 7))) {

    period_ends[length(period_ends) + 1] = t
    t = t + (60 * 60 * 24 * 7)

  }

  # Function to calculate error of forecast with one of the given seasonality options
  error_single_season = function(season) {

    # Build model and forecast 1 week ahead
    f = function(x) {
      # Select model data
      model_period = data[data$time <= x,]

      # Build model
      model = dockless::build_single_model(
        data = model_period,
        auto_seasonality = FALSE,
        seasons = season
      )

      # Select forecast data
      forecast_period = data[data$time <= x + (60 * 60 * 24 * 7),]

      # Forecast
      forecast = dockless::forecast_lastweek(
        data = forecast_period,
        method = 'DBAFS',
        model = model
      )

      # Return RMSE of forecast
      dockless::error(forecast, type = 'RMSE', return = 'all')

    }

    errors = sapply(period_ends, f)

    mean(errors, na.rm = TRUE)

  }

  # Calculate the forecast errors for all given (sets of) seasonal periods
  f = function(x) {
    error_single_season(
      season = x
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
#' @return Returns a list of models, one for each \code{dockless_df} time series.
#' Each model is an object of class \code{ARIMA} for non-seasonal data and an
#' object of class \code{stlm} for seasonal data. Both classes come from the
#' \code{forecast} package.
#' @export
build_models = function(data, auto_seasonality = TRUE,
                        seasons = list(NULL, 96, 672, c(96, 672))) {

  # Run the build single model function for all the given data frames
  # Test automatically for the best seasonality
  if (auto_seasonality) {

    f = function(x) {
      build_single_model(
        data = x,
        auto_seasonality = TRUE,
        seasons = seasons
      )
    }

    all_models = lapply(data, f)

  } else {

    g = function(x, y) {
      build_single_model(
        data = x,
        auto_seasonality = FALSE,
        seasons = y
      )
    }

    all_models = mapply(data, seasons, g)
  }

  return(all_models)

}
