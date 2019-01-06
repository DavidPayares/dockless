#' Forecast last day of a \code{dockless_df} object
#'
#' Forecasts the last day of a \code{dockless_df} time series, using a model
#' generated with either \link{build_single_model} or \link{build_models}.
#'
#' @param data object of class \code{dockless_df}.
#' @param model forecasting model. Should be either of class \code{ARIMA}, for
#' non-seasonal data, or class \code{stlm}, for seasonal data.
#' @param weeks_of_data how many weeks of historical data to use for forecasting.
#' If set to \code{NULL}, all available data is used.
#' @return Returns an object of class \code{dockless_fc_df}, which is a data
#' frame containing the observed values, forecasted values and corresponding
#' prediction intervals.
#' @export
forecast_lastday = function(data, model, weeks_of_data = NULL) {

  # Filter data when weeks_of_data is specified
  if (!is.null(weeks_of_data)) {
    period_end = data[nrow(data), 'time']
    period_start = period_end - (60 * 60 * 24 * 7 * weeks_of_data)
    data = data[data$time > period_start,]
  }

  # Split the data in train and test set
  # The model from the model object will be fitted to the train set
  # The test set will be forecasted
  sample = c(1:(nrow(data)-96))
  train = data[sample,]
  test = data[-sample,]

  if (methods::is(model, 'ARIMA')) {
    # Convert to ts object
    train_ts = stats::ts(train$distance)

    # Fit the ARIMA model from the model object
    train_model = forecast::Arima(train_ts, model = model)

  } else if (methods::is(model, 'stlm')) {
    # Convert to msts object
    train_msts = forecast::msts(
      train$distance,
      seasonal.periods = attr(model$stl, 'seasonal.periods')
    )

    # Decompose with STL and fit the ARIMA model from the model object to trend+remainder
    train_model = forecast::stlm(train_msts, robust = TRUE, model = model)

  } else {
    # Stop the function
    stop("The model must be of class 'ARIMA' or class 'stlm'")
  }

  # Forecast
  forecast = forecast::forecast(train_model, h = 96)

  # Convert to data frame
  forecast = as.data.frame(forecast)

  # Add the corresponding time stamps to the forecast values
  forecast$time = test$time

  # Add the real data to the forecast values
  forecast$observation = test$distance

  # Set column names
  names(forecast) = c(
    'forecast',
    'lower80',
    'upper80',
    'lower95',
    'upper95',
    'time',
    'observation'
  )

  # Change column order
  forecast = forecast[c(6, 7, 1, 2, 3, 4, 5)]

  structure(forecast, class = c("dockless_fc_df", "data.frame"))

}


#' Forecast last week of a \code{dockless_df} object
#'
#' Forecasts the last week of a \code{dockless_df} time series, using a model
#' generated with either \link{build_single_model} or \link{build_models}.
#' Each day will be forecasted seperately, with a period of data of given length.
#' The period of data to forecast with will always end at the start of the day
#' that is forecasted.
#'
#' @param data object of class \code{dockless_df}.
#' @param model forecasting model. Should be either of class \code{ARIMA}, for
#' non-seasonal data, or class \code{stlm}, for seasonal data.
#' @param weeks_of_data how many weeks of historical data to use for forecasting.
#' If set to \code{NULL}, all available data is used.
#' @return Returns a list containing seven  - one for each day - objects of
#' class \code{dockless_fc_df}, which is a data frame containing the observed
#' values, forecasted values and corresponding prediction intervals of a one-day
#' forecast for one single time series.
#' @export
forecast_lastweek = function(data, model, weeks_of_data = NULL) {

  # Time lags for each day
  daylags = rev(seq(0, 6*96, 96))

  # Apply the forecast_lastday function to each of the days
  f = function(x) {
    forecast_lastday(
      data = data[1:(nrow(data)-x),],
      model = model,
      weeks_of_data = weeks_of_data
    )
  }

  lapply(daylags, f)

}


#' Forecast a \code{dockless_dfc} object for a given period
#'
#' Forecasts a given number of weeks for all \code{dockless_df} time series in
#' a \code{dockless_dfc}, using a set of models generated with \link{build_models}.
#' Each day will be forecasted seperately, with a period of data of given length.
#' The period of data to forecast with will always end at the start of the day
#' that is forecasted. Which of the models to use for each time series depends
#' on the cluster in which the corresponding forecast point is located.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param clusters vector specifying to which cluster each \code{dockless_df}
#' time series belongs.
#' @param models list of forecasting models, with each model corresponding to one
#' of the clusters. Models Should be either of class \code{ARIMA}, for non-seasonal
#' data, or class \code{stlm}, for seasonal data.
#' @param length length in weeks of the forecasting period.
#' @param last_day character specifying the last day of the forecasting period.
#' Should be in the format 'YYYY-mm-dd'.
#' @param weeks_of_data how many weeks of historical data to use for forecasting.
#' If set to \code{NULL}, all available data is used.
#' @return Returns an object of class \code{dockless_fc_dfc}, which is a list
#' containing several  - one for each week - matrices. Each matrix consists of
#' seven rows - one for each day - and a number of columns equal to the number
#' of forecasted \code{dockless_df} time series. Each element in such a matrix
#' is an object of class \code{dockless_fc_df}, which is a data frame containing
#' the observed values, forecasted values and corresponding prediction intervals
#' of a one-day forecast for one single time series.
#' @export
forecast_period = function(data, clusters, models, length,
                           last_day, weeks_of_data = 8) {

  # Convert last day to a time lag
  end_time = as.POSIXct(
    paste(last_day, '23:45:00'),
    format = '%Y-%m-%d %H:%M:%S',
    tz = 'America/Los_Angeles'
  )

  # Create to a vector from 1 to length
  weeks_back = seq(0, (length-1), 1)

  # Create a vector of 'last days', with one 'last day' for each week
  end_times = lapply(weeks_back, function(x) end_time - (60 * 60 * 24 * 7 * x))

  # Function to forecast a single week
  forecast_singleweek = function(data, clusters, models,
                                 last_timestamp, weeks_of_data) {

    # Select rows from the data that lie before the end of the given last timestamp
    f = function(x) {
      x[x$time <= last_timestamp,]
    }

    selected_data = lapply(data, f)

    # Forecast the last week of the forecasting period for all forecasting points
    g = function(x, y) {
      model = models[[y]]

      forecast_lastweek(
        data = x,
        model = model,
        weeks_of_data = weeks_of_data
      )
    }

    mapply(g, selected_data, clusters)

  }

  # Depending on the length of the last_day argument, forecast multiple weeks
  h = function(x) {
    forecast_singleweek(
      data = data,
      clusters = clusters,
      models = models,
      last_timestamp = x,
      weeks_of_data = weeks_of_data
    )
  }

  all_forecasts = lapply(rev(end_times), h)

  # Return as object of class dockless_fc_dfc
  structure(all_forecasts, class = c("dockless_fc_dfc", "list"))

}
