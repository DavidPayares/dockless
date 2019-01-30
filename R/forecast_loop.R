#' Create test points
#'
#' Creates an object of class \code{sf} containing the geographical locations
#' and timestamps of the test points. All location-timestamp combinations are
#' sample from the usage data of the test period.
#'
#' @param data the usage data of the test period as object of class \code{sf}
#' with point geometry.
#' @param area the system area of the bike sharing system as object of class
#' \code{sf}, with polygon geometry.
#' @param clusters the geographical outlines of the clusters as object of class
#' \code{sf}, with polygon geometry.
#' @param n the desired number of test points.
#' @return Returns an object of class \code{sf}, with point geometry.
#' @export
create_testpoints = function(data, area, clusters, n) {

  # Project inputs
  data_projected = dockless::project_sf(data)
  area_projected = dockless::project_sf(area)
  clusters_projected = dockless::project_sf(clusters)

  # Clip the data by the system area
  data_projected$in_area = as.vector(
    sf::st_intersects(
      data_projected,
      area_projected,
      sparse = FALSE
    )
  )

  data_clipped = data_projected[data_projected$in_area, ]

  # Sample n location-time combinations from the clipped data
  sample_indices = sample(x = c(1:nrow(data_clipped)), size = n)
  data_sampled = data_clipped[sample_indices, ]

  # Add cluster information to the data
  data_joined = sf::st_join(data_sampled, clusters_projected)

  # Back to WGS84
  testpoints = sf::st_transform(data_joined, crs = 4326)

  # Return only necessary information
  testpoints[, c('time', 'location', 'cluster')]

}

#' Forecast a \code{dockless_df} object
#'
#' Forecasts a \code{dockless_df} time series one day ahead.
#'
#' @param forecast_data time series to be forecasted as object of
#' class \code{dockless_df}.
#' @param evaluate_data true observations of forecasted day, as
#' object of class \code{dockless_df}
#' @param method one of 'DBAFS', 'NFS' or 'EFS', specifying the forecasting method
#' to be used.
#' @param model forecasting model, either of class \code{ARIMA}, for
#' non-seasonal data, or class \code{stlm}, for seasonal data. Ignored if \code{method}
#' is set to either 'NFS' or 'EFS'.
#' @return Returns an object of class \code{dockless_fc}, which is a data
#' frame containing the observed values, forecasted values and corresponding
#' prediction intervals.
#' @export
forecast_single = function(forecast_data, evaluate_data, method, model = NULL) {

  # Forecast the forecast_data 96 time lags ahead with the given method
  if (method == 'DBAFS') {

    # Use the model to forecast the forecast_data
    if (methods::is(model, 'ARIMA')) {

      # Convert data to ts object
      data_ts = stats::ts(forecast_data$distance)

      # Forecast with the model
      forecast = forecast::forecast(
        object = forecast::Arima(data_ts, model = model),
        h = 96
      )

    } else if (methods::is(model, 'stlm')) {

      # Convert data to msts object
      # STL with the attached seasonal periods
      data_msts = forecast::msts(
        forecast_data$distance,
        seasonal.periods = attr(model$stl, 'seasonal.periods')
      )

      # Forecast the non-seasonal part with the ARIMA model and ...
      # ... the seasonal part with a seasonal naïve method
      forecast = forecast::forecast(
        object = forecast::stlm(data_msts, robust = TRUE, model = model),
        h = 96
      )

    } else {

      # Stop the function
      stop("The models must be either of class 'ARIMA' or class 'stlm'")

    }

  } else if (method == 'NFS') {

    # Convert data to ts object
    data_ts = stats::ts(forecast_data$distance)

    # Forecast with naive method
    forecast = forecast::naive(data_ts, h = 96)

  } else if (method == 'EFS') {

    # Fit model to the data
    model = dockless::build_single_model(forecast_data)

    # Forecast 96 time lags ahead with the fitted model
    forecast = forecast::forecast(model, h = 96)

  } else {

    # Stop the function
    stop("Only available methods are 'DBAFS', 'NFS' and 'EFS'")

  }

  # Convert forecast to data frame
  forecast = as.data.frame(forecast)

  # Set column names
  names(forecast) = c(
    'forecast',
    'lower80',
    'upper80',
    'lower95',
    'upper95'
  )

  # Add the corresponding time stamps to the forecast values
  forecast$time = evaluate_data$time

  # Add the true observations to the forecast values
  forecast$observation = evaluate_data$distance

  # Calculate the forecast errors
  forecast$error = forecast$observation - forecast$forecast

  # Change column order
  forecast = forecast[c(6, 7, 1, 8, 2, 3, 4, 5)]

  # Return as dockless_fc object
  structure(forecast, class = c("dockless_fc", "data.frame"))

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
#' @param method one of 'DBAFS', 'NFS' or 'EFS', specifying the forecasting method
#' to be used.
#' @param model forecasting model, either of class \code{ARIMA}, for
#' non-seasonal data, or class \code{stlm}, for seasonal data. Ignored if \code{method}
#' is set to either 'NFS' or 'EFS'.
#' @return Returns an object of class \code{dockless_fc_df}, which is a data
#' frame containing the observed values, forecasted values and corresponding
#' prediction intervals.
#' @export
forecast_lastweek = function(data, method, model = NULL) {

  # Days in a week
  days = rev(seq(1, 7, 1))

  # Apply the forecast_lastday function to each of the days
  forecast_single_day = function(x) {

    # Data used for forecasting
    if (method == 'EFS') {
      forecast_data = data[(nrow(data) - (x + 28) * 96 + 1):(nrow(data) - x * 96), ]
    } else {
      forecast_data = data[(nrow(data) - (x + 14) * 96):(nrow(data) - x * 96), ]
    }

    # Data used to evaluate forecasts
    evaluate_data = data[(nrow(data) - x * 96 + 1):(nrow(data) - (x - 1) * 96), ]

    # Forecast
    dockless::forecast_single(
      forecast_data = forecast_data,
      evaluate_data = evaluate_data,
      method = method,
      model = model
    )
  }

  # Run forecast_single_day function for all days
  all_days = lapply(days, forecast_single_day)

  # Combine and return
  do.call('rbind', all_days)

}

#' Forecast a \code{dockless_dfc} object
#'
#' Forecast a \code{dockless_dfc} object, using either DBAFS, NFS or EFS, from either
#' the user perspective or the operator perspective.
#'
#' @param data object of class \code{dockless_dfc}.
#' @param method one of 'DBAFS', 'NFS' or 'EFS', specifying the forecasting method
#' to be used.
#' @param perspective one of 'user' or 'operator', specifying from which perspective
#' forecasts should be made.
#' @param points if \code{perspective} is set to 'user', the test points as object of
#' class \code{sf} with point geometry, obtained with the \code{create_testpoints}
#' function. If \code{perspective} is set to 'operator', the grid cell centroids as
#' object of class \code{sf} with point geometry, obtained with the \code{create_grid}
#' function.
#' @param models list of objects of either class \code{ARIMA} or class \code{stlm},
#' each representing the forecasting model belonging to a cluster. Must be obtained
#' with the \code{build_models} function. Ignored if \code{method} is set to either
#' 'NFS' or 'EFS'.
#' @return Returns an object of class \code{dockless_fcc}, which is a collection of
#' \code{dockless_fc} data frames.
#' @export
forecast_multiple = function(data, method, perspective,
                    points, models = NULL) {

  if (perspective == 'user') {

    # Function to forecast one test point
    forecast_one_testpoint = function(point, data) {

      # Extract time from testpoint
      # Distance data is available only for every quarter of an hour. Therefore, the given ...
      # ... time is rounded down to the nearest quartely hour timestamp
      time = lubridate::floor_date(point$time, '15 minutes')

      # Define the time from which data is needed, as the given time..
      # ..minus two weeks
      from_time = time - (60 * 60 * 24 * 7 * 2)

      # Define the time to which data is needed as the given time..
      # ..plus one day
      to_time = time + (60 * 60 * 24)

      # Select the needed data
      # Make distinction between the data to be used for the forecasting, ...
      # ... and the data to be used to evaluate the forecasts
      forecast_data = data[data$time > from_time & data$time <= time, ]
      evaluate_data = data[data$time > time & data$time <= to_time, ]

      # Forecast the forecast_data 96 time lags ahead with the given method
      if (method == 'DBAFS') {

        # Choose model based on the cluster in which the test point is located
        model = models[[point$cluster]]

        # Forecast
        dockless::forecast_single(
          forecast_data = forecast_data,
          evaluate_data = evaluate_data,
          method = 'DBAFS',
          model = model
        )

      } else if (method == 'NFS') {

        # Forecast
        dockless::forecast_single(
          forecast_data = forecast_data,
          evaluate_data = evaluate_data,
          method = 'NFS'
        )

      } else if (method == 'EFS') {

        # Redefine from time
        # Now, use for weeks instead of two weeks
        from_time = time - (60 * 60 * 24 * 7 * 4)

        # Select forecast data based on new from time
        forecast_data = data[data$time > from_time & data$time <= time, ]

        # Forecast
        dockless::forecast_single(
          forecast_data = forecast_data,
          evaluate_data = evaluate_data,
          method = 'EFS'
        )

      } else {

        # Stop the function
        stop("Only available methods are 'DBAFS', 'NFS' and 'EFS'")

      }

    }

    # Run the forecast_one_testpoint function for all testpoints
    f = function(x, y) {
      forecast_one_testpoint(
        points = points[x, ],
        data = y
      )
    }

    all_forecasts = mapply(
      f,
      c(1:nrow(points)),
      data,
      SIMPLIFY = FALSE
    )

    # Return as dockless_fcc object
    structure(all_forecasts, class = c("dockless_fcc", "list"))

  } else if (perspective == 'operator') {

    # Function to forecast one grid cell centroid
    forecast_one_centroid = function(point, data) {

      if (method == 'DBAFS') {

        # Choose model based on the cluster in which the test point is located
        model = models[[point$cluster]]

        dockless::forecast_lastweek(
          data = data,
          method = 'DBAFS',
          model = model
        )

      } else if (method == 'NFS') {

        dockless::forecast_lastweek(
          data = data,
          method = 'NFS'
        )

      } else if (method == 'EFS') {

        dockless::forecast_lastweek(
          data = data,
          method = 'EFS'
        )

      } else {

        stop("Only available methods are 'DBAFS', 'NFS' and 'EFS'")

      }

    }

    # Run the forecast_one_centroid function for all grid cell centroids
    f = function(x, y) {
      forecast_one_centroid(
        points = points[x, ],
        data = y
      )
    }

    all_forecasts = mapply(
      f,
      c(1:nrow(points)),
      data,
      SIMPLIFY = FALSE
    )

    # Return as dockless_fcc object
    structure(all_forecasts, class = c("dockless_fcc", "list"))

  } else {

    # Stop the function
    stop("Only available perspectives are 'user' and 'operator'")

  }

}
