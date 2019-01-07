#' Create a test set
#'
#' A test set is created inside the function by sampling 'location, time of the day,
#' day of the week'-combinations from a dataframe with all the pick ups in the
#' historical data from the bike sharing system. The days of the week from those
#' combinations are translated into the same days of the week in the test set period.
#'
#' @param intensity_data object of class \code{sf} with point geometry, containing the
#' locations and times of all pick-ups in the bike sharing system.
#' @param area object of class \code{sf}, with polygon geometry, representing the
#' service area of the bike sharing system.
#' @param first_day character specifying the first day of the test set period.
#' Should be in the format 'YYYY-mm-dd'.
#' @param n_weeks the length in weeks of the test set period.
#' @param n_sample the size of the sample for each week.
#' @return Returns an object of class \code{sf}, with point geometry
#' @export
create_testset = function(intensity_data, area, first_day,
                          n_weeks = 2, n_sample = 500) {

  # Convert 'first_day' to a date
  first_day = as.Date(first_day, format = '%Y-%m-%d')

  # Create a vector of 'first_days', one for each week
  f = function(x) first_day + (x - 1) * 7
  week_firstdays = do.call('c', lapply(seq(1, n_weeks, 1), f))

  # Clip the intensity data with the service area
  data_projected = project_sf(intensity_data)
  area_projected = project_sf(area)

  data_projected$intersects = as.vector(
    sf::st_intersects(data_projected, area_projected, sparse = FALSE)
  )

  data_clipped = data_projected[data_projected$intersects,]
  data_clipped$intersects = NULL

  # Sample one week
  sample_oneweek = function(first_day) {

    # Sample n (location, time)-combinations from the intensity data
    sample_indices = sample(x = c(1:nrow(data_clipped)), size = n_sample)
    sample = data_clipped[sample_indices, ]

    # Replace the timestamps in the sample such that the time stays the same but..
    # ..the days are replaced by the same day of the week in the test set week
    f = function(x) {
      time = format(x, '%H:%M:%S')
      wday = lubridate::wday(x, week_start = 1)
      day = first_day + (wday - 1)

      timestamp = as.POSIXct(
        paste(as.character(day), time),
        format = '%Y-%m-%d %H:%M:%S',
        tz = 'America/Los_Angeles'
      )

      return(timestamp)
    }

    times = do.call('c', lapply(sample$time, f))
    sample$time = times

    return(sf::st_as_sf(sample, sf_column_name = 'location'))

  }

  # Run sample_oneweek function for all weeks
  all_samples = lapply(week_firstdays, function(x) sample_oneweek(first_day = x))
  do.call('rbind', all_samples)

}


#' Forecast a test set
#'
#' For all 'location, time'-combinations in a test set, forecast all the future
#' timestamps - i.e. every 15min - until one day ahead. The test set is created inside
#' the function by sampling 'location, time of the day, day of the week'-combinations
#' from a dataframe with all the pick ups in the historical data from the bike sharing
#' system. The days of the week from those combinations are translated into the same
#' days of the week in the test set period. Database credentials are needed for this
#' function.
#'
#' @param data object of class \code{sf} with point geometry, containing the
#' locations and times of the points in a test set.
#' @param clusters list of objects of class \code{sf}, \code{sfc} or \code{sfg},
#' with polygon geometry, each representing the geographical outline of a cluster.
#' @param models list of objects of either class \code{ARIMA} or class \code{stlm},
#' each representing the forecasting model belonging to the corresponding cluster.
#' @param naive logical; if set to TRUE, only naive forecasts are made and the
#' parameters \code{clusters} and \code{models} are ignored.
#' @param weeks_of_data how many weeks of historical data to use for forecasting.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns an object of class \code{forecast}, coming from the
#' \code{forecast} package.
#' @export
forecast_testset = function(data, clusters = NULL, models = NULL,
                            naive = FALSE, weeks_of_data = 8,
                            database_user, database_password) {

  # Function to forecast one (location, time)-combination of the test set
  forecast_testset_onepoint = function(location, time) {

    # Define the time from which data needs to be queried as the given time..
    # ..minus the given weeks of data
    from_time = time - (60 * 60 * 24 * 7 * weeks_of_data)

    # Define the time to which data needs to be queried as the given time..
    # ..plus one day
    to_time = time + (60 * 60 * 24)

    # Query data for the given location, from the 'from_time' to the 'to-time'
    # Data will come as class dockless_dfc
    data_list = query_distances(
      locations = location,
      from = from_time,
      to = to_time,
      database_user = database_user,
      database_password = database_password
    )

    # Retrieve the dockless_df from the dockless_dfc
    data = data_list[[1]]

    # Split the data into forecast data and evaluation data
    # The forecast data is used to fit the models on
    # The evaluation data is used to compare the forecasts with the real observations
    indices = which(data$time <= time)
    fc_data = data[indices,]
    ev_data = data[-indices,]

    # If naive is TRUE, forecast the data with naive forecasts
    # If naive is FALSE, forecast the data with one of the given models
    if (naive) {

      # Convert data to ts object
      fc_data_ts = stats::ts(fc_data$distance)

      # Forecast with naive method
      forecast = forecast::naive(fc_data_ts, h = 96)

    } else {

      # Choose model based on the cluster in which the location is located
      location_projected = project_sf(location)
      clusters_projected = project_sf(clusters)

      f = function(x) {
        sf::st_intersects(location_projected, x, sparse = FALSE)
      }

      cluster_index = which(sapply(clusters_projected, f))
      model = models[[cluster_index]]

      # Fit the model to the queried data
      if (methods::is(model, 'ARIMA')) {

        # Convert data to ts object
        fc_data_ts = stats::ts(fc_data$distance)

        # Fit the ARIMA model from the model object
        fc_data_model = forecast::Arima(fc_data_ts, model = model)

      } else if (methods::is(model, 'stlm')) {

        # Convert to msts object
        fc_data_msts = forecast::msts(
          fc_data$distance,
          seasonal.periods = attr(model$stl, 'seasonal.periods')
        )

        # Decompose with STL and fit the ARIMA model from the model object..
        # ..to trend+remainder
        fc_data_model = forecast::stlm(fc_data_msts, robust = TRUE, model = model)

      } else {

        # Stop the function
        stop("The models must be either of class 'ARIMA' or class 'stlm'")

      }

    }

    # Forecast
    forecast = forecast::forecast(fc_data_model, h = 96)

    # Convert to data frame
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
    forecast$time = ev_data$time

    # Add the real data to the forecast values
    forecast$observation = ev_data$distance

    # Calculate the forecast errors
    forecast$error = forecast$observation - forecast$forecast

    # Change column order
    forecast = forecast[c(6, 7, 1, 8, 2, 3, 4, 5)]

    # Return as dockless_fc object
    structure(forecast, class = c("dockless_fc", "data.frame"))

  }

  # Run forecast_testset_onepoint function for all points in the sample
  g = function(x, y) forecast_testset_onepoint(location = x, time = y)
  all_forecasts = mapply(g, data$location, data$time, SIMPLIFY = FALSE)

  # Return as dockless_fcc object
  structure(all_forecasts, class = c("dockless_fcc", "list"))

}
