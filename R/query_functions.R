#' Query historical distance data
#'
#' Retrieves, for each of the given geographical locations, distances to the
#' nearest available bike for several timestamps in the past, from the JUMP Bikes
#' database. Database credentials are needed for this function.
#'
#' @param locations object of class \code{sf} with point geometry.
#' @param from timestamp of class \code{POSIXct} defining from which timestamp
#' on data should be queried. Default is set to the first timestamp
#' in the database (i.e. earlier timestamps than the default are not possible).
#' @param to timestamp of class \code{POSIXct} defining until which timestamp
#' data should be queried. Default is set to the system time.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns an object of class \code{dockless_dfc}, which is a collection
#' of data frames of class \code{dockless_df}. Each \code{dockless_df} object is
#' a time series of historical distance data for a specific geographical location.
#' @export
query_distances = function(locations,
                      from = as.POSIXct('2018-09-10 16:30:00',
                                        format = '%Y-%m-%d %H:%M:%S',
                                        tz = 'America/Los_Angeles'),
                      to = Sys.time(),
                      database_user, database_password) {

  # Create a matrix with the coordinates of the given locations
  coordinates = sf::st_coordinates(locations)

  # Set the 'from' and 'to' timestamp in UTM timezone
  attr(from, 'tzone') = 'UTC'
  attr(to, 'tzone') = 'UTC'

  # Convert to POSIXlt
  from = as.POSIXlt(from)
  to = as.POSIXlt(to)

  # Connect to the database
  db_connection = RPostgreSQL::dbConnect(
    drv = 'PostgreSQL',
    dbname = 'jumpbikes',
    host = 'jumpbikes.cpu2z0a5bugq.us-east-2.rds.amazonaws.com',
    port = 5432,
    user = database_user,
    password = database_password
  )

  # Function to query data for one single location
  query_single_location = function(location) {

    # SQL code to create a geographical point from the location coordinates
    sql_point = paste0(
      "ST_SetSRID(ST_MakePoint(", location[1], ", ", location[2], "), 4326)"
    )

    # SQL code to query historical data (distance nearest bike) for that point
    query = paste0(
      "SELECT DISTINCT ON (time)
        bike_id,
        date_trunc('minute', time) AS time,
        ST_DistanceSphere(location, ", sql_point, ") AS distance
      FROM
        map_sf_gbfs_history
      WHERE
        EXTRACT(MINUTE FROM time) IN ('00', '15', '30', '45')
        AND date_trunc('minute', time) >= make_timestamptz(", from$year + 1900, ", ",
                                                            from$mon + 1, ", ",
                                                            from$mday, ", ",
                                                            from$hour, ", ",
                                                            from$min, ", ",
                                                            from$sec, ", ",
                                                            "'+00')
        AND date_trunc('minute', time) < make_timestamptz(", to$year + 1900, ", ",
                                                            to$mon + 1, ", ",
                                                            to$mday, ", ",
                                                            to$hour, ", ",
                                                            to$min, ", ",
                                                            to$sec, ", ",
                                                            "'+00')
      ORDER BY
        time,
        location <-> ", sql_point
    )

    # Read from database
    data = RPostgreSQL::dbGetQuery(conn = db_connection, statement = query)

    # Set time in correct time zone
    attr(data$time, 'tzone') = 'America/Los_Angeles'

    # Fill NA's with the tsibble package
    data = as.data.frame(tsibble::fill_na(tsibble::as_tsibble(data)))

    # Return the data frame as an object of class dockless_df
    structure(data, class = c("dockless_df", "data.frame"))

  }

  # Run the query single location function for all the given locations
  all_data = apply(
    coordinates,
    1,
    function(x) query_single_location(x)
  )

  # Disconnect to the database
  RPostgreSQL::dbDisconnect(db_connection)

  # When querying for a lot of different locations, the first locations will..
  # ..have less timestamps than later ones. For other functions it is important..
  # ..that all the individual data objects end at the same timestamp. Therefore..
  # ..we need to erase the extra data for some locations to make sure all..
  # ..individual data objects have the same amount of rows as the first one.
  nrow_data_1 = nrow(all_data[[1]])
  all_data_sliced = lapply(all_data, function(x) x[c(1:nrow_data_1),])

  # Return as an object of class dockless_dfc
  structure(all_data_sliced, class = c("dockless_dfc", "list"))

}

#' Updates a set of distance data with new data
#'
#' Queries, for each of the given geographical locations, distances to the
#' nearest available bike for several timestamps in the past, from the JUMP Bikes
#' database. Queries only data after the last timestamp of the provided
#' \code{dockless_dfc} object. Database credentials are needed for this function.
#'
#' @param data object of class \code{dockless_dfc} to be updated.
#' @param locations object of class \code{sf} with point geometry.
#' @param to timestamp of class \code{POSIXct} defining until which timestamp
#' data should be queried. Default is set to the system time.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns the updated object of class \code{dockless_dfc}.
#' @export
update_distances = function(data, locations, to = Sys.time(),
                            database_user, database_password) {

  # Check if the number of data frames equals the number of locations
  if (nrow(locations) != length(data)) {
    stop('The number of locations should be equal to the number of data frames')
  }

  # Check if all the elements from the data list end at the same time
  end_times = lapply(
    data,
    function(x) x[nrow(x), 'time']
  )

  if (length(unique(end_times)) != 1) {
    stop('Not all the provided data frames end at the same timestamp')
  }

  # Retrieve the last timestamp of the data
  end_time = (data[[1]])[nrow(data[[1]]), 'time']

  # Query the data from that end time on
  newdata = query_distances(
    locations = locations,
    from = end_time,
    to = to,
    database_user = database_user,
    database_password = database_password
  )

  # Merge the original data and the new data
  mapply(
    function(x,y) {rbind(x,y)},
    data,
    newdata,
    SIMPLIFY = FALSE
  )

}

#' Query unique bike id's
#'
#' Retrieves unique bike identification numbers from the JUMP Bikes database,
#' during a given period. Database credentials are needed for this function.
#'
#' @param from timestamp of class \code{POSIXct} defining from which timestamp
#' on data should be queried. Default is set to the first timestamp
#' in the database (i.e. earlier timestamps than the default are not possible).
#' @param to timestamp of class \code{POSIXct} defining until which timestamp
#' data should be queried. Default is set to the system time.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns an object of class \code{sf}, containing all pick-ups,
#' with geographical location and timestamp.
#' @export
query_bikes = function(from = as.POSIXct('2018-09-10 16:30:00',
                                         format = '%Y-%m-%d %H:%M:%S',
                                         tz = 'America/Los_Angeles'),
                       to = Sys.time(),
                       database_user, database_password) {

  # Set the 'from' and 'to' timestamp in UTM timezone
  attr(from, 'tzone') = 'UTC'
  attr(to, 'tzone') = 'UTC'

  # Convert to POSIXlt
  from = as.POSIXlt(from)
  to = as.POSIXlt(to)

  # Connect to the database
  db_connection = RPostgreSQL::dbConnect(
    drv = 'PostgreSQL',
    dbname = 'jumpbikes',
    host = 'jumpbikes.cpu2z0a5bugq.us-east-2.rds.amazonaws.com',
    port = 5432,
    user = database_user,
    password = database_password
  )

  # SQL code to query all unique bike id's during the given period
  query = paste0(
    "SELECT DISTINCT
      bike_id
    FROM
      map_sf_gbfs_history
    WHERE
      date_trunc('minute', time) >= make_timestamptz(", from$year + 1900, ", ",
                                                      from$mon + 1, ", ",
                                                      from$mday, ", ",
                                                      from$hour, ", ",
                                                      from$min, ", ",
                                                      from$sec, ", ",
                                                      "'+00')
      AND date_trunc('minute', time) < make_timestamptz(", to$year + 1900, ", ",
                                                        to$mon + 1, ", ",
                                                        to$mday, ", ",
                                                        to$hour, ", ",
                                                        to$min, ", ",
                                                        to$sec, ", ",
                                                        "'+00')"
    )

  # Retrieve all unique bike id's during the given period
  bikes = RPostgreSQL::dbGetQuery(conn = db_connection, statement = query)

  # Disconnect to the database
  RPostgreSQL::dbDisconnect(db_connection)

  # Return the bike id's
  return(bikes)

}

#' Query historical usage data
#'
#' Retrieves individual pick-ups from the JUMP Bikes database, for a given set
#' of bikes, during a given period. Database credentials are needed for this function.
#'
#' @param bikes a vector that contains the ID's of the bikes of interest.
#' Should be obtained with the \code{dockless::query_bikes} function.
#' @param from timestamp of class \code{POSIXct} defining from which timestamp
#' on data should be queried. Default is set to the first timestamp
#' in the database (i.e. earlier timestamps than the default are not possible).
#' @param to timestamp of class \code{POSIXct} defining until which timestamp
#' data should be queried. Default is set to the system time.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns an object of class \code{sf}, containing all pick-ups,
#' with geographical location and timestamp.
#' @export
query_usage = function(bikes,
                       from = as.POSIXct('2018-09-10 16:30:00',
                                         format = '%Y-%m-%d %H:%M:%S',
                                         tz = 'America/Los_Angeles'),
                       to = Sys.time(),
                       database_user, database_password) {

  # Create a vector with the given bike id's
  bike_indices = bikes

  # Set the 'from' and 'to' timestamp in UTM timezone
  attr(from, 'tzone') = 'UTC'
  attr(to, 'tzone') = 'UTC'

  # Convert to POSIXlt
  from = as.POSIXlt(from)
  to = as.POSIXlt(to)

  # Connect to the database
  db_connection = RPostgreSQL::dbConnect(
    drv = 'PostgreSQL',
    dbname = 'jumpbikes',
    host = 'jumpbikes.cpu2z0a5bugq.us-east-2.rds.amazonaws.com',
    port = 5432,
    user = database_user,
    password = database_password
  )

  # Function to query data for one single bike
  query_single_bike = function(bike) {

    # SQL code to query historical location data for a bike
    query = paste0(
      "SELECT
        bike_id,
        date_trunc('minute', time) AS time,
        location,
        jump_ebike_battery_level AS battery_level
      FROM
        map_sf_gbfs_history
      WHERE
        bike_id = '", bike, "'
        AND date_trunc('minute', time) >= make_timestamptz(", from$year + 1900, ", ",
                                                            from$mon + 1, ", ",
                                                            from$mday, ", ",
                                                            from$hour, ", ",
                                                            from$min, ", ",
                                                            from$sec, ", ",
                                                            "'+00')
        AND date_trunc('minute', time) < make_timestamptz(", to$year + 1900, ", ",
                                                            to$mon + 1, ", ",
                                                            to$mday, ", ",
                                                            to$hour, ", ",
                                                            to$min, ", ",
                                                            to$sec, ", ",
                                                            "'+00')"
      )

    # Read from database as sf object
    data = sf::st_read(dsn = db_connection, query = query)

    # Set time in correct time zone
    attr(data$time, 'tzone') = 'America/Los_Angeles'

    # Remove duplicated timestamps with the tsibble package
    data$duplicated = tsibble::find_duplicates(data)
    data = data[!(data$duplicated),]

    # Fill NA's with the tsibble package
    data = as.data.frame(
      tsibble::fill_na(tsibble::as_tsibble(data))
    )

    # Create a data.frame containing all timestamps t
    data_t = data[-nrow(data),]

    # Create a data.frame containing all timestamps t+1
    data_t1 = data[-1,]

    # If data_t has zero rows, return NULL
    if (nrow(data_t) == 0) {
      return(NULL)
    }

    # Test first requirement (i.e. t is not an NA, t+1 is an NA)
    f = function(x, y) {
      ifelse(is.na(y) & !is.na(x), TRUE, FALSE)
    }

    data_t$pickup = mapply(f, data_t$bike_id, data_t1$bike_id)

    # Test second requirement (i.e. time difference t to t+1 < )
    g = function(x) {
      ifelse(data_t[x, 'pickup'] & !all(is.na(data_t$bike_id[(x+1):(x+120)])), TRUE, FALSE)
    }

    data_t$true_pickup = sapply(c(1:nrow(data_t)), g)

    # Only keep the pick-ups
    pickups = data_t[data_t$true_pickup, c(1:4)]

    # If there are no pick-ups, return NULL
    if (nrow(pickups) == 0) {
      return(NULL)
    }

    # Return as sf object
    sf::st_as_sf(pickups, crs = 4326)

  }

  # Run the query single bike function for all the given bike indices
  all_data = lapply(
    bike_indices,
    function(x) query_single_bike(x)
  )

  # Disconnect to the database
  RPostgreSQL::dbDisconnect(db_connection)

  # Return as a single object
  do.call('rbind', all_data)

}
