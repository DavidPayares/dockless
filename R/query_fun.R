#' Query historical data for set of geographical locations
#'
#' Queries, for each of the given geographical locations, distances to the
#' nearest available bike for several timestamps in the past, from the JUMP Bikes
#' database. Database credentials are needed for this function.
#'
#' @param locations object of class \code{sf} with point geometry.
#' @param from timestamp of class \code{POSIXct} defining from which timestamp
#' on historical data should be queried. Default is set to the first timestamp
#' in the database (i.e. earlier timestamps than the default are not possible).
#' @param to timestamp of class \code{POSIXct} defining until which timestamp
#' historical data should be queried. Default is set to the system time.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns an object of class \code{dockless_dfc}, which is a collection
#' of data frames of class \code{dockless_df}. Each \code{dockless_df} object is
#' a time serie of historical distance data for a specific geographical location.
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
  query_single_location = function(location, from, connection) {

    # SQL code to create a geographical point from the locations coordinates
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
        AND time > make_timestamptz(", from$year + 1900, ", ",
                                      from$mon + 1, ", ",
                                      from$mday, ", ",
                                      from$hour, ", ",
                                      from$min + 1, ", ",
                                      from$sec, ", ",
                                      "'+00')
        AND time <= make_timestamptz(", to$year + 1900, ", ",
                                      to$mon + 1, ", ",
                                      to$mday, ", ",
                                      to$hour, ", ",
                                      to$min + 1, ", ",
                                      to$sec, ", ",
                                      "'+00')
      ORDER BY
        time,
        location <-> ", sql_point
    )

    # Read from database
    data = RPostgreSQL::dbGetQuery(conn = connection, statement = query)

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
    function(x) {
      query_single_location(
        location = x,
        from = from,
        connection = db_connection
      )
    }
  )

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

#' Updates a \code{dockless_dfc} object with new data
#'
#' Queries, for each of the given geographical locations, distances to the
#' nearest available bike for several timestamps in the past, from the JUMP Bikes
#' database. Queries only data after the last timestamp of the provided
#' \code{dockless_dfc} object. Database credentials are needed for this function.
#'
#' @param locations object of class \code{sf} with point geometry.
#' @param data object of class \code{dockless_dfc} to be updated.
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns the updated object of class \code{dockless_dfc}.
#' @export
update_distances = function(locations, data, database_user, database_password) {

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
    database_user = database_user,
    database_password = database_password
  )

  # Merge the 'old' data and the new data
  mapply(
    function(x,y) {rbind(x,y)},
    data,
    newdata,
    SIMPLIFY = FALSE
  )

}

#' Query pick-ups and drop-offs from historical data
#'
#' Queries individual pick-ups and drop-offs from the complete JUMP Bikes
#' database. Database credentials are needed for this function.
#'
#' @param database_user character defining the user name to access the database.
#' @param database_password character defining the password to access the database.
#' @return Returns an object of class \code{sf}, containing all pick-ups and
#' drop-offs, with geographical location and timestamp.
#' @export
query_usage = function(database_user, database_password) {

  # Connect to the database
  db_connection = RPostgreSQL::dbConnect(
    drv = 'PostgreSQL',
    dbname = 'jumpbikes',
    host = 'jumpbikes.cpu2z0a5bugq.us-east-2.rds.amazonaws.com',
    port = 5432,
    user = database_user,
    password = database_password
  )

  # Define query to select all unique bike id's in the data
  query = "SELECT DISTINCT bike_id FROM map_sf_gbfs_history"

  # Query all unique bike id's as a vector
  bike_id = RPostgreSQL::dbGetQuery(conn = db_connection, statement = query)
  bike_id_vec = bike_id$bike_id

  # Function to query data for one single bike
  query_single_bike = function(bike_id, db_connection) {

    # Define query to select data only from the particular bike
    query = paste0(
      "SELECT
      bike_id,
      jump_ebike_battery_level AS battery_level,
      date_trunc('minute', time) AS time,
      location
      FROM
      map_sf_gbfs_history
      WHERE
      bike_id = '", bike_id, "'"
    )

    # Read from database as sf object
    data = sf::st_read(dsn = db_connection, query = query)

    # Remove duplicated timestamps with the tsibble package
    data$duplicated = tsibble::find_duplicates(data)
    data = data[!(data$duplicated),]

    # Set time in correct time zone
    attr(data$time, 'tzone') = 'America/Los_Angeles'

    # Fill NA's with the tsibble package
    data = as.data.frame(tsibble::fill_na(tsibble::as_tsibble(data)))

    # Create seperate dataframe for startpoints
    data_start = data[-nrow(data),]

    # Create seperate dataframe for endpoints
    data_end = data[-1,]

    # For each row of data_start, specify if it is a pick up or not
    # For each row of data_end, specify if it is a drop off or not
    f = function(x, y) {
      ifelse(is.na(y) & !is.na(x), TRUE, FALSE)
    }

    pickup = mapply(f, data_start$bike_id, data_end$bike_id)
    dropoff = mapply(f, data_end$bike_id, data_start$bike_id)

    # Add pickup information to data_start
    # Add dropoff information to data_end
    data_start$pickup = pickup
    data_start$dropoff = FALSE
    data_end$pickup = FALSE
    data_end$dropoff = dropoff

    # From data_start, only keep the pick ups
    # From data_end, only keep the drop offs
    data_start = data_start[data_start$pickup,]
    data_end = data_end[data_end$dropoff,]

    # Return the row binded data_start and data_end, as sf object
    sf::st_as_sf(rbind(data_start, data_end))

  }

  # Run the query single bike function for all the bike_ids
  f = function(x) {
    query_single_bike(x, db_connection = db_connection)
  }

  all_data = lapply(bike_id_vec, f)

  # Return the row binded data
  do.call(rbind, all_data)

}
