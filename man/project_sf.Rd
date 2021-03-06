% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{project_sf}
\alias{project_sf}
\title{Project \code{sf} object to State Plane California Zone III}
\usage{
project_sf(data)
}
\arguments{
\item{data}{object of class \code{sf}.}
}
\value{
Returns an object of class \code{sf}.
}
\description{
Transforms the CRS of an object of class \code{sf} to the projection
optimized for San Francisco with units in meters, i.e. State Plane
California Zone III, EPSG:26943.
}
