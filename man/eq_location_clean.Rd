% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquakes.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Print "Earthquakes City Location"}
\usage{
eq_location_clean(country, full_name)
}
\arguments{
\item{country}{The name of the country.}

\item{full_name}{The full name of the country and city.}
}
\value{
This function returns a city name without the country name and returned in Title Case format.
}
\description{
This function reads in the country and full name for an earthquake as 2 string fields like ITALY: POMPEI
}
\note{
Some earthquakes have impacted multiple city locations and in these instances, country name is currently returned with all subsequent cities.
}
\examples{
\dontrun{
eq_location_clean(country, full_name)
}
}
