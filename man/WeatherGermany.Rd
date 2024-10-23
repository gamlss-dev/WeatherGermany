\name{WeatherGermany}
\alias{WeatherGermany}

\encoding{UTF-8}

\title{Weather data for Germany from the Deutscher Wetterdienst (DWD)}

\description{
  Daily weather data for all stations in Germany as provided by the Deutscher Wetterdienst (DWD).
  The data is pre-processed and only the observations with the highest quality flags are provided.
  Please see the documentation of the DWD provided in

  \url{https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/}
}

\usage{data("WeatherGermany", package = "WeatherGermany")}

\format{
A data frame containing 6734119 observations on 11 variables.
\describe{
  \item{id}{Factor, the weather station id.}
  \item{date}{Dat, the day the data is recored.}
  \item{Wmax}{Numeric, the maximum wind speed in m/s.}
  \item{Tmax}{Numeric, the maximum 2m temperature.}
  \item{Tmin}{Numeric, the manimum 2m temperature.}
  \item{sun}{Numeric, number of hours of sunshine.}
  \item{name}{Character, the staion name.}
  \item{alt}{Numeric, the altitude in meters above sea level of the weather station.}
  \item{lon}{Numeric, the longitude coordinate of the weather station.}
  \item{lat}{Numeric, the latitude coordinate of the weather station.}
}
}

\source{
  Daily German Weather Data:
  \describe{
    \item{Data Source:}{Deutscher Wetterdienst (DWD), Climate Data Center (CDC).}
    \item{Licence:}{CC BY 4.0}
    \item{URL:}{\url{https://opendata.dwd.de/climate_environment/CDC/}}
    \item{Coordinate Reference System:}{Longitude/latitude and the WGS84 datum.}
  }
}

\examples{
## load the data
data("WeatherGermany", package = "WeatherGermany")

## yearly averages of maximum temperature
WeatherGermany$year <- as.POSIXlt(WeatherGermany$date)$year + 1900
a <- aggregate(Tmax ~ year, data = WeatherGermany, FUN = mean)

## plot
plot(Tmax ~ year, data = a, type = "b")
}}

\keyword{datasets}

