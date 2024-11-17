\name{ElevationGermany}
\alias{ElevationGermany}

\encoding{UTF-8}

\title{Elevation Data for Germany}

\description{
  Digital elevation model data for Germany as provided from the \pkg{geodata} package.
}

\usage{data("ElevationGermany", package = "WeatherGermany")}

\format{
  A \code{"stars"} object.
}

\source{
  Data is obtained from the \pkg{geodata} package. The original data source is
  provied by the Radar Topography Mission (SRTM) from \url{https://srtm.csi.cgiar.org/}.
}

\examples{
## load the data
data("ElevationGermany", package = "WeatherGermany")

## plot elevation data
plot(ElevationGermany)
}

\keyword{datasets}

