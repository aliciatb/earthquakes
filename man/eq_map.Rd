% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/earthquakes.R
\name{eq_map}
\alias{eq_map}
\title{Print "Earthquakes Timeline Visualization with Label"}
\usage{
eq_map(data, annot_col)
}
\arguments{
\item{data}{The data has been cleaned using eq_clean_data function}

\item{annot_col}{The column chosen by user to be annotated.}
}
\value{
This function returns a leaflet map visualization with earthquakes are represented by circles and size of circle indicates magnitude(EQ_PRIMARY)
}
\description{
This function creates a Plotly map of significant earthquake data provide by NOAA (National Centers for Environmental Information).
}
\note{
Earthquakes that occurred BCE (Before Current Epoch) have been removed from the dataset because negative year values are not handled well.
}
\examples{
\dontrun{
eq_map(raw, "DATE")
}
}
