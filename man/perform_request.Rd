% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weather_forecast.R
\name{perform_request}
\alias{perform_request}
\title{perform_request}
\usage{
perform_request(latitude, longitude)
}
\arguments{
\item{latitude}{Latitude numérique.}

\item{longitude}{Longitude numérique.}
}
\value{
Un tibble avec les données de prévisions météorologiques basées sur les coordonnées GPS.
}
\description{
Cette fonction retourne les données de prévisions météorologiques sous forme de tibble en fonction des coordonnées GPS.
}
