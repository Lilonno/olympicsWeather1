library(devtools)
library(httr2)
library(jsonlite)
library(tibble)
library(usethis)
library(lubridate)
library(tidygeocoder)
library(leaflet)
library(sf)
library(testthat)

#' perform_request
#'
#' Cette fonction retourne les données de prévisions météorologiques sous forme de tibble en fonction des coordonnées GPS.
#'
#' @param latitude Latitude numérique.
#' @param longitude Longitude numérique.
#'
#' @return Un tibble avec les données de prévisions météorologiques basées sur les coordonnées GPS.
#'
#' @export
#'

perform_request <- function(latitude, longitude) {
  url <- "https://api.open-meteo.com/v1/forecast"
  request(url) |>
    req_url_query(
      latitude = latitude,
      longitude = longitude,
      hourly = c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"),
      .multi = "comma"
    ) |>
    req_perform() |>
    resp_body_json() |>
    as_tibble()
}

#' unnest_response
#'
#' Extrait les données de température en fonction de l'heure.
#'
#' @param perf7 Données de prévisions météorologiques.
#'
#' @return Un tibble avec les données de prévisions météorologiques.
#'
#' @export
#'

unnest_response <- function(perf7) {
  hourly_data <- perf7$hourly

  if (length(hourly_data) == 0) {
    stop("Pas de données dans la colonne 'hourly'.")
  }

  output_tibble <- tibble(
    "heure en fuseau horaire UTC" = with_tz(unlist(hourly_data[[1]]), tzone = "UTC"),
    "données de température" = unlist(hourly_data[[2]]),
    "données de température ressentie" = unlist(hourly_data[[3]]),
    "probabilité de pluie" = unlist(hourly_data[[4]]),
    "précipitation en mm" = unlist(hourly_data[[5]])
  )

  return(output_tibble)
}

#' address_to_gps : Convertir une adresse en coordonnées GPS
#'
#' @param address Adresse en caractères.
#'
#' @return Coordonnées GPS.
#'
#' @export
#'

address_to_gps <- function(address) {
  adresse1 <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)

  adresse1 <- rbind(adresse1, data.frame(addr = address), stringsAsFactors = FALSE)

  resultat_geocodage <- adresse1 |>
    geocode(addr, method = 'arcgis')

  adresse1 <- resultat_geocodage

  return(adresse1)
}

#' get_gps_coordinate: Obtenir les coordonnées GPS à partir d'une adresse
#'
#' @param address Adresse en caractères.
#'
#' @return Coordonnées GPS.
#'
#' @export
#'

get_gps_coordinate <- function(address) {
  resultat_geocodage <- address_to_gps(address)
  coordinates <- c(resultat_geocodage$lat, resultat_geocodage$long)
  return(coordinates)
}

#' get_forecast.numeric: Obtenir des prévisions météorologiques basées sur des coordonnées numériques
#'
#' @param coordinate Vecteur numérique de coordonnées.
#'
#' @return Un tibble avec des données de prévisions météorologiques.
#'
#' @export
#'

get_forecast.numeric <- function(coordinate) {
  if (!is.numeric(coordinate) || length(coordinate) != 2) {
    stop("Il n'y a pas de vecteur numérique, ERREUR")
  }

  resultat_previsions <- perform_request(latitude = coordinate[1], longitude = coordinate[2])
  resultat_traitement <- unnest_response(resultat_previsions)

  return(resultat_traitement)
}

#' get_forecast.character : Obtenir des prévisions météorologiques basées sur une adresse en caractères
#'
#' @param address Adresse en caractères.
#'
#' @return Un tibble avec des données de prévisions météorologiques.
#'
#' @export
#'

get_forecast.character <- function(address) {
  if (!is.character(address) || length(address) != 1) {
    stop("Il n'y a pas de vecteur de type caractère, ERREUR")
  }

  coordinates <- get_gps_coordinate(address)
  resultat_previsions <- perform_request(latitude = coordinates[1], longitude = coordinates[2])
  resultat_traitement <- unnest_response(resultat_previsions)

  return(resultat_traitement)
}

#' get_forecast : Obtenir des prévisions météorologiques en fonction d'une adresse ou de coordonnées numériques
#'
#' @param x Adresse en caractères ou vecteur numérique de coordonnées.
#'
#' @return Un tibble avec des données de prévisions météorologiques.
#'
#' @export
#'
#' @examples
#' get_forecast(c(47.83706, -4.34687))
#' get_forecast("Paris, France")
#'

get_forecast <- function(x) {
  if (is.numeric(x)) {
    get_forecast.numeric(x)
  } else if (is.character(x)) {
    get_forecast.character(x)
  } else {
    stop("ERREUR, il doit y avoir un vecteur numérique ou de type caractère.")
  }
}

