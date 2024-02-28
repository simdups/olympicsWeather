library(httr2)
library(tibble)
library(testthat)
library(tidygeocoder)
library(dplyr)
library(tidyverse)

perform_request <- function(lat, long) {
  api_url <- "https://api.open-meteo.com/v1/forecast"
  params <- list(
    latitude = lat,
    longitude = long,
    hourly = c(
      "temperature_2m",
      "apparent_temperature", "precipitation_probability",
      "precipitation"
    )
  )
  req <- request(api_url) |>
    req_url_query(!!!params, .multi = "comma") |>
    # Exécution de la requête et récupération des résultats
    req_perform() |>
    # sauf qu'ils sont au format json (Content-Type: application/json)
    # donc on a besoin de cette étape :
    # Extraction des données JSON de la réponse
    resp_body_json() |>
    as_tibble()

  return(req)
}



unnest_response <- function(resp) {
  tibble(
    date_heure = unlist(resp$hourly[1][[1]]),
    temperature_celsius = unlist(resp$hourly[2][[1]]),
    temperature_ressentie_celsius = unlist(resp$hourly[3][[1]]),
    precipation_proba = unlist(resp$hourly[4][[1]]),
    precipitation = unlist(resp$hourly[5][[1]])
  )
}

address_to_gps = function(adresse){
  lieu = tibble(
    adresse) |>
    geocode(address = adresse, method = "osm") |>
    select(lat, long)

  unlist(lieu)
}







#' Prévisions Météo
#'
#' @description Cette fonction permet d'obtenir les prévisions météo en temps réel d'un lieu pour chaque heure des 7 prochains jours. Voici les 4 renseignements obtenus : la "temperature_celsius", la "temperature_ressentie_celsius", la "precipation_proba" et les "precipitation".
#'
#' @param add_ou_coord Adresse littérale ou coordonnées géographiques du lieu souhaité
#'
#' @return un tibble contenant la météo horaire de l'endroit en question des 7 prochains jours
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom tidygeocoder geocode
#' @importFrom tibble tibble
#' @importFrom httr2 resp_body_json req_perform req_url_query request
#'
#' @examples
#' get_forecast("Nantes")
#'
#' @examples
#' coord = c(47.218371, -1.553621)
#' get_forecast(coord)
#'
#' @rdname get_forecast
get_forecast = function(add_ou_coord){
  UseMethod("get_forecast")
}

#' @export
get_forecast.numeric = function(xy){
  if (is.numeric(xy) && length(xy) == 2) {
    météo_tibble = unnest_response(perform_request(xy[1],xy[2]))
    print(météo_tibble)
  } else {
    stop("L'argument xy doit être un vecteur numérique de taille 2.")
  }
}

#' @export
get_forecast.character = function(address){
  if (is.character(address) && length(address) == 1){
    get_forecast.numeric(address_to_gps(address))
  } else {
    stop("L'argument address doit être une chaîne de caractères de taille 1.")
  }
}




