

url <- "https://api.open-meteo.com/v1/forecast"
address <- tibble::tribble(~address,"Paris")

#' @title Prévision météo des sites olympiques
#'
#' @description Cette fonction permet de récupérer une table de prévisions météo pour une coordonnée GPS donnée. Ces prévisions météo incluent des informations telles que la température, la probabilité de précipitations, etc.
#' @param localisation Latitude et longitude de la coordonnée GPS (vecteur numérique) ou adresse (chaîne de caractères).
#' @return Un tibble contenant les prévisions météo.
#' @export
#' @examples
#'
#' get_weather_forecast(c(48.85, 2.35))
#' get_weather_forecast("Paris")
#'
#' @import ggmap
#' @import httr2
#' @import testthat
#' @import tibble
#' @import tidygeocoder
#' @import devtools
#' @import httr
#' @import jsonlite
#' @import tidyverse

get_weather_forecast <- function(localisation) {

  adress_to_gps <- function(address) {
    adress_tibble <- tibble::tribble(~address, address) |> tidygeocoder::geocode(address)
    coordonnees <- adress_tibble |>
      dplyr::select(lat, long) |>
      dplyr::slice(1) |>
      unlist()
    return(coordonnees)
    }

  get_forecast.numeric <- function(xy) {
    lat <- xy[1]
    lon <- xy[2]
    result <- olympicsWeather::unnest_response(olympicsWeather::perform_request(lat, lon))
    return(tibble::as_tibble(result))
  }

  get_forecast.character <- function(address) {
    coordinates <- adress_to_gps(address)
    result <- get_forecast.numeric(coordinates)
    return(result)
    }

  if (is.numeric(localisation) && length(localisation) == 2) {
    return(get_forecast.numeric(localisation))
    } else if (is.character(localisation) && length(localisation) == 1) {
      return(get_forecast.character(localisation))
    }
  }
