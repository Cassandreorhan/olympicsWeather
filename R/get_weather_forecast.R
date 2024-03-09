
#' @title Prévision météo des sites olympiques
#'
#' @description Cette fonction permet de récupérer une table de prévisions météo pour une coordonnée GPS donnée.
#' Ces prévisions météo incluent des informations telles que la température, la probabilité de précipitations, etc.
#'
#' @param localisation Latitude et longitude de la coordonnée GPS (vecteur numérique) ou adresse (chaîne de caractères).
#'
#' @return Un tibble contenant les prévisions météo.
#'
#' @usage get_weather_forecast(localisation)
#'
#' @examples
#' get_weather_forecast(c(48.841319, 2.253076))
#' get_weather_forecast("Parc des princes")
#'
#' @import testthat
#' @import tibble
#' @import tidygeocoder
#' @import httr2
#' @import dplyr
#'
#' @export

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
