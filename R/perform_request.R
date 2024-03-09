url <- "https://api.open-meteo.com/v1/forecast"

#' @title Fonction interne pour effectuer la requête API et obtenir les données brutes
#'
#' @description Cette fonction interne effectue une requête API pour récupérer les données brutes de prévisions météo.
#'
#' @param lat Latitude de la coordonnée GPS.
#' @param lon Longitude de la coordonnée GPS.
#'
#' @return Les données brutes de prévisions météo.
#'
#' @usage perform_request(lat,lon)
#'
#' @examples perform_request(48.841319, 2.253076)
#'
#' @import httr2
#' @import tibble
#' @import tidyverse
#'
#' @export

perform_request <- function(lat, lon) {
  response_table <-
    httr2::request(url) |>
    httr2::req_url_query(latitude=lat, longitude=lon,
                         hourly= c("temperature_2m",
                                   "apparent_temperature",
                                   "precipitation_probability",
                                   "precipitation"),
                         .multi = "comma") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    tibble::as_tibble()
  return(response_table)
}
