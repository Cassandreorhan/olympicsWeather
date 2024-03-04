#' @title Fonction interne pour transformer les données brutes en format de prévisions météo
#'
#' @description Cette fonction interne prend les données brutes de prévisions météo et les transforme en un format plus lisible.
#'
#' @param donnees_brute Les données de prévisions météo.
#' @return Un tibble contenant les prévisions météo formatées.
#' @examples
#' unnest_response(olympicsWeather::perform_request(48.85,2.35))
#'
#' @export

unnest_response <- function(donnees_brute){
  tibble::tibble(
    date_heure = unlist(donnees_brute$hourly[1][[1]]),
    temperature_celsius = unlist(donnees_brute$hourly[2][[1]]),
    temperature_ressentie_celsius = unlist(donnees_brute$hourly[3][[1]]),
    precipitation_proba = unlist(donnees_brute$hourly[4][[1]]),
    precipitation = unlist(donnees_brute$hourly[5][[1]]))
  }
