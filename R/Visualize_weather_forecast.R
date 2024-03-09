#' @title Visualise les prévisions météorologiques de manière graphique.
#'
#' @description Cette fonction prend en entrée les résultats obtenus à partir de la fonction \code{get__weather_forecast} et produit une sortie visuelle, telle qu'un graphique représentant les températures prévues.
#'
#' @param localisation Latitude et longitude de la coordonnée GPS ou d'une adresse .
#' @param forecast Résultats obtenus à partir de la fonction \code{get__weather_forecast}.
#'
#' @return Un graphique représentant les prévisions météorologiques.
#'
#' @usage visualize_weather_forecast(localisation)
#'
#' @examples
#' visualize_weather_forecast("Parc des princes")
#' visualize_weather_forecast(c(48.841319, 2.253076))
#'
#' @import ggplot2
#' @import gridExtra
#' @import testthat
#' @import tibble
#' @import tidygeocoder
#' @import httr2
#' @import dplyr
#'
#' @export

visualize_weather_forecast <- function(localisation) {

  forecast <- olympicsWeather::get_weather_forecast(localisation)

  forecast$date_heure <- as.POSIXct(forecast$date_heure, format = "%Y-%m-%dT%H:%M")

  plot_temperature <- ggplot2::ggplot(forecast) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = temperature_celsius, color = "Température")) +
    ggplot2::labs(title = "Température",
                  x = "Date et heure",
                  y = "Température (°C)") +
    ggplot2::scale_color_manual(values = c("blue")) +
    ggplot2::theme(legend.position = "bottom")


  plot_temperature_ressentie <- ggplot2::ggplot(forecast) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = temperature_ressentie_celsius, color = "Température ressentie")) +
    ggplot2::labs(title = "Température ressentie",
                  x = "Date et heure",
                  y = "Température ressentie (°C)") +
    ggplot2::scale_color_manual(values = c("red")) +
    ggplot2::theme(legend.position = "bottom")


  plot_precipitation_proba <- ggplot2::ggplot(forecast) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = precipitation_proba, color = "Probabilité de précipitation")) +
    ggplot2::labs(title = "Probabilité de précipitation",
                  x = "Date et heure",
                  y = "Probabilité de précipitation") +
    ggplot2::scale_color_manual(values = c("Probabilité de précipitation" = "green")) +
    ggplot2::theme(legend.position = "bottom")

  plot_precipitation <- ggplot2::ggplot(forecast) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = precipitation, color = "Précipitation")) +
    ggplot2::labs(title = "Précipitation",
                  x = "Date et heure",
                  y = "Précipitation") +
    ggplot2::scale_color_manual(values = c("Précipitation" = "orange")) +
    ggplot2::theme(legend.position = "bottom")

  graph_temperature <- gridExtra::grid.arrange(plot_temperature, plot_temperature_ressentie, ncol = 1)
  graph_precipitation <- gridExtra::grid.arrange(plot_precipitation, plot_precipitation_proba, ncol = 1)
}
