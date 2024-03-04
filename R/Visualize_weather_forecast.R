#' @title Visualise les prévisions météorologiques de manière graphique.
#'
#' @description Cette fonction prend en entrée les résultats obtenus à partir de la fonction get_forecast et produit une sortie visuelle, telle qu'un graphique représentant les températures prévues.
#'
#' @param forecast Résultats obtenus à partir de la fonction get_forecast.
#' @return Un graphique ou une autre sortie visuelle représentant les prévisions météorologiques.
#' @export
#'
#' @examples
#' paris<-get_weather_forecast("Paris")
#'


Visualize_weather_forecast <- function(forecast) {

  paris$date_heure <- as.POSIXct(paris$date_heure, format = "%Y-%m-%dT%H:%M")

  plot_temperature <- ggplot2::ggplot(paris) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = temperature_celsius, color = "Température")) +
    ggplot2::labs(title = "Température à Paris",
                  x = "Date et heure",
                  y = "Température (°C)") +
    ggplot2::scale_color_manual(values = c("blue")) +  # Utiliser la couleur bleue uniquement pour la température
    ggplot2::theme(legend.position = "bottom")


  plot_temperature_ressentie <- ggplot2::ggplot(paris) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = temperature_ressentie_celsius, color = "Température ressentie")) +
    ggplot2::labs(title = "Température ressentie à Paris",
                  x = "Date et heure",
                  y = "Température ressentie (°C)") +
    ggplot2::scale_color_manual(values = c("red")) +  # Utiliser la couleur rouge uniquement pour la température ressentie
    ggplot2::theme(legend.position = "bottom")

  # Troisième graphique pour la probabilité de précipitation
  plot_precipitation_proba <- ggplot2::ggplot(paris) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = precipitation_proba * 10, color = "Probabilité de précipitation")) +
    ggplot2::labs(title = "Probabilité de précipitation à Paris",
                  x = "Date et heure",
                  y = "Probabilité de précipitation") +
    ggplot2::scale_color_manual(values = c("Probabilité de précipitation" = "green")) +  # Utiliser la couleur verte uniquement pour la probabilité de précipitation
    ggplot2::theme(legend.position = "bottom")

  # Quatrième graphique pour la précipitation
  plot_precipitation <- ggplot2::ggplot(paris) +
    ggplot2::geom_line(ggplot2::aes(x = date_heure, y = precipitation * 10, color = "Précipitation")) +
    ggplot2::labs(title = "Précipitation à Paris",
                  x = "Date et heure",
                  y = "Précipitation") +
    ggplot2::scale_color_manual(values = c("Précipitation" = "orange")) +  # Utiliser la couleur orange uniquement pour la précipitation
    ggplot2::theme(legend.position = "bottom")

  graph_temperature <- gridExtra::grid.arrange(plot_temperature, plot_temperature_ressentie, ncol = 1)
  graph_precipitation <- gridExtra::grid.arrange(plot_precipitation, plot_precipitation_proba, ncol = 1)
}
