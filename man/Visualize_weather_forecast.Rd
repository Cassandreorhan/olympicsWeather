% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Visualize_weather_forecast.R
\name{Visualize_weather_forecast}
\alias{Visualize_weather_forecast}
\title{Visualise les prévisions météorologiques de manière graphique.}
\usage{
Visualize_weather_forecast(forecast)
}
\arguments{
\item{forecast}{Résultats obtenus à partir de la fonction get_forecast.}
}
\value{
Un graphique ou une autre sortie visuelle représentant les prévisions météorologiques.
}
\description{
Cette fonction prend en entrée les résultats obtenus à partir de la fonction get_forecast et produit une sortie visuelle, telle qu'un graphique représentant les températures prévues.
}
\examples{
paris<-get_weather_forecast("Paris")

}