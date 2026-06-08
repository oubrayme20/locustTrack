#' Calculer le verdissement post-pluie (Greenup)
#'
#' Analyse le verdissement de la végétation après les précipitations.
#' Détecte les zones où le NDVI augmente significativement après la pluie,
#' ce qui indique des conditions favorables au développement des criquets.
#'
#' @param ndvi_avant SpatRaster NDVI avant les pluies
#' @param ndvi_apres SpatRaster NDVI après les pluies
#' @param seuil_greenup Seuil minimal d'augmentation NDVI. Par défaut 0.1
#' @param seuil_pluie Seuil minimal de précipitations (mm). Par défaut 10
#'
#' @return Une liste contenant :
#'   \item{anomalie}{Raster de l'anomalie NDVI (après - avant)}
#'   \item{greenup}{Raster binaire des zones de verdissement}
#'   \item{stats}{Statistiques résumées du greenup}
#'
#' @examples
#' \dontrun{
#' ndvi_jan <- download_ndvi(2023, mois = 1)
#' ndvi_jul <- download_ndvi(2023, mois = 7)
#' result <- calculate_greenup(ndvi_jan, ndvi_jul)
#' terra::plot(result$anomalie, main = "Anomalie NDVI")
#' terra::plot(result$greenup,  main = "Zones de verdissement")
#' }
#'
#' @export
calculate_greenup <- function(ndvi_avant,
                              ndvi_apres,
                              seuil_greenup = 0.1,
                              seuil_pluie   = 10) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis. Installez-le avec : install.packages('terra')")
  }

  # Vérifier que les deux rasters ont la même étendue
  if (!terra::compareGeom(ndvi_avant, ndvi_apres, stopOnError = FALSE)) {
    message("Les rasters n'ont pas la même géométrie. Rééchantillonnage en cours...")
    ndvi_apres <- terra::resample(ndvi_apres, ndvi_avant, method = "bilinear")
  }

  # Calcul de l'anomalie NDVI (différence après - avant)
  anomalie <- ndvi_apres - ndvi_avant
  names(anomalie) <- "anomalie_NDVI"

  # Détection des zones de greenup (anomalie > seuil)
  greenup <- terra::ifel(anomalie >= seuil_greenup, 1, 0)
  names(greenup) <- "greenup"

  # Statistiques
  vals_anomalie <- terra::values(anomalie, na.rm = TRUE)
  vals_greenup  <- terra::values(greenup,  na.rm = TRUE)

  n_total   <- length(vals_greenup)
  n_greenup <- sum(vals_greenup == 1, na.rm = TRUE)
  pct_greenup <- round(n_greenup / n_total * 100, 1)

  stats <- data.frame(
    indicateur    = c("Anomalie NDVI moyenne",
                      "Anomalie NDVI maximale",
                      "Anomalie NDVI minimale",
                      "Pixels en verdissement",
                      "Pourcentage verdissement (%)"),
    valeur        = c(round(mean(vals_anomalie, na.rm = TRUE), 3),
                      round(max(vals_anomalie,  na.rm = TRUE), 3),
                      round(min(vals_anomalie,  na.rm = TRUE), 3),
                      n_greenup,
                      pct_greenup)
  )

  message("Greenup calculé : ", n_greenup, " pixels en verdissement (",
          pct_greenup, "% de la zone)")

  return(list(
    anomalie = anomalie,
    greenup  = greenup,
    stats    = stats
  ))
}
