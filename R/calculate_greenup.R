#' Calculer le verdissement post-pluie (Greenup)
#'
#' Analyse le verdissement de la végétation après les précipitations.
#' Détecte les zones où le NDVI augmente significativement après la pluie
#' ET où les précipitations dépassent un seuil minimal.
#' Indique les conditions favorables au développement des criquets.
#'
#' @param ndvi_avant SpatRaster NDVI avant les pluies
#' @param ndvi_apres SpatRaster NDVI après les pluies
#' @param precipitations SpatRaster des précipitations (optionnel).
#'   Par défaut NULL
#' @param seuil_greenup Seuil minimal d'augmentation NDVI. Par défaut 0.1
#' @param seuil_pluie Seuil minimal de précipitations (mm). Par défaut 10
#'
#' @return Une liste contenant :
#'   \item{anomalie}{Raster anomalie NDVI (après - avant)}
#'   \item{greenup}{Raster binaire zones de verdissement}
#'   \item{greenup_pluie}{Raster greenup conditionné aux précipitations}
#'   \item{stats}{Statistiques résumées du greenup}
#'
#' @examples
#' \dontrun{
#' ndvi_jan <- download_ndvi(2023, mois = 1)
#' ndvi_jul <- download_ndvi(2023, mois = 7)
#' clim     <- download_climate_data(var = "prec")
#'
#' # Sans précipitations
#' result <- calculate_greenup(ndvi_jan, ndvi_jul)
#'
#' # Avec précipitations
#' result <- calculate_greenup(ndvi_jan, ndvi_jul,
#'                              precipitations = clim[[6]])
#'
#' terra::plot(result$anomalie,     main = "Anomalie NDVI")
#' terra::plot(result$greenup,      main = "Zones verdissement")
#' terra::plot(result$greenup_pluie,main = "Greenup + Pluie")
#' }
#'
#' @export
calculate_greenup <- function(ndvi_avant,
                              ndvi_apres,
                              precipitations = NULL,
                              seuil_greenup  = 0.1,
                              seuil_pluie    = 10) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Vérifier géométrie des rasters
  if (!terra::compareGeom(ndvi_avant, ndvi_apres,
                          stopOnError = FALSE)) {
    message("Géométries différentes — rééchantillonnage...")
    ndvi_apres <- terra::resample(ndvi_apres, ndvi_avant,
                                  method = "bilinear")
  }

  # ── 1. Anomalie NDVI (après - avant) ──────────────────────
  anomalie <- ndvi_apres - ndvi_avant
  names(anomalie) <- "anomalie_NDVI"

  # ── 2. Greenup NDVI seul ──────────────────────────────────
  greenup <- terra::ifel(anomalie >= seuil_greenup, 1, 0)
  names(greenup) <- "greenup_NDVI"

  # ── 3. Greenup conditionné aux précipitations ─────────────
  if (!is.null(precipitations)) {

    message("Utilisation des précipitations (seuil = ",
            seuil_pluie, " mm)...")

    # Aligner le raster de précipitations
    if (!terra::compareGeom(precipitations, ndvi_avant,
                            stopOnError = FALSE)) {
      precipitations <- terra::resample(precipitations,
                                        ndvi_avant,
                                        method = "bilinear")
    }

    # Greenup conditionné : NDVI augmente ET précipitations suffisantes
    # Condition 1 : anomalie NDVI > seuil_greenup
    # Condition 2 : précipitations > seuil_pluie
    mask_pluie    <- terra::ifel(precipitations >= seuil_pluie,
                                 1, 0)
    greenup_pluie <- terra::ifel(
      anomalie >= seuil_greenup & precipitations >= seuil_pluie,
      1, 0
    )
    names(greenup_pluie) <- "greenup_avec_pluie"

    # Statistiques avec précipitations
    vals_prec  <- terra::values(precipitations, na.rm = TRUE)
    vals_gp    <- terra::values(greenup_pluie,  na.rm = TRUE)
    n_gp       <- sum(vals_gp == 1, na.rm = TRUE)
    pct_gp     <- round(n_gp / length(vals_gp) * 100, 1)

    message("Précipitations moyenne : ",
            round(mean(vals_prec, na.rm = TRUE), 1), " mm")
    message("Zones greenup + pluie : ",
            n_gp, " pixels (", pct_gp, "%)")

  } else {

    message("Aucun raster de précipitations fourni.")
    message("Greenup calculé sur NDVI seul.")
    greenup_pluie <- greenup
    names(greenup_pluie) <- "greenup_avec_pluie"
    n_gp   <- 0
    pct_gp <- 0
  }

  # ── 4. Statistiques globales ──────────────────────────────
  vals_anomalie <- terra::values(anomalie, na.rm = TRUE)
  vals_greenup  <- terra::values(greenup,  na.rm = TRUE)

  n_total   <- length(vals_greenup)
  n_greenup <- sum(vals_greenup == 1, na.rm = TRUE)
  pct_greenup <- round(n_greenup / n_total * 100, 1)

  stats <- data.frame(
    indicateur = c(
      "Anomalie NDVI moyenne",
      "Anomalie NDVI maximale",
      "Anomalie NDVI minimale",
      "Pixels en verdissement (NDVI seul)",
      "Pourcentage verdissement NDVI (%)",
      "Pixels greenup + pluie",
      "Pourcentage greenup + pluie (%)",
      "Seuil NDVI utilisé",
      "Seuil précipitations utilisé (mm)"
    ),
    valeur = c(
      round(mean(vals_anomalie, na.rm = TRUE), 3),
      round(max(vals_anomalie,  na.rm = TRUE), 3),
      round(min(vals_anomalie,  na.rm = TRUE), 3),
      n_greenup,
      pct_greenup,
      n_gp,
      pct_gp,
      seuil_greenup,
      seuil_pluie
    )
  )

  message("Greenup calculé : ", n_greenup,
          " pixels NDVI (", pct_greenup, "%) | ",
          n_gp, " pixels avec pluie (", pct_gp, "%)")

  return(list(
    anomalie      = anomalie,
    greenup       = greenup,
    greenup_pluie = greenup_pluie,
    stats         = stats
  ))
}
