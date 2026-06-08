#' Résumer les régions à risque d'invasion des criquets
#'
#' Calcule des statistiques par région sur la carte de risque :
#' surface à risque, pourcentage, et détection des hotspots principaux.
#'
#' @param risk_result Liste issue de \code{predict_risk_map()}
#' @param seuil_hotspot Seuil probabilité pour hotspot. Par défaut 0.7
#'
#' @return Une liste contenant :
#'   \item{stats_globales}{Statistiques globales de risque}
#'   \item{hotspots}{Coordonnées des zones hotspot}
#'   \item{resume}{Résumé par niveau de risque}
#'
#' @examples
#' \dontrun{
#' risk    <- predict_risk_map(rf, clim, ndvi)
#' summary <- summarize_risk_regions(risk)
#' print(summary$stats_globales)
#' print(summary$hotspots)
#' }
#'
#' @export
summarize_risk_regions <- function(risk_result,
                                   seuil_hotspot = 0.7) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Extraire les rasters
  risque_continu <- risk_result$risque_continu
  risque_classe  <- risk_result$risque_classe

  # ── Statistiques globales ─────────────────────────────────────────────────
  vals_continu <- terra::values(risque_continu, na.rm = TRUE)
  vals_classe  <- terra::values(risque_classe,  na.rm = TRUE)

  n_total  <- length(vals_continu)
  n_faible <- sum(vals_classe == 1, na.rm = TRUE)
  n_moyen  <- sum(vals_classe == 2, na.rm = TRUE)
  n_eleve  <- sum(vals_classe == 3, na.rm = TRUE)

  stats_globales <- data.frame(
    Indicateur = c(
      "Probabilite moyenne de presence",
      "Probabilite maximale",
      "Probabilite minimale",
      "Pixels risque faible",
      "Pixels risque moyen",
      "Pixels risque eleve",
      "Pourcentage risque eleve (%)"
    ),
    Valeur = c(
      round(mean(vals_continu, na.rm = TRUE), 3),
      round(max(vals_continu,  na.rm = TRUE), 3),
      round(min(vals_continu,  na.rm = TRUE), 3),
      n_faible,
      n_moyen,
      n_eleve,
      round(n_eleve / n_total * 100, 1)
    )
  )

  cat("=== Statistiques globales du risque ===\n")
  print(stats_globales)

  # ── Résumé par niveau de risque ───────────────────────────────────────────
  resume <- data.frame(
    Niveau       = c("Faible", "Moyen", "Eleve"),
    Code         = c(1, 2, 3),
    N_pixels     = c(n_faible, n_moyen, n_eleve),
    Pourcentage  = round(
      c(n_faible, n_moyen, n_eleve) / n_total * 100, 1
    ),
    Prob_moyenne = c(
      round(mean(vals_continu[vals_classe == 1], na.rm = TRUE), 3),
      round(mean(vals_continu[vals_classe == 2], na.rm = TRUE), 3),
      round(mean(vals_continu[vals_classe == 3], na.rm = TRUE), 3)
    )
  )

  cat("\n=== Résumé par niveau de risque ===\n")
  print(resume)

  # ── Détection des hotspots (pixels > seuil_hotspot) ───────────────────────
  message("Détection des hotspots (probabilité > ", seuil_hotspot, ")...")

  hotspot_raster <- terra::ifel(risque_continu >= seuil_hotspot,
                                risque_continu, NA)

  # Convertir en points
  hotspot_pts <- terra::as.data.frame(hotspot_raster,
                                      xy   = TRUE,
                                      na.rm = TRUE)

  names(hotspot_pts) <- c("longitude", "latitude", "probabilite")
  hotspot_pts <- hotspot_pts[order(-hotspot_pts$probabilite), ]

  if (nrow(hotspot_pts) > 0) {
    cat("\n=== Top 10 Hotspots ===\n")
    print(head(hotspot_pts, 10))
  } else {
    cat("\nAucun hotspot détecté avec le seuil", seuil_hotspot, "\n")
    cat("Essayez de réduire seuil_hotspot\n")
  }

  message("Nombre total de hotspots : ", nrow(hotspot_pts), " pixels")

  return(list(
    stats_globales = stats_globales,
    hotspots       = hotspot_pts,
    resume         = resume
  ))
}
