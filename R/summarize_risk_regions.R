#' Résumer les régions à risque d'invasion des criquets
#'
#' Calcule des statistiques globales et par région géographique
#' sur la carte de risque : surface à risque, pourcentage,
#' et détection des hotspots principaux.
#'
#' @param risk_result Liste issue de \code{predict_risk_map()}
#' @param seuil_hotspot Seuil probabilité pour hotspot. Par défaut 0.7
#'
#' @return Une liste contenant :
#'   \item{stats_globales}{Statistiques globales de risque}
#'   \item{stats_pays}{Statistiques par région}
#'   \item{hotspots}{Coordonnées des zones hotspot}
#'   \item{resume}{Résumé par niveau de risque}
#'
#' @examples
#' \dontrun{
#' risk    <- predict_risk_map(rf, clim, ndvi)
#' summary <- summarize_risk_regions(risk)
#' print(summary$stats_pays)
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

  # ── Statistiques globales ─────────────────────────────────
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

  # ── Statistiques par région ───────────────────────────────
  regions <- data.frame(
    region  = c("Afrique_Ouest", "Afrique_Est",
                "Afrique_Nord",  "Sahel",
                "Moyen_Orient",  "Peninsule_Arabique"),
    lon_min = c(-20,  25,  -5, -15,  35,  45),
    lon_max = c(  5,  45,  35,  25,  55,  60),
    lat_min = c(  5,  -5,  20,  10,  15,  15),
    lat_max = c( 20,  15,  38,  20,  38,  30)
  )

  stats_pays <- data.frame()

  for (i in 1:nrow(regions)) {
    reg <- regions[i, ]

    zone <- terra::ext(
      reg$lon_min, reg$lon_max,
      reg$lat_min, reg$lat_max
    )

    tryCatch({
      r_crop  <- terra::crop(risque_continu, zone)
      vals_r  <- terra::values(r_crop, na.rm = TRUE)

      if (length(vals_r) > 0) {
        r_cls   <- terra::crop(risque_classe, zone)
        vals_c  <- terra::values(r_cls, na.rm = TRUE)
        n_tot   <- length(vals_c)
        n_el    <- sum(vals_c == 3, na.rm = TRUE)
        n_mo    <- sum(vals_c == 2, na.rm = TRUE)
        n_fa    <- sum(vals_c == 1, na.rm = TRUE)

        stats_pays <- rbind(stats_pays, data.frame(
          Region       = reg$region,
          Prob_moyenne = round(mean(vals_r, na.rm = TRUE), 3),
          Prob_max     = round(max(vals_r,  na.rm = TRUE), 3),
          Pct_faible   = round(n_fa / n_tot * 100, 1),
          Pct_moyen    = round(n_mo / n_tot * 100, 1),
          Pct_eleve    = round(n_el / n_tot * 100, 1)
        ))
      }
    }, error = function(e) {
      cat("Région ignorée :", reg$region, "\n")
    })
  }

  cat("\n=== Statistiques par région ===\n")
  print(stats_pays)

  # ── Résumé par niveau de risque ───────────────────────────
  resume <- data.frame(
    Niveau      = c("Faible", "Moyen", "Eleve"),
    Code        = c(1, 2, 3),
    N_pixels    = c(n_faible, n_moyen, n_eleve),
    Pourcentage = round(
      c(n_faible, n_moyen, n_eleve) / n_total * 100, 1
    ),
    Prob_moyenne = c(
      round(mean(vals_continu[vals_classe == 1],
                 na.rm = TRUE), 3),
      round(mean(vals_continu[vals_classe == 2],
                 na.rm = TRUE), 3),
      round(mean(vals_continu[vals_classe == 3],
                 na.rm = TRUE), 3)
    )
  )

  cat("\n=== Résumé par niveau de risque ===\n")
  print(resume)

  # ── Détection des hotspots ────────────────────────────────
  message("Détection des hotspots (probabilité > ",
          seuil_hotspot, ")...")

  hotspot_raster <- terra::ifel(
    risque_continu >= seuil_hotspot,
    risque_continu, NA
  )

  hotspot_pts <- terra::as.data.frame(
    hotspot_raster,
    xy    = TRUE,
    na.rm = TRUE
  )

  names(hotspot_pts) <- c("longitude", "latitude", "probabilite")
  hotspot_pts <- hotspot_pts[order(-hotspot_pts$probabilite), ]

  if (nrow(hotspot_pts) > 0) {
    cat("\n=== Top 10 Hotspots ===\n")
    print(head(hotspot_pts, 10))
  } else {
    cat("\nAucun hotspot détecté avec seuil", seuil_hotspot, "\n")
  }

  message("Hotspots détectés : ", nrow(hotspot_pts), " pixels")

  return(list(
    stats_globales = stats_globales,
    stats_pays     = stats_pays,
    hotspots       = hotspot_pts,
    resume         = resume
  ))
}
