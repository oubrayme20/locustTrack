#' Résumer les régions à risque d'invasion des criquets
#'
#' Calcule des statistiques globales et par région géographique
#' sur la carte de risque : surface à risque en km²,
#' pourcentage, et détection des hotspots principaux.
#'
#' @param risk_result Liste issue de \code{predict_risk_map()}
#' @param seuil_hotspot Seuil probabilité pour hotspot. Par défaut 0.7
#'
#' @return Une liste contenant :
#'   \item{stats_globales}{Statistiques globales de risque}
#'   \item{stats_pays}{Statistiques par région avec surface km²}
#'   \item{hotspots}{Coordonnées des zones hotspot}
#'   \item{resume}{Résumé par niveau de risque avec km²}
#'
#' @examples
#' \dontrun{
#' risk    <- predict_risk_map(rf, clim, ndvi)
#' summary <- summarize_risk_regions(risk)
#' print(summary$resume)
#' print(summary$stats_pays)
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

  # ── Calculer la surface d'un pixel en km² ─────────────────
  # Résolution en degrés → conversion en km²
  # 1 degré latitude ≈ 111 km
  # 1 degré longitude ≈ 111 * cos(latitude) km
  res_deg     <- terra::res(risque_continu)
  lat_centre  <- mean(c(terra::ymin(risque_continu),
                        terra::ymax(risque_continu)))
  km_par_deg_lat <- 111.0
  km_par_deg_lon <- 111.0 * cos(lat_centre * pi / 180)
  surface_pixel_km2 <- res_deg[1] * km_par_deg_lon *
    res_deg[2] * km_par_deg_lat

  message("Surface par pixel : ",
          round(surface_pixel_km2, 2), " km²")

  # ── Statistiques globales ─────────────────────────────────
  vals_continu <- terra::values(risque_continu, na.rm = TRUE)
  vals_classe  <- terra::values(risque_classe,  na.rm = TRUE)

  n_total  <- length(vals_continu)
  n_faible <- sum(vals_classe == 1, na.rm = TRUE)
  n_moyen  <- sum(vals_classe == 2, na.rm = TRUE)
  n_eleve  <- sum(vals_classe == 3, na.rm = TRUE)

  # Surface en km²
  s_faible <- round(n_faible * surface_pixel_km2)
  s_moyen  <- round(n_moyen  * surface_pixel_km2)
  s_eleve  <- round(n_eleve  * surface_pixel_km2)
  s_total  <- round(n_total  * surface_pixel_km2)

  stats_globales <- data.frame(
    Indicateur = c(
      "Probabilite moyenne de presence",
      "Probabilite maximale",
      "Probabilite minimale",
      "Surface totale zone etude (km2)",
      "Surface risque faible (km2)",
      "Surface risque moyen (km2)",
      "Surface risque eleve (km2)",
      "Pourcentage risque eleve (%)"
    ),
    Valeur = c(
      round(mean(vals_continu, na.rm = TRUE), 3),
      round(max(vals_continu,  na.rm = TRUE), 3),
      round(min(vals_continu,  na.rm = TRUE), 3),
      s_total,
      s_faible,
      s_moyen,
      s_eleve,
      round(n_eleve / n_total * 100, 1)
    )
  )

  cat("=== Statistiques globales du risque ===\n")
  print(stats_globales)

  # ── Statistiques par région avec km² ─────────────────────
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
    reg  <- regions[i, ]
    zone <- terra::ext(reg$lon_min, reg$lon_max,
                       reg$lat_min, reg$lat_max)

    tryCatch({
      r_crop  <- terra::crop(risque_continu, zone)
      vals_r  <- terra::values(r_crop, na.rm = TRUE)

      if (length(vals_r) > 0) {
        r_cls   <- terra::crop(risque_classe, zone)
        vals_c  <- terra::values(r_cls, na.rm = TRUE)

        n_tot_r <- length(vals_c)
        n_el_r  <- sum(vals_c == 3, na.rm = TRUE)
        n_mo_r  <- sum(vals_c == 2, na.rm = TRUE)
        n_fa_r  <- sum(vals_c == 1, na.rm = TRUE)

        # Surface en km²
        s_el_r <- round(n_el_r * surface_pixel_km2)
        s_mo_r <- round(n_mo_r * surface_pixel_km2)
        s_fa_r <- round(n_fa_r * surface_pixel_km2)
        s_tot_r <- round(n_tot_r * surface_pixel_km2)

        stats_pays <- rbind(stats_pays, data.frame(
          Region           = reg$region,
          Surface_tot_km2  = s_tot_r,
          Surface_eleve_km2= s_el_r,
          Surface_moyen_km2= s_mo_r,
          Surface_faible_km2= s_fa_r,
          Prob_moyenne     = round(mean(vals_r,
                                        na.rm = TRUE), 3),
          Pct_eleve        = round(n_el_r / n_tot_r * 100, 1)
        ))
      }
    }, error = function(e) {
      cat("Région ignorée :", reg$region, "\n")
    })
  }

  cat("\n=== Statistiques par région (avec surface km²) ===\n")
  print(stats_pays)

  # ── Résumé par niveau de risque avec km² ─────────────────
  resume <- data.frame(
    Niveau       = c("Faible", "Moyen", "Eleve"),
    Code         = c(1, 2, 3),
    N_pixels     = c(n_faible, n_moyen, n_eleve),
    Surface_km2  = c(s_faible, s_moyen, s_eleve),
    Pourcentage  = round(
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
    hotspot_raster, xy = TRUE, na.rm = TRUE
  )
  names(hotspot_pts) <- c("longitude", "latitude",
                          "probabilite")
  hotspot_pts <- hotspot_pts[
    order(-hotspot_pts$probabilite), ]

  # Surface hotspots en km²
  s_hotspot <- round(nrow(hotspot_pts) * surface_pixel_km2)

  if (nrow(hotspot_pts) > 0) {
    cat("\n=== Top 10 Hotspots ===\n")
    print(head(hotspot_pts, 10))
    cat("Surface totale hotspots :", s_hotspot, "km²\n")
  } else {
    cat("\nAucun hotspot détecté\n")
  }

  message("Hotspots : ", nrow(hotspot_pts),
          " pixels (", s_hotspot, " km²)")

  return(list(
    stats_globales    = stats_globales,
    stats_pays        = stats_pays,
    hotspots          = hotspot_pts,
    resume            = resume,
    surface_pixel_km2 = surface_pixel_km2
  ))
}
