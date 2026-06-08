#' Générer des points de background (pseudo-absences)
#'
#' Génère des points background par échantillonnage spatial depuis
#' un raster de référence via spatSample() de terra.
#' Inclut un contrôle spatial simple pour s'assurer que les points
#' sont bien dans la zone d'étude et hors des zones de présence.
#'
#' @param occurrences data.frame des occurrences (issu de clean_occurrences)
#' @param raster_ref SpatRaster de référence pour l'échantillonnage
#' @param n_points Nombre de points background. Par défaut égal aux occurrences
#' @param lon_min Longitude minimale zone étude. Par défaut -20
#' @param lon_max Longitude maximale zone étude. Par défaut 65
#' @param lat_min Latitude minimale zone étude. Par défaut -10
#' @param lat_max Latitude maximale zone étude. Par défaut 40
#' @param seed Graine aléatoire pour reproductibilité. Par défaut 42
#'
#' @return Un data.frame combinant occurrences (presence=1)
#'   et background points (presence=0)
#'
#' @examples
#' \dontrun{
#' df_clean   <- clean_occurrences(import_locust_data("data.csv"))
#' clim       <- download_climate_data(var = "prec")
#' df_complet <- generate_background_points(df_clean,
#'                                           raster_ref = clim)
#' table(df_complet$presence)
#' }
#'
#' @export
generate_background_points <- function(occurrences,
                                       raster_ref,
                                       n_points = NULL,
                                       lon_min  = -20,
                                       lon_max  =  65,
                                       lat_min  = -10,
                                       lat_max  =  40,
                                       seed     = 42) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Vérifier les colonnes
  cols_requises   <- c("latitude", "longitude")
  cols_manquantes <- cols_requises[
    !cols_requises %in% names(occurrences)]

  if (length(cols_manquantes) > 0) {
    stop("Colonnes manquantes : ",
         paste(cols_manquantes, collapse = ", "))
  }

  # Nombre de points = nombre d'occurrences (ratio 1:1)
  if (is.null(n_points)) {
    n_points <- nrow(occurrences)
  }

  message("Génération de ", n_points,
          " points background (méthode spatSample)...")

  set.seed(seed)

  # ── Échantillonnage depuis le raster ──────────────────────
  # Méthode exacte de la prof : spatSample()
  bg <- terra::spatSample(
    raster_ref,
    size      = n_points * 2,  # Sur-échantillonnage pour filtrage
    method    = "random",
    na.rm     = TRUE,
    as.points = TRUE
  )

  # ── Contrôle spatial 1 : zone d'étude ─────────────────────
  bg_xy <- terra::crds(bg)
  bg_df <- data.frame(
    longitude = round(bg_xy[, 1], 4),
    latitude  = round(bg_xy[, 2], 4)
  )

  # Filtrer dans la zone d'étude
  n_avant <- nrow(bg_df)
  bg_df   <- bg_df[
    bg_df$longitude >= lon_min &
      bg_df$longitude <= lon_max &
      bg_df$latitude  >= lat_min &
      bg_df$latitude  <= lat_max, ]

  n_hors_zone <- n_avant - nrow(bg_df)
  if (n_hors_zone > 0) {
    message(n_hors_zone,
            " point(s) hors zone d'étude supprimé(s)")
  }

  # ── Contrôle spatial 2 : exclusion zones présence ─────────
  # Supprimer les background points trop proches des présences
  # Distance minimale = 0.5 degrés (~55 km)
  dist_min  <- 0.5
  n_avant   <- nrow(bg_df)

  idx_garder <- sapply(1:nrow(bg_df), function(i) {
    distances <- sqrt(
      (occurrences$longitude - bg_df$longitude[i])^2 +
        (occurrences$latitude  - bg_df$latitude[i])^2
    )
    min(distances) >= dist_min
  })

  bg_df <- bg_df[idx_garder, ]

  n_exclus <- n_avant - nrow(bg_df)
  if (n_exclus > 0) {
    message(n_exclus,
            " point(s) exclu(s) : trop proches des présences")
  }

  # ── Contrôle spatial 3 : vérification doublons ────────────
  n_avant <- nrow(bg_df)
  bg_df   <- bg_df[!duplicated(bg_df[,
                                     c("latitude", "longitude")]), ]

  n_doublons <- n_avant - nrow(bg_df)
  if (n_doublons > 0) {
    message(n_doublons, " doublon(s) supprimé(s)")
  }

  # ── Limiter au nombre demandé ─────────────────────────────
  if (nrow(bg_df) > n_points) {
    bg_df <- bg_df[1:n_points, ]
  } else if (nrow(bg_df) < n_points) {
    warning("Seulement ", nrow(bg_df),
            " points générés sur ", n_points, " demandés")
  }

  # ── Ajouter colonne presence ───────────────────────────────
  bg_df$presence <- 0

  # ── Préparer les présences ────────────────────────────────
  pres_df <- data.frame(
    latitude  = occurrences$latitude,
    longitude = occurrences$longitude,
    presence  = 1
  )

  # ── Combiner présences + background ───────────────────────
  sdm_data <- rbind(pres_df, bg_df)

  message("Dataset complet : ",
          sum(sdm_data$presence == 1), " présences + ",
          sum(sdm_data$presence == 0), " background points")

  # ── Résumé contrôle spatial ───────────────────────────────
  cat("=== Contrôle spatial ===\n")
  cat("Zone d'étude    : lon [", lon_min, ",", lon_max,
      "] lat [", lat_min, ",", lat_max, "]\n")
  cat("Points filtrés  : hors zone =", n_hors_zone,
      "| proches présences =", n_exclus,
      "| doublons =", n_doublons, "\n")
  cat("Points finaux   : ",
      sum(sdm_data$presence == 0), "background\n")

  return(sdm_data)
}
