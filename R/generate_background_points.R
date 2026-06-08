#' Générer des points de background (pseudo-absences)
#'
#' Génère des points background par échantillonnage spatial depuis
#' un raster de référence via spatSample() de terra.
#'
#' @param occurrences data.frame des occurrences (issu de clean_occurrences)
#' @param raster_ref SpatRaster de référence pour l'échantillonnage
#' @param n_points Nombre de points background. Par défaut égal aux occurrences
#' @param seed Graine aléatoire pour reproductibilité. Par défaut 42
#'
#' @return Un data.frame combinant occurrences (presence=1)
#'   et background points (presence=0)
#'
#' @examples
#' \dontrun{
#' df_clean  <- clean_occurrences(import_locust_data("data.csv"))
#' clim      <- download_climate_data(var = "prec")
#' df_complet <- generate_background_points(df_clean, raster_ref = clim)
#' table(df_complet$presence)
#' }
#'
#' @export
generate_background_points <- function(occurrences,
                                       raster_ref,
                                       n_points = NULL,
                                       seed     = 42) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Vérifier les colonnes
  cols_requises <- c("latitude", "longitude")
  cols_manquantes <- cols_requises[!cols_requises %in% names(occurrences)]
  if (length(cols_manquantes) > 0) {
    stop("Colonnes manquantes : ", paste(cols_manquantes, collapse = ", "))
  }

  # Nombre de points = nombre d'occurrences (ratio 1:1)
  if (is.null(n_points)) {
    n_points <- nrow(occurrences)
  }

  message("Génération de ", n_points,
          " points background (méthode spatSample)...")

  set.seed(seed)

  # Échantillonnage depuis le raster
  # bg <- spatSample(bio_rsa, size = 1000, method = "random",
  #                  na.rm = TRUE, as.points = TRUE)
  bg <- terra::spatSample(
    raster_ref,
    size      = n_points,
    method    = "random",
    na.rm     = TRUE,
    as.points = TRUE
  )

  # Extraire les coordonnées
  bg_xy <- terra::crds(bg)
  bg_df <- data.frame(
    latitude  = round(bg_xy[, 2], 4),
    longitude = round(bg_xy[, 1], 4),
    presence  = 0
  )

  # Préparer les présences
  pres_df <- data.frame(
    latitude  = occurrences$latitude,
    longitude = occurrences$longitude,
    presence  = 1
  )

  # Combiner présences + background
  # pres_df$presence <- 1
  # bg_df$presence   <- 0
  # sdm_data <- rbind(pres_df, bg_df)
  sdm_data <- rbind(pres_df, bg_df)

  message("Dataset complet : ",
          sum(sdm_data$presence == 1), " présences + ",
          sum(sdm_data$presence == 0), " background points")

  return(sdm_data)
}
