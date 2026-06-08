#' Télécharger les données climatiques WorldClim et CHIRPS
#'
#' Télécharge les variables climatiques depuis WorldClim
#' (température, précipitations, humidité) et optionnellement
#' les précipitations CHIRPS pour la zone d'étude des criquets.
#'
#' @param var Variable climatique : "tmax","tmin","tavg","prec","bio".
#'   Par défaut "prec"
#' @param res Résolution en minutes : 10, 5, 2.5. Par défaut 10
#' @param lon_min Longitude minimale. Par défaut -20
#' @param lon_max Longitude maximale. Par défaut 65
#' @param lat_min Latitude minimale. Par défaut -10
#' @param lat_max Latitude maximale. Par défaut 40
#' @param path Dossier de téléchargement. Par défaut "data/climate"
#' @param chirps Si TRUE, télécharge aussi CHIRPS. Par défaut FALSE
#' @param humidite Si TRUE, ajoute humidité relative. Par défaut FALSE
#'
#' @return Un objet SpatRaster avec les données climatiques
#'
#' @examples
#' \dontrun{
#' # Précipitations WorldClim seulement
#' clim <- download_climate_data(var = "prec", res = 10)
#'
#' # Avec humidité
#' clim <- download_climate_data(var = "prec", humidite = TRUE)
#'
#' # Avec CHIRPS
#' clim <- download_climate_data(var = "prec", chirps = TRUE)
#'
#' terra::plot(clim[[1]])
#' }
#'
#' @export
download_climate_data <- function(var      = "prec",
                                  res      = 10,
                                  lon_min  = -20,
                                  lon_max  =  65,
                                  lat_min  = -10,
                                  lat_max  =  40,
                                  path     = "data/climate",
                                  chirps   = FALSE,
                                  humidite = FALSE) {

  # Vérifier geodata et terra
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop("Le package 'geodata' est requis : install.packages('geodata')")
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Vérifier les paramètres
  vars_valides <- c("tmax", "tmin", "tavg", "prec", "bio")
  if (!var %in% vars_valides) {
    stop("Variable invalide. Choisissez parmi : ",
         paste(vars_valides, collapse = ", "))
  }

  res_valides <- c(10, 5, 2.5)
  if (!res %in% res_valides) {
    stop("Résolution invalide. Choisissez parmi : ",
         paste(res_valides, collapse = ", "))
  }

  # Créer le dossier
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Dossier créé : ", path)
  }

  # Zone d'étude
  zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)

  # ── Téléchargement WorldClim ──────────────────────────────
  message("Téléchargement WorldClim '", var,
          "' à ", res, " minutes...")

  clim_global <- geodata::worldclim_global(
    var  = var,
    res  = res,
    path = path
  )

  clim_zone <- terra::crop(clim_global, zone)
  message("WorldClim prêt : ", terra::nlyr(clim_zone), " couches")

  # ── Humidité relative (optionnelle) ───────────────────────
  if (humidite) {
    message("Ajout humidité relative via variables bioclimatiques...")

    tryCatch({
      bio_global <- geodata::worldclim_global(
        var  = "bio",
        res  = res,
        path = path
      )
      bio_zone      <- terra::crop(bio_global, zone)
      humidite_r    <- bio_zone[[15]]
      names(humidite_r) <- "humidite_relative"
      clim_zone     <- c(clim_zone, humidite_r)

      message("Humidité relative ajoutée (",
              terra::nlyr(clim_zone), " couches au total)")

    }, error = function(e) {
      message("Erreur humidité : ", e$message)
      message("Humidité non ajoutée")
    })
  }

  # ── Précipitations CHIRPS (optionnelles) ──────────────────
  if (chirps) {
    message("Téléchargement précipitations CHIRPS...")

    if (!requireNamespace("chirps", quietly = TRUE)) {
      message("Installation du package chirps...")
      install.packages("chirps")
    }

    tryCatch({
      library(chirps)

      # Points de la zone d'étude
      lon_pts <- seq(lon_min, lon_max, by = 2)
      lat_pts <- seq(lat_min, lat_max, by = 2)
      coords  <- expand.grid(lon = lon_pts, lat = lat_pts)

      # Télécharger CHIRPS
      chirps_data <- get_chirps(
        object = coords,
        dates  = c("2023-01-01", "2023-12-31"),
        server = "CHC"
      )

      # Convertir en raster
      chirps_r <- terra::rast(zone,
                              resolution = 0.5,
                              crs = "EPSG:4326")
      terra::values(chirps_r) <- mean(
        chirps_data$chirps, na.rm = TRUE
      )
      names(chirps_r) <- "precipitation_CHIRPS"

      # Ajouter au stack
      clim_zone <- c(clim_zone, chirps_r)
      message("CHIRPS ajouté (",
              terra::nlyr(clim_zone), " couches au total)")

    }, error = function(e) {
      message("Erreur CHIRPS : ", e$message)
      message("CHIRPS non ajouté — WorldClim utilisé seul")
    })
  }

  # ── Résumé final ──────────────────────────────────────────
  message("Données climatiques prêtes : ",
          terra::nlyr(clim_zone), " couches")
  message("Étendue : ",
          paste(round(as.vector(terra::ext(clim_zone)), 2),
                collapse = ", "))

  return(clim_zone)
}
