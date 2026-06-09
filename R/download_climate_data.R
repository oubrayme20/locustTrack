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
#' # Avec CHIRPS (précipitations spatialement résolues)
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

  # ── Vérifications ────────────────────────────────────────────
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop("Le package 'geodata' est requis : install.packages('geodata')")
  }
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  vars_valides <- c("tmax", "tmin", "tavg", "prec", "bio")
  if (!var %in% vars_valides) {
    stop("Variable invalide. Choisissez parmi : ",
         paste(vars_valides, collapse = ", "))
  }

  res_valides <- c(10, 5, 2.5)
  if (!res %in% res_valides) {
    stop("Resolution invalide. Choisissez parmi : ",
         paste(res_valides, collapse = ", "))
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Dossier cree : ", path)
  }

  zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)

  # ── Téléchargement WorldClim ─────────────────────────────────
  message("Telechargement WorldClim '", var, "' a ", res, " minutes...")

  clim_global <- geodata::worldclim_global(
    var  = var,
    res  = res,
    path = path
  )

  clim_zone <- terra::crop(clim_global, zone)
  message("WorldClim pret : ", terra::nlyr(clim_zone), " couches")

  # ── Humidité relative (optionnelle) ──────────────────────────
  if (humidite) {
    message("Ajout humidite relative via variables bioclimatiques...")

    tryCatch({
      bio_global    <- geodata::worldclim_global(
        var  = "bio",
        res  = res,
        path = path
      )
      bio_zone      <- terra::crop(bio_global, zone)
      humidite_r    <- bio_zone[[15]]
      names(humidite_r) <- "humidite_relative"
      clim_zone     <- c(clim_zone, humidite_r)
      message("Humidite relative ajoutee (",
              terra::nlyr(clim_zone), " couches au total)")

    }, error = function(e) {
      message("Erreur humidite : ", e$message)
    })
  }

  # ── Précipitations CHIRPS (optionnelles) ─────────────────────
  # CHIRPS = Climate Hazards Group InfraRed Precipitation with Station data
  # Source : https://www.chc.ucsb.edu/data/chirps
  # Résolution native : 0.05° (~5 km) — spatialement résolue pixel par pixel
  if (chirps) {
    message("Telechargement precipitations CHIRPS...")
    message("  Source : CHC UCSB — resolution native 0.05 degres")

    if (!requireNamespace("chirps", quietly = TRUE)) {
      stop(paste0(
        "Le package 'chirps' est requis pour les donnees CHIRPS.\n",
        "Installation : install.packages('chirps')"
      ))
    }

    tryCatch({

      # Grille de points couvrant la zone (résolution 0.5°)
      lon_pts <- seq(lon_min + 0.25, lon_max - 0.25, by = 0.5)
      lat_pts <- seq(lat_min + 0.25, lat_max - 0.25, by = 0.5)
      coords  <- expand.grid(lon = lon_pts, lat = lat_pts)

      # Année courante pour les dates (CHIRPS disponible ~2 mois de délai)
      annee_chirps <- as.integer(format(Sys.Date(), "%Y")) - 1
      date_debut   <- paste0(annee_chirps, "-01-01")
      date_fin     <- paste0(annee_chirps, "-12-31")

      message("  Periode CHIRPS : ", date_debut, " a ", date_fin)
      message("  Points d'echantillonnage : ", nrow(coords))

      chirps_data <- chirps::get_chirps(
        object = coords,
        dates  = c(date_debut, date_fin),
        server = "CHC"
      )

      # Agréger par point géographique (moyenne annuelle)
      # chirps_data contient : lon, lat, date, chirps
      chirps_annuel <- aggregate(
        chirps ~ lon + lat,
        data = chirps_data,
        FUN  = sum,     # précipitations annuelles totales
        na.rm = TRUE
      )

      # Rasteriser les points géoréférencés → grille spatiale
      # Chaque point a sa propre valeur de précipitation
      pts_chirps <- terra::vect(
        data.frame(
          x     = chirps_annuel$lon,
          y     = chirps_annuel$lat,
          value = chirps_annuel$chirps
        ),
        geom = c("x", "y"),
        crs  = "EPSG:4326"
      )

      # Grille cible à 0.5° de résolution
      r_cible <- terra::rast(
        zone,
        resolution = 0.5,
        crs        = "EPSG:4326"
      )

      # Rasterisation pixel par pixel
      chirps_r <- terra::rasterize(
        pts_chirps, r_cible,
        field = "value",
        fun   = "mean"
      )
      names(chirps_r) <- "precipitation_CHIRPS_mm_an"

      clim_zone <- c(clim_zone, chirps_r)
      message("CHIRPS ajoute : ",
              sum(!is.na(terra::values(chirps_r))),
              " pixels valides (",
              terra::nlyr(clim_zone), " couches au total)")

    }, error = function(e) {
      message("Erreur CHIRPS : ", e$message)
      message("CHIRPS non ajoute — WorldClim utilise seul")
    })
  }

  # ── Résumé final ─────────────────────────────────────────────
  message("Donnees climatiques pretes : ",
          terra::nlyr(clim_zone), " couches")
  message("Etendue : ",
          paste(round(as.vector(terra::ext(clim_zone)), 2),
                collapse = ", "))

  return(clim_zone)
}
