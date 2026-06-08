#' Télécharger les données climatiques WorldClim
#'
#' Télécharge les variables climatiques (température et précipitations)
#' depuis WorldClim et les découpe sur la zone d'étude des criquets.
#'
#' @param var Variable climatique : "tmax", "tmin", "tavg", "prec". Par défaut "prec"
#' @param res Résolution en minutes : 10, 5, 2.5. Par défaut 10
#' @param lon_min Longitude minimale de la zone. Par défaut -20
#' @param lon_max Longitude maximale de la zone. Par défaut 65
#' @param lat_min Latitude minimale de la zone. Par défaut -10
#' @param lat_max Latitude maximale de la zone. Par défaut 40
#' @param path Dossier de téléchargement. Par défaut "data/climate"
#'
#' @return Un objet SpatRaster (terra) avec les données climatiques
#'
#' @examples
#' \dontrun{
#' clim <- download_climate_data(var = "prec", res = 10)
#' terra::plot(clim[[1]])
#' }
#'
#' @export
download_climate_data <- function(var     = "prec",
                                  res     = 10,
                                  lon_min = -20,
                                  lon_max =  65,
                                  lat_min = -10,
                                  lat_max =  40,
                                  path    = "data/climate") {

  # Vérifier que geodata est installé
  if (!requireNamespace("geodata", quietly = TRUE)) {
    stop("Le package 'geodata' est requis. Installez-le avec : install.packages('geodata')")
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis. Installez-le avec : install.packages('terra')")
  }

  # Vérifier les paramètres
  vars_valides <- c("tmax", "tmin", "tavg", "prec")
  if (!var %in% vars_valides) {
    stop("Variable invalide. Choisissez parmi : ", paste(vars_valides, collapse = ", "))
  }

  res_valides <- c(10, 5, 2.5)
  if (!res %in% res_valides) {
    stop("Résolution invalide. Choisissez parmi : ", paste(res_valides, collapse = ", "))
  }

  # Créer le dossier si nécessaire
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Dossier créé : ", path)
  }

  message("Téléchargement de la variable '", var, "' à ", res, " minutes de résolution...")

  # Télécharger depuis WorldClim
  clim_global <- geodata::worldclim_global(
    var  = var,
    res  = res,
    path = path
  )

  # Découper sur la zone d'étude
  zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)
  clim_zone <- terra::crop(clim_global, zone)

  message("Données climatiques prêtes : ", terra::nlyr(clim_zone), " couches")
  message("Étendue : ", paste(round(as.vector(terra::ext(clim_zone)), 2), collapse = ", "))

  return(clim_zone)
}
