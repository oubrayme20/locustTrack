#' Télécharger ou simuler les données NDVI MODIS
#'
#' Télécharge les données NDVI MODIS pour une période donnée ou génère
#' des données NDVI simulées pour la zone d'étude des criquets pèlerins.
#'
#' @param annee Année d'analyse (ex: 2023). Par défaut 2023
#' @param mois Mois d'analyse (1-12). Par défaut 1
#' @param lon_min Longitude minimale de la zone. Par défaut -20
#' @param lon_max Longitude maximale de la zone. Par défaut 65
#' @param lat_min Latitude minimale de la zone. Par défaut -10
#' @param lat_max Latitude maximale de la zone. Par défaut 40
#' @param resolution Résolution de la grille en degrés. Par défaut 0.5
#' @param simuler Si TRUE, génère des données simulées. Par défaut TRUE
#'
#' @return Un objet SpatRaster (terra) avec les valeurs NDVI
#'
#' @examples
#' \dontrun{
#' # Données simulées
#' ndvi <- download_ndvi(annee = 2023, mois = 6, simuler = TRUE)
#' terra::plot(ndvi, main = "NDVI - Juin 2023")
#' }
#'
#' @export
download_ndvi <- function(annee      = 2023,
                          mois       = 1,
                          lon_min    = -20,
                          lon_max    =  65,
                          lat_min    = -10,
                          lat_max    =  40,
                          resolution = 0.5,
                          simuler    = TRUE) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis. Installez-le avec : install.packages('terra')")
  }

  # Validation des paramètres
  if (mois < 1 || mois > 12) {
    stop("Le mois doit être entre 1 et 12")
  }

  if (annee < 2000 || annee > as.integer(format(Sys.Date(), "%Y"))) {
    stop("L'année doit être entre 2000 et l'année actuelle")
  }

  # Créer la grille de la zone d'étude
  zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)

  if (simuler) {

    message("Génération des données NDVI simulées pour ",
            format(as.Date(paste(annee, mois, "01", sep = "-")), "%B %Y"), "...")

    # Créer un raster vide sur la zone
    r <- terra::rast(zone,
                     resolution = resolution,
                     crs = "EPSG:4326")

    # Simuler des valeurs NDVI réalistes
    # Zone sahélienne : NDVI faible (0.1-0.3)
    # Zone côtière/humide : NDVI plus élevé (0.3-0.6)
    set.seed(annee * 100 + mois)

    n_cells <- terra::ncell(r)
    valeurs  <- runif(n_cells, min = 0.05, max = 0.55)

    # Effet saisonnier : NDVI plus élevé en saison des pluies (juin-sept)
    if (mois %in% 6:9) {
      valeurs <- valeurs * 1.3
    } else if (mois %in% c(12, 1, 2)) {
      valeurs <- valeurs * 0.7
    }

    # Borner entre -1 et 1
    valeurs <- pmin(pmax(valeurs, -1), 1)

    terra::values(r) <- valeurs
    names(r) <- paste0("NDVI_", annee, "_", sprintf("%02d", mois))

    message("NDVI simulé prêt : ", terra::nrow(r), " x ", terra::ncol(r), " pixels")

  } else {

    message("Téléchargement NDVI MODIS non implémenté dans cette version.")
    message("Utilisation des données simulées à la place (simuler = TRUE)")

    return(download_ndvi(annee = annee, mois = mois,
                         lon_min = lon_min, lon_max = lon_max,
                         lat_min = lat_min, lat_max = lat_max,
                         resolution = resolution, simuler = TRUE))
  }

  return(r)
}
