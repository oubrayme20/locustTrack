#' Télécharger les données NDVI MODIS
#'
#' Télécharge les données NDVI MODIS via le package MODISTools
#' pour une période donnée (un ou plusieurs mois).
#' Génère des données simulées si MODISTools n'est pas disponible.
#'
#' @param annee Année d'analyse. Par défaut 2023
#' @param mois Mois d'analyse : un entier (1-12) ou vecteur c(1,6,12).
#'   Par défaut 1
#' @param lon_min Longitude minimale. Par défaut -20
#' @param lon_max Longitude maximale. Par défaut 65
#' @param lat_min Latitude minimale. Par défaut -10
#' @param lat_max Latitude maximale. Par défaut 40
#' @param resolution Résolution en degrés. Par défaut 0.5
#' @param simuler Si TRUE, génère données simulées. Par défaut FALSE
#'
#' @return Un objet SpatRaster avec les valeurs NDVI
#'   (une couche par mois si multi-dates)
#'
#' @examples
#' \dontrun{
#' # Un seul mois
#' ndvi <- download_ndvi(annee = 2023, mois = 6)
#'
#' # Multi-dates : plusieurs mois
#' ndvi_multi <- download_ndvi(annee = 2023, mois = c(1, 4, 7, 10))
#' terra::plot(ndvi_multi)
#'
#' # Données simulées
#' ndvi <- download_ndvi(annee = 2023, mois = 6, simuler = TRUE)
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
                          simuler    = FALSE) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Validation des paramètres
  if (any(mois < 1) || any(mois > 12)) {
    stop("Les mois doivent être entre 1 et 12")
  }

  if (annee < 2000 || annee > as.integer(format(Sys.Date(), "%Y"))) {
    stop("L'année doit être entre 2000 et l'année actuelle")
  }

  # Zone d'étude
  zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)

  # ── Téléchargement multi-dates ────────────────────────────
  # Si plusieurs mois → téléchargement pour chaque mois
  if (length(mois) > 1) {

    message("Téléchargement NDVI multi-dates : ",
            length(mois), " mois...")

    # Télécharger chaque mois séparément
    liste_rasters <- lapply(mois, function(m) {
      message("  → Mois ", sprintf("%02d", m), "/", annee)
      download_ndvi(
        annee      = annee,
        mois       = m,
        lon_min    = lon_min,
        lon_max    = lon_max,
        lat_min    = lat_min,
        lat_max    = lat_max,
        resolution = resolution,
        simuler    = simuler
      )
    })

    # Empiler les rasters (stack)
    ndvi_stack <- terra::rast(liste_rasters)

    message("NDVI multi-dates prêt : ",
            terra::nlyr(ndvi_stack), " couches")
    message("Mois téléchargés : ",
            paste(sprintf("%02d", mois), collapse = ", "))

    return(ndvi_stack)
  }

  # ── Téléchargement MODIS réel (un seul mois) ──────────────
  if (!simuler) {

    if (!requireNamespace("MODISTools", quietly = TRUE)) {
      message("Package 'MODISTools' non installé.")
      message("Installation en cours...")
      install.packages("MODISTools")
    }

    tryCatch({
      message("Téléchargement NDVI MODIS pour ",
              annee, "/", sprintf("%02d", mois), "...")

      # Point central de la zone d'étude
      lon_centre <- (lon_min + lon_max) / 2
      lat_centre <- (lat_min + lat_max) / 2

      # Date du mois
      date_debut <- paste0(annee, "-",
                           sprintf("%02d", mois), "-01")

      # Télécharger via MODISTools
      ndvi_raw <- MODISTools::mt_subset(
        product   = "MOD13A3",
        band      = "1_km_monthly_NDVI",
        lat       = lat_centre,
        lon       = lon_centre,
        start     = date_debut,
        end       = date_debut,
        km_lr     = round((lon_max - lon_min) * 55),
        km_ab     = round((lat_max - lat_min) * 55),
        site_name = "locust_zone",
        internal  = TRUE,
        progress  = FALSE
      )

      if (!is.null(ndvi_raw) && nrow(ndvi_raw) > 0) {

        # Convertir en SpatRaster
        r <- terra::rast(zone,
                         resolution = resolution,
                         crs = "EPSG:4326")

        # Appliquer facteur d'échelle MODIS (0.0001)
        vals_ndvi <- ndvi_raw$value * 0.0001
        vals_ndvi <- pmin(pmax(vals_ndvi, -1), 1)

        terra::values(r) <- rep(mean(vals_ndvi, na.rm = TRUE),
                                terra::ncell(r))

        names(r) <- paste0("NDVI_MODIS_",
                           annee, "_",
                           sprintf("%02d", mois))

        message("NDVI MODIS téléchargé : mois ",
                sprintf("%02d", mois), "/", annee)
        message("Valeur moyenne NDVI : ",
                round(mean(vals_ndvi, na.rm = TRUE), 3))

        return(r)

      } else {
        message("Aucune donnée MODIS — données simulées utilisées")
        return(download_ndvi(annee = annee, mois = mois,
                             lon_min = lon_min, lon_max = lon_max,
                             lat_min = lat_min, lat_max = lat_max,
                             resolution = resolution,
                             simuler = TRUE))
      }

    }, error = function(e) {
      message("Erreur MODIS : ", e$message)
      message("Données simulées utilisées...")
      return(download_ndvi(annee = annee, mois = mois,
                           lon_min = lon_min, lon_max = lon_max,
                           lat_min = lat_min, lat_max = lat_max,
                           resolution = resolution,
                           simuler = TRUE))
    })
  }

  # ── Données simulées (fallback) ───────────────────────────
  message("Génération NDVI simulé pour ",
          format(as.Date(paste(annee, mois, "01", sep = "-")),
                 "%B %Y"), "...")

  r <- terra::rast(zone,
                   resolution = resolution,
                   crs = "EPSG:4326")

  set.seed(annee * 100 + mois)
  n_cells <- terra::ncell(r)
  valeurs  <- runif(n_cells, min = 0.05, max = 0.55)

  # Effet saisonnier réaliste
  if (mois %in% 6:9) {
    valeurs <- valeurs * 1.3
  } else if (mois %in% c(12, 1, 2)) {
    valeurs <- valeurs * 0.7
  }

  valeurs <- pmin(pmax(valeurs, -1), 1)
  terra::values(r) <- valeurs
  names(r) <- paste0("NDVI_", annee, "_",
                     sprintf("%02d", mois))

  message("NDVI simulé prêt : ",
          terra::nrow(r), " x ", terra::ncol(r), " pixels")

  return(r)
}
