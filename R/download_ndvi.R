#' Télécharger les données NDVI MODIS
#'
#' Télécharge les données NDVI MODIS (MOD13Q1) via le package
#' \strong{Produit :} MOD13Q1 v006, bande \code{250m_16_days_NDVI},
#' pour une période et une zone d'étude données.
#'
#' La zone d'extraction est toujours dérivée des coordonnées réelles
#' des occurrences de criquets — aucune valeur n'est simulée ou générée
#' aléatoirement.
#'
#' @param annee Année d'analyse (>= 2000). Par défaut 2023
#' @param mois Mois d'analyse : un entier (1-12) ou vecteur c(1,6,12).
#'   Par défaut 1
#' @param occurrences data.frame avec colonnes \code{latitude} et
#'   \code{longitude} (issu de \code{import_locust_data()} ou
#'   \code{clean_occurrences()}). Quand fourni, la bbox est calculée
#'   automatiquement depuis ces coordonnées réelles. Par défaut NULL
#' @param lon_min Longitude minimale. Utilisé si \code{occurrences = NULL}.
#'   Par défaut -20
#' @param lon_max Longitude maximale. Utilisé si \code{occurrences = NULL}.
#'   Par défaut 65
#' @param lat_min Latitude minimale. Utilisé si \code{occurrences = NULL}.
#'   Par défaut -10
#' @param lat_max Latitude maximale. Utilisé si \code{occurrences = NULL}.
#'   Par défaut 40
#' @param marge Marge en degrés ajoutée autour de la bbox des occurrences.
#'   Par défaut 2
#' @param resolution Résolution du raster de sortie en degrés. Par défaut 0.5
#' @param path Dossier de cache pour les fichiers téléchargés.
#'   Par défaut \code{"data/ndvi"}
#'
#' @return Un objet \code{SpatRaster} (\pkg{terra}) avec :
#'   \itemize{
#'     \item une couche par mois demandé
#'     \item valeurs NDVI réelles MODIS entre -1 et 1
#'     \item étendue spatiale couvrant la zone des occurrences + marge
#'     \item noms de couches au format \code{NDVI_YYYY_MM}
#'   }
#'
#' @details
#' \strong{Source :} MODISTools interroge l'API publique ORNL DAAC
#' (\url{https://modis.ornl.gov/rst/api/v1/}) — aucune authentification
#' requise, aucun compte NASA nécessaire.
#'
#' \strong{Produit :} MOD13A3 v006, bande \code{1_km_monthly_NDVI},
#' résolution native 1 km, agrégation mensuelle.
#'
#' \strong{Logique spatiale :} Si \code{occurrences} est fourni, la bbox
#' est calculée depuis les coordonnées réelles des observations de criquets,
#' garantissant que le raster NDVI couvre exactement les zones d'intérêt
#' pour l'extraction dans \code{\link{prepare_predictors}}.
#'
#' \strong{Rasterisation :} MODISTools retourne des pixels individuels
#' avec leurs coordonnées lon/lat. Ces pixels sont rasterisés sur une
#' grille régulière via \code{terra::rasterize()} — chaque pixel NDVI
#' est ainsi géoréférencé et extractible aux coordonnées des occurrences.
#'
#' @examples
#' \dontrun{
#' # ── Utilisation recommandée : depuis les occurrences réelles ──
#' df       <- import_locust_data(source = "gbif", limit = 200)
#' df_clean <- clean_occurrences(df)
#'
#' # La bbox est calculée automatiquement depuis les criquets observés
#' ndvi <- download_ndvi(
#'   annee       = 2023,
#'   mois        = 6,
#'   occurrences = df_clean
#' )
#' terra::plot(ndvi, main = "NDVI Juin 2023 — zone criquets")
#'
#' # ── Multi-dates ───────────────────────────────────────────────
#' ndvi_multi <- download_ndvi(
#'   annee       = 2023,
#'   mois        = c(1, 4, 7, 10),
#'   occurrences = df_clean
#' )
#' terra::plot(ndvi_multi)
#'
#' # ── Bbox explicite sans occurrences ──────────────────────────
#' ndvi <- download_ndvi(
#'   annee   = 2023,
#'   mois    = 6,
#'   lon_min = -5,
#'   lon_max = 40,
#'   lat_min = 10,
#'   lat_max = 30
#' )
#' }
#'
#' @seealso \code{\link{import_locust_data}}, \code{\link{clean_occurrences}},
#'   \code{\link{prepare_predictors}}, \code{\link{calculate_greenup}}
#'
#' @export
download_ndvi <- function(annee       = 2023,
                          mois        = 1,
                          occurrences = NULL,
                          lon_min     = -20,
                          lon_max     =  65,
                          lat_min     = -10,
                          lat_max     =  40,
                          marge       =  2,
                          resolution  =  0.5,
                          path        = "data/ndvi") {

  # ── Vérification des packages ────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }
  if (!requireNamespace("MODISTools", quietly = TRUE)) {
    stop(paste0(
      "Le package 'MODISTools' est requis.\n",
      "Installation : install.packages('MODISTools')\n",
      "Source : API ORNL DAAC — gratuit, sans compte requis."
    ))
  }

  # ── Validation des paramètres ────────────────────────────────
  if (any(mois < 1) || any(mois > 12)) {
    stop("Les mois doivent être entre 1 et 12.")
  }
  annee_actuelle <- as.integer(format(Sys.Date(), "%Y"))
  if (annee < 2000 || annee > annee_actuelle) {
    stop("L'année doit être entre 2000 et ", annee_actuelle, ".")
  }

  # ── Dériver la bbox depuis les occurrences réelles ───────────
  # Logique clé : le raster NDVI doit couvrir exactement
  # les zones où des criquets ont été observés
  if (!is.null(occurrences)) {

    cols_req <- c("latitude", "longitude")
    cols_abs <- cols_req[!cols_req %in% names(occurrences)]
    if (length(cols_abs) > 0) {
      stop("occurrences doit contenir les colonnes : ",
           paste(cols_abs, collapse = ", "))
    }

    coords_ok <- occurrences[
      !is.na(occurrences$latitude)  &
        !is.na(occurrences$longitude) &
        occurrences$latitude  >= -90  & occurrences$latitude  <= 90 &
        occurrences$longitude >= -180 & occurrences$longitude <= 180, ]

    if (nrow(coords_ok) == 0) {
      stop("Aucune coordonnée valide dans occurrences.")
    }

    # Bbox calculée depuis les coordonnées réelles + marge
    lon_min <- max(floor(min(coords_ok$longitude))   - marge, -180)
    lon_max <- min(ceiling(max(coords_ok$longitude)) + marge,  180)
    lat_min <- max(floor(min(coords_ok$latitude))    - marge,  -90)
    lat_max <- min(ceiling(max(coords_ok$latitude))  + marge,   90)

    message("Bbox calculee depuis ", nrow(coords_ok),
            " occurrences reelles :")
    message("  Longitudes : ", lon_min, " a ", lon_max)
    message("  Latitudes  : ", lat_min, " a ", lat_max)

  } else {
    message("Bbox explicite (lon ", lon_min, "/", lon_max,
            " | lat ", lat_min, "/", lat_max, ")")
  }

  zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)

  # Créer le dossier de cache
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Dossier cache cree : ", path)
  }

  # ── Téléchargement multi-dates ───────────────────────────────
  if (length(mois) > 1) {
    message("Telechargement NDVI multi-dates : ",
            length(mois), " mois...")

    liste_rasters <- lapply(mois, function(m) {
      message("  -> Mois ", sprintf("%02d", m), "/", annee)
      download_ndvi(
        annee       = annee,
        mois        = m,
        occurrences = NULL,     # bbox déjà calculée ci-dessus
        lon_min     = lon_min,
        lon_max     = lon_max,
        lat_min     = lat_min,
        lat_max     = lat_max,
        marge       = 0,
        resolution  = resolution,
        path        = path
      )
    })

    ndvi_stack <- terra::rast(liste_rasters)
    message("NDVI multi-dates pret : ",
            terra::nlyr(ndvi_stack), " couches")
    return(ndvi_stack)
  }

  # ── Fichier cache (évite de re-télécharger) ──────────────────
  nom_cache <- file.path(
    path,
    paste0("ndvi_", annee, "_", sprintf("%02d", mois),
           "_", lon_min, "_", lon_max,
           "_", lat_min, "_", lat_max, ".tif")
  )

  if (file.exists(nom_cache)) {
    message("NDVI charge depuis le cache : ", nom_cache)
    return(terra::rast(nom_cache))
  }

  # ── Téléchargement MODISTools ────────────────────────────────
  # MODISTools interroge l'API publique ORNL DAAC
  # sans authentification, retourne les pixels géoréférencés

  message("Telechargement NDVI MODIS via MODISTools...")
  message("  Produit : MOD13Q1 | Bande : 250m_16_days_NDVI")

  date_debut <- paste0(annee, "-", sprintf("%02d", mois), "-01")

  # Centre de la zone et rayon couvrant toute la bbox
  lon_centre <- (lon_min + lon_max) / 2
  lat_centre <- (lat_min + lat_max) / 2

  # Rayon en km (1 degré ≈ 111 km)
  km_lr <- min(ceiling((lon_max - lon_min) / 2 * 55), 100)
  km_ab <- min(ceiling((lat_max - lat_min) / 2 * 55), 100)

  ndvi_raw <- tryCatch({
    MODISTools::mt_subset(
      product   = "MOD13Q1",
      band      = "250m_16_days_NDVI",
      lat       = lat_centre,
      lon       = lon_centre,
      start     = date_debut,
      end       = date_debut,
      km_lr     = km_lr,
      km_ab     = km_ab,
      site_name = "locust_zone",
      internal  = TRUE,
      progress  = FALSE
    )
  }, error = function(e) {
    stop(paste0(
      "Echec du telechargement MODISTools : ", e$message, "\n",
      "Verifiez votre connexion internet.\n",
      "API utilisee : https://modis.ornl.gov/rst/api/v1/\n",
      "(Gratuit, sans inscription)"
    ))
  })

  if (is.null(ndvi_raw) || nrow(ndvi_raw) == 0) {
    stop(paste0(
      "MODISTools n'a retourne aucun pixel pour cette zone/periode.\n",
      "Zone : lon [", lon_min, ", ", lon_max, "] ",
      "lat [", lat_min, ", ", lat_max, "]\n",
      "Periode : ", date_debut, "\n",
      "Conseil : verifiez que la zone contient des terres emergees."
    ))
  }

  message("  Pixels MODIS recus : ", nrow(ndvi_raw))

  # ── Vérifier que les colonnes géographiques sont présentes ───
  cols_geo <- c("longitude", "latitude", "value")
  cols_abs  <- cols_geo[!cols_geo %in% names(ndvi_raw)]
  if (length(cols_abs) > 0) {
    stop("Colonnes manquantes dans la reponse MODISTools : ",
         paste(cols_abs, collapse = ", "))
  }

  # ── Appliquer le facteur d'échelle MODIS (0.0001) ────────────
  ndvi_raw$ndvi_reel <- ndvi_raw$value * 0.0001
  ndvi_raw$ndvi_reel <- pmin(pmax(ndvi_raw$ndvi_reel, -1), 1)

  # Filtrer les valeurs NDVI invalides (fill value MODIS = -3000)
  ndvi_raw <- ndvi_raw[!is.na(ndvi_raw$ndvi_reel), ]
  ndvi_raw <- ndvi_raw[ndvi_raw$value > -3000, ]

  if (nrow(ndvi_raw) == 0) {
    stop(paste0(
      "Aucune valeur NDVI valide apres filtrage.\n",
      "La zone est peut-etre couverte de nuages ou d'ocean."
    ))
  }

  # ── Rasterisation : pixels géoréférencés → grille régulière ──
  # C'est l'étape clé : chaque pixel MODISTools a ses propres
  # coordonnées lon/lat → on les rasterise sur la grille cible
  pts_ndvi <- terra::vect(
    data.frame(
      x     = ndvi_raw$longitude,
      y     = ndvi_raw$latitude,
      value = ndvi_raw$ndvi_reel
    ),
    geom = c("x", "y"),
    crs  = "EPSG:4326"
  )

  # Grille cible à la résolution souhaitée
  r_cible <- terra::rast(
    zone,
    resolution = resolution,
    crs        = "EPSG:4326"
  )

  # Rasterisation — moyenne si plusieurs pixels tombent dans la même cellule
  r_ndvi <- terra::rasterize(
    pts_ndvi, r_cible,
    field = "value",
    fun   = "mean"
  )

  names(r_ndvi) <- paste0("NDVI_MODIS_", annee, "_",
                          sprintf("%02d", mois))

  # ── Résumé ───────────────────────────────────────────────────
  n_valides <- sum(!is.na(terra::values(r_ndvi)))
  n_total   <- terra::ncell(r_ndvi)
  ndvi_moy  <- round(mean(terra::values(r_ndvi), na.rm = TRUE), 3)

  message("NDVI MODIS pret :")
  message("  Pixels valides : ", n_valides, " / ", n_total)
  message("  NDVI moyen     : ", ndvi_moy)
  message("  Etendue        : lon [", lon_min, ", ", lon_max,
          "] lat [", lat_min, ", ", lat_max, "]")

  # ── Sauvegarder dans le cache ────────────────────────────────
  terra::writeRaster(r_ndvi, nom_cache, overwrite = TRUE)
  message("NDVI sauvegarde en cache : ", nom_cache)

  return(r_ndvi)
}

