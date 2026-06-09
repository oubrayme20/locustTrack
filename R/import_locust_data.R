#' Importer les données d'occurrence des criquets pèlerins
#'
#' Importe les données de criquets pèlerins depuis quatre sources :
#' GBIF via rgbif, FAO Locust Hub via API ArcGIS REST, iNaturalist
#' via API REST, ou un fichier CSV local.
#'
#' @param filepath Chemin vers le fichier CSV. Par défaut NULL
#' @param source Source des données : "gbif", "fao", "inaturalist", "csv".
#'   Par défaut "gbif"
#' @param lat_col Nom colonne latitude (pour source = "csv").
#'   Par défaut "latitude"
#' @param lon_col Nom colonne longitude (pour source = "csv").
#'   Par défaut "longitude"
#' @param date_col Nom colonne date (pour source = "csv").
#'   Par défaut "date"
#' @param limit Nombre max d'occurrences. Par défaut 500
#'
#' @return Un data.frame avec les colonnes :
#'   \item{latitude}{Latitude de l'observation (degrés décimaux)}
#'   \item{longitude}{Longitude de l'observation (degrés décimaux)}
#'   \item{date}{Date de l'observation (format Date)}
#'   \item{presence}{Présence du criquet (toujours 1)}
#'   L'attribut \code{attr(result, "sf")} contient l'objet sf correspondant.
#'
#' @details
#' \strong{Sources disponibles :}
#' \itemize{
#'   \item \strong{gbif} (recommandée) — GBIF via \pkg{rgbif} :
#'     \url{https://www.gbif.org/species/1711088}.
#'     Données validées, coordonnées géoréférencées, intègre
#'     les données partenaires FAO/DLIS.
#'   \item \strong{fao} — FAO Locust Hub via API ArcGIS REST :
#'     \url{https://locust-hub-hqfao.hub.arcgis.com}.
#'     Données officielles FAO DLIS (Desert Locust Information Service).
#'     Si indisponible, bascule automatiquement sur iNaturalist puis GBIF.
#'   \item \strong{inaturalist} — API publique iNaturalist :
#'     \url{https://api.inaturalist.org/v1/}.
#'     Données citoyennes géoréférencées.
#'   \item \strong{csv} — Fichier local. Nécessite les colonnes
#'     latitude, longitude, date.
#' }
#'
#' @examples
#' \dontrun{
#' # Source recommandée : GBIF
#' df <- import_locust_data(source = "gbif", limit = 200)
#'
#' # FAO Locust Hub (fallback automatique si indisponible)
#' df <- import_locust_data(source = "fao", limit = 200)
#'
#' # iNaturalist
#' df <- import_locust_data(source = "inaturalist", limit = 100)
#'
#' # CSV local
#' df <- import_locust_data("occurrences.csv", source = "csv")
#'
#' head(df)
#' }
#'
#' @seealso \code{\link{clean_occurrences}}, \code{\link{download_ndvi}}
#'
#' @export
import_locust_data <- function(filepath = NULL,
                               source   = "gbif",
                               lat_col  = "latitude",
                               lon_col  = "longitude",
                               date_col = "date",
                               limit    = 500) {

  # ── Source GBIF ───────────────────────────────────────────────
  if (source == "gbif") {

    if (!requireNamespace("rgbif", quietly = TRUE)) {
      stop(paste0(
        "Le package 'rgbif' est requis.\n",
        "Installation : install.packages('rgbif')"
      ))
    }

    message("Telechargement depuis GBIF — Schistocerca gregaria...")
    message("  URL : https://www.gbif.org/species/1711088")

    tryCatch({
      gbif_data <- rgbif::occ_search(
        scientificName = "Schistocerca gregaria",
        hasCoordinate  = TRUE,
        limit          = limit
      )

      df_raw <- gbif_data$data

      if (is.null(df_raw) || nrow(df_raw) == 0) {
        stop("Aucune donnee GBIF disponible pour Schistocerca gregaria")
      }

      df <- data.frame(
        latitude  = df_raw$decimalLatitude,
        longitude = df_raw$decimalLongitude,
        date      = as.Date(
          substr(as.character(df_raw$eventDate), 1, 10)
        ),
        presence  = 1
      )

      message("GBIF : ", nrow(df), " occurrences telechargees")

    }, error = function(e) {
      stop("Erreur GBIF : ", e$message)
    })

    # ── Source FAO Locust Hub ─────────────────────────────────────
  } else if (source == "fao") {

    message("Telechargement depuis FAO Locust Hub (DLIS)...")
    df <- NULL

    # ── Essai 1 : API ArcGIS REST — endpoint principal ────────
    # URL stable documentée par le FAO Locust Hub
    # Format : ArcGIS Feature Service REST API (JSON/GeoJSON)
    fao_endpoints <- c(
      # Endpoint ArcGIS REST Service FAO DLIS
      paste0(
        "https://services5.arcgis.com/ug8GbBKiJj0xJJCn/arcgis/rest/",
        "services/Locust_Presence_Data/FeatureServer/0/query",
        "?where=1%3D1",
        "&outFields=LONGITUDE%2CLATITUDE%2CSTARTDATE",
        "&resultRecordCount=", limit,
        "&f=geojson"
      ),
      # Endpoint alternatif via le portail open data FAO
      paste0(
        "https://locust-hub-hqfao.hub.arcgis.com/api/download/v1/",
        "items/locust-presence-data/geojson",
        "?layers=0"
      )
    )

    if (!requireNamespace("sf", quietly = TRUE)) {
      stop(paste0(
        "Le package 'sf' est requis pour la source FAO.\n",
        "Installation : install.packages('sf')"
      ))
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop(paste0(
        "Le package 'jsonlite' est requis pour la source FAO.\n",
        "Installation : install.packages('jsonlite')"
      ))
    }

    for (url_fao in fao_endpoints) {
      if (!is.null(df)) break

      tryCatch({
        message("  Essai endpoint FAO : ")
        message("  ", substr(url_fao, 1, 60), "...")

        fao_raw  <- jsonlite::fromJSON(url_fao)
        features <- fao_raw$features

        if (!is.null(features) && length(features) > 0) {

          # Extraire coordonnées et dates depuis GeoJSON
          coords <- do.call(rbind, lapply(
            features$geometry$coordinates,
            function(x) if (length(x) >= 2) c(x[1], x[2]) else c(NA, NA)
          ))

          props  <- features$properties

          # Chercher la colonne de date (STARTDATE ou DATE)
          col_date <- intersect(
            c("STARTDATE", "DATE", "startdate", "date"),
            names(props)
          )
          date_val <- if (length(col_date) > 0) {
            as.Date(substr(as.character(props[[col_date[1]]]), 1, 10))
          } else {
            as.Date(NA)
          }

          df <- data.frame(
            latitude  = as.numeric(coords[, 2]),
            longitude = as.numeric(coords[, 1]),
            date      = date_val,
            presence  = 1
          )

          df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
          message("FAO Locust Hub : ", nrow(df),
                  " occurrences telechargees")
        }

      }, error = function(e) {
        message("  Endpoint FAO indisponible : ", e$message)
      })
    }

    # ── Essai 2 : iNaturalist (fallback) ──────────────────────
    if (is.null(df) || nrow(df) == 0) {
      message("FAO indisponible — utilisation iNaturalist...")

      tryCatch({
        url_inat <- paste0(
          "https://api.inaturalist.org/v1/observations?",
          "taxon_name=Schistocerca+gregaria&",
          "has_geo=true&",
          "per_page=", min(limit, 200), "&",
          "order=desc&order_by=created_at"
        )

        data_inat <- jsonlite::fromJSON(url_inat)
        obs       <- data_inat$results

        if (!is.null(obs) && nrow(obs) > 0) {
          lats <- sapply(obs$geojson$coordinates,
                         function(x) if (length(x) >= 2) x[2] else NA)
          lons <- sapply(obs$geojson$coordinates,
                         function(x) if (length(x) >= 1) x[1] else NA)

          df <- data.frame(
            latitude  = as.numeric(lats),
            longitude = as.numeric(lons),
            date      = as.Date(substr(obs$observed_on, 1, 10)),
            presence  = 1
          )
          df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
          message("iNaturalist (fallback) : ", nrow(df),
                  " occurrences telechargees")
        }

      }, error = function(e) {
        message("  iNaturalist indisponible : ", e$message)
      })
    }

    # ── Essai 3 : GBIF (dernier recours) ──────────────────────
    if (is.null(df) || nrow(df) == 0) {
      message("Utilisation GBIF comme dernier recours...")

      if (!requireNamespace("rgbif", quietly = TRUE)) {
        stop(paste0(
          "Le package 'rgbif' est requis.\n",
          "Installation : install.packages('rgbif')"
        ))
      }

      tryCatch({
        gbif_data <- rgbif::occ_search(
          scientificName = "Schistocerca gregaria",
          hasCoordinate  = TRUE,
          limit          = limit
        )
        df_raw <- gbif_data$data
        df <- data.frame(
          latitude  = df_raw$decimalLatitude,
          longitude = df_raw$decimalLongitude,
          date      = as.Date(
            substr(as.character(df_raw$eventDate), 1, 10)
          ),
          presence  = 1
        )
        message("GBIF (fallback final) : ", nrow(df),
                " occurrences chargees")

      }, error = function(e) {
        stop(paste0(
          "Toutes les sources ont echoue (FAO, iNaturalist, GBIF).\n",
          "Verifiez votre connexion internet."
        ))
      })
    }

    # ── Source iNaturalist directe ────────────────────────────────
  } else if (source == "inaturalist") {

    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop(paste0(
        "Le package 'jsonlite' est requis.\n",
        "Installation : install.packages('jsonlite')"
      ))
    }

    message("Telechargement depuis iNaturalist...")
    message("  API : https://api.inaturalist.org/v1/")

    tryCatch({
      url_inat <- paste0(
        "https://api.inaturalist.org/v1/observations?",
        "taxon_name=Schistocerca+gregaria&",
        "has_geo=true&",
        "per_page=", min(limit, 200), "&",
        "order=desc&order_by=created_at"
      )

      data_inat <- jsonlite::fromJSON(url_inat)
      obs       <- data_inat$results

      if (is.null(obs) || nrow(obs) == 0) {
        stop("Aucune observation iNaturalist disponible")
      }

      lats <- sapply(obs$geojson$coordinates,
                     function(x) if (length(x) >= 2) x[2] else NA)
      lons <- sapply(obs$geojson$coordinates,
                     function(x) if (length(x) >= 1) x[1] else NA)

      df <- data.frame(
        latitude  = as.numeric(lats),
        longitude = as.numeric(lons),
        date      = as.Date(substr(obs$observed_on, 1, 10)),
        presence  = 1
      )
      df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
      message("iNaturalist : ", nrow(df), " occurrences telechargees")

    }, error = function(e) {
      stop("Erreur iNaturalist : ", e$message)
    })

    # ── Source CSV local ──────────────────────────────────────────
  } else if (source == "csv") {

    if (is.null(filepath)) {
      stop("filepath requis pour source = 'csv'")
    }
    if (!file.exists(filepath)) {
      stop("Fichier introuvable : ", filepath)
    }

    message("Chargement depuis CSV : ", filepath)
    df_raw <- read.csv(filepath, stringsAsFactors = FALSE)

    cols_requises   <- c(lat_col, lon_col, date_col)
    cols_manquantes <- cols_requises[
      !cols_requises %in% names(df_raw)]

    if (length(cols_manquantes) > 0) {
      stop("Colonnes manquantes dans le CSV : ",
           paste(cols_manquantes, collapse = ", "))
    }

    df <- df_raw[, cols_requises, drop = FALSE]
    names(df) <- c("latitude", "longitude", "date")
    df$date     <- as.Date(df$date)
    df$presence <- 1
    message("CSV charge : ", nrow(df), " lignes")

  } else {
    stop("Source invalide : '", source,
         "'. Choisissez parmi : 'gbif', 'fao', 'inaturalist', 'csv'")
  }

  # ── Nettoyage commun ──────────────────────────────────────────
  n_avant <- nrow(df)
  df <- df[!is.na(df$latitude)  &
             !is.na(df$longitude) &
             !is.na(df$date), ]
  if (n_avant > nrow(df)) {
    message(n_avant - nrow(df),
            " ligne(s) supprimee(s) : valeurs manquantes")
  }

  n_avant <- nrow(df)
  df      <- unique(df)
  if (n_avant > nrow(df)) {
    message(n_avant - nrow(df), " doublon(s) supprime(s)")
  }

  # ── Conversion en objet sf spatial ───────────────────────────
  if (requireNamespace("sf", quietly = TRUE)) {
    df_sf <- sf::st_as_sf(
      df,
      coords = c("longitude", "latitude"),
      crs    = 4326
    )
    attr(df, "sf") <- df_sf
    message("Objet sf cree avec ", nrow(df_sf), " points")
  }

  message("Import reussi (", source, ") : ",
          nrow(df), " occurrences chargees")

  return(df)
}
