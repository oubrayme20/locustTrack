#' Importer les données d'occurrence des criquets pèlerins
#'
#' Importe les données de criquets pèlerins depuis trois sources :
#' un fichier CSV local, GBIF directement via rgbif,
#' ou FAO Locust Hub. Nettoie et convertit en objet spatial.
#'
#' @param filepath Chemin vers le fichier CSV. Par défaut NULL
#' @param source Source des données : "csv", "gbif", "fao".
#'   Par défaut "csv"
#' @param lat_col Nom colonne latitude. Par défaut "latitude"
#' @param lon_col Nom colonne longitude. Par défaut "longitude"
#' @param date_col Nom colonne date. Par défaut "date"
#' @param limit Nombre max d'occurrences GBIF. Par défaut 500
#'
#' @return Un data.frame nettoyé avec les colonnes :
#'   \item{latitude}{Latitude de l'observation}
#'   \item{longitude}{Longitude de l'observation}
#'   \item{date}{Date de l'observation}
#'   \item{presence}{Présence du criquet (1)}
#'
#' @examples
#' \dontrun{
#' # Depuis CSV local
#' df <- import_locust_data("occurrences.csv", source = "csv")
#'
#' # Depuis GBIF directement
#' df <- import_locust_data(source = "gbif", limit = 200)
#'
#' # Depuis FAO Locust Hub
#' df <- import_locust_data(source = "fao")
#'
#' head(df)
#' }
#'
#' @export
import_locust_data <- function(filepath = NULL,
                               source   = "csv",
                               lat_col  = "latitude",
                               lon_col  = "longitude",
                               date_col = "date",
                               limit    = 500) {

  # ── Source GBIF ───────────────────────────────────────────
  if (source == "gbif") {

    if (!requireNamespace("rgbif", quietly = TRUE)) {
      message("Installation de rgbif...")
      install.packages("rgbif")
    }

    message("Téléchargement depuis GBIF — Schistocerca gregaria...")

    tryCatch({
      gbif_data <- rgbif::occ_search(
        scientificName = "Schistocerca gregaria",
        hasCoordinate  = TRUE,
        limit          = limit
      )

      df_raw <- gbif_data$data

      if (is.null(df_raw) || nrow(df_raw) == 0) {
        stop("Aucune donnée GBIF disponible")
      }

      df <- data.frame(
        latitude  = df_raw$decimalLatitude,
        longitude = df_raw$decimalLongitude,
        date      = as.Date(df_raw$eventDate),
        presence  = 1
      )

      message("GBIF : ", nrow(df), " occurrences téléchargées")

    }, error = function(e) {
      stop("Erreur GBIF : ", e$message,
           "\nVérifiez votre connexion internet")
    })

    # ── Source FAO Locust Hub ─────────────────────────────────
  } else if (source == "fao") {

    message("Téléchargement depuis FAO Locust Hub...")

    tryCatch({
      # URL FAO Locust Hub API
      fao_url <- paste0(
        "https://locust-hub-hqfao.hub.arcgis.com/",
        "datasets/FAO::locust-presence-data.geojson"
      )

      if (!requireNamespace("sf", quietly = TRUE)) {
        install.packages("sf")
      }

      fao_data <- sf::st_read(fao_url, quiet = TRUE)

      coords <- sf::st_coordinates(fao_data)

      df <- data.frame(
        latitude  = coords[, 2],
        longitude = coords[, 1],
        date      = as.Date(fao_data$STARTDATE),
        presence  = 1
      )

      message("FAO : ", nrow(df), " occurrences téléchargées")

    }, error = function(e) {
      message("Erreur FAO : ", e$message)
      message("Vérifiez votre connexion ou utilisez source='csv'")
      stop(e$message)
    })

    # ── Source CSV local ──────────────────────────────────────
  } else {

    if (is.null(filepath)) {
      stop("filepath requis pour source='csv'")
    }

    if (!file.exists(filepath)) {
      stop("Fichier introuvable : ", filepath)
    }

    message("Chargement depuis CSV : ", filepath)

    df_raw <- read.csv(filepath, stringsAsFactors = FALSE)

    # Vérifier colonnes
    cols_requises   <- c(lat_col, lon_col, date_col)
    cols_manquantes <- cols_requises[
      !cols_requises %in% names(df_raw)]

    if (length(cols_manquantes) > 0) {
      stop("Colonnes manquantes : ",
           paste(cols_manquantes, collapse = ", "))
    }

    df <- df_raw[, cols_requises]
    names(df) <- c("latitude", "longitude", "date")
    df$date     <- as.Date(df$date)
    df$presence <- 1
  }

  # ── Nettoyage commun ──────────────────────────────────────
  # Supprimer NA
  n_avant <- nrow(df)
  df <- df[!is.na(df$latitude) &
             !is.na(df$longitude) &
             !is.na(df$date), ]

  if (n_avant > nrow(df)) {
    message(n_avant - nrow(df),
            " ligne(s) supprimée(s) : valeurs manquantes")
  }

  # Supprimer doublons
  n_avant <- nrow(df)
  df <- unique(df)

  if (n_avant > nrow(df)) {
    message(n_avant - nrow(df), " doublon(s) supprimé(s)")
  }

  # ── Conversion en objet sf spatial ────────────────────────
  if (requireNamespace("sf", quietly = TRUE)) {
    df_sf <- sf::st_as_sf(
      df,
      coords = c("longitude", "latitude"),
      crs    = 4326
    )
    message("Objet sf créé avec ", nrow(df_sf), " points")
    attr(df, "sf") <- df_sf
  }

  message("Import réussi (", source, ") : ",
          nrow(df), " occurrences chargées")

  return(df)
}
