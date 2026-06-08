#' Importer les données d'occurrence des criquets pèlerins
#'
#' Importe un fichier CSV contenant les observations de criquets pèlerins,
#' vérifie les colonnes obligatoires, supprime les doublons et les
#' coordonnées manquantes. Convertit en objet sf spatial.
#'
#' @param filepath Chemin vers le fichier CSV des occurrences
#' @param lat_col Nom de la colonne latitude. Par défaut "latitude"
#' @param lon_col Nom de la colonne longitude. Par défaut "longitude"
#' @param date_col Nom de la colonne date. Par défaut "date"
#'
#' @return Un data.frame nettoyé avec les colonnes :
#'   \item{latitude}{Latitude de l'observation}
#'   \item{longitude}{Longitude de l'observation}
#'   \item{date}{Date de l'observation}
#'   \item{presence}{Présence du criquet (1)}
#'
#' @examples
#' \dontrun{
#' data <- import_locust_data("occurrences.csv")
#' head(data)
#' }
#'
#' @export
import_locust_data <- function(filepath,
                               lat_col  = "latitude",
                               lon_col  = "longitude",
                               date_col = "date") {

  # Vérifier que le fichier existe
  if (!file.exists(filepath)) {
    stop("Fichier introuvable : ", filepath)
  }

  # Lire le fichier CSV
  df <- read.csv(filepath, stringsAsFactors = FALSE)

  # Vérifier les colonnes obligatoires
  cols_requises <- c(lat_col, lon_col, date_col)
  cols_manquantes <- cols_requises[!cols_requises %in% names(df)]

  if (length(cols_manquantes) > 0) {
    stop("Colonnes manquantes dans le fichier : ",
         paste(cols_manquantes, collapse = ", "))
  }

  # Renommer les colonnes en noms standards
  df <- df[, cols_requises]
  names(df) <- c("latitude", "longitude", "date")

  # Supprimer les lignes avec coordonnées manquantes
  n_avant <- nrow(df)
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  n_apres <- nrow(df)

  if (n_avant > n_apres) {
    message(n_avant - n_apres, " ligne(s) supprimée(s) : coordonnées manquantes")
  }

  # Supprimer les doublons
  n_avant <- nrow(df)
  df <- unique(df)
  n_apres <- nrow(df)

  if (n_avant > n_apres) {
    message(n_avant - n_apres, " doublon(s) supprimé(s)")
  }

  # Ajouter colonne présence
  df$presence <- 1

  # Convertir la date
  df$date <- as.Date(df$date)

  # Conversion en objet sf (spatial)
  if (requireNamespace("sf", quietly = TRUE)) {
    df_sf <- sf::st_as_sf(df,
                          coords = c("longitude", "latitude"),
                          crs    = 4326)
    message("Objet sf créé avec ", nrow(df_sf), " points")
    attr(df, "sf") <- df_sf
  }

  message("Import réussi : ", nrow(df), " occurrences chargées")

  return(df)
}
