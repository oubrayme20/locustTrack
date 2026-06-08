#' Nettoyer les occurrences spatiales des criquets pèlerins
#'
#' Supprime les coordonnées invalides, les points marins,
#' les points en dehors de la zone Afrique/Maghreb/Moyen-Orient,
#' les doublons spatiaux et détecte les outliers simples.
#'
#' @param df data.frame issu de \code{import_locust_data()}
#' @param lon_min Longitude minimale de la zone. Par défaut -20
#' @param lon_max Longitude maximale de la zone. Par défaut 65
#' @param lat_min Latitude minimale de la zone. Par défaut -10
#' @param lat_max Latitude maximale de la zone. Par défaut 40
#' @param seuil_outlier Seuil Z-score pour détection outliers.
#'   Par défaut 3
#'
#' @return Un data.frame avec les occurrences nettoyées
#'
#' @examples
#' \dontrun{
#' df_raw   <- import_locust_data("occurrences.csv")
#' df_clean <- clean_occurrences(df_raw)
#' nrow(df_clean)
#' }
#'
#' @export
clean_occurrences <- function(df,
                              lon_min       = -20,
                              lon_max       =  65,
                              lat_min       = -10,
                              lat_max       =  40,
                              seuil_outlier =   3) {

  # Vérifier les colonnes
  cols_requises <- c("latitude", "longitude")
  cols_manquantes <- cols_requises[!cols_requises %in% names(df)]

  if (length(cols_manquantes) > 0) {
    stop("Colonnes manquantes : ",
         paste(cols_manquantes, collapse = ", "),
         ". Utilisez d'abord import_locust_data()")
  }

  n_initial <- nrow(df)

  # ── 1. Supprimer coordonnées invalides ────────────────────
  df <- df[df$latitude  >= -90  & df$latitude  <= 90  &
             df$longitude >= -180 & df$longitude <= 180, ]

  n_invalides <- n_initial - nrow(df)
  if (n_invalides > 0) {
    message(n_invalides,
            " point(s) supprimé(s) : coordonnées invalides")
  }

  # ── 2. Suppression des points marins ──────────────────────
  n_avant <- nrow(df)
  df <- df[!(df$longitude < -17 & df$latitude < 15), ]
  df <- df[!(df$longitude >  55 & df$latitude < 10), ]

  n_marins <- n_avant - nrow(df)
  if (n_marins > 0) {
    message(n_marins,
            " point(s) supprimé(s) : probables points marins")
  }

  # ── 3. Filtrage spatial Afrique/Maghreb/Moyen-Orient ──────
  n_avant <- nrow(df)
  df <- df[df$longitude >= lon_min & df$longitude <= lon_max &
             df$latitude  >= lat_min & df$latitude  <= lat_max, ]

  n_hors_zone <- n_avant - nrow(df)
  if (n_hors_zone > 0) {
    message(n_hors_zone,
            " point(s) supprimé(s) : hors zone d'étude")
  }

  # ── 4. Suppression doublons spatiaux ──────────────────────
  n_avant <- nrow(df)
  df <- df[!duplicated(df[, c("latitude", "longitude")]), ]

  n_doublons <- n_avant - nrow(df)
  if (n_doublons > 0) {
    message(n_doublons, " doublon(s) spatial(aux) supprimé(s)")
  }

  # ── 5. Détection outliers par Z-score ─────────────────────
  if (nrow(df) > 3) {

    z_lat <- abs((df$latitude  - mean(df$latitude))  /
                   sd(df$latitude))
    z_lon <- abs((df$longitude - mean(df$longitude)) /
                   sd(df$longitude))

    idx_outliers <- which(z_lat > seuil_outlier |
                            z_lon > seuil_outlier)

    if (length(idx_outliers) > 0) {
      message(length(idx_outliers),
              " outlier(s) détecté(s) (Z-score > ",
              seuil_outlier, ") et supprimé(s)")
      df <- df[-idx_outliers, ]
    } else {
      message("Aucun outlier détecté")
    }
  }

  # ── Résumé final ──────────────────────────────────────────
  message("Nettoyage terminé : ", nrow(df),
          " occurrences valides conservées (",
          n_initial - nrow(df), " supprimées au total)")

  return(df)
}
