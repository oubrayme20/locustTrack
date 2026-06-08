#' Préparer les variables environnementales pour la modélisation
#'
#' Extrait les valeurs climatiques (température, précipitations) et NDVI
#' pour chaque point d'occurrence, supprime les variables trop corrélées
#' et génère un dataset prêt pour la modélisation Random Forest.
#'
#' @param occurrences data.frame des occurrences nettoyées (issu de clean_occurrences)
#' @param climat SpatRaster des variables climatiques (issu de download_climate_data)
#' @param ndvi SpatRaster NDVI (issu de download_ndvi)
#' @param greenup SpatRaster greenup optionnel (issu de calculate_greenup). Par défaut NULL
#' @param seuil_cor Seuil de corrélation pour suppression. Par défaut 0.9
#'
#' @return Un data.frame avec les colonnes :
#'   \item{latitude}{Latitude du point}
#'   \item{longitude}{Longitude du point}
#'   \item{presence}{1 = présence, 0 = absence}
#'   \item{...}{Variables climatiques et NDVI extraites}
#'
#' @examples
#' \dontrun{
#' df_clean <- clean_occurrences(import_locust_data("data.csv"))
#' clim     <- download_climate_data(var = "prec")
#' ndvi     <- download_ndvi(2023, mois = 6)
#' dataset  <- prepare_predictors(df_clean, clim, ndvi)
#' head(dataset)
#' }
#'
#' @export
prepare_predictors <- function(occurrences,
                               climat,
                               ndvi,
                               greenup    = NULL,
                               seuil_cor  = 0.9) {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis. Installez-le avec : install.packages('terra')")
  }

  # Vérifier les colonnes
  cols_requises <- c("latitude", "longitude", "presence")
  cols_manquantes <- cols_requises[!cols_requises %in% names(occurrences)]
  if (length(cols_manquantes) > 0) {
    stop("Colonnes manquantes : ", paste(cols_manquantes, collapse = ", "))
  }

  message("Extraction des variables environnementales pour ",
          nrow(occurrences), " points...")

  # Convertir les points en matrice lon/lat
  coords <- cbind(occurrences$longitude, occurrences$latitude)

  # Extraire les valeurs climatiques
  vals_climat <- terra::extract(climat, coords)
  vals_climat <- vals_climat[, -1, drop = FALSE]

  # Renommer les colonnes climatiques
  n_clim <- ncol(vals_climat)
  if (n_clim == 12) {
    names(vals_climat) <- paste0("clim_", sprintf("%02d", 1:12))
  } else {
    names(vals_climat) <- paste0("clim_", 1:n_clim)
  }

  # Extraire les valeurs NDVI
  vals_ndvi <- terra::extract(ndvi, coords)
  vals_ndvi <- vals_ndvi[, -1, drop = FALSE]
  names(vals_ndvi) <- "ndvi"

  # Construire le dataset
  dataset <- data.frame(
    latitude  = occurrences$latitude,
    longitude = occurrences$longitude,
    presence  = occurrences$presence,
    vals_climat,
    vals_ndvi
  )

  # Ajouter greenup si fourni
  if (!is.null(greenup)) {
    vals_greenup <- terra::extract(greenup, coords)
    vals_greenup <- vals_greenup[, -1, drop = FALSE]
    names(vals_greenup) <- "greenup"
    dataset <- cbind(dataset, vals_greenup)
    message("Variable greenup ajoutée")
  }

  # Supprimer les lignes avec NA
  n_avant <- nrow(dataset)
  dataset <- dataset[complete.cases(dataset), ]
  n_apres <- nrow(dataset)

  if (n_avant > n_apres) {
    message(n_avant - n_apres, " ligne(s) supprimée(s) : valeurs manquantes")
  }

  # Suppression des variables trop corrélées (|r| > seuil_cor)
  vars_num <- dataset[, !names(dataset) %in% c("latitude", "longitude", "presence")]

  if (ncol(vars_num) > 1) {
    cor_matrix <- cor(vars_num, use = "complete.obs")
    cor_haute  <- which(abs(cor_matrix) > seuil_cor &
                          upper.tri(cor_matrix), arr.ind = TRUE)

    if (nrow(cor_haute) > 0) {
      vars_suppr <- unique(rownames(cor_haute))
      dataset    <- dataset[, !names(dataset) %in% vars_suppr]
      message(length(vars_suppr), " variable(s) trop corrélée(s) supprimée(s) : ",
              paste(vars_suppr, collapse = ", "))
    } else {
      message("Aucune variable trop corrélée détectée")
    }
  }

  message("Dataset final : ", nrow(dataset), " observations x ",
          ncol(dataset), " variables")

  return(dataset)
}
