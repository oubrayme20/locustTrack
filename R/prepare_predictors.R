#' Préparer les variables environnementales pour la modélisation
#'
#' Extrait les valeurs climatiques (température, précipitations/pluie)
#' et NDVI pour chaque point d'occurrence, supprime les variables
#' trop corrélées et génère un dataset prêt pour Random Forest.
#'
#' Variables extraites conformément au guide :
#' \itemize{
#'   \item \strong{pluie} — précipitations via raster climatique
#'   \item \strong{température} — via raster climatique
#'   \item \strong{NDVI} — indice de végétation
#'   \item \strong{anomalie végétation} — via raster greenup
#' }
#'
#' @param occurrences data.frame des occurrences nettoyées
#'   (issu de \code{clean_occurrences()})
#' @param climat SpatRaster des variables climatiques
#'   (pluie + température, issu de \code{download_climate_data()})
#' @param ndvi SpatRaster NDVI (issu de \code{download_ndvi()})
#' @param greenup SpatRaster anomalie végétation optionnel
#'   (issu de \code{calculate_greenup()}). Par défaut NULL
#' @param seuil_cor Seuil corrélation pour suppression variables.
#'   Par défaut 0.9
#'
#' @return Un data.frame avec les colonnes :
#'   \item{latitude}{Latitude du point}
#'   \item{longitude}{Longitude du point}
#'   \item{presence}{1 = présence, 0 = absence}
#'   \item{clim_XX}{Variables pluie et température extraites}
#'   \item{ndvi}{Indice de végétation NDVI}
#'   \item{greenup}{Anomalie végétation (si fourni)}
#'
#' @examples
#' \dontrun{
#' df_clean <- clean_occurrences(import_locust_data("data.csv"))
#'
#' # Précipitations (pluie)
#' clim_prec <- download_climate_data(var = "prec")
#'
#' # Température
#' clim_temp <- download_climate_data(var = "tavg")
#'
#' # Empiler pluie + température
#' clim_stack <- c(clim_prec, clim_temp)
#'
#' # NDVI
#' ndvi <- download_ndvi(2023, mois = 6)
#'
#' # Anomalie végétation
#' greenup <- calculate_greenup(ndvi_jan, ndvi_jul)
#'
#' # Préparer dataset ML
#' dataset <- prepare_predictors(df_clean,
#'                                climat  = clim_stack,
#'                                ndvi    = ndvi,
#'                                greenup = greenup$anomalie)
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
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Vérifier les colonnes
  cols_requises <- c("latitude", "longitude", "presence")
  cols_manquantes <- cols_requises[
    !cols_requises %in% names(occurrences)]

  if (length(cols_manquantes) > 0) {
    stop("Colonnes manquantes : ",
         paste(cols_manquantes, collapse = ", "))
  }

  message("Extraction des variables environnementales pour ",
          nrow(occurrences), " points...")
  message("Variables : pluie + température (clim) + NDVI",
          if (!is.null(greenup)) " + anomalie végétation" else "")

  # Convertir les points en matrice lon/lat
  coords <- cbind(occurrences$longitude, occurrences$latitude)

  # ── Extraire variables climatiques (pluie + température) ──
  vals_climat <- terra::extract(climat, coords)

  # Supprimer colonne ID si présente
  if ("ID" %in% names(vals_climat)) {
    vals_climat <- vals_climat[, -which(names(vals_climat) == "ID"),
                               drop = FALSE]
  } else if (ncol(vals_climat) > 1) {
    vals_climat <- vals_climat[, -1, drop = FALSE]
  }

  # Vérifier qu'il y a des colonnes
  if (ncol(vals_climat) == 0) {
    stop("Aucune valeur climatique extraite. ",
         "Vérifiez que les coordonnées sont dans l'étendue du raster.")
  }

  n_clim <- ncol(vals_climat)
  if (n_clim == 12) {
    names(vals_climat) <- paste0("clim_", sprintf("%02d", 1:12))
  } else {
    names(vals_climat) <- paste0("clim_", 1:n_clim)
  }

  # ── Extraire NDVI ─────────────────────────────────────────
  vals_ndvi <- terra::extract(ndvi, coords)

  # Supprimer colonne ID si présente
  if ("ID" %in% names(vals_ndvi)) {
    vals_ndvi <- vals_ndvi[, -which(names(vals_ndvi) == "ID"),
                           drop = FALSE]
  } else if (ncol(vals_ndvi) > 1) {
    vals_ndvi <- vals_ndvi[, -1, drop = FALSE]
  }

  # Vérifier qu'il y a des valeurs NDVI
  if (ncol(vals_ndvi) == 0) {
    stop("Aucune valeur NDVI extraite. ",
         "Vérifiez que les coordonnées sont dans l'étendue du raster NDVI.")
  }

  names(vals_ndvi) <- "ndvi"

  # ── Construire le dataset ─────────────────────────────────
  dataset <- data.frame(
    latitude  = occurrences$latitude,
    longitude = occurrences$longitude,
    presence  = occurrences$presence,
    vals_climat,
    vals_ndvi
  )

  # ── Ajouter anomalie végétation (greenup) si fourni ───────
  if (!is.null(greenup)) {
    vals_greenup <- terra::extract(greenup, coords)
    vals_greenup <- vals_greenup[, -1, drop = FALSE]
    names(vals_greenup) <- "greenup"
    dataset <- cbind(dataset, vals_greenup)
    message("Anomalie végétation (greenup) ajoutée")
  }

  # ── Supprimer les lignes avec NA ──────────────────────────
  n_avant <- nrow(dataset)
  dataset <- dataset[complete.cases(dataset), ]
  n_apres <- nrow(dataset)

  if (n_avant > n_apres) {
    message(n_avant - n_apres,
            " ligne(s) supprimée(s) : valeurs manquantes")
  }

  # ── Suppression variables trop corrélées (|r| > seuil) ────
  vars_num <- dataset[, !names(dataset) %in%
                        c("latitude", "longitude", "presence")]

  if (ncol(vars_num) > 1) {
    cor_matrix <- cor(vars_num, use = "complete.obs")
    cor_haute  <- which(
      abs(cor_matrix) > seuil_cor &
        upper.tri(cor_matrix),
      arr.ind = TRUE
    )

    if (nrow(cor_haute) > 0) {
      vars_suppr <- unique(rownames(cor_haute))
      dataset    <- dataset[,
                            !names(dataset) %in% vars_suppr]
      message(length(vars_suppr),
              " variable(s) trop corrélée(s) supprimée(s) : ",
              paste(vars_suppr, collapse = ", "))
    } else {
      message("Aucune variable trop corrélée détectée")
    }
  }

  message("Dataset final : ", nrow(dataset),
          " observations x ", ncol(dataset), " variables")

  return(dataset)
}
