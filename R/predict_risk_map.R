#' Prédire la carte spatiale du risque d'invasion des criquets
#'
#' Applique le modèle Random Forest directement sur le raster
#' via terra::predict() —.
#' Génère une carte de probabilité classée en 3 niveaux de risque.
#'
#' @param rf_result Liste issue de \code{train_rf_model()}
#' @param climat SpatRaster climatique (issu de download_climate_data)
#' @param ndvi SpatRaster NDVI (issu de download_ndvi)
#' @param greenup SpatRaster greenup optionnel. Par défaut NULL
#' @param seuil_moyen Seuil probabilité risque moyen. Par défaut 0.3
#' @param seuil_eleve Seuil probabilité risque élevé. Par défaut 0.6
#'
#' @return Une liste contenant :
#'   \item{risque_continu}{Raster probabilité de présence (0-1)}
#'   \item{risque_classe}{Raster classé 1=faible 2=moyen 3=élevé}
#'   \item{stats}{Statistiques de la carte de risque}
#'
#' @examples
#' \dontrun{
#' rf   <- train_rf_model(dataset)
#' clim <- download_climate_data(var = "prec")
#' ndvi <- download_ndvi(2023, mois = 6)
#' risk <- predict_risk_map(rf, clim, ndvi)
#' terra::plot(risk$risque_continu, main = "Probabilite presence")
#' terra::plot(risk$risque_classe,  main = "Carte de risque")
#' }
#'
#' @export
predict_risk_map <- function(rf_result,
                             climat,
                             ndvi,
                             greenup     = NULL,
                             seuil_moyen = 0.3,
                             seuil_eleve = 0.6) {

  # Vérifier terra et randomForest
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Le package 'randomForest' est requis : install.packages('randomForest')")
  }

  modele <- rf_result$modele

  # ── Aligner les rasters sur la même résolution ─────────────
  message("Alignement des rasters...")
  ndvi_r <- terra::resample(ndvi, climat[[1]], method = "bilinear")

  # ── Construire le stack de prédicteurs ────────────────────────────────────
  if (!is.null(greenup)) {
    greenup_r     <- terra::resample(greenup, climat[[1]], method = "bilinear")
    stack_pred    <- c(climat, ndvi_r, greenup_r)
  } else {
    stack_pred    <- c(climat, ndvi_r)
  }

  # ── Renommer les couches pour correspondre aux variables du modèle ─────────
  n_clim     <- terra::nlyr(climat)
  noms_clim  <- paste0("clim_", 1:n_clim)

  if (!is.null(greenup)) {
    names(stack_pred) <- c(noms_clim, "ndvi", "greenup")
  } else {
    names(stack_pred) <- c(noms_clim, "ndvi")
  }

  # ── Variables utilisées par le modèle ─────────────────────────────────────
  vars_modele <- attr(modele$terms, "term.labels")
  stack_pred_sel <- stack_pred[[vars_modele]]

  # ── Prédiction directe sur le raster ──────────
  # pred <- predict(bio_rsa_sel, model, na.rm = TRUE)
  message("Prédiction spatiale en cours (terra::predict)...")

  risque_continu <- terra::predict(
    stack_pred_sel,
    modele,
    type  = "prob",
    index = 2,       # colonne "presence"
    na.rm = TRUE
  )

  names(risque_continu) <- "probabilite_presence"

  # ── Afficher le résultat  ──────────────────
  terra::plot(risque_continu,
              main = "Probabilite de presence — Criquet pelerin")

  # ── Classifier en 3 niveaux de risque ─────────────────────────────────────
  risque_classe <- terra::ifel(
    risque_continu < seuil_moyen, 1,
    terra::ifel(risque_continu < seuil_eleve, 2, 3)
  )
  names(risque_classe) <- "niveau_risque"

  # ── Statistiques de la carte ──────────────────────────────────────────────
  vals    <- terra::values(risque_classe, na.rm = TRUE)
  n_total <- length(vals)

  n_faible <- sum(vals == 1, na.rm = TRUE)
  n_moyen  <- sum(vals == 2, na.rm = TRUE)
  n_eleve  <- sum(vals == 3, na.rm = TRUE)

  stats <- data.frame(
    Niveau   = c("Faible (1)", "Moyen (2)", "Eleve (3)"),
    N_pixels = c(n_faible, n_moyen, n_eleve),
    Pct      = round(c(n_faible, n_moyen, n_eleve) / n_total * 100, 1)
  )

  cat("=== Carte de risque générée ===\n")
  cat("Seuils : moyen >=", seuil_moyen, "| élevé >=", seuil_eleve, "\n")
  print(stats)

  return(list(
    risque_continu = risque_continu,
    risque_classe  = risque_classe,
    stats          = stats
  ))
}
