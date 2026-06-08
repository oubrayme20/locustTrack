#' Évaluer les performances du modèle Random Forest
#'
#' Calcule les métriques de performance du modèle sur le jeu de test :
#' Accuracy, Sensibilité, Spécificité, AUC et trace la courbe ROC
#' ainsi que le graphique Observed vs Predicted.
#'
#' @param rf_result Liste issue de \code{train_rf_model()}
#' @param export Si TRUE, exporte les graphiques en PNG. Par défaut FALSE
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Une liste contenant :
#'   \item{metriques}{data.frame des métriques de performance}
#'   \item{matrice_confusion}{Matrice de confusion}
#'   \item{auc}{Valeur AUC}
#'   \item{predictions}{data.frame obs vs pred}
#'
#' @examples
#' \dontrun{
#' dataset <- prepare_predictors(df_clean, clim, ndvi)
#' rf      <- train_rf_model(dataset)
#' eval    <- evaluate_model(rf)
#' print(eval$metriques)
#' }
#'
#' @export
evaluate_model <- function(rf_result,
                           export  = FALSE,
                           dossier = "outputs") {

  # Vérifier randomForest
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Le package 'randomForest' est requis : install.packages('randomForest')")
  }

  # Créer dossier si export
  if (export && !dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  # Extraire modèle et données test
  modele <- rf_result$modele
  test   <- rf_result$test

  # ── Prédictions ───────────────────────────────────────────
  pred_classe <- predict(modele, newdata = test, type = "class")
  pred_proba  <- predict(modele, newdata = test,
                         type = "prob")[, "presence"]

  # ── Matrice de confusion ──────────────────────────────────
  obs     <- test$presence
  matrice <- table(Observe = obs, Predit = pred_classe)

  cat("=== Matrice de Confusion ===\n")
  print(matrice)

  # ── TP, TN, FP, FN ───────────────────────────────────────
  TP <- matrice["presence", "presence"]
  TN <- matrice["absence",  "absence"]
  FP <- matrice["absence",  "presence"]
  FN <- matrice["presence", "absence"]

  # ── Métriques ─────────────────────────────────────────────
  accuracy    <- round((TP + TN) / (TP + TN + FP + FN), 4)
  sensibilite <- round(TP / (TP + FN), 4)
  specificite <- round(TN / (TN + FP), 4)
  precision   <- round(TP / (TP + FP), 4)
  f1          <- round(2 * precision * sensibilite /
                         (precision + sensibilite), 4)

  # ── AUC ───────────────────────────────────────────────────
  obs_bin <- ifelse(obs == "presence", 1, 0)
  ordre   <- order(pred_proba, decreasing = TRUE)
  obs_tri <- obs_bin[ordre]
  n_pos   <- sum(obs_bin == 1)
  n_neg   <- sum(obs_bin == 0)
  tpr     <- cumsum(obs_tri == 1) / n_pos
  fpr     <- cumsum(obs_tri == 0) / n_neg
  auc     <- round(abs(sum(diff(fpr) *
                             (tpr[-1] + tpr[-length(tpr)]) / 2)),
                   4)

  # ── Tableau métriques ─────────────────────────────────────
  metriques <- data.frame(
    Metrique = c("Accuracy",
                 "Sensibilite (Recall)",
                 "Specificite",
                 "Precision",
                 "F1-Score",
                 "AUC"),
    Valeur   = c(accuracy, sensibilite, specificite,
                 precision, f1, auc)
  )

  cat("\n=== Performances du modèle (jeu de test) ===\n")
  print(metriques)

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 1 — Courbe ROC
  # ══════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier, "courbe_ROC.png"),
      width = 800, height = 700, res = 150
    )
  }

  plot(fpr, tpr,
       type = "l",
       col  = "darkgreen",
       lwd  = 2,
       xlab = "Taux de Faux Positifs (1 - Spécificité)",
       ylab = "Taux de Vrais Positifs (Sensibilité)",
       main = paste0("Courbe ROC — Random Forest (AUC = ",
                     auc, ")"))
  abline(0, 1, lty = 2, col = "gray")
  legend("bottomright",
         legend = paste("AUC =", auc),
         col    = "darkgreen",
         lwd    = 2)

  if (export) {
    grDevices::dev.off()
    message("Courbe ROC exportée")
  }

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 2 — Observed vs Predicted
  # ══════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier, "observed_vs_predicted.png"),
      width = 800, height = 700, res = 150
    )
  }

  # Données observed vs predicted
  obs_num  <- as.numeric(obs == "presence")
  pred_num <- round(pred_proba, 2)

  # Scatter plot observed vs predicted
  plot(obs_num, pred_num,
       pch  = 20,
       col  = ifelse(obs_num == 1, "#e74c3c", "#3498db"),
       xlab = "Observé (0 = absence, 1 = présence)",
       ylab = "Probabilité prédite",
       main = "Observed vs Predicted — Random Forest",
       xlim = c(-0.2, 1.2),
       ylim = c(0, 1))

  # Ligne de référence
  abline(h   = 0.5, lty = 2, col = "gray", lwd = 1)
  abline(v   = 0.5, lty = 2, col = "gray", lwd = 1)

  # Ajouter jitter pour mieux voir les points
  points(jitter(obs_num, 0.1), pred_num,
         pch = 20,
         col = ifelse(obs_num == 1,
                      adjustcolor("#e74c3c", 0.5),
                      adjustcolor("#3498db", 0.5)))

  legend("topleft",
         legend = c("Présence observée",
                    "Absence observée",
                    "Seuil 0.5"),
         col    = c("#e74c3c", "#3498db", "gray"),
         pch    = c(20, 20, NA),
         lty    = c(NA, NA, 2),
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Graphique Observed vs Predicted exporté")
  }

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 3 — Distribution des probabilités prédites
  # ══════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier, "distribution_probabilites.png"),
      width = 800, height = 600, res = 150
    )
  }

  # Histogramme des probabilités par classe
  hist(pred_proba[obs == "presence"],
       col    = adjustcolor("#e74c3c", 0.6),
       main   = "Distribution des probabilités prédites",
       xlab   = "Probabilité de présence",
       ylab   = "Fréquence",
       xlim   = c(0, 1),
       breaks = 20,
       border = "white")

  hist(pred_proba[obs == "absence"],
       col    = adjustcolor("#3498db", 0.6),
       add    = TRUE,
       breaks = 20,
       border = "white")

  abline(v   = 0.5, lty = 2, col = "black", lwd = 2)

  legend("topright",
         legend = c("Présence réelle",
                    "Absence réelle",
                    "Seuil 0.5"),
         fill   = c(adjustcolor("#e74c3c", 0.6),
                    adjustcolor("#3498db", 0.6),
                    NA),
         lty    = c(NA, NA, 2),
         border = c("white", "white", NA),
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Distribution probabilités exportée")
  }

  # ── Predictions data.frame ────────────────────────────────
  predictions_df <- data.frame(
    observe  = obs,
    predit   = pred_classe,
    proba    = round(pred_proba, 4),
    correct  = ifelse(obs == pred_classe, 1, 0)
  )

  cat("\nTaux de bonne classification : ",
      round(mean(predictions_df$correct) * 100, 1), "%\n")

  return(list(
    metriques         = metriques,
    matrice_confusion = matrice,
    auc               = auc,
    predictions       = predictions_df
  ))
}
