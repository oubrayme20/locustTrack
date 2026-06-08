#' Évaluer les performances du modèle Random Forest
#'
#' Calcule les métriques de performance du modèle sur le jeu de test :
#' Accuracy, Sensibilité, Spécificité, AUC et trace la courbe ROC.
#' Métriques conformes au cours (TP, TN, FP, FN).
#'
#' @param rf_result Liste issue de \code{train_rf_model()}
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
evaluate_model <- function(rf_result) {

  # Vérifier randomForest
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Le package 'randomForest' est requis : install.packages('randomForest')")
  }

  # Extraire modèle et données test
  modele <- rf_result$modele
  test   <- rf_result$test

  # ── Prédictions sur le jeu de test ────────────────────────────────────────
  pred_classe <- predict(modele, newdata = test, type = "class")
  pred_proba  <- predict(modele, newdata = test, type = "prob")[, "presence"]

  # ── Matrice de confusion ───────────────────────────────────────────────────
  obs             <- test$presence
  matrice         <- table(Observe = obs, Predit = pred_classe)

  cat("=== Matrice de Confusion ===\n")
  print(matrice)

  # ── Extraire TP, TN, FP, FN (formules du cours) ───────────────────────────
  TP <- matrice["presence", "presence"]
  TN <- matrice["absence",  "absence"]
  FP <- matrice["absence",  "presence"]
  FN <- matrice["presence", "absence"]

  # ── Métriques (formules exactes du cours) ─────────────────────────────────
  accuracy    <- round((TP + TN) / (TP + TN + FP + FN), 4)
  sensibilite <- round(TP / (TP + FN), 4)   # Recall
  specificite <- round(TN / (TN + FP), 4)
  precision   <- round(TP / (TP + FP), 4)
  f1          <- round(2 * precision * sensibilite /
                         (precision + sensibilite), 4)

  # ── AUC (calcul manuel sans package externe) ──────────────────────────────
  obs_bin <- ifelse(obs == "presence", 1, 0)

  # Trier par probabilité décroissante
  ordre   <- order(pred_proba, decreasing = TRUE)
  obs_tri <- obs_bin[ordre]

  # Calcul TPR et FPR pour la courbe ROC
  n_pos <- sum(obs_bin == 1)
  n_neg <- sum(obs_bin == 0)

  tpr <- cumsum(obs_tri == 1)     / n_pos
  fpr <- cumsum(obs_tri == 0)     / n_neg

  # AUC par méthode trapèze
  auc <- round(sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2), 4)
  auc <- abs(auc)

  # ── Tableau des métriques (comme la prof) ─────────────────────────────────
  metriques <- data.frame(
    Metrique = c("Accuracy",
                 "Sensibilite (Recall)",
                 "Specificite",
                 "Precision",
                 "F1-Score",
                 "AUC"),
    Valeur   = c(accuracy,
                 sensibilite,
                 specificite,
                 precision,
                 f1,
                 auc)
  )

  cat("\n=== Performances du modèle (jeu de test) ===\n")
  print(metriques)

  # ── Courbe ROC ──────────────────────────────
  plot(fpr, tpr,
       type = "l",
       col  = "darkgreen",
       lwd  = 2,
       xlab = "Taux de Faux Positifs (1 - Spécificité)",
       ylab = "Taux de Vrais Positifs (Sensibilité)",
       main = paste0("Courbe ROC — Random Forest (AUC = ", auc, ")"))
  abline(0, 1, lty = 2, col = "gray")
  legend("bottomright",
         legend = paste("AUC =", auc),
         col    = "darkgreen",
         lwd    = 2)

  # ── Observed vs Predicted ─────────────────────────────────
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
