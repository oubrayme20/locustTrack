#' Entraîner le modèle Random Forest pour la prédiction du risque
#'
#' Entraîne un modèle Random Forest (présence/absence) sur le dataset
#' préparé par prepare_predictors(). Inclut la validation train/test
#' et le calcul de l'importance des variables.
#' Méthodologie : split 70/30, seed=42, importance=TRUE.
#'
#' @param dataset data.frame issu de prepare_predictors()
#' @param prop_train Proportion train. Par défaut 0.7
#' @param ntree Nombre d'arbres. Par défaut 500
#' @param mtry Variables testées à chaque noeud. Par défaut NULL (auto = sqrt)
#' @param seed Graine aléatoire. Par défaut 42
#'
#' @return Une liste contenant :
#'   \item{modele}{Le modèle Random Forest entraîné}
#'   \item{train}{Données d'entraînement}
#'   \item{test}{Données de test}
#'   \item{importance}{data.frame importance des variables}
#'   \item{params}{Paramètres utilisés}
#'
#' @examples
#' \dontrun{
#' dataset <- prepare_predictors(df_clean, clim, ndvi)
#' rf      <- train_rf_model(dataset)
#' print(rf$importance)
#' varImpPlot(rf$modele)
#' }
#'
#' @export
train_rf_model <- function(dataset,
                           prop_train = 0.7,
                           ntree      = 500,
                           mtry       = NULL,
                           seed       = 42) {

  # Vérifier randomForest
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    stop("Le package 'randomForest' est requis : install.packages('randomForest')")
  }

  # Vérifier la colonne presence
  if (!"presence" %in% names(dataset)) {
    stop("Colonne 'presence' manquante. Utilisez d'abord prepare_predictors()")
  }

  # Convertir presence en facteur (classification binaire)
  dataset$presence <- factor(dataset$presence,
                             levels = c(0, 1),
                             labels = c("absence", "presence"))

  # Supprimer lat/lon du modèle (non prédictives)
  vars_modele <- dataset[, !names(dataset) %in% c("latitude", "longitude")]

  # ── Split train / test  ──────────────────
  set.seed(seed)
  n         <- nrow(vars_modele)
  idx_train <- sample(1:n, size = round(prop_train * n))
  train     <- vars_modele[ idx_train, ]
  test      <- vars_modele[-idx_train, ]

  cat("Train :", nrow(train), "lignes\n")
  cat("Test  :", nrow(test),  "lignes\n")
  cat("Présences train  :", sum(train$presence == "presence"), "\n")
  cat("Absences  train  :", sum(train$presence == "absence"),  "\n")

  # ── Valeur mtry par défaut (sqrt des variables) ────────────────────────────
  if (is.null(mtry)) {
    mtry <- floor(sqrt(ncol(train) - 1))
  }

  cat("Entraînement Random Forest (ntree=", ntree, ", mtry=", mtry, ")...\n")

  # ── Entraîner le modèle Random Forest  ──────────────────────
  modele <- randomForest::randomForest(
    presence ~ .,
    data       = train,
    ntree      = ntree,
    mtry       = mtry,
    importance = TRUE,
    seed       = seed
  )

  # Afficher le résumé
  print(modele)

  # ── Importance des variables  ────────
  imp    <- randomForest::importance(modele)
  imp_df <- as.data.frame(imp)
  imp_df$variable <- rownames(imp_df)
  imp_df <- imp_df[order(-imp_df$MeanDecreaseGini), ]

  cat("\n--- Importance des variables ---\n")
  print(head(imp_df, 5))

  # Graphique importance
  randomForest::varImpPlot(modele,
                           main = "Random Forest — Importance des variables",
                           type = 1)

  return(list(
    modele     = modele,
    train      = train,
    test       = test,
    importance = imp_df,
    params     = list(ntree      = ntree,
                      mtry       = mtry,
                      prop_train = prop_train,
                      seed       = seed)
  ))
}
