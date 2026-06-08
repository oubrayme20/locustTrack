# ============================================================
# Séance 2 — Machine Learning (Random Forest)
# Package locustTrack — Salma Oubrayme
# ============================================================

library(locustTrack)

# ── 1. Préparer les données ──────────────────────────────────
data("locust_sample")
df_clean <- clean_occurrences(locust_sample)

# Télécharger climat et NDVI
clim <- download_climate_data(var = "prec", res = 10)
ndvi <- download_ndvi(2023, mois = 6, simuler = TRUE)

# Background points (pseudo-absences)
dataset_bg <- generate_background_points(
  occurrences = df_clean,
  raster_ref  = clim
)
cat("Présences :", sum(dataset_bg$presence == 1), "\n")
cat("Absences  :", sum(dataset_bg$presence == 0), "\n")

# Préparer les prédicteurs
dataset <- prepare_predictors(
  occurrences = dataset_bg,
  climat      = clim,
  ndvi        = ndvi
)
head(dataset)

# ── 2. Entraîner Random Forest ───────────────────────────────
# Split train/test 70/30
rf <- train_rf_model(
  dataset    = dataset,
  prop_train = 0.7,
  ntree      = 500,
  seed       = 42
)

# Importance des variables
print(rf$importance)

# ── 3. Évaluation du modèle ──────────────────────────────────
eval <- evaluate_model(rf)

# Métriques
print(eval$metriques)
#   Accuracy, Sensibilité, Spécificité, AUC, F1-Score

# Matrice de confusion
print(eval$matrice_confusion)

# AUC
cat("AUC :", eval$auc, "\n")
