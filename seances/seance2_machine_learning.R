# ============================================================
# Séance 2 — Modélisation Machine Learning (Random Forest)
# Appliqué au projet locustTrack
# Auteur : Salma Oubrayme — IAV Hassan II
# ============================================================

library(locustTrack)
library(randomForest)

# ══════════════════════════════════════════════════════════
# 1. PRÉPARER LES DONNÉES (vu en séance)
# ══════════════════════════════════════════════════════════

data("locust_sample")
df_clean <- clean_occurrences(locust_sample)

# Télécharger les variables environnementales
clim <- download_climate_data(var = "prec", res = 10)
ndvi <- download_ndvi(2023, mois = 6, simuler = TRUE)

# Background points (pseudo-absences) — ratio 1:1
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

str(dataset)
head(dataset)

# ══════════════════════════════════════════════════════════
# 2. SPLIT TRAIN/TEST 70/30 (vu en séance)
# ══════════════════════════════════════════════════════════

set.seed(42)
n         <- nrow(dataset)
idx_train <- sample(1:n, size = round(0.7 * n))
train     <- dataset[ idx_train, ]
test      <- dataset[-idx_train, ]

cat("Train :", nrow(train), "lignes\n")
cat("Test  :", nrow(test),  "lignes\n")

# ══════════════════════════════════════════════════════════
# 3. ENTRAÎNER LE MODÈLE RANDOM FOREST (vu en séance)
# ══════════════════════════════════════════════════════════

rf <- train_rf_model(
  dataset    = dataset,
  prop_train = 0.7,
  ntree      = 500,
  seed       = 42
)

# Importance des variables
print(rf$importance)
randomForest::varImpPlot(rf$modele,
                         main = "Importance des variables — RF")

# ══════════════════════════════════════════════════════════
# 4. ÉVALUATION DU MODÈLE (vu en séance)
# ══════════════════════════════════════════════════════════

eval <- evaluate_model(rf)

# Métriques (formules du cours)
# Accuracy    = (TP + TN) / (TP + TN + FP + FN)
# Sensibilité = TP / (TP + FN)
# Spécificité = TN / (TN + FP)
# AUC         = Aire sous la courbe ROC
print(eval$metriques)

# Matrice de confusion
print(eval$matrice_confusion)

# Courbe ROC
cat("AUC :", eval$auc, "\n")
