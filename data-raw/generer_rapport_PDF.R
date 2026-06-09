# ============================================================
# Script de génération du rapport PDF — locustTrack
# Version simplifiée — utilise locust_sample intégré
# Auteur : Salma Oubrayme — IAV Hassan II
# ============================================================

library(locustTrack)

# ── Packages requis ──────────────────────────────────────────
pkgs <- c("randomForest", "terra", "geodata",
          "sf", "base64enc", "pagedown")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# ══════════════════════════════════════════════════════════════
# ÉTAPE 1 — Données d'exemple intégrées (pas de téléchargement)
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 1 : Chargement données ===\n")

# Utiliser les données GBIF intégrées — pas de téléchargement
data("locust_sample")
df_clean <- locust_sample
cat("Occurrences disponibles :", nrow(df_clean), "\n")

# ══════════════════════════════════════════════════════════════
# ÉTAPE 2 — Climat WorldClim uniquement (rapide ~30 sec)
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 2 : Climat WorldClim ===\n")

clim <- download_climate_data(var = "prec", res = 10)
cat("Couches climatiques :", terra::nlyr(clim), "\n")

# ══════════════════════════════════════════════════════════════
# ÉTAPE 3 — NDVI simulé depuis WorldClim (proxy végétation)
# Pour le rapport uniquement — pas pour la modélisation finale
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 3 : Proxy NDVI depuis WorldClim ===\n")

# Utiliser la précipitation comme proxy du NDVI
# (corrélation forte dans les zones arides — Sahel/Maghreb)
zone <- terra::ext(-20, -4, 21, 35)
prec_zone <- terra::crop(clim[[1]], zone)

# Normaliser entre 0 et 0.6 pour simuler NDVI réaliste
prec_vals <- terra::values(prec_zone)
ndvi_vals <- scales_ndvi <- (prec_vals - min(prec_vals, na.rm=TRUE)) /
  (max(prec_vals, na.rm=TRUE) - min(prec_vals, na.rm=TRUE)) * 0.6

ndvi_jan <- prec_zone
terra::values(ndvi_jan) <- ndvi_vals
names(ndvi_jan) <- "NDVI_proxy_2023_01"

ndvi_jul <- ndvi_jan * 1.3
ndvi_jul <- terra::clamp(ndvi_jul, 0, 1)
names(ndvi_jul) <- "NDVI_proxy_2023_07"

cat("NDVI proxy prêt\n")

# ══════════════════════════════════════════════════════════════
# ÉTAPE 4 — Greenup
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 4 : Greenup ===\n")

greenup <- calculate_greenup(
  ndvi_avant     = ndvi_jan,
  ndvi_apres     = ndvi_jul,
  precipitations = clim[[6]],
  seuil_greenup  = 0.05,
  seuil_pluie    = 5
)
cat("Greenup calculé\n")
print(greenup$stats)

# ══════════════════════════════════════════════════════════════
# ÉTAPE 5 — Modélisation Random Forest
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 5 : Modélisation ===\n")

bg <- generate_background_points(
  occurrences = df_clean,
  raster_ref  = clim
)

# Recadrer greenup sur la même étendue que clim
greenup_crop <- terra::resample(greenup$anomalie, clim[[1]],
                                method = "bilinear")

dataset <- prepare_predictors(
  occurrences = bg,
  climat      = clim,
  ndvi        = ndvi_jan,
  greenup     = greenup_crop
)
cat("Dataset :", nrow(dataset), "observations\n")

rf <- train_rf_model(
  dataset    = dataset,
  prop_train = 0.7,
  ntree      = 500,
  seed       = 42
)

eval <- evaluate_model(rf, export = TRUE, dossier = "outputs")
cat("\nMétriques :\n")
print(eval$metriques)

# ══════════════════════════════════════════════════════════════
# ÉTAPE 6 — Carte de risque
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 6 : Carte de risque ===\n")

# Renommer clim pour correspondre aux variables du modèle
clim_pred <- clim
names(clim_pred) <- paste0("clim_", 1:terra::nlyr(clim_pred))

# Ajouter greenup au stack
clim_pred <- c(clim_pred, greenup_crop)
names(clim_pred)[terra::nlyr(clim_pred)] <- "greenup"

risk <- predict_risk_map(
  rf_result   = rf,
  climat      = clim_pred,
  ndvi        = ndvi_jan,
  greenup     = greenup_crop,
  seuil_moyen = 0.3,
  seuil_eleve = 0.6
)

summary_risk <- summarize_risk_regions(risk)
cat("\nRésumé régional :\n")
print(summary_risk$resume)

if (!dir.exists("outputs")) dir.create("outputs")

plot_risk_map(
  risk_result    = risk,
  ndvi           = ndvi_jan,
  greenup_result = greenup,
  occurrences    = df_clean,
  export         = TRUE,
  format         = "png",
  dossier        = "outputs"
)

# ══════════════════════════════════════════════════════════════
# ÉTAPE 7 — Rapport PDF
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 7 : Génération rapport PDF ===\n")

rapport <- generate_report(
  occurrences    = df_clean,
  rf_result      = rf,
  eval_result    = eval,
  risk_result    = risk,
  summary_result = summary_risk,
  greenup_result = greenup,
  annee          = 2023,
  dossier        = "outputs"
)

cat("\n================================================\n")
cat("HTML :", rapport$html, "\n")
cat("PDF  :", rapport$pdf,  "\n")
cat("================================================\n")
cat("Soumettez outputs/rapport_locusttrack_2023.pdf\n")
