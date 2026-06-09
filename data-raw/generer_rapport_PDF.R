# ============================================================
# Script de génération du rapport PDF — locustTrack
# Fichier : data-raw/generer_rapport_PDF.R
# Auteur  : Salma Oubrayme — IAV Hassan II
#
# Instructions :
#   1. Ouvrir ce fichier dans RStudio
#   2. Exécuter section par section (Ctrl+Enter)
#   3. Le PDF est généré dans outputs/rapport_locusttrack_2023.pdf
#   4. Soumettre ce PDF à la prof avec le lien GitHub
# ============================================================

library(locustTrack)

# ── Packages requis ──────────────────────────────────────────
pkgs <- c("randomForest", "terra", "geodata",
          "MODISTools", "rgbif", "sf",
          "base64enc", "pagedown")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# ══════════════════════════════════════════════════════════════
# ÉTAPE 1 — Import et nettoyage des occurrences
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 1 : Import des occurrences ===\n")

df <- import_locust_data(source = "gbif", limit = 200)
cat("Occurrences importées :", nrow(df), "\n")

df_clean <- clean_occurrences(df, seuil_outlier = 3)
cat("Occurrences nettoyées :", nrow(df_clean), "\n")

# ══════════════════════════════════════════════════════════════
# ÉTAPE 2 — Données environnementales
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 2 : Données environnementales ===\n")

# Climat WorldClim
clim <- download_climate_data(var = "prec", res = 10)
cat("Couches climatiques :", terra::nlyr(clim), "\n")

# NDVI MODIS — bbox depuis les coordonnées réelles des criquets
ndvi_jan <- download_ndvi(
  annee       = 2023,
  mois        = 1,
  occurrences = df_clean
)
ndvi_jul <- download_ndvi(
  annee       = 2023,
  mois        = 7,
  occurrences = df_clean
)
cat("NDVI janvier — pixels valides :",
    sum(!is.na(terra::values(ndvi_jan))), "\n")

# Greenup post-pluie
greenup <- calculate_greenup(
  ndvi_avant     = ndvi_jan,
  ndvi_apres     = ndvi_jul,
  precipitations = clim[[6]],
  seuil_greenup  = 0.1,
  seuil_pluie    = 10
)
cat("\nStatistiques greenup :\n")
print(greenup$stats)

# ══════════════════════════════════════════════════════════════
# ÉTAPE 3 — Modélisation Random Forest
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 3 : Modélisation ===\n")

# Pseudo-absences
bg <- generate_background_points(
  occurrences = df_clean,
  raster_ref  = clim
)
cat("Présences :", sum(bg$presence == 1),
    "| Absences :", sum(bg$presence == 0), "\n")

# Dataset ML
dataset <- prepare_predictors(
  occurrences = bg,
  climat      = clim,
  ndvi        = ndvi_jan,
  greenup     = greenup$anomalie
)
cat("Dataset :", nrow(dataset), "observations x",
    ncol(dataset), "variables\n")

# Entraînement RF
rf <- train_rf_model(
  dataset    = dataset,
  prop_train = 0.7,
  ntree      = 500,
  seed       = 42
)

# Évaluation
eval <- evaluate_model(rf, export = TRUE, dossier = "outputs")
cat("\nMétriques du modèle :\n")
print(eval$metriques)

# ══════════════════════════════════════════════════════════════
# ÉTAPE 4 — Carte de risque
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 4 : Carte de risque ===\n")

risk <- predict_risk_map(
  rf_result   = rf,
  climat      = clim,
  ndvi        = ndvi_jan,
  greenup     = greenup$anomalie,
  seuil_moyen = 0.3,
  seuil_eleve = 0.6
)

summary_risk <- summarize_risk_regions(risk, seuil_hotspot = 0.7)
cat("\nRésumé régional :\n")
print(summary_risk$resume)
print(summary_risk$stats_pays)

# Cartes exportées
plot_risk_map(
  risk_result    = risk,
  ndvi           = ndvi_jan,
  greenup_result = greenup,
  occurrences    = df_clean,
  export         = TRUE,
  format         = "png",
  dossier        = "outputs"
)

# Graphiques temporels
temporal <- plot_temporal(
  annee       = 2023,
  mois_debut  = 1,
  mois_fin    = 12,
  occurrences = df_clean,
  climat      = clim,
  export      = TRUE,
  dossier     = "outputs"
)

# ══════════════════════════════════════════════════════════════
# ÉTAPE 5 — Génération du rapport PDF
# ══════════════════════════════════════════════════════════════
cat("\n=== Étape 5 : Génération du rapport PDF ===\n")

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

cat("\n=== RAPPORT GÉNÉRÉ ===\n")
cat("HTML :", rapport$html, "\n")
cat("PDF  :", rapport$pdf,  "\n")

# Bulletin mensuel
bulletin <- generate_alert_bulletin(
  risk_result    = risk,
  summary_result = summary_risk,
  greenup_result = greenup,
  mois           = 7,
  annee          = 2023,
  dossier        = "outputs"
)
cat("Bulletin HTML :", bulletin$html, "\n")
cat("Bulletin PDF  :", bulletin$pdf,  "\n")

cat("\n")
cat("================================================\n")
cat("Tous les fichiers sont dans le dossier outputs/\n")
cat("Soumettez outputs/rapport_locusttrack_2023.pdf\n")
cat("================================================\n")
