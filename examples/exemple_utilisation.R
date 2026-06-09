# ============================================================
# Exemples d'utilisation du package locustTrack
# Auteur : Salma Oubrayme — IAV Hassan II
# ============================================================

library(locustTrack)

# ── 1. Importer les données (source fiable : GBIF) ───────────
df <- import_locust_data(source = "gbif", limit = 200)
head(df)

# ── 2. Nettoyer les occurrences ──────────────────────────────
df_clean <- clean_occurrences(df, seuil_outlier = 3)
cat("Occurrences nettoyées :", nrow(df_clean), "\n")

# ── 3. Données exemple intégrées ─────────────────────────────
data("locust_sample")
head(locust_sample)

# ── 4. Télécharger le NDVI — bbox dérivée des occurrences ────
# La zone d'extraction est calculée automatiquement depuis
# les coordonnées réelles des criquets observés
ndvi_juin <- download_ndvi(
  annee       = 2023,
  mois        = 6,
  occurrences = df_clean      # ← coordonnées réelles
)
terra::plot(ndvi_juin, main = "NDVI Juin 2023 — zone criquets")

# ── 5. Multi-dates (bbox toujours depuis les occurrences) ────
ndvi_multi <- download_ndvi(
  annee       = 2023,
  mois        = c(1, 4, 7, 10),
  occurrences = df_clean
)
terra::plot(ndvi_multi)

# ── 6. Greenup ───────────────────────────────────────────────
ndvi_jan <- download_ndvi(2023, mois = 1,  occurrences = df_clean)
ndvi_jul <- download_ndvi(2023, mois = 7,  occurrences = df_clean)

greenup <- calculate_greenup(
  ndvi_avant    = ndvi_jan,
  ndvi_apres    = ndvi_jul,
  seuil_greenup = 0.1,
  seuil_pluie   = 10
)
print(greenup$stats)

# ── 7. Graphiques temporels ──────────────────────────────────
temporal <- plot_temporal(
  annee      = 2023,
  mois_debut = 1,
  mois_fin   = 12
)
print(temporal)

# ── 8. Télécharger le climat ─────────────────────────────────
clim <- download_climate_data(var = "prec", res = 10)
cat("Couches climatiques :", terra::nlyr(clim), "\n")

# ── 9. Background points ─────────────────────────────────────
dataset_bg <- generate_background_points(
  occurrences = df_clean,
  raster_ref  = clim
)
table(dataset_bg$presence)

# ── 10. Préparer les prédicteurs ─────────────────────────────
dataset <- prepare_predictors(
  occurrences = dataset_bg,
  climat      = clim,
  ndvi        = ndvi_juin
)
head(dataset)

# ── 11. Entraîner le modèle ──────────────────────────────────
rf <- train_rf_model(
  dataset    = dataset,
  prop_train = 0.7,
  ntree      = 500,
  seed       = 42
)
print(rf$importance)

# ── 12. Évaluer le modèle ────────────────────────────────────
eval <- evaluate_model(rf)
print(eval$metriques)

# ── 13. Carte de risque ──────────────────────────────────────
risk <- predict_risk_map(
  rf_result   = rf,
  climat      = clim,
  ndvi        = ndvi_juin,
  seuil_moyen = 0.3,
  seuil_eleve = 0.6
)
terra::plot(risk$risque_classe,
            main = "Carte de risque d'invasion")

# ── 14. Résumé des régions ───────────────────────────────────
summary_risk <- summarize_risk_regions(risk)
print(summary_risk$resume)
print(summary_risk$stats_pays)
