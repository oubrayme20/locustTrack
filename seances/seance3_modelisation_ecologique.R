# ============================================================
# Séance 3 — Modélisation écologique (SDM)
# Package locustTrack — Salma Oubrayme
# ============================================================

library(locustTrack)

# ── 1. Données d'occurrence ──────────────────────────────────
data("locust_sample")
df_clean <- clean_occurrences(locust_sample)

# ── 2. Variables environnementales ───────────────────────────
clim <- download_climate_data(var = "prec", res = 10)
ndvi <- download_ndvi(2023, mois = 6, simuler = TRUE)

# ── 3. Background points (pseudo-absences) ───────────────────
# Ratio 1:1 présences:pseudo-absences
dataset_bg <- generate_background_points(
  occurrences = df_clean,
  raster_ref  = clim,
  n_points    = nrow(df_clean)
)
table(dataset_bg$presence)

# ── 4. Préparer les prédicteurs ──────────────────────────────
dataset <- prepare_predictors(
  occurrences = dataset_bg,
  climat      = clim,
  ndvi        = ndvi
)

# ── 5. Modèle SDM Random Forest ──────────────────────────────
rf <- train_rf_model(dataset, ntree = 500)

# ── 6. Carte de risque d'invasion ────────────────────────────
risk <- predict_risk_map(
  rf_result   = rf,
  climat      = clim,
  ndvi        = ndvi,
  seuil_moyen = 0.3,
  seuil_eleve = 0.6
)

# Visualiser
terra::plot(risk$risque_classe,
            col  = c("#2ecc71", "#f39c12", "#e74c3c"),
            main = "Carte de risque — Schistocerca gregaria")

# ── 7. Résumé par région ─────────────────────────────────────
summary_risk <- summarize_risk_regions(risk)
print(summary_risk$resume)
print(summary_risk$stats_pays)
