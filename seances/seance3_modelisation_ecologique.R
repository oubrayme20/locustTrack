# ============================================================
# Séance 3 — Modélisation Écologique (SDM)
# Appliqué au projet locustTrack
# Auteur : Salma Oubrayme — IAV Hassan II
# ============================================================

library(locustTrack)
library(terra)
library(sf)
library(geodata)

# ══════════════════════════════════════════════════════════
# 1. DONNÉES D'OCCURRENCE (vu en séance)
# ══════════════════════════════════════════════════════════

# Télécharger depuis GBIF (rgbif)
df <- import_locust_data(source = "gbif", limit = 100)
cat("Occurrences GBIF :", nrow(df), "\n")
head(df, 5)

# Nettoyage spatial
df_clean <- clean_occurrences(df)

# Conversion en objet sf (données vectorielles)
df_sf <- sf::st_as_sf(
  df_clean,
  coords = c("longitude", "latitude"),
  crs    = 4326
)
plot(sf::st_geometry(df_sf),
     main = "Occurrences Schistocerca gregaria",
     pch  = 16,
     col  = "red")

# ══════════════════════════════════════════════════════════
# 2. VARIABLES ENVIRONNEMENTALES (vu en séance)
# ══════════════════════════════════════════════════════════

# WorldClim — données bioclimatiques
clim <- download_climate_data(var = "prec", res = 10)
cat("Couches climatiques :", terra::nlyr(clim), "\n")

# NDVI MODIS
ndvi <- download_ndvi(2023, mois = 6, simuler = TRUE)
terra::plot(ndvi, main = "NDVI MODIS — Juin 2023")

# ══════════════════════════════════════════════════════════
# 3. BACKGROUND POINTS (vu en séance)
# ══════════════════════════════════════════════════════════

# spatSample() — méthode vue en cours
set.seed(42)
dataset_bg <- generate_background_points(
  occurrences = df_clean,
  raster_ref  = clim,
  n_points    = nrow(df_clean)  # ratio 1:1
)

table(dataset_bg$presence)

# ══════════════════════════════════════════════════════════
# 4. MODÈLE SDM RANDOM FOREST (vu en séance)
# ══════════════════════════════════════════════════════════

dataset <- prepare_predictors(dataset_bg, clim, ndvi)

rf <- train_rf_model(dataset, ntree = 500)
eval <- evaluate_model(rf)

cat("AUC :", eval$auc, "\n")
print(eval$metriques)

# ══════════════════════════════════════════════════════════
# 5. CARTE DE RISQUE D'INVASION (vu en séance)
# ══════════════════════════════════════════════════════════

risk <- predict_risk_map(
  rf_result   = rf,
  climat      = clim,
  ndvi        = ndvi,
  seuil_moyen = 0.3,
  seuil_eleve = 0.6
)

# Carte continue
terra::plot(risk$risque_continu,
            main = "Probabilité de présence",
            col  = colorRampPalette(
              c("#ffffcc", "#fd8d3c", "#800026"))(100))

# Carte classée faible/moyen/élevé
terra::plot(risk$risque_classe,
            main = "Carte de risque d'invasion",
            col  = c("#2ecc71", "#f39c12", "#e74c3c"))

# Résumé par région + surface km²
summary_risk <- summarize_risk_regions(risk)
print(summary_risk$resume)
