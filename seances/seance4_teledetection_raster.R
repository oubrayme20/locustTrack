# ============================================================
# Séance 4 — Télédétection et analyse raster
# Package locustTrack — Salma Oubrayme
# ============================================================

library(locustTrack)
library(terra)

# ── 1. Données NDVI MODIS ────────────────────────────────────
# Un seul mois
ndvi_juin <- download_ndvi(
  annee   = 2023,
  mois    = 6,
  simuler = TRUE
)
terra::plot(ndvi_juin, main = "NDVI Juin 2023")

# Multi-dates
ndvi_multi <- download_ndvi(
  annee   = 2023,
  mois    = c(1, 4, 7, 10),
  simuler = TRUE
)
terra::plot(ndvi_multi,
            main = paste("NDVI 2023 - Mois",
                         c(1, 4, 7, 10)))

# ── 2. Données climatiques WorldClim ─────────────────────────
clim <- download_climate_data(
  var     = "prec",
  res     = 10,
  lon_min = -20,
  lon_max =  65,
  lat_min = -10,
  lat_max =  40
)

cat("Couches climatiques :", terra::nlyr(clim), "\n")
terra::plot(clim[[6]], main = "Précipitations Juin")

# ── 3. Analyse Greenup (verdissement post-pluie) ─────────────
ndvi_jan <- download_ndvi(2023, mois = 1, simuler = TRUE)
ndvi_jul <- download_ndvi(2023, mois = 7, simuler = TRUE)

greenup <- calculate_greenup(
  ndvi_avant    = ndvi_jan,
  ndvi_apres    = ndvi_jul,
  seuil_greenup = 0.1
)

# Anomalie NDVI
terra::plot(greenup$anomalie,
            main = "Anomalie NDVI (Juillet - Janvier)",
            col  = colorRampPalette(
              c("#d73027", "#ffffff", "#1a9850"))(100))

# Zones de verdissement
terra::plot(greenup$greenup,
            main = "Zones de verdissement",
            col  = c("gray90", "darkgreen"))

print(greenup$stats)

# ── 4. Graphiques temporels ──────────────────────────────────
temporal <- plot_temporal(
  annee      = 2023,
  mois_debut = 1,
  mois_fin   = 12
)
print(temporal)

# ── 5. Extraction de valeurs raster ──────────────────────────
data("locust_sample")
df_clean <- clean_occurrences(locust_sample)

dataset <- prepare_predictors(
  occurrences = df_clean,
  climat      = clim,
  ndvi        = ndvi_juin
)

cat("Variables extraites :", ncol(dataset), "\n")
head(dataset)
