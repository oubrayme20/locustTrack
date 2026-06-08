# ============================================================
# Séance 4 — Télédétection et Analyse Raster
# Appliqué au projet locustTrack
# Auteur : Salma Oubrayme — IAV Hassan II
# ============================================================

library(locustTrack)
library(terra)
library(geodata)

# ══════════════════════════════════════════════════════════
# 1. DONNÉES RASTER WORLDCLIM (vu en séance)
# ══════════════════════════════════════════════════════════

# Télécharger WorldClim via geodata
clim <- download_climate_data(
  var     = "prec",
  res     = 10,
  lon_min = -20,
  lon_max =  65,
  lat_min = -10,
  lat_max =  40
)

# Informations sur le raster
cat("Couches :", terra::nlyr(clim), "\n")
cat("Résolution :", terra::res(clim), "\n")
cat("Étendue :", as.vector(terra::ext(clim)), "\n")

# Visualiser
terra::plot(clim[[1]],
            main = "Précipitations Janvier (WorldClim)",
            col  = colorRampPalette(c("white", "blue"))(100))

# Stack des 12 mois
terra::plot(clim,
            main = paste("Précipitations — Mois", 1:12))

# ══════════════════════════════════════════════════════════
# 2. NDVI MODIS (vu en séance)
# ══════════════════════════════════════════════════════════

# Un seul mois
ndvi_juin <- download_ndvi(
  annee   = 2023,
  mois    = 6,
  simuler = TRUE
)

terra::plot(ndvi_juin,
            main = "NDVI MODIS — Juin 2023",
            col  = colorRampPalette(
              c("#d73027", "#fee08b", "#1a9850"))(100))

# Multi-dates (stack temporel)
ndvi_multi <- download_ndvi(
  annee   = 2023,
  mois    = c(1, 4, 7, 10),
  simuler = TRUE
)
cat("Couches NDVI :", terra::nlyr(ndvi_multi), "\n")
terra::plot(ndvi_multi)

# ══════════════════════════════════════════════════════════
# 3. ANALYSE GREENUP (vu en séance)
# ══════════════════════════════════════════════════════════

# Verdissement post-pluie
ndvi_jan <- download_ndvi(2023, mois = 1, simuler = TRUE)
ndvi_jul <- download_ndvi(2023, mois = 7, simuler = TRUE)

greenup <- calculate_greenup(
  ndvi_avant    = ndvi_jan,
  ndvi_apres    = ndvi_jul,
  seuil_greenup = 0.1
)

# Anomalie NDVI (après - avant)
terra::plot(greenup$anomalie,
            main = "Anomalie NDVI (Juillet - Janvier)",
            col  = colorRampPalette(
              c("#d73027", "#ffffff", "#1a9850"))(100))

# Zones de verdissement
terra::plot(greenup$greenup,
            main = "Zones de verdissement (Greenup)",
            col  = c("gray90", "darkgreen"))

print(greenup$stats)

# ══════════════════════════════════════════════════════════
# 4. EXTRACTION DE VALEURS RASTER (vu en séance)
# ══════════════════════════════════════════════════════════

data("locust_sample")
df_clean <- clean_occurrences(locust_sample)

# Extraction des valeurs climatiques aux points d'occurrence
coords <- cbind(df_clean$longitude, df_clean$latitude)
vals   <- terra::extract(clim, coords)
head(vals)

# ══════════════════════════════════════════════════════════
# 5. GRAPHIQUES TEMPORELS (vu en séance)
# ══════════════════════════════════════════════════════════

temporal <- plot_temporal(
  annee      = 2023,
  mois_debut = 1,
  mois_fin   = 12
)
print(temporal)
