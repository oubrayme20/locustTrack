# ============================================================
# Séance 1 — Manipulation de données en R
# Package locustTrack — Salma Oubrayme
# ============================================================

library(locustTrack)

# ── 1. Chargement des données ────────────────────────────────
data("locust_sample")

# Vue brute
head(locust_sample, 10)
tail(locust_sample, 6)
dim(locust_sample)
str(locust_sample)
summary(locust_sample)

# ── 2. Data Cleaning ─────────────────────────────────────────
# Vérifier les doublons
sum(duplicated(locust_sample))

# Vérifier les NA
colSums(is.na(locust_sample))

# Nettoyer les occurrences
df_clean <- clean_occurrences(
  locust_sample,
  lon_min       = -20,
  lon_max       =  65,
  lat_min       = -10,
  lat_max       =  40,
  seuil_outlier =   3
)

cat("Avant nettoyage :", nrow(locust_sample), "\n")
cat("Après nettoyage :", nrow(df_clean), "\n")

# ── 3. Exploration ───────────────────────────────────────────
# Distribution géographique
hist(df_clean$latitude,
     main = "Distribution des latitudes",
     xlab = "Latitude",
     col  = "steelblue")

hist(df_clean$longitude,
     main = "Distribution des longitudes",
     xlab = "Longitude",
     col  = "darkgreen")

# Distribution temporelle
df_clean$annee <- format(df_clean$date, "%Y")
table(df_clean$annee)

# ── 4. Import depuis différentes sources ─────────────────────
# GBIF
df_gbif <- import_locust_data(source = "gbif", limit = 50)
cat("GBIF :", nrow(df_gbif), "occurrences\n")

# iNaturalist
df_inat <- import_locust_data(source = "fao", limit = 50)
cat("iNaturalist :", nrow(df_inat), "occurrences\n")
