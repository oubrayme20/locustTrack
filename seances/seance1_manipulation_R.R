# ============================================================
# Séance 1 — Manipulation de données en R
# Appliqué au projet locustTrack
# Auteur : Salma Oubrayme — IAV Hassan II
# ============================================================

library(locustTrack)

# ══════════════════════════════════════════════════════════
# 1. CHARGEMENT DES DONNÉES (vu en séance)
# ══════════════════════════════════════════════════════════

# Données d'exemple du package
data("locust_sample")

# Vue brute — TOUJOURS faire ça en premier
head(locust_sample, 10)      # 10 premières lignes
tail(locust_sample, 6)       # 6 dernières lignes
dim(locust_sample)           # dimensions
names(locust_sample)         # noms des colonnes

# ══════════════════════════════════════════════════════════
# 2. STRUCTURE DU DATASET (vu en séance)
# ══════════════════════════════════════════════════════════

# Vue compacte : types + premières valeurs
glimpse(locust_sample)

# Version technique (format liste)
str(locust_sample)

# Résumé statistique
summary(locust_sample)

# ══════════════════════════════════════════════════════════
# 3. DATA CLEANING (vu en séance)
# ══════════════════════════════════════════════════════════

# Vérifier les doublons
cat("Doublons :", sum(duplicated(locust_sample)), "\n")

# Vérifier les NA
cat("NA par colonne :\n")
colSums(is.na(locust_sample))

# Proportion NA en %
round(colMeans(is.na(locust_sample)) * 100, 3)

# Nettoyage complet avec la fonction du package
df_clean <- clean_occurrences(
  locust_sample,
  lon_min       = -20,
  lon_max       =  65,
  lat_min       = -10,
  lat_max       =  40,
  seuil_outlier =   3
)

cat("Avant nettoyage :", nrow(locust_sample), "lignes\n")
cat("Après nettoyage :", nrow(df_clean), "lignes\n")

# ══════════════════════════════════════════════════════════
# 4. EXPLORATION DES DONNÉES (vu en séance)
# ══════════════════════════════════════════════════════════

# Distribution géographique
hist(df_clean$latitude,
     main = "Distribution des latitudes — Schistocerca gregaria",
     xlab = "Latitude",
     col  = "steelblue",
     border = "white")

hist(df_clean$longitude,
     main = "Distribution des longitudes",
     xlab = "Longitude",
     col  = "darkgreen",
     border = "white")

# Distribution temporelle
df_clean$annee <- format(df_clean$date, "%Y")
table(df_clean$annee)

# Boxplot des latitudes
boxplot(df_clean$latitude,
        main = "Boxplot Latitude",
        ylab = "Latitude",
        col  = "steelblue")

# ══════════════════════════════════════════════════════════
# 5. IMPORT DEPUIS PLUSIEURS SOURCES (vu en séance)
# ══════════════════════════════════════════════════════════

# Depuis GBIF (source fiable officielle)
df_gbif <- import_locust_data(source = "gbif", limit = 50)
cat("GBIF :", nrow(df_gbif), "occurrences\n")
head(df_gbif, 3)

# Depuis iNaturalist (fallback FAO)
df_inat <- import_locust_data(source = "fao", limit = 50)
cat("iNaturalist :", nrow(df_inat), "occurrences\n")
head(df_inat, 3)
