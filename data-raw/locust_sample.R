# ============================================================
# Création des données d'exemple du package locustTrack
# Fichier : data-raw/locust_sample.R
# Auteur  : Salma Oubrayme
# Source  : GBIF — Schistocerca gregaria occurrences réelles
# URL     : https://www.gbif.org/species/1711088
# ============================================================

# Installer rgbif si nécessaire
if (!requireNamespace("rgbif", quietly = TRUE)) {
  install.packages("rgbif")
}

library(rgbif)

# ── Télécharger les données réelles depuis GBIF ────────────
cat("Téléchargement des données GBIF...\n")

gbif_data <- occ_search(
  scientificName = "Schistocerca gregaria",
  hasCoordinate  = TRUE,
  limit          = 200
)

# Extraire les occurrences
df_raw <- gbif_data$data

cat("Occurrences téléchargées :", nrow(df_raw), "\n")

# ── Sélectionner les colonnes nécessaires ──────────────────
locust_sample <- data.frame(
  latitude  = df_raw$decimalLatitude,
  longitude = df_raw$decimalLongitude,
  date      = as.Date(df_raw$eventDate),
  presence  = 1
)

# ── Nettoyage basique ──────────────────────────────────────
# Supprimer les NA
locust_sample <- locust_sample[
  !is.na(locust_sample$latitude)  &
    !is.na(locust_sample$longitude) &
    !is.na(locust_sample$date), ]

# Supprimer les doublons
locust_sample <- unique(locust_sample)

# Filtrer zone Afrique/Maghreb/Moyen-Orient
locust_sample <- locust_sample[
  locust_sample$longitude >= -20 &
    locust_sample$longitude <=  65 &
    locust_sample$latitude  >= -10 &
    locust_sample$latitude  <=  40, ]

# ── Vérification ──────────────────────────────────────────
cat("=== Données GBIF nettoyées ===\n")
cat("Occurrences finales :", nrow(locust_sample), "\n")
cat("Colonnes            :", names(locust_sample), "\n")
cat("Période             :", as.character(min(locust_sample$date)),
    "à", as.character(max(locust_sample$date)), "\n")
cat("Zone latitude       :", min(locust_sample$latitude),
    "à", max(locust_sample$latitude), "\n")
cat("Zone longitude      :", min(locust_sample$longitude),
    "à", max(locust_sample$longitude), "\n")
print(head(locust_sample))

# ── Sauvegarder dans data/ ────────────────────────────────
usethis::use_data(locust_sample, overwrite = TRUE)
cat("✓ Données GBIF sauvegardées dans data/locust_sample.rda\n")
