# ============================================================
# Création des données d'exemple du package locustTrack
# Fichier : data-raw/locust_sample.R
# Auteur  : Salma Oubrayme
# Source  : GBIF — Schistocerca gregaria occurrences réelles
# URL     : https://www.gbif.org/species/1711088
# Date    : 2026
# ============================================================

# Installer rgbif si nécessaire
if (!requireNamespace("rgbif", quietly = TRUE)) {
  install.packages("rgbif")
}

library(rgbif)

# Télécharger vraies données GBIF
cat("Téléchargement données GBIF...\n")

gbif_data <- occ_search(
  scientificName = "Schistocerca gregaria",
  hasCoordinate  = TRUE,
  limit          = 100
)

df_raw <- gbif_data$data
cat("Occurrences téléchargées :", nrow(df_raw), "\n")

# Créer locust_sample avec vraies données
locust_sample <- data.frame(
  latitude  = df_raw$decimalLatitude,
  longitude = df_raw$decimalLongitude,
  date      = as.Date(substr(df_raw$eventDate, 1, 10)),
  presence  = 1
)

# Nettoyage basique
locust_sample <- locust_sample[
  !is.na(locust_sample$latitude)  &
    !is.na(locust_sample$longitude) &
    !is.na(locust_sample$date), ]

locust_sample <- unique(locust_sample)

# Vérification
cat("=== Vraies données GBIF ===\n")
cat("Occurrences :", nrow(locust_sample), "\n")
cat("Période     :", as.character(min(locust_sample$date)),
    "à", as.character(max(locust_sample$date)), "\n")
print(head(locust_sample, 5))

# Sauvegarder dans le package
usethis::use_data(locust_sample, overwrite = TRUE)
cat("✓ locust_sample sauvegardé avec données GBIF réelles\n")
