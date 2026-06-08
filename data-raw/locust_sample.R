# ============================================================
# Création des données d'exemple du package locustTrack
# Fichier : data-raw/locust_sample.R
# Auteur  : Salma Oubrayme
#
# SOURCE des données réelles :
# - GBIF  : https://www.gbif.org
# - FAO Locust Hub : https://locust-hub-hqfao.hub.arcgis.com
#
# Ces données d'exemple représentent des occurrences typiques
# de Schistocerca gregaria dans les zones définies par le guide :
# Afrique subsaharienne, Maghreb, Moyen-Orient
# (locusttrack.docx — section "Espèce étudiée")
#
# Pour des données réelles, utilisez import_locust_data()
# avec un fichier CSV téléchargé depuis GBIF ou FAO
# ============================================================
# Objectif: Générer et sauvegarder le jeu de données exemple
#           d'occurrences de criquets pèlerins
# ============================================================

set.seed(42)

locust_sample <- data.frame(
  latitude  = c(
    15.2, 12.8, 18.5, 14.1, 16.7, 13.3, 17.9, 11.5,
    19.2, 10.8, 22.4, 25.1, 28.3, 23.7, 20.5, 24.9,
    27.1, 21.3, 26.8, 29.4, 30.2, 31.5, 33.1, 35.2,
    32.7, 34.8, 12.1, 14.5, 16.3, 18.9
  ),
  longitude = c(
    38.5, 42.1, 35.7, 40.3, 37.2, 43.8, 36.1, 41.5,
    39.8, 44.2, 15.3, 18.7, 12.4, 20.1, 17.6, 14.8,
    19.3, 16.5, 13.2, 11.7, 45.2, 48.6, 43.1, 50.3,
    47.8, 49.5, 25.4, 28.9, 22.7, 30.1
  ),
  date      = as.Date(c(
    "2022-03-15", "2022-04-02", "2022-03-28", "2022-05-10",
    "2022-04-18", "2022-06-05", "2022-05-22", "2022-07-14",
    "2022-06-30", "2022-08-08", "2021-07-12", "2021-08-03",
    "2021-06-25", "2021-09-15", "2021-07-28", "2021-08-20",
    "2021-09-05", "2021-10-12", "2021-06-18", "2021-07-30",
    "2023-02-14", "2023-03-08", "2023-04-22", "2023-02-28",
    "2023-03-15", "2023-05-10", "2022-11-20", "2022-12-08",
    "2023-01-15", "2022-10-25"
  )),
  presence  = 1
)

# Vérification
cat("=== Vérification des données ===\n")
cat("Nombre d'occurrences :", nrow(locust_sample), "\n")
cat("Colonnes             :", names(locust_sample), "\n")
cat("Période              :", as.character(min(locust_sample$date)),
    "à", as.character(max(locust_sample$date)), "\n")
cat("Zone latitude        :", min(locust_sample$latitude),
    "à", max(locust_sample$latitude), "\n")
cat("Zone longitude       :", min(locust_sample$longitude),
    "à", max(locust_sample$longitude), "\n")

# Aperçu
print(head(locust_sample))

# Sauvegarder dans data/ (méthode de la prof)
usethis::use_data(locust_sample, overwrite = TRUE)

cat("✓ Données sauvegardées dans data/locust_sample.rda\n")
