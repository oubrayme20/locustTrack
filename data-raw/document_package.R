# ============================================================
# Documentation et construction du package locustTrack
# Fichier : data-raw/document_package.R
# Etudiante : Salma Oubrayme
# Objectif: Générer automatiquement la documentation (man/)
# Méthode : devtools::document()
# ============================================================

# Installer les packages nécessaires si absent
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  install.packages("roxygen2")
}

library(devtools)

# ── Étape 1 : Générer la documentation ────────────────────
# Génère automatiquement :
# - les fichiers .Rd dans man/
# - le fichier NAMESPACE
devtools::document()

# ── Étape 2 : Vérifier la documentation ───────────────────
# Tester que chaque fonction est bien documentée
?import_locust_data
?clean_occurrences
?download_climate_data
?download_ndvi
?calculate_greenup
?prepare_predictors
?generate_background_points
?train_rf_model
?evaluate_model
?predict_risk_map
?summarize_risk_regions
?plot_risk_map
?generate_alert_bulletin
?generate_report

# ── Étape 3 : Installer le package localement ─────────────
# Comme la prof : devtools::install()
devtools::install()

# ── Étape 4 : Tester le chargement ────────────────────────
library(locustTrack)

# Vérifier que les fonctions sont accessibles
cat("✓ Package locustTrack chargé avec succès\n")
cat("✓ Fonctions disponibles :\n")
print(ls("package:locustTrack"))

#TEST
devtools::test()
devtools::check()
#Cas d'Erreur
remove.packages("locustTrack")
.rs.restartR()
devtools::document()
devtools::install()
devtools::test()
