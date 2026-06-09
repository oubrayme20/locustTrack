# ============================================================
# Tests unitaires — download_ndvi()
# Fichier : tests/testthat/test-download_ndvi.R
# ============================================================
# Stratégie :
#   - Tests de validation (paramètres) : SANS réseau
#   - Tests fonctionnels (données réelles) : skip_if_offline()
# ============================================================

# ── Occurrences réelles minimales pour les tests ─────────────
occ_sahel <- data.frame(
  latitude  = c(15.2, 12.8, 18.5, 14.1, 16.7),
  longitude = c(12.5, 15.3,  9.8, 18.2, 11.4),
  presence  = 1
)

# ============================================================
# TESTS DE VALIDATION — sans réseau
# ============================================================

test_that("download_ndvi erreur si mois > 12", {
  expect_error(
    download_ndvi(annee = 2023, mois = 13),
    "entre 1 et 12"
  )
})

test_that("download_ndvi erreur si mois = 0", {
  expect_error(
    download_ndvi(annee = 2023, mois = 0),
    "entre 1 et 12"
  )
})

test_that("download_ndvi erreur si annee < 2000", {
  expect_error(
    download_ndvi(annee = 1999, mois = 6),
    "entre 2000"
  )
})

test_that("download_ndvi erreur si occurrences sans colonne latitude", {
  occ_bad <- data.frame(x = 1:5, longitude = 10:14)
  expect_error(
    download_ndvi(annee = 2023, mois = 6, occurrences = occ_bad),
    "latitude"
  )
})

test_that("download_ndvi erreur si occurrences sans colonne longitude", {
  occ_bad <- data.frame(latitude = 10:14, y = 1:5)
  expect_error(
    download_ndvi(annee = 2023, mois = 6, occurrences = occ_bad),
    "longitude"
  )
})

test_that("download_ndvi erreur si toutes coordonnees NA", {
  occ_na <- data.frame(
    latitude  = c(NA, NA),
    longitude = c(NA, NA)
  )
  expect_error(
    download_ndvi(annee = 2023, mois = 6, occurrences = occ_na),
    "coordonn"
  )
})

# ============================================================
# TESTS FONCTIONNELS — nécessitent internet + MODISTools
# ============================================================

test_that("download_ndvi retourne un SpatRaster depuis occurrences", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  ndvi <- download_ndvi(
    annee       = 2022,
    mois        = 6,
    occurrences = occ_sahel
  )
  expect_true(inherits(ndvi, "SpatRaster"))
})

test_that("download_ndvi etendue couvre toutes les occurrences", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  ndvi <- download_ndvi(
    annee       = 2022,
    mois        = 6,
    occurrences = occ_sahel,
    marge       = 2
  )
  ext <- terra::ext(ndvi)

  # Le raster doit couvrir tous les points (coordonnées réelles)
  expect_true(as.numeric(ext$xmin) <= min(occ_sahel$longitude))
  expect_true(as.numeric(ext$xmax) >= max(occ_sahel$longitude))
  expect_true(as.numeric(ext$ymin) <= min(occ_sahel$latitude))
  expect_true(as.numeric(ext$ymax) >= max(occ_sahel$latitude))
})

test_that("download_ndvi valeurs NDVI entre -1 et 1", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  ndvi <- download_ndvi(
    annee       = 2022,
    mois        = 6,
    occurrences = occ_sahel
  )
  vals <- terra::values(ndvi, na.rm = TRUE)
  expect_true(all(vals >= -1 & vals <= 1))
})

test_that("download_ndvi multi-dates retourne le bon nombre de couches", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  ndvi_multi <- download_ndvi(
    annee       = 2022,
    mois        = c(1, 6, 12),
    occurrences = occ_sahel
  )
  expect_equal(terra::nlyr(ndvi_multi), 3)
})

test_that("download_ndvi noms de couches au format NDVI_YYYY_MM", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  ndvi <- download_ndvi(
    annee       = 2022,
    mois        = 6,
    occurrences = occ_sahel
  )
  expect_true(grepl("NDVI_MODIS_2022_06", names(ndvi)[1]))
})

test_that("download_ndvi avec bbox explicite fonctionne", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  ndvi <- download_ndvi(
    annee   = 2022,
    mois    = 6,
    lon_min = 9,
    lon_max = 20,
    lat_min = 12,
    lat_max = 20
  )
  expect_true(inherits(ndvi, "SpatRaster"))
  ext <- terra::ext(ndvi)
  expect_true(as.numeric(ext$xmin) <= 9)
  expect_true(as.numeric(ext$xmax) >= 20)
})

test_that("download_ndvi extrait valeurs aux coordonnees des occurrences", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  # Vérification de bout en bout :
  # les valeurs NDVI aux coordonnées des criquets ne sont pas NA
  ndvi <- download_ndvi(
    annee       = 2022,
    mois        = 6,
    occurrences = occ_sahel
  )
  coords <- cbind(occ_sahel$longitude, occ_sahel$latitude)
  vals   <- terra::extract(ndvi, coords)
  # Au moins 80% des points doivent avoir une valeur réelle
  n_valides <- sum(!is.na(vals[, 2]))
  expect_gte(n_valides / nrow(occ_sahel), 0.8)
})
