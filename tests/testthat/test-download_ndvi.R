# ============================================================
# Tests unitaires — download_ndvi()
# Fichier : tests/testthat/test-download_ndvi.R
# ============================================================

test_that("download_ndvi retourne un SpatRaster", {
  ndvi <- download_ndvi(annee = 2023, mois = 6, simuler = TRUE)
  expect_true(inherits(ndvi, "SpatRaster"))
})

test_that("download_ndvi a la bonne étendue spatiale", {
  ndvi <- download_ndvi(annee   = 2023,
                        mois    = 6,
                        lon_min = -20,
                        lon_max =  65,
                        lat_min = -10,
                        lat_max =  40,
                        simuler = TRUE)
  ext  <- terra::ext(ndvi)
  expect_equal(as.numeric(ext$xmin), -20)
  expect_equal(as.numeric(ext$xmax),  65)
})

test_that("download_ndvi multi-dates retourne plusieurs couches", {
  ndvi_multi <- download_ndvi(annee   = 2023,
                              mois    = c(1, 6, 12),
                              simuler = TRUE)
  expect_equal(terra::nlyr(ndvi_multi), 3)
})

test_that("download_ndvi valeurs NDVI entre -1 et 1", {
  ndvi <- download_ndvi(annee = 2023, mois = 1, simuler = TRUE)
  vals <- terra::values(ndvi, na.rm = TRUE)
  expect_true(all(vals >= -1 & vals <= 1))
})

test_that("download_ndvi erreur si mois invalide", {
  expect_error(download_ndvi(annee = 2023, mois = 13,
                             simuler = TRUE))
})
