# ============================================================
# Tests unitaires — calculate_greenup()
# Fichier : tests/testthat/test-calculate_greenup.R
# ============================================================

test_that("calculate_greenup retourne une liste", {
  ndvi_av <- download_ndvi(2023, mois = 1, simuler = TRUE)
  ndvi_ap <- download_ndvi(2023, mois = 7, simuler = TRUE)
  result  <- calculate_greenup(ndvi_av, ndvi_ap)
  expect_type(result, "list")
})

test_that("calculate_greenup contient anomalie et greenup", {
  ndvi_av <- download_ndvi(2023, mois = 1, simuler = TRUE)
  ndvi_ap <- download_ndvi(2023, mois = 7, simuler = TRUE)
  result  <- calculate_greenup(ndvi_av, ndvi_ap)
  expect_true("anomalie"      %in% names(result))
  expect_true("greenup"       %in% names(result))
  expect_true("greenup_pluie" %in% names(result))
  expect_true("stats"         %in% names(result))
})

test_that("calculate_greenup anomalie est un SpatRaster", {
  ndvi_av <- download_ndvi(2023, mois = 1, simuler = TRUE)
  ndvi_ap <- download_ndvi(2023, mois = 7, simuler = TRUE)
  result  <- calculate_greenup(ndvi_av, ndvi_ap)
  expect_true(inherits(result$anomalie, "SpatRaster"))
})

test_that("calculate_greenup stats contient les bons indicateurs", {
  ndvi_av <- download_ndvi(2023, mois = 1, simuler = TRUE)
  ndvi_ap <- download_ndvi(2023, mois = 7, simuler = TRUE)
  result  <- calculate_greenup(ndvi_av, ndvi_ap)
  expect_true("indicateur" %in% names(result$stats))
  expect_true("valeur"     %in% names(result$stats))
  expect_gt(nrow(result$stats), 0)
})
