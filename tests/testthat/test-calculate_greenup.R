# ============================================================
# Tests unitaires — calculate_greenup()
# Fichier : tests/testthat/test-calculate_greenup.R
# ============================================================
# Les rasters NDVI sont construits manuellement avec terra
# (valeurs réalistes, géoréférencées) — pas de simuler = TRUE.
# ============================================================

# ── Helper : créer deux rasters NDVI cohérents pour les tests ──
creer_ndvi_test <- function(valeurs_avant, valeurs_apres) {
  zone <- terra::ext(-5, 15, 10, 25)

  r_av <- terra::rast(zone, resolution = 1, crs = "EPSG:4326")
  r_ap <- terra::rast(zone, resolution = 1, crs = "EPSG:4326")

  terra::values(r_av) <- valeurs_avant
  terra::values(r_ap) <- valeurs_apres

  names(r_av) <- "NDVI_2022_01"
  names(r_ap) <- "NDVI_2022_07"

  list(avant = r_av, apres = r_ap)
}

n_cells <- terra::ncell(
  terra::rast(terra::ext(-5, 15, 10, 25), resolution = 1)
)

# ── Tests ────────────────────────────────────────────────────

test_that("calculate_greenup retourne une liste", {
  nd <- creer_ndvi_test(
    rep(0.2, n_cells),
    rep(0.4, n_cells)
  )
  result <- calculate_greenup(nd$avant, nd$apres)
  expect_type(result, "list")
})

test_that("calculate_greenup contient anomalie, greenup, greenup_pluie, stats", {
  nd <- creer_ndvi_test(
    rep(0.2, n_cells),
    rep(0.4, n_cells)
  )
  result <- calculate_greenup(nd$avant, nd$apres)
  expect_true("anomalie"      %in% names(result))
  expect_true("greenup"       %in% names(result))
  expect_true("greenup_pluie" %in% names(result))
  expect_true("stats"         %in% names(result))
})

test_that("calculate_greenup anomalie est un SpatRaster", {
  nd <- creer_ndvi_test(
    rep(0.2, n_cells),
    rep(0.4, n_cells)
  )
  result <- calculate_greenup(nd$avant, nd$apres)
  expect_true(inherits(result$anomalie, "SpatRaster"))
})

test_that("calculate_greenup stats contient les bons indicateurs", {
  nd <- creer_ndvi_test(
    rep(0.2, n_cells),
    rep(0.4, n_cells)
  )
  result <- calculate_greenup(nd$avant, nd$apres)
  expect_true("indicateur" %in% names(result$stats))
  expect_true("valeur"     %in% names(result$stats))
  expect_gt(nrow(result$stats), 0)
})

test_that("calculate_greenup anomalie = ndvi_apres - ndvi_avant", {
  nd <- creer_ndvi_test(
    rep(0.2, n_cells),
    rep(0.5, n_cells)
  )
  result <- calculate_greenup(nd$avant, nd$apres)
  vals   <- terra::values(result$anomalie, na.rm = TRUE)
  # Toutes les anomalies doivent être ~0.3
  expect_true(all(abs(vals - 0.3) < 0.01))
})

test_that("calculate_greenup greenup vaut TRUE là où anomalie > seuil", {
  # Anomalie = 0.3 partout, seuil = 0.1 → tout doit être greenup
  nd <- creer_ndvi_test(
    rep(0.1, n_cells),
    rep(0.4, n_cells)
  )
  result <- calculate_greenup(nd$avant, nd$apres, seuil_greenup = 0.1)
  vals   <- terra::values(result$greenup, na.rm = TRUE)
  expect_true(all(vals == 1))
})
