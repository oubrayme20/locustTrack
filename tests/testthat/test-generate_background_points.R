# ============================================================
# Tests unitaires — generate_background_points()
# Fichier : tests/testthat/test-generate_background_points.R
# ============================================================

# Créer données de test
create_bg_data <- function() {
  set.seed(42)
  occ <- data.frame(
    latitude  = runif(20, 10, 35),
    longitude = runif(20, -10, 50),
    presence  = 1
  )
  zone <- terra::ext(-10, 50, 10, 35)
  r    <- terra::rast(zone, resolution = 2,
                      crs = "EPSG:4326")
  terra::values(r) <- runif(terra::ncell(r))
  list(occ = occ, raster = r)
}

test_that("generate_background_points retourne un data.frame", {
  d      <- create_bg_data()
  result <- generate_background_points(d$occ, d$raster,
                                       n_points = 20)
  expect_s3_class(result, "data.frame")
})

test_that("generate_background_points contient presence 0 et 1", {
  d      <- create_bg_data()
  result <- generate_background_points(d$occ, d$raster,
                                       n_points = 20)
  expect_true(0 %in% result$presence)
  expect_true(1 %in% result$presence)
})

test_that("generate_background_points ratio 1:1 par défaut", {
  d      <- create_bg_data()
  result <- generate_background_points(d$occ, d$raster)
  n_pres <- sum(result$presence == 1)
  n_abs  <- sum(result$presence == 0)
  expect_equal(n_pres, nrow(d$occ))
})

test_that("generate_background_points erreur colonnes manquantes", {
  df_bad <- data.frame(x = 1:5, y = 1:5)
  r      <- terra::rast(nrows = 5, ncols = 5)
  expect_error(generate_background_points(df_bad, r))
})
