# ============================================================
# Tests unitaires — prepare_predictors()
# Fichier : tests/testthat/test-prepare_predictors.R
# ============================================================

# Créer données de test correctes
create_test_data <- function() {
  set.seed(42)

  # Occurrences dans la zone du raster
  occ <- data.frame(
    latitude  = c(15, 20, 25, 18, 22),
    longitude = c(10, 20, 30, 15, 25),
    presence  = 1
  )

  # Créer raster climatique avec valeurs
  clim <- terra::rast(
    nrows = 10, ncols = 10,
    xmin  = 0,  xmax  = 40,
    ymin  = 10, ymax  = 30,
    crs   = "EPSG:4326"
  )
  terra::values(clim) <- runif(terra::ncell(clim))
  names(clim) <- "clim_01"

  # Créer raster NDVI avec valeurs
  ndvi <- terra::rast(
    nrows = 10, ncols = 10,
    xmin  = 0,  xmax  = 40,
    ymin  = 10, ymax  = 30,
    crs   = "EPSG:4326"
  )
  terra::values(ndvi) <- runif(terra::ncell(ndvi), 0, 0.8)
  names(ndvi) <- "ndvi"

  list(occ = occ, clim = clim, ndvi = ndvi)
}

test_that("prepare_predictors retourne un data.frame", {
  d       <- create_test_data()
  dataset <- prepare_predictors(d$occ, d$clim, d$ndvi)
  expect_s3_class(dataset, "data.frame")
})

test_that("prepare_predictors contient colonne presence", {
  d       <- create_test_data()
  dataset <- prepare_predictors(d$occ, d$clim, d$ndvi)
  expect_true("presence" %in% names(dataset))
})

test_that("prepare_predictors contient colonne ndvi", {
  d       <- create_test_data()
  dataset <- prepare_predictors(d$occ, d$clim, d$ndvi)
  expect_true("ndvi" %in% names(dataset))
})

test_that("prepare_predictors erreur si colonnes manquantes", {
  df_bad <- data.frame(x = 1:5, y = 1:5)
  clim   <- terra::rast(nrows = 5, ncols = 5,
                        xmin = 0, xmax = 10,
                        ymin = 0, ymax = 10)
  terra::values(clim) <- runif(25)
  ndvi   <- terra::rast(nrows = 5, ncols = 5,
                        xmin = 0, xmax = 10,
                        ymin = 0, ymax = 10)
  terra::values(ndvi) <- runif(25)
  expect_error(prepare_predictors(df_bad, clim, ndvi))
})
