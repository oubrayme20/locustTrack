# ============================================================
# Tests unitaires — train_rf_model()
# Fichier : tests/testthat/test-train_rf_model.R
# ============================================================

# Créer un dataset de test minimal
create_test_dataset <- function() {
  set.seed(42)
  n <- 100
  data.frame(
    latitude  = runif(n, 10, 35),
    longitude = runif(n, -10, 50),
    presence  = sample(c(0, 1), n, replace = TRUE),
    clim_01   = runif(n),
    clim_02   = runif(n),
    ndvi      = runif(n, 0, 0.8)
  )
}

test_that("train_rf_model retourne une liste", {
  dataset <- create_test_dataset()
  rf      <- train_rf_model(dataset, ntree = 10)
  expect_type(rf, "list")
})

test_that("train_rf_model contient le modèle et les données", {
  dataset <- create_test_dataset()
  rf      <- train_rf_model(dataset, ntree = 10)
  expect_true("modele"     %in% names(rf))
  expect_true("train"      %in% names(rf))
  expect_true("test"       %in% names(rf))
  expect_true("importance" %in% names(rf))
  expect_true("params"     %in% names(rf))
})

test_that("train_rf_model split 70/30 correct", {
  dataset <- create_test_dataset()
  rf      <- train_rf_model(dataset,
                            prop_train = 0.7,
                            ntree = 10)
  n_total <- nrow(rf$train) + nrow(rf$test)
  expect_equal(n_total, nrow(dataset))
  expect_true(nrow(rf$train) > nrow(rf$test))
})

test_that("train_rf_model erreur si colonne presence absente", {
  dataset <- data.frame(x = 1:10, y = 1:10)
  expect_error(train_rf_model(dataset))
})
