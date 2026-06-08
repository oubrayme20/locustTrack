# ============================================================
# Tests unitaires — evaluate_model()
# Fichier : tests/testthat/test-evaluate_model.R
# ============================================================

# Dataset minimal pour les tests
create_rf_result <- function() {
  set.seed(42)
  n <- 100
  dataset <- data.frame(
    latitude  = runif(n, 10, 35),
    longitude = runif(n, -10, 50),
    presence  = sample(c(0, 1), n, replace = TRUE),
    clim_01   = runif(n),
    clim_02   = runif(n),
    ndvi      = runif(n, 0, 0.8)
  )
  train_rf_model(dataset, ntree = 10)
}

test_that("evaluate_model retourne une liste", {
  rf   <- create_rf_result()
  eval <- evaluate_model(rf)
  expect_type(eval, "list")
})

test_that("evaluate_model contient les métriques", {
  rf   <- create_rf_result()
  eval <- evaluate_model(rf)
  expect_true("metriques"         %in% names(eval))
  expect_true("matrice_confusion" %in% names(eval))
  expect_true("auc"               %in% names(eval))
  expect_true("predictions"       %in% names(eval))
})

test_that("evaluate_model AUC entre 0 et 1", {
  rf   <- create_rf_result()
  eval <- evaluate_model(rf)
  expect_gte(eval$auc, 0)
  expect_lte(eval$auc, 1)
})

test_that("evaluate_model metriques contient AUC et Accuracy", {
  rf   <- create_rf_result()
  eval <- evaluate_model(rf)
  expect_true("AUC"      %in% eval$metriques$Metrique)
  expect_true("Accuracy" %in% eval$metriques$Metrique)
})

test_that("evaluate_model predictions a les bonnes colonnes", {
  rf   <- create_rf_result()
  eval <- evaluate_model(rf)
  expect_true("observe" %in% names(eval$predictions))
  expect_true("predit"  %in% names(eval$predictions))
  expect_true("proba"   %in% names(eval$predictions))
  expect_true("correct" %in% names(eval$predictions))
})
