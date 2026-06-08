# ============================================================
# Tests unitaires — import_locust_data()
# Fichier : tests/testthat/test-import_locust_data.R
# Méthode : test_that() + expect_*() comme la prof
# ============================================================

test_that("import_locust_data retourne une erreur si fichier inexistant", {
  expect_error(import_locust_data("fichier_inexistant.csv"))
})

test_that("import_locust_data retourne un data.frame", {
  # Créer un fichier CSV temporaire
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    latitude  = c(15.2, 12.8, 18.5),
    longitude = c(38.5, 42.1, 35.7),
    date      = c("2022-03-15", "2022-04-02", "2022-03-28")
  ), tmp, row.names = FALSE)

  result <- import_locust_data(tmp)
  expect_s3_class(result, "data.frame")
})

test_that("import_locust_data contient les bonnes colonnes", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    latitude  = c(15.2, 12.8),
    longitude = c(38.5, 42.1),
    date      = c("2022-03-15", "2022-04-02")
  ), tmp, row.names = FALSE)

  result <- import_locust_data(tmp)
  expect_true("latitude"  %in% names(result))
  expect_true("longitude" %in% names(result))
  expect_true("presence"  %in% names(result))
  expect_true("date"      %in% names(result))
})

test_that("import_locust_data ajoute presence = 1", {
  tmp <- tempfile(fileext = ".csv")
  write.csv(data.frame(
    latitude  = c(15.2, 12.8),
    longitude = c(38.5, 42.1),
    date      = c("2022-03-15", "2022-04-02")
  ), tmp, row.names = FALSE)

  result <- import_locust_data(tmp)
  expect_true(all(result$presence == 1))
})
