# ============================================================
# Tests unitaires — clean_occurrences()
# Fichier : tests/testthat/test-clean_occurrences.R
# ============================================================

test_that("clean_occurrences retourne un data.frame", {
  df <- data.frame(
    latitude  = c(15.2, 12.8, 18.5),
    longitude = c(38.5, 42.1, 35.7),
    presence  = 1
  )
  result <- clean_occurrences(df)
  expect_s3_class(result, "data.frame")
})

test_that("clean_occurrences supprime les coordonnées invalides", {
  df <- data.frame(
    latitude  = c(15.2, 999, 18.5),
    longitude = c(38.5, 42.1, 35.7),
    presence  = 1
  )
  result <- clean_occurrences(df)
  expect_true(all(result$latitude >= -90 & result$latitude <= 90))
})

test_that("clean_occurrences supprime les doublons spatiaux", {
  df <- data.frame(
    latitude  = c(15.2, 15.2, 18.5),
    longitude = c(38.5, 38.5, 35.7),
    presence  = 1
  )
  result <- clean_occurrences(df)
  expect_equal(nrow(result), 2)
})

test_that("clean_occurrences retourne erreur si colonnes manquantes", {
  df <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_error(clean_occurrences(df))
})
