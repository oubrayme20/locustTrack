# ============================================================
# Tests unitaires — plot_temporal()
# Fichier : tests/testthat/test-plot_temporal.R
# ============================================================

test_that("plot_temporal retourne un data.frame", {
  result <- plot_temporal(annee      = 2023,
                          mois_debut = 1,
                          mois_fin   = 3)
  expect_s3_class(result, "data.frame")
})

test_that("plot_temporal a le bon nombre de lignes", {
  result <- plot_temporal(annee      = 2023,
                          mois_debut = 1,
                          mois_fin   = 6)
  expect_equal(nrow(result), 6)
})

test_that("plot_temporal contient les bonnes colonnes", {
  result <- plot_temporal(annee      = 2023,
                          mois_debut = 1,
                          mois_fin   = 3)
  expect_true("mois"       %in% names(result))
  expect_true("nom_mois"   %in% names(result))
  expect_true("ndvi_moyen" %in% names(result))
  expect_true("annee"      %in% names(result))
})

test_that("plot_temporal NDVI entre 0 et 1", {
  result <- plot_temporal(annee      = 2023,
                          mois_debut = 1,
                          mois_fin   = 3)
  expect_true(all(result$ndvi_moyen >= 0 &
                    result$ndvi_moyen <= 1))
})

test_that("plot_temporal erreur si mois_debut > mois_fin", {
  expect_error(plot_temporal(annee      = 2023,
                             mois_debut = 6,
                             mois_fin   = 3))
})
