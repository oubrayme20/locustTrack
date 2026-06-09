# ============================================================
# Tests unitaires — plot_temporal()
# Fichier : tests/testthat/test-plot_temporal.R
# ============================================================
# Tests réseau marqués skip_if_offline() + skip_if_not_installed()
# ============================================================

# ── Tests de validation — sans réseau ────────────────────────

test_that("plot_temporal erreur si mois_debut > mois_fin", {
  expect_error(
    plot_temporal(annee = 2023, mois_debut = 6, mois_fin = 3),
    "mois_debut"
  )
})

# ── Tests fonctionnels — nécessitent internet ─────────────────

test_that("plot_temporal retourne un data.frame", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  result <- plot_temporal(
    annee      = 2022,
    mois_debut = 1,
    mois_fin   = 3,
    lon_min    = 9, lon_max = 20,
    lat_min    = 12, lat_max = 20
  )
  expect_s3_class(result, "data.frame")
})

test_that("plot_temporal a le bon nombre de lignes", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  result <- plot_temporal(
    annee      = 2022,
    mois_debut = 1,
    mois_fin   = 4,
    lon_min    = 9, lon_max = 20,
    lat_min    = 12, lat_max = 20
  )
  expect_equal(nrow(result), 4)
})

test_that("plot_temporal contient les bonnes colonnes", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  result <- plot_temporal(
    annee      = 2022,
    mois_debut = 1,
    mois_fin   = 2,
    lon_min    = 9, lon_max = 20,
    lat_min    = 12, lat_max = 20
  )
  expect_true("mois"       %in% names(result))
  expect_true("nom_mois"   %in% names(result))
  expect_true("ndvi_moyen" %in% names(result))
  expect_true("annee"      %in% names(result))
})

test_that("plot_temporal NDVI entre -1 et 1", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  result <- plot_temporal(
    annee      = 2022,
    mois_debut = 1,
    mois_fin   = 2,
    lon_min    = 9, lon_max = 20,
    lat_min    = 12, lat_max = 20
  )
  expect_true(all(result$ndvi_moyen >= -1 &
                    result$ndvi_moyen <= 1,
                  na.rm = TRUE))
})

test_that("plot_temporal avec occurrences utilise la bonne zone", {
  skip_if_offline()
  skip_if_not_installed("MODISTools")

  occ <- data.frame(
    latitude  = c(14, 16, 15),
    longitude = c(10, 12, 11),
    presence  = 1
  )
  result <- plot_temporal(
    annee       = 2022,
    mois_debut  = 1,
    mois_fin    = 2,
    occurrences = occ
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
})
