#' Graphiques temporels de l'évolution NDVI et précipitations
#'
#' Génère des graphiques temporels montrant l'évolution mensuelle
#' du NDVI et des précipitations sur la zone d'étude.
#' Permet de visualiser les conditions favorables aux criquets.
#'
#' @param annee Année d'analyse. Par défaut 2023
#' @param mois_debut Mois de début (1-12). Par défaut 1
#' @param mois_fin Mois de fin (1-12). Par défaut 12
#' @param lon_min Longitude minimale. Par défaut -20
#' @param lon_max Longitude maximale. Par défaut 65
#' @param lat_min Latitude minimale. Par défaut -10
#' @param lat_max Latitude maximale. Par défaut 40
#' @param climat SpatRaster climatique optionnel pour les
#'   précipitations. Par défaut NULL
#' @param export Si TRUE, exporte en PNG. Par défaut FALSE
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Un data.frame avec les valeurs NDVI et
#'   précipitations moyennes par mois
#'
#' @examples
#' \dontrun{
#' # NDVI seul
#' temporal <- plot_temporal(annee = 2023,
#'                           mois_debut = 1,
#'                           mois_fin   = 12)
#'
#' # NDVI + précipitations
#' clim <- download_climate_data(var = "prec")
#' temporal <- plot_temporal(annee  = 2023,
#'                           climat = clim,
#'                           export = TRUE)
#' }
#'
#' @export
plot_temporal <- function(annee      = 2023,
                          mois_debut = 1,
                          mois_fin   = 12,
                          lon_min    = -20,
                          lon_max    =  65,
                          lat_min    = -10,
                          lat_max    =  40,
                          climat     = NULL,
                          export     = FALSE,
                          dossier    = "outputs") {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Validation
  if (mois_debut > mois_fin) {
    stop("mois_debut doit être <= mois_fin")
  }

  # Séquence des mois
  sequence_mois <- mois_debut:mois_fin
  noms_mois <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun",
                 "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc")

  message("Génération des graphiques temporels pour ",
          annee, " (mois ", mois_debut, " à ", mois_fin, ")...")

  # Créer dossier si export
  if (export && !dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  # ── 1. Télécharger NDVI pour chaque mois ──────────────────
  message("Calcul NDVI mensuel...")
  ndvi_moyennes <- sapply(sequence_mois, function(m) {
    r <- download_ndvi(
      annee   = annee,
      mois    = m,
      lon_min = lon_min,
      lon_max = lon_max,
      lat_min = lat_min,
      lat_max = lat_max,
      simuler = TRUE
    )
    round(mean(terra::values(r), na.rm = TRUE), 3)
  })

  # ── 2. Extraire précipitations si fourni ──────────────────
  prec_moyennes <- NULL

  if (!is.null(climat)) {
    message("Extraction des précipitations mensuelles...")

    tryCatch({
      zone <- terra::ext(lon_min, lon_max, lat_min, lat_max)

      # Extraire les précipitations pour chaque mois disponible
      n_couches <- terra::nlyr(climat)

      prec_moyennes <- sapply(sequence_mois, function(m) {
        idx <- min(m, n_couches)
        r   <- terra::crop(climat[[idx]], zone)
        round(mean(terra::values(r), na.rm = TRUE), 1)
      })

      message("Précipitations extraites pour ",
              length(sequence_mois), " mois")

    }, error = function(e) {
      message("Erreur précipitations : ", e$message)
      prec_moyennes <<- NULL
    })
  }

  # ── 3. Créer le data.frame temporel ───────────────────────
  df_temporal <- data.frame(
    mois       = sequence_mois,
    nom_mois   = noms_mois[sequence_mois],
    ndvi_moyen = ndvi_moyennes,
    annee      = annee
  )

  if (!is.null(prec_moyennes)) {
    df_temporal$precipitation_mm <- prec_moyennes
  }

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 1 — Evolution NDVI mensuelle
  # ══════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier,
                paste0("temporal_ndvi_", annee, ".png")),
      width = 1200, height = 600, res = 150
    )
  }

  plot(sequence_mois, ndvi_moyennes,
       type  = "b",
       pch   = 19,
       col   = "darkgreen",
       lwd   = 2,
       xlab  = "Mois",
       ylab  = "NDVI moyen",
       main  = paste0("Évolution NDVI mensuelle — ", annee),
       xaxt  = "n",
       ylim  = c(0, max(ndvi_moyennes) * 1.2))

  axis(1, at = sequence_mois,
       labels = noms_mois[sequence_mois])

  abline(h   = mean(ndvi_moyennes),
         lty = 2, col = "gray", lwd = 1)

  text(x      = sequence_mois,
       y      = ndvi_moyennes + max(ndvi_moyennes) * 0.05,
       labels = round(ndvi_moyennes, 2),
       cex    = 0.7,
       col    = "darkgreen")

  legend("topright",
         legend = c("NDVI moyen", "Moyenne annuelle"),
         col    = c("darkgreen", "gray"),
         lty    = c(1, 2),
         lwd    = 2,
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Graphique NDVI exporté")
  }

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 2 — Précipitations mensuelles (si disponible)
  # ══════════════════════════════════════════════════════════
  if (!is.null(prec_moyennes)) {

    if (export) {
      grDevices::png(
        file.path(dossier,
                  paste0("temporal_precipitation_",
                         annee, ".png")),
        width = 1200, height = 600, res = 150
      )
    }

    barplot(prec_moyennes,
            names.arg = noms_mois[sequence_mois],
            col       = "#3498db",
            main      = paste0("Précipitations mensuelles — ",
                               annee),
            xlab      = "Mois",
            ylab      = "Précipitations (mm)",
            border    = "white")

    abline(h   = mean(prec_moyennes),
           lty = 2, col = "darkblue", lwd = 2)

    legend("topright",
           legend = c("Précipitations",
                      paste("Moyenne :",
                            round(mean(prec_moyennes), 1),
                            "mm")),
           fill   = c("#3498db", NA),
           lty    = c(NA, 2),
           col    = c(NA, "darkblue"),
           border = c("white", NA),
           cex    = 0.8)

    if (export) {
      grDevices::dev.off()
      message("Graphique précipitations exporté")
    }
  }

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 3 — NDVI vs Précipitations (si disponible)
  # ══════════════════════════════════════════════════════════
  if (!is.null(prec_moyennes)) {

    if (export) {
      grDevices::png(
        file.path(dossier,
                  paste0("temporal_ndvi_vs_prec_",
                         annee, ".png")),
        width = 1200, height = 600, res = 150
      )
    }

    # Double axe Y : NDVI + précipitations
    par(mar = c(5, 4, 4, 5))

    plot(sequence_mois, ndvi_moyennes,
         type  = "b",
         pch   = 19,
         col   = "darkgreen",
         lwd   = 2,
         xlab  = "Mois",
         ylab  = "NDVI moyen",
         main  = paste0("NDVI vs Précipitations — ", annee),
         xaxt  = "n",
         ylim  = c(0, max(ndvi_moyennes) * 1.3))

    axis(1, at = sequence_mois,
         labels = noms_mois[sequence_mois])

    # Axe droit pour précipitations
    par(new = TRUE)
    plot(sequence_mois, prec_moyennes,
         type  = "b",
         pch   = 17,
         col   = "#3498db",
         lwd   = 2,
         axes  = FALSE,
         xlab  = "",
         ylab  = "")

    axis(4, col = "#3498db",
         col.axis = "#3498db")
    mtext("Précipitations (mm)",
          side = 4, line = 3,
          col  = "#3498db")

    legend("topleft",
           legend = c("NDVI moyen",
                      "Précipitations (mm)"),
           col    = c("darkgreen", "#3498db"),
           lty    = c(1, 1),
           pch    = c(19, 17),
           lwd    = 2,
           cex    = 0.8)

    par(mar = c(5, 4, 4, 2))

    if (export) {
      grDevices::dev.off()
      message("Graphique NDVI vs Précipitations exporté")
    }
  }

  # ══════════════════════════════════════════════════════════
  # GRAPHIQUE 4 — Mois favorables aux criquets
  # ══════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier,
                paste0("temporal_saisons_", annee, ".png")),
      width = 1200, height = 600, res = 150
    )
  }

  seuil_fav <- mean(ndvi_moyennes)
  couleurs   <- ifelse(ndvi_moyennes >= seuil_fav,
                       "#e74c3c", "#2ecc71")

  barplot(ndvi_moyennes,
          names.arg = noms_mois[sequence_mois],
          col       = couleurs,
          main      = paste0("Mois favorables aux criquets — ",
                             annee),
          xlab      = "Mois",
          ylab      = "NDVI moyen",
          border    = "white")

  abline(h = seuil_fav, lty = 2, col = "black", lwd = 2)

  legend("topright",
         legend = c("Favorable (NDVI élevé)",
                    "Défavorable (NDVI faible)"),
         fill   = c("#e74c3c", "#2ecc71"),
         border = "white",
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Graphique saisons exporté")
  }

  # ── Résumé ────────────────────────────────────────────────
  cat("\n=== Résumé temporel", annee, "===\n")
  cat("NDVI moyen annuel :", round(mean(ndvi_moyennes), 3), "\n")
  cat("NDVI maximum      :", round(max(ndvi_moyennes), 3),
      "(mois", sequence_mois[which.max(ndvi_moyennes)], ")\n")
  cat("NDVI minimum      :", round(min(ndvi_moyennes), 3),
      "(mois", sequence_mois[which.min(ndvi_moyennes)], ")\n")

  if (!is.null(prec_moyennes)) {
    cat("Préc. moyenne     :",
        round(mean(prec_moyennes), 1), "mm\n")
    cat("Préc. maximum     :",
        round(max(prec_moyennes), 1), "mm",
        "(mois", sequence_mois[which.max(prec_moyennes)], ")\n")
  }

  print(df_temporal)
  return(df_temporal)
}
