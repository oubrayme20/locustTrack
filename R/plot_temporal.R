#' Graphiques temporels de l'évolution NDVI et précipitations
#'
#' Génère des graphiques temporels montrant l'évolution mensuelle
#' du NDVI et des précipitations sur la zone d'étude.
#' Permet de visualiser les conditions favorables aux criquets.
#'
#' @param annee Année d'analyse. Par défaut 2023
#' @param mois_debut Mois de début (1-12). Par défaut 1
#' @param mois_fin Mois de fin (1-12). Par défaut 12
#' @param occurrences data.frame avec colonnes \code{latitude} et
#'   \code{longitude} (issu de \code{clean_occurrences()}).
#'   Si fourni, la bbox NDVI est calculée depuis les coordonnées réelles.
#'   Par défaut NULL
#' @param lon_min Longitude minimale. Utilisé si occurrences = NULL.
#'   Par défaut -20
#' @param lon_max Longitude maximale. Utilisé si occurrences = NULL.
#'   Par défaut 65
#' @param lat_min Latitude minimale. Utilisé si occurrences = NULL.
#'   Par défaut -10
#' @param lat_max Latitude maximale. Utilisé si occurrences = NULL.
#'   Par défaut 40
#' @param climat SpatRaster climatique optionnel pour les précipitations
#'   (issu de \code{download_climate_data()}). Par défaut NULL
#' @param export Si TRUE, exporte les graphiques en PNG. Par défaut FALSE
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Un data.frame avec les colonnes :
#'   \item{mois}{Numéro du mois}
#'   \item{nom_mois}{Nom abrégé du mois}
#'   \item{ndvi_moyen}{NDVI moyen mensuel réel (MODISTools)}
#'   \item{annee}{Année d'analyse}
#'   \item{precipitation_mm}{Précipitations moyennes (si climat fourni)}
#'
#' @examples
#' \dontrun{
#' df_clean <- clean_occurrences(import_locust_data(source = "gbif"))
#'
#' # NDVI temporel sur la zone des occurrences réelles
#' temporal <- plot_temporal(
#'   annee       = 2023,
#'   mois_debut  = 1,
#'   mois_fin    = 12,
#'   occurrences = df_clean
#' )
#'
#' # Avec précipitations WorldClim
#' clim <- download_climate_data(var = "prec")
#' temporal <- plot_temporal(
#'   annee       = 2023,
#'   occurrences = df_clean,
#'   climat      = clim,
#'   export      = TRUE
#' )
#' }
#'
#' @seealso \code{\link{download_ndvi}}, \code{\link{download_climate_data}}
#'
#' @export
plot_temporal <- function(annee      = 2023,
                          mois_debut = 1,
                          mois_fin   = 12,
                          occurrences = NULL,
                          lon_min    = -20,
                          lon_max    =  65,
                          lat_min    = -10,
                          lat_max    =  40,
                          climat     = NULL,
                          export     = FALSE,
                          dossier    = "outputs") {

  # ── Vérifications ────────────────────────────────────────────
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  if (mois_debut > mois_fin) {
    stop("mois_debut doit etre <= mois_fin")
  }

  sequence_mois <- mois_debut:mois_fin
  noms_mois <- c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun",
                 "Jul", "Aou", "Sep", "Oct", "Nov", "Dec")

  message("Generation des graphiques temporels pour ",
          annee, " (mois ", mois_debut, " a ", mois_fin, ")...")

  if (export && !dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  # ── 1. Télécharger NDVI réel pour chaque mois ────────────────
  # download_ndvi() avec occurrences → bbox depuis coordonnées réelles
  # download_ndvi() sans occurrences → bbox explicite
  message("Telechargement NDVI MODIS mensuel (source : ORNL DAAC)...")

  ndvi_moyennes <- sapply(sequence_mois, function(m) {
    message("  -> Mois ", sprintf("%02d", m), "/", annee)
    r <- download_ndvi(
      annee       = annee,
      mois        = m,
      occurrences = occurrences,
      lon_min     = lon_min,
      lon_max     = lon_max,
      lat_min     = lat_min,
      lat_max     = lat_max
    )
    round(mean(terra::values(r), na.rm = TRUE), 3)
  })

  # ── 2. Extraire précipitations depuis WorldClim si fourni ─────
  prec_moyennes <- NULL

  if (!is.null(climat)) {
    message("Extraction des precipitations mensuelles (WorldClim)...")

    tryCatch({
      zone      <- terra::ext(lon_min, lon_max, lat_min, lat_max)
      n_couches <- terra::nlyr(climat)

      prec_moyennes <- sapply(sequence_mois, function(m) {
        idx <- min(m, n_couches)
        r   <- terra::crop(climat[[idx]], zone)
        round(mean(terra::values(r), na.rm = TRUE), 1)
      })

      message("Precipitations extraites pour ",
              length(sequence_mois), " mois")

    }, error = function(e) {
      message("Erreur precipitations : ", e$message)
      prec_moyennes <<- NULL
    })
  }

  # ── 3. Construire le data.frame temporel ─────────────────────
  df_temporal <- data.frame(
    mois       = sequence_mois,
    nom_mois   = noms_mois[sequence_mois],
    ndvi_moyen = ndvi_moyennes,
    annee      = annee
  )

  if (!is.null(prec_moyennes)) {
    df_temporal$precipitation_mm <- prec_moyennes
  }

  # ══════════════════════════════════════════════════════════════
  # GRAPHIQUE 1 — Evolution NDVI mensuelle
  # ══════════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier, paste0("temporal_ndvi_", annee, ".png")),
      width = 1200, height = 600, res = 150
    )
  }

  plot(sequence_mois, ndvi_moyennes,
       type = "b", pch = 19, col = "darkgreen", lwd = 2,
       xlab = "Mois", ylab = "NDVI moyen",
       main = paste0("Evolution NDVI mensuelle - ", annee),
       xaxt = "n",
       ylim = c(0, max(ndvi_moyennes, na.rm = TRUE) * 1.2))

  axis(1, at = sequence_mois, labels = noms_mois[sequence_mois])
  abline(h = mean(ndvi_moyennes, na.rm = TRUE),
         lty = 2, col = "gray", lwd = 1)
  text(x = sequence_mois,
       y = ndvi_moyennes + max(ndvi_moyennes, na.rm = TRUE) * 0.05,
       labels = round(ndvi_moyennes, 2), cex = 0.7, col = "darkgreen")
  legend("topright",
         legend = c("NDVI moyen", "Moyenne annuelle"),
         col = c("darkgreen", "gray"), lty = c(1, 2), lwd = 2, cex = 0.8)

  if (export) { grDevices::dev.off(); message("Graphique NDVI exporte") }

  # ══════════════════════════════════════════════════════════════
  # GRAPHIQUE 2 — Précipitations mensuelles
  # ══════════════════════════════════════════════════════════════
  if (!is.null(prec_moyennes)) {

    if (export) {
      grDevices::png(
        file.path(dossier,
                  paste0("temporal_precipitation_", annee, ".png")),
        width = 1200, height = 600, res = 150
      )
    }

    barplot(prec_moyennes,
            names.arg = noms_mois[sequence_mois],
            col = "#3498db",
            main = paste0("Precipitations mensuelles - ", annee),
            xlab = "Mois", ylab = "Precipitations (mm)",
            border = "white")
    abline(h = mean(prec_moyennes, na.rm = TRUE),
           lty = 2, col = "darkblue", lwd = 2)
    legend("topright",
           legend = c("Precipitations",
                      paste("Moyenne :",
                            round(mean(prec_moyennes, na.rm = TRUE), 1),
                            "mm")),
           fill = c("#3498db", NA), lty = c(NA, 2),
           col = c(NA, "darkblue"), border = c("white", NA), cex = 0.8)

    if (export) {
      grDevices::dev.off()
      message("Graphique precipitations exporte")
    }
  }

  # ══════════════════════════════════════════════════════════════
  # GRAPHIQUE 3 — NDVI vs Précipitations
  # ══════════════════════════════════════════════════════════════
  if (!is.null(prec_moyennes)) {

    if (export) {
      grDevices::png(
        file.path(dossier,
                  paste0("temporal_ndvi_vs_prec_", annee, ".png")),
        width = 1200, height = 600, res = 150
      )
    }

    par(mar = c(5, 4, 4, 5))
    plot(sequence_mois, ndvi_moyennes,
         type = "b", pch = 19, col = "darkgreen", lwd = 2,
         xlab = "Mois", ylab = "NDVI moyen",
         main = paste0("NDVI vs Precipitations - ", annee),
         xaxt = "n",
         ylim = c(0, max(ndvi_moyennes, na.rm = TRUE) * 1.3))
    axis(1, at = sequence_mois, labels = noms_mois[sequence_mois])

    par(new = TRUE)
    plot(sequence_mois, prec_moyennes,
         type = "b", pch = 17, col = "#3498db", lwd = 2,
         axes = FALSE, xlab = "", ylab = "")
    axis(4, col = "#3498db", col.axis = "#3498db")
    mtext("Precipitations (mm)", side = 4, line = 3, col = "#3498db")

    legend("topleft",
           legend = c("NDVI moyen", "Precipitations (mm)"),
           col = c("darkgreen", "#3498db"),
           lty = c(1, 1), pch = c(19, 17), lwd = 2, cex = 0.8)
    par(mar = c(5, 4, 4, 2))

    if (export) {
      grDevices::dev.off()
      message("Graphique NDVI vs Precipitations exporte")
    }
  }

  # ══════════════════════════════════════════════════════════════
  # GRAPHIQUE 4 — Mois favorables aux criquets
  # ══════════════════════════════════════════════════════════════
  if (export) {
    grDevices::png(
      file.path(dossier,
                paste0("temporal_saisons_", annee, ".png")),
      width = 1200, height = 600, res = 150
    )
  }

  seuil_fav <- mean(ndvi_moyennes, na.rm = TRUE)
  couleurs   <- ifelse(ndvi_moyennes >= seuil_fav, "#e74c3c", "#2ecc71")

  barplot(ndvi_moyennes,
          names.arg = noms_mois[sequence_mois],
          col = couleurs,
          main = paste0("Mois favorables aux criquets - ", annee),
          xlab = "Mois", ylab = "NDVI moyen", border = "white")
  abline(h = seuil_fav, lty = 2, col = "black", lwd = 2)
  legend("topright",
         legend = c("Favorable (NDVI eleve)", "Defavorable (NDVI faible)"),
         fill = c("#e74c3c", "#2ecc71"), border = "white", cex = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Graphique saisons exporte")
  }

  # ── Résumé ───────────────────────────────────────────────────
  cat("\n=== Resume temporel", annee, "===\n")
  cat("NDVI moyen annuel :", round(mean(ndvi_moyennes, na.rm=TRUE), 3), "\n")
  cat("NDVI maximum      :", round(max(ndvi_moyennes, na.rm=TRUE), 3),
      "(mois", sequence_mois[which.max(ndvi_moyennes)], ")\n")
  cat("NDVI minimum      :", round(min(ndvi_moyennes, na.rm=TRUE), 3),
      "(mois", sequence_mois[which.min(ndvi_moyennes)], ")\n")

  if (!is.null(prec_moyennes)) {
    cat("Prec. moyenne     :", round(mean(prec_moyennes, na.rm=TRUE), 1),
        "mm\n")
    cat("Prec. maximum     :", round(max(prec_moyennes, na.rm=TRUE), 1),
        "mm (mois", sequence_mois[which.max(prec_moyennes)], ")\n")
  }

  print(df_temporal)
  return(df_temporal)
}
