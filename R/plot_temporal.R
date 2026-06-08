#' Graphiques temporels de l'évolution NDVI et précipitations
#'
#' Génère des graphiques temporels montrant l'évolution du NDVI
#' et des précipitations sur plusieurs mois/années.
#' Permet de visualiser les conditions favorables aux criquets.
#'
#' @param annee Année d'analyse. Par défaut 2023
#' @param mois_debut Mois de début (1-12). Par défaut 1
#' @param mois_fin Mois de fin (1-12). Par défaut 12
#' @param lon_min Longitude minimale. Par défaut -20
#' @param lon_max Longitude maximale. Par défaut 65
#' @param lat_min Latitude minimale. Par défaut -10
#' @param lat_max Latitude maximale. Par défaut 40
#' @param export Si TRUE, exporte en PNG. Par défaut FALSE
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Un data.frame avec les valeurs NDVI moyennes par mois
#'
#' @examples
#' \dontrun{
#' # Graphique temporel annuel
#' temporal <- plot_temporal(annee = 2023,
#'                           mois_debut = 1,
#'                           mois_fin   = 12)
#' print(temporal)
#'
#' # Avec export PNG
#' plot_temporal(annee = 2023, export = TRUE)
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

  # ── Télécharger NDVI pour chaque mois ─────────────────────
  ndvi_moyennes <- sapply(sequence_mois, function(m) {
    message("  → NDVI mois ", sprintf("%02d", m), "...")
    r <- download_ndvi(
      annee      = annee,
      mois       = m,
      lon_min    = lon_min,
      lon_max    = lon_max,
      lat_min    = lat_min,
      lat_max    = lat_max,
      simuler    = TRUE
    )
    round(mean(terra::values(r), na.rm = TRUE), 3)
  })

  # ── Créer le data.frame temporel ──────────────────────────
  df_temporal <- data.frame(
    mois        = sequence_mois,
    nom_mois    = noms_mois[sequence_mois],
    ndvi_moyen  = ndvi_moyennes,
    annee       = annee
  )

  # ── Créer dossier si export ────────────────────────────────
  if (export && !dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  # ── Graphique 1 : Evolution NDVI mensuelle ─────────────────
  if (export) {
    grDevices::png(
      file.path(dossier, paste0("temporal_ndvi_", annee, ".png")),
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

  axis(1,
       at     = sequence_mois,
       labels = noms_mois[sequence_mois])

  abline(h   = mean(ndvi_moyennes),
         lty = 2,
         col = "gray",
         lwd = 1)

  text(x      = sequence_mois,
       y      = ndvi_moyennes + 0.01,
       labels = round(ndvi_moyennes, 2),
       cex    = 0.7,
       col    = "darkgreen")

  legend("topright",
         legend = c(paste("NDVI moyen annuel :",
                          round(mean(ndvi_moyennes), 3)),
                    "Moyenne annuelle"),
         col    = c("darkgreen", "gray"),
         lty    = c(1, 2),
         lwd    = 2,
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Graphique NDVI exporté")
  }

  # ── Graphique 2 : Saisons favorables aux criquets ──────────
  if (export) {
    grDevices::png(
      file.path(dossier,
                paste0("temporal_saisons_", annee, ".png")),
      width = 1200, height = 600, res = 150
    )
  }

  # Identifier les mois favorables (NDVI > moyenne)
  seuil_favorable <- mean(ndvi_moyennes)
  couleurs <- ifelse(ndvi_moyennes >= seuil_favorable,
                     "#e74c3c", "#2ecc71")

  barplot(ndvi_moyennes,
          names.arg = noms_mois[sequence_mois],
          col       = couleurs,
          main      = paste0("Mois favorables aux criquets — ",
                             annee),
          xlab      = "Mois",
          ylab      = "NDVI moyen",
          border    = "white")

  abline(h   = seuil_favorable,
         lty = 2,
         col = "black",
         lwd = 2)

  legend("topright",
         legend = c("Favorable (NDVI élevé)",
                    "Défavorable (NDVI faible)",
                    "Seuil moyen"),
         fill   = c("#e74c3c", "#2ecc71", NA),
         lty    = c(NA, NA, 2),
         border = c("white", "white", NA),
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Graphique saisons exporté")
  }

  # ── Résumé ────────────────────────────────────────────────
  cat("\n=== Résumé temporel ", annee, "===\n")
  cat("NDVI moyen annuel  :", round(mean(ndvi_moyennes), 3), "\n")
  cat("NDVI maximum       :", round(max(ndvi_moyennes), 3),
      "(mois", sequence_mois[which.max(ndvi_moyennes)], ")\n")
  cat("NDVI minimum       :", round(min(ndvi_moyennes), 3),
      "(mois", sequence_mois[which.min(ndvi_moyennes)], ")\n")
  cat("Mois favorables    :",
      sum(ndvi_moyennes >= seuil_favorable), "/",
      length(sequence_mois), "\n")

  print(df_temporal)

  return(df_temporal)
}
