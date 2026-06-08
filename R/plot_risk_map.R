#' Cartographier le risque d'invasion des criquets pèlerins
#'
#' Produit les cartes visuelles du risque acridien : NDVI, greenup,
#' probabilité de présence, carte classée et hotspots.
#' Export automatique en PNG ou PDF.
#'
#' @param risk_result Liste issue de \code{predict_risk_map()}
#' @param ndvi SpatRaster NDVI (issu de download_ndvi)
#' @param greenup_result Liste issue de \code{calculate_greenup()}. Par défaut NULL
#' @param occurrences data.frame des occurrences. Par défaut NULL
#' @param export Exporter les cartes ? Par défaut FALSE
#' @param format Format export : "png" ou "pdf". Par défaut "png"
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Invisible NULL. Produit les graphiques et exports.
#'
#' @examples
#' \dontrun{
#' risk <- predict_risk_map(rf, clim, ndvi)
#' ndvi <- download_ndvi(2023, mois = 6)
#' plot_risk_map(risk, ndvi)
#' plot_risk_map(risk, ndvi, export = TRUE, format = "png")
#' }
#'
#' @export
plot_risk_map <- function(risk_result,
                          ndvi,
                          greenup_result = NULL,
                          occurrences    = NULL,
                          export         = FALSE,
                          format         = "png",
                          dossier        = "outputs") {

  # Vérifier terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Le package 'terra' est requis : install.packages('terra')")
  }

  # Créer dossier de sortie si export demandé
  if (export && !dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
    message("Dossier créé : ", dossier)
  }

  # Extraire les rasters
  risque_continu <- risk_result$risque_continu
  risque_classe  <- risk_result$risque_classe

  # ── Fonction utilitaire export ─────────────────────────────────────────────
  ouvrir_export <- function(nom_fichier, format) {
    chemin <- file.path(dossier, nom_fichier)
    if (format == "png") {
      grDevices::png(chemin, width = 1200, height = 800, res = 150)
    } else {
      grDevices::pdf(chemin, width = 10, height = 7)
    }
    return(chemin)
  }

  # ══════════════════════════════════════════════════════════════════════════
  # CARTE 1 — NDVI
  # ══════════════════════════════════════════════════════════════════════════
  if (export) ouvrir_export(paste0("carte_ndvi.", format), format)

  terra::plot(ndvi,
              main   = "NDVI — Indice de Végétation",
              col    = grDevices::colorRampPalette(
                c("#d73027", "#fee08b", "#1a9850"))(100),
              legend = TRUE)

  if (!is.null(occurrences)) {
    points(occurrences$longitude, occurrences$latitude,
           pch = 20, col = "blue", cex = 0.5)
    legend("bottomleft",
           legend = "Occurrences criquets",
           pch    = 20,
           col    = "blue",
           cex    = 0.7)
  }

  if (export) {
    grDevices::dev.off()
    message("Carte NDVI exportée")
  }

  # ══════════════════════════════════════════════════════════════════════════
  # CARTE 2 — GREENUP (si fourni)
  # ══════════════════════════════════════════════════════════════════════════
  if (!is.null(greenup_result)) {

    if (export) ouvrir_export(paste0("carte_greenup.", format), format)

    terra::plot(greenup_result$anomalie,
                main = "Anomalie NDVI — Verdissement post-pluie",
                col  = grDevices::colorRampPalette(
                  c("#d73027", "#ffffff", "#1a9850"))(100),
                legend = TRUE)

    if (export) {
      grDevices::dev.off()
      message("Carte Greenup exportée")
    }
  }

  # ══════════════════════════════════════════════════════════════════════════
  # CARTE 3 — PROBABILITÉ CONTINUE DE PRÉSENCE
  # ══════════════════════════════════════════════════════════════════════════
  if (export) ouvrir_export(paste0("carte_probabilite.", format), format)

  terra::plot(risque_continu,
              main   = "Probabilité de Présence — Criquet pèlerin",
              col    = grDevices::colorRampPalette(
                c("#ffffcc", "#fd8d3c", "#800026"))(100),
              legend = TRUE)

  if (!is.null(occurrences)) {
    points(occurrences$longitude, occurrences$latitude,
           pch = 20, col = "blue", cex = 0.5)
  }

  if (export) {
    grDevices::dev.off()
    message("Carte probabilité exportée")
  }

  # ══════════════════════════════════════════════════════════════════════════
  # CARTE 4 — RISQUE CLASSÉ (faible / moyen / élevé)
  # ══════════════════════════════════════════════════════════════════════════
  if (export) ouvrir_export(paste0("carte_risque_classe.", format), format)

  terra::plot(risque_classe,
              main   = "Carte de Risque d'Invasion — Criquet pèlerin",
              col    = c("#2ecc71", "#f39c12", "#e74c3c"),
              legend = FALSE)

  legend("bottomleft",
         legend = c("Faible", "Moyen", "Élevé"),
         fill   = c("#2ecc71", "#f39c12", "#e74c3c"),
         title  = "Niveau de risque",
         cex    = 0.8)

  if (!is.null(occurrences)) {
    points(occurrences$longitude, occurrences$latitude,
           pch = 20, col = "black", cex = 0.5)
  }

  if (export) {
    grDevices::dev.off()
    message("Carte risque classé exportée")
  }

  # ══════════════════════════════════════════════════════════════════════════
  # CARTE 5 — HOTSPOTS (zones > 0.7)
  # ══════════════════════════════════════════════════════════════════════════
  if (export) ouvrir_export(paste0("carte_hotspots.", format), format)

  hotspots <- terra::ifel(risque_continu >= 0.7, risque_continu, NA)

  terra::plot(risque_continu,
              main   = "Hotspots — Zones de Très Haut Risque (> 0.7)",
              col    = grDevices::colorRampPalette(
                c("#ffffcc", "#fd8d3c", "#800026"))(100),
              legend = TRUE)

  terra::plot(hotspots,
              col    = "#e74c3c",
              legend = FALSE,
              add    = TRUE)

  legend("bottomleft",
         legend = "Hotspot (prob > 0.7)",
         fill   = "#e74c3c",
         cex    = 0.8)

  if (export) {
    grDevices::dev.off()
    message("Carte hotspots exportée")
  }

  if (export) {
    message("Toutes les cartes exportées dans : ", dossier)
  }

  invisible(NULL)
}
