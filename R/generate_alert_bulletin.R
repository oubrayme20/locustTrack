#' Générer un bulletin mensuel d'alerte acridienne
#'
#' Produit un bulletin d'alerte HTML synthétisant les zones favorables,
#' le niveau de risque, les anomalies de végétation et les
#' recommandations de surveillance pour le mois en cours.
#'
#' @param risk_result Liste issue de \code{predict_risk_map()}
#' @param summary_result Liste issue de \code{summarize_risk_regions()}
#' @param greenup_result Liste issue de \code{calculate_greenup()}. Par défaut NULL
#' @param mois Mois du bulletin (1-12). Par défaut mois actuel
#' @param annee Année du bulletin. Par défaut année actuelle
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Chemin vers le fichier HTML généré
#'
#' @examples
#' \dontrun{
#' risk    <- predict_risk_map(rf, clim, ndvi)
#' summary <- summarize_risk_regions(risk)
#' bulletin <- generate_alert_bulletin(risk, summary, mois = 6, annee = 2024)
#' }
#'
#' @export
generate_alert_bulletin <- function(risk_result,
                                    summary_result,
                                    greenup_result = NULL,
                                    mois           = as.integer(format(Sys.Date(), "%m")),
                                    annee          = as.integer(format(Sys.Date(), "%Y")),
                                    dossier        = "outputs") {

  # Créer le dossier de sortie
  if (!dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  # ── Extraire les informations clés ────────────────────────────────────────
  resume         <- summary_result$resume
  stats_globales <- summary_result$stats_globales
  hotspots       <- summary_result$hotspots

  pct_eleve  <- resume$Pourcentage[resume$Niveau == "Eleve"]
  pct_moyen  <- resume$Pourcentage[resume$Niveau == "Moyen"]
  prob_max   <- stats_globales$Valeur[stats_globales$Indicateur ==
                                        "Probabilite maximale"]
  n_hotspots <- nrow(hotspots)

  # ── Déterminer le niveau d'alerte global ──────────────────────────────────
  if (pct_eleve > 20) {
    niveau_alerte <- "ÉLEVÉ"
    couleur_alerte <- "#e74c3c"
    emoji_alerte   <- "🔴"
  } else if (pct_eleve > 10 || pct_moyen > 30) {
    niveau_alerte <- "MODÉRÉ"
    couleur_alerte <- "#f39c12"
    emoji_alerte   <- "🟠"
  } else {
    niveau_alerte <- "FAIBLE"
    couleur_alerte <- "#2ecc71"
    emoji_alerte   <- "🟢"
  }

  # ── Noms des mois ─────────────────────────────────────────────────────────
  noms_mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
                 "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  nom_mois  <- noms_mois[mois]

  # ── Recommandations selon le niveau ───────────────────────────────────────
  if (niveau_alerte == "ÉLEVÉ") {
    recommandations <- c(
      "Mobiliser immédiatement les équipes de surveillance terrain",
      "Activer le protocole de traitement préventif",
      "Alerter les autorités phytosanitaires nationales",
      "Renforcer la surveillance des zones hotspot identifiées",
      "Coordonner avec les pays voisins pour le suivi transfrontalier"
    )
  } else if (niveau_alerte == "MODÉRÉ") {
    recommandations <- c(
      "Intensifier les tournées de prospection dans les zones à risque moyen",
      "Préparer les équipements de traitement en cas d'escalade",
      "Suivre l'évolution du NDVI et des précipitations hebdomadairement",
      "Informer les agriculteurs des zones concernées"
    )
  } else {
    recommandations <- c(
      "Maintenir la surveillance de routine mensuelle",
      "Continuer le suivi des données NDVI et climatiques",
      "Aucune intervention immédiate requise"
    )
  }

  # ── Informations greenup ───────────────────────────────────────────────────
  greenup_html <- ""
  if (!is.null(greenup_result)) {
    pct_greenup    <- greenup_result$stats$valeur[
      greenup_result$stats$indicateur == "Pourcentage verdissement (%)"]
    anomalie_moy   <- greenup_result$stats$valeur[
      greenup_result$stats$indicateur == "Anomalie NDVI moyenne"]

    greenup_html <- paste0('
      <div class="section">
        <h2>🌿 Analyse de la Végétation (Greenup)</h2>
        <table>
          <tr><th>Indicateur</th><th>Valeur</th></tr>
          <tr><td>Anomalie NDVI moyenne</td><td>', anomalie_moy, '</td></tr>
          <tr><td>Surface en verdissement</td><td>', pct_greenup, '%</td></tr>
        </table>
      </div>')
  }

  # ── Tableau des recommandations HTML ──────────────────────────────────────
  reco_html <- paste0("<li>", recommandations, "</li>", collapse = "\n")

  # ── Tableau hotspots HTML ─────────────────────────────────────────────────
  if (n_hotspots > 0) {
    top_hotspots  <- head(hotspots, 5)
    hotspot_rows  <- paste0(
      "<tr><td>", round(top_hotspots$latitude,  3), "</td>",
      "<td>",     round(top_hotspots$longitude, 3), "</td>",
      "<td>",     round(top_hotspots$probabilite, 3), "</td></tr>",
      collapse = "\n"
    )
    hotspot_html <- paste0('
      <div class="section">
        <h2>📍 Top 5 Hotspots Identifiés</h2>
        <table>
          <tr><th>Latitude</th><th>Longitude</th><th>Probabilité</th></tr>',
                           hotspot_rows, '
        </table>
      </div>')
  } else {
    hotspot_html <- '<div class="section"><h2>📍 Hotspots</h2>
                     <p>Aucun hotspot détecté ce mois.</p></div>'
  }

  # ── Générer le HTML complet ───────────────────────────────────────────────
  html_content <- paste0('
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <title>Bulletin Acridien — ', nom_mois, ' ', annee, '</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 40px;
           background: #f5f5f5; color: #333; }
    .header { background: #2c3e50; color: white;
              padding: 20px; border-radius: 8px; text-align: center; }
    .alerte { background: ', couleur_alerte, '; color: white;
               padding: 15px; border-radius: 8px;
               text-align: center; font-size: 1.4em;
               margin: 20px 0; }
    .section { background: white; padding: 20px;
                margin: 15px 0; border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
    h2 { color: #2c3e50; border-bottom: 2px solid #3498db;
         padding-bottom: 8px; }
    table { width: 100%; border-collapse: collapse; margin-top: 10px; }
    th { background: #3498db; color: white; padding: 10px; text-align: left; }
    td { padding: 8px; border-bottom: 1px solid #ddd; }
    tr:hover { background: #f0f8ff; }
    ul { line-height: 2; }
    .footer { text-align: center; color: #888;
               margin-top: 30px; font-size: 0.9em; }
  </style>
</head>
<body>

  <div class="header">
    <h1>🦗 Bulletin Mensuel Acridien</h1>
    <h2>', nom_mois, ' ', annee, '</h2>
    <p>Surveillance du Criquet Pèlerin — Schistocerca gregaria</p>
  </div>

  <div class="alerte">
    ', emoji_alerte, ' NIVEAU DE RISQUE GLOBAL : ', niveau_alerte, '
  </div>

  <div class="section">
    <h2>📊 Statistiques du Risque</h2>
    <table>
      <tr><th>Niveau</th><th>Pixels</th><th>Pourcentage</th><th>Prob. Moyenne</th></tr>
      <tr>
        <td>🟢 Faible</td>
        <td>', resume$N_pixels[1], '</td>
        <td>', resume$Pourcentage[1], '%</td>
        <td>', resume$Prob_moyenne[1], '</td>
      </tr>
      <tr>
        <td>🟠 Moyen</td>
        <td>', resume$N_pixels[2], '</td>
        <td>', resume$Pourcentage[2], '%</td>
        <td>', resume$Prob_moyenne[2], '</td>
      </tr>
      <tr>
        <td>🔴 Élevé</td>
        <td>', resume$N_pixels[3], '</td>
        <td>', resume$Pourcentage[3], '%</td>
        <td>', resume$Prob_moyenne[3], '</td>
      </tr>
    </table>
    <p><strong>Probabilité maximale observée :</strong> ', prob_max, '</p>
    <p><strong>Nombre de hotspots détectés :</strong> ', n_hotspots, ' pixels</p>
  </div>

  ', greenup_html, '

  ', hotspot_html, '

  <div class="section">
    <h2>📋 Recommandations de Surveillance</h2>
    <ul>', reco_html, '</ul>
  </div>

  <div class="footer">
    <p>Bulletin généré par le package locustTrack — ', Sys.time(), '</p>
    <p>Données : WorldClim | NDVI MODIS | Modèle Random Forest</p>
  </div>

</body>
</html>')

  # ── Sauvegarder le fichier HTML ───────────────────────────────────────────
  nom_fichier <- paste0("bulletin_acridien_", annee, "_",
                        sprintf("%02d", mois), ".html")
  chemin      <- file.path(dossier, nom_fichier)

  writeLines(html_content, chemin, useBytes = TRUE)

  message("Bulletin généré : ", chemin)

  return(chemin)
}
