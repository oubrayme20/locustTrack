#' Générer un rapport complet d'analyse acridienne
#'
#' Produit un rapport HTML complet synthétisant toutes les étapes
#' de l'analyse : données utilisées, performances du modèle,
#' cartes de risque et interprétation écologique.
#'
#' @param occurrences data.frame des occurrences nettoyées
#' @param rf_result Liste issue de \code{train_rf_model()}
#' @param eval_result Liste issue de \code{evaluate_model()}
#' @param risk_result Liste issue de \code{predict_risk_map()}
#' @param summary_result Liste issue de \code{summarize_risk_regions()}
#' @param greenup_result Liste issue de \code{calculate_greenup()}. Par défaut NULL
#' @param annee Année d'analyse. Par défaut année actuelle
#' @param dossier Dossier de sortie. Par défaut "outputs"
#'
#' @return Chemin vers le fichier HTML généré
#'
#' @examples
#' \dontrun{
#' rapport <- generate_report(
#'   occurrences    = df_clean,
#'   rf_result      = rf,
#'   eval_result    = eval,
#'   risk_result    = risk,
#'   summary_result = summary,
#'   annee          = 2024
#' )
#' }
#'
#' @export
generate_report <- function(occurrences,
                            rf_result,
                            eval_result,
                            risk_result,
                            summary_result,
                            greenup_result = NULL,
                            annee          = as.integer(format(Sys.Date(), "%Y")),
                            dossier        = "outputs") {

  # Créer le dossier de sortie
  if (!dir.exists(dossier)) {
    dir.create(dossier, recursive = TRUE)
  }

  # ── Extraire les informations ──────────────────────────────────────────────
  metriques  <- eval_result$metriques
  importance <- rf_result$importance
  resume     <- summary_result$resume
  params     <- rf_result$params

  n_presences  <- sum(occurrences$presence == 1)
  n_total_occ  <- nrow(occurrences)
  auc_val      <- metriques$Valeur[metriques$Metrique == "AUC"]
  accuracy_val <- metriques$Valeur[metriques$Metrique == "Accuracy"]
  pct_eleve    <- resume$Pourcentage[resume$Niveau == "Eleve"]

  # ── Niveau de performance du modèle ───────────────────────────────────────
  if (auc_val >= 0.9) {
    perf_label <- "Excellent"
    perf_col   <- "#2ecc71"
  } else if (auc_val >= 0.7) {
    perf_label <- "Bon"
    perf_col   <- "#f39c12"
  } else {
    perf_label <- "Acceptable"
    perf_col   <- "#e74c3c"
  }

  # ── Tableau métriques HTML ─────────────────────────────────────────────────
  metriques_rows <- paste0(
    "<tr><td>", metriques$Metrique, "</td>",
    "<td><strong>", metriques$Valeur, "</strong></td></tr>",
    collapse = "\n"
  )

  # ── Tableau importance variables HTML ─────────────────────────────────────
  top5_imp      <- head(importance, 5)
  importance_rows <- paste0(
    "<tr><td>", top5_imp$variable, "</td>",
    "<td>", top5_imp$MeanDecreaseGini, "</td></tr>",
    collapse = "\n"
  )

  # ── Tableau résumé risque HTML ────────────────────────────────────────────
  resume_rows <- paste0(
    "<tr><td>", resume$Niveau, "</td>",
    "<td>", resume$N_pixels, "</td>",
    "<td>", resume$Pourcentage, "%</td>",
    "<td>", resume$Prob_moyenne, "</td></tr>",
    collapse = "\n"
  )

  # ── Paramètres modèle HTML ────────────────────────────────────────────────
  params_html <- paste0(
    "<tr><td>Nombre d'arbres (ntree)</td><td>", params$ntree, "</td></tr>",
    "<tr><td>Variables par noeud (mtry)</td><td>", params$mtry, "</td></tr>",
    "<tr><td>Proportion train</td><td>", params$prop_train * 100, "%</td></tr>",
    "<tr><td>Graine (seed)</td><td>", params$seed, "</td></tr>"
  )

  # ── Section greenup HTML ──────────────────────────────────────────────────
  greenup_section <- ""
  if (!is.null(greenup_result)) {
    greenup_section <- paste0('
    <div class="section">
      <h2>🌿 3. Analyse du Verdissement (Greenup)</h2>
      <p>L\'analyse du verdissement post-pluie permet d\'identifier les zones
      où la végétation se développe après les précipitations, créant des
      conditions favorables au développement des criquets pèlerins.</p>
      <table>
        <tr><th>Indicateur</th><th>Valeur</th></tr>',
                              paste0("<tr><td>", greenup_result$stats$indicateur,
                                     "</td><td>", greenup_result$stats$valeur,
                                     "</td></tr>", collapse = "\n"), '
      </table>
    </div>')
  }

  # ── Interprétation écologique ─────────────────────────────────────────────
  if (pct_eleve > 20) {
    interpretation <- paste0(
      "L'analyse révèle une situation préoccupante avec ", pct_eleve,
      "% de la zone d'étude classée à risque élevé. ",
      "Les conditions climatiques et végétatives actuelles sont très favorables ",
      "au développement et à la reproduction de Schistocerca gregaria. ",
      "Une intervention rapide est fortement recommandée."
    )
  } else if (pct_eleve > 10) {
    interpretation <- paste0(
      "L'analyse indique un risque modéré avec ", pct_eleve,
      "% de la zone classée à risque élevé. ",
      "Des foyers de développement localisés sont possibles. ",
      "Une surveillance renforcée est conseillée dans les zones identifiées."
    )
  } else {
    interpretation <- paste0(
      "L'analyse montre un risque globalement faible avec seulement ", pct_eleve,
      "% de la zone à risque élevé. ",
      "Les conditions actuelles ne semblent pas particulièrement favorables ",
      "à une invasion massive. La surveillance de routine suffit."
    )
  }

  # ── Générer le HTML complet ───────────────────────────────────────────────
  html_content <- paste0('
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <title>Rapport locustTrack — ', annee, '</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 0;
           background: #f5f5f5; color: #333; }
    .header { background: linear-gradient(135deg, #2c3e50, #3498db);
              color: white; padding: 40px; text-align: center; }
    .header h1 { font-size: 2.2em; margin: 0; }
    .header p  { font-size: 1.1em; opacity: 0.9; }
    .container { max-width: 1100px; margin: 0 auto; padding: 20px; }
    .section   { background: white; padding: 25px; margin: 20px 0;
                  border-radius: 10px;
                  box-shadow: 0 2px 6px rgba(0,0,0,0.1); }
    .badge     { display: inline-block; padding: 8px 20px;
                  border-radius: 20px; color: white;
                  font-weight: bold; font-size: 1.1em;
                  background: ', perf_col, '; }
    h2  { color: #2c3e50; border-left: 4px solid #3498db;
          padding-left: 12px; }
    h3  { color: #555; }
    table { width: 100%; border-collapse: collapse; margin-top: 15px; }
    th  { background: #2c3e50; color: white; padding: 12px;
          text-align: left; }
    td  { padding: 10px; border-bottom: 1px solid #eee; }
    tr:hover { background: #f8f9fa; }
    .metric-good  { color: #2ecc71; font-weight: bold; }
    .metric-warn  { color: #f39c12; font-weight: bold; }
    .metric-bad   { color: #e74c3c; font-weight: bold; }
    .interpretation { background: #eaf4fb; border-left: 4px solid #3498db;
                       padding: 15px; border-radius: 5px;
                       font-style: italic; margin: 15px 0; }
    .footer { text-align: center; padding: 30px;
               color: #888; font-size: 0.9em; }
    .grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; }
  </style>
</head>
<body>

<div class="header">
  <h1>🦗 Rapport d\'Analyse Acridienne</h1>
  <p>Package locustTrack — Prédiction des zones favorables aux criquets pèlerins</p>
  <p><em>Schistocerca gregaria</em> | Année ', annee, '</p>
</div>

<div class="container">

  <!-- SECTION 1 : DONNÉES -->
  <div class="section">
    <h2>📂 1. Données Utilisées</h2>
    <div class="grid-2">
      <div>
        <h3>Occurrences biologiques</h3>
        <table>
          <tr><th>Paramètre</th><th>Valeur</th></tr>
          <tr><td>Total occurrences</td><td>', n_total_occ, '</td></tr>
          <tr><td>Points de présence</td><td>', n_presences, '</td></tr>
          <tr><td>Source</td><td>GBIF / FAO / CSV</td></tr>
          <tr><td>Espèce</td><td><em>Schistocerca gregaria</em></td></tr>
        </table>
      </div>
      <div>
        <h3>Données environnementales</h3>
        <table>
          <tr><th>Variable</th><th>Source</th></tr>
          <tr><td>Précipitations</td><td>WorldClim</td></tr>
          <tr><td>Température</td><td>WorldClim</td></tr>
          <tr><td>NDVI</td><td>MODIS</td></tr>
          <tr><td>Greenup</td><td>Calculé</td></tr>
        </table>
      </div>
    </div>
  </div>

  <!-- SECTION 2 : MODÈLE -->
  <div class="section">
    <h2>🤖 2. Modèle Random Forest</h2>
    <p>Performance globale :
      <span class="badge">', perf_label, ' — AUC = ', auc_val, '</span>
    </p>
    <div class="grid-2">
      <div>
        <h3>Paramètres du modèle</h3>
        <table>
          <tr><th>Paramètre</th><th>Valeur</th></tr>
          ', params_html, '
        </table>
      </div>
      <div>
        <h3>Métriques de performance</h3>
        <table>
          <tr><th>Métrique</th><th>Valeur</th></tr>
          ', metriques_rows, '
        </table>
      </div>
    </div>
    <h3>Top 5 Variables les plus importantes</h3>
    <table>
      <tr><th>Variable</th><th>MeanDecreaseGini</th></tr>
      ', importance_rows, '
    </table>
  </div>

  <!-- SECTION 3 : GREENUP -->
  ', greenup_section, '

  <!-- SECTION 4 : CARTE DE RISQUE -->
  <div class="section">
    <h2>🗺️ 4. Carte de Risque d\'Invasion</h2>
    <table>
      <tr>
        <th>Niveau de risque</th>
        <th>Nombre de pixels</th>
        <th>Pourcentage</th>
        <th>Probabilité moyenne</th>
      </tr>
      ', resume_rows, '
    </table>
  </div>

  <!-- SECTION 5 : INTERPRÉTATION -->
  <div class="section">
    <h2>🔬 5. Interprétation Écologique</h2>
    <div class="interpretation">
      ', interpretation, '
    </div>
    <h3>Contexte biologique</h3>
    <p>Le criquet pèlerin <em>Schistocerca gregaria</em> est l\'un des
    ravageurs agricoles les plus dévastateurs au monde. Il se développe
    principalement dans les zones semi-arides d\'Afrique subsaharienne,
    du Maghreb et du Moyen-Orient, notamment après des épisodes de pluies
    favorisant la croissance de la végétation.</p>
    <p>Le modèle Random Forest utilisé dans cette analyse combine les
    variables climatiques (température, précipitations) et l\'indice de
    végétation NDVI pour prédire les zones à risque d\'invasion.</p>
  </div>

</div>

<div class="footer">
  <p>Rapport généré par <strong>locustTrack</strong> — ', Sys.time(), '</p>
  <p>Données : WorldClim | NDVI MODIS | Modèle Random Forest</p>
  <p>Auteur : Salma Oubrayme</p>
</div>

</body>
</html>')

  # ── Sauvegarder le fichier HTML ───────────────────────────
  nom_html <- paste0("rapport_locusttrack_", annee, ".html")
  nom_pdf  <- paste0("rapport_locusttrack_", annee, ".pdf")
  chemin_html <- file.path(dossier, nom_html)
  chemin_pdf  <- file.path(dossier, nom_pdf)

  writeLines(html_content, chemin_html, useBytes = TRUE)
  message("Rapport HTML généré : ", chemin_html)

  # ── Export PDF ────────────────────────────────────────────
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    install.packages("pagedown")
  }

  tryCatch({
    pagedown::chrome_print(
      input  = chemin_html,
      output = chemin_pdf
    )
    message("Rapport PDF généré : ", chemin_pdf)
  }, error = function(e) {
    message("Export PDF non disponible : ", e$message)
    message("Ouvrez le HTML dans Chrome et faites File → Print → Save as PDF")
  })

  return(list(
    html = chemin_html,
    pdf  = chemin_pdf
  ))
}
