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

  # ── Extraire les informations ─────────────────────────────
  metriques  <- eval_result$metriques
  importance <- rf_result$importance
  resume     <- summary_result$resume
  params     <- rf_result$params

  n_presences  <- sum(occurrences$presence == 1)
  n_total_occ  <- nrow(occurrences)
  auc_val      <- metriques$Valeur[metriques$Metrique == "AUC"]
  accuracy_val <- metriques$Valeur[metriques$Metrique == "Accuracy"]
  pct_eleve    <- resume$Pourcentage[resume$Niveau == "Eleve"]

  # Surface en km² si disponible
  surf_eleve <- if ("Surface_km2" %in% names(resume)) {
    paste0(resume$Surface_km2[resume$Niveau == "Eleve"],
           " km²")
  } else {
    "N/A"
  }

  # ── Niveau de performance ─────────────────────────────────
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

  # ── Générer les cartes en base64 ──────────────────────────
  message("Génération des cartes pour le rapport...")

  # Fonction pour convertir un plot en base64
  plot_to_base64 <- function(plot_func) {
    tmp <- tempfile(fileext = ".png")
    grDevices::png(tmp, width = 800, height = 500, res = 120)
    tryCatch(plot_func(), error = function(e) {
      plot(1, type = "n", main = "Graphique non disponible")
    })
    grDevices::dev.off()
    img_data <- base64enc::base64encode(tmp)
    unlink(tmp)
    paste0("data:image/png;base64,", img_data)
  }

  # Vérifier base64enc
  if (!requireNamespace("base64enc", quietly = TRUE)) {
    stop(paste0(
      "Le package 'base64enc' est requis pour les cartes dans le rapport.\n",
      "Installation : install.packages('base64enc')"
    ))
  }

  # Carte risque continu
  img_risque <- plot_to_base64(function() {
    terra::plot(risk_result$risque_continu,
                main = "Probabilité de présence",
                col  = grDevices::colorRampPalette(
                  c("#ffffcc", "#fd8d3c", "#800026"))(100))
  })

  # Carte risque classé
  img_classe <- plot_to_base64(function() {
    terra::plot(risk_result$risque_classe,
                main = "Carte de risque classée",
                col  = c("#2ecc71", "#f39c12", "#e74c3c"))
    legend("bottomleft",
           legend = c("Faible", "Moyen", "Élevé"),
           fill   = c("#2ecc71", "#f39c12", "#e74c3c"),
           cex    = 0.8)
  })

  # Courbe ROC
  img_roc <- plot_to_base64(function() {
    obs_bin <- ifelse(eval_result$predictions$observe ==
                        "presence", 1, 0)
    pred_p  <- eval_result$predictions$proba
    ordre   <- order(pred_p, decreasing = TRUE)
    obs_tri <- obs_bin[ordre]
    n_pos   <- sum(obs_bin == 1)
    n_neg   <- sum(obs_bin == 0)
    tpr     <- cumsum(obs_tri == 1) / n_pos
    fpr     <- cumsum(obs_tri == 0) / n_neg
    plot(fpr, tpr, type = "l", col = "darkgreen", lwd = 2,
         xlab = "FPR", ylab = "TPR",
         main = paste0("ROC (AUC = ", auc_val, ")"))
    abline(0, 1, lty = 2, col = "gray")
  })

  # Observed vs Predicted
  img_obs_pred <- plot_to_base64(function() {
    obs_num  <- as.numeric(
      eval_result$predictions$observe == "presence")
    pred_num <- eval_result$predictions$proba
    plot(jitter(obs_num, 0.1), pred_num,
         pch  = 20,
         col  = ifelse(obs_num == 1, "#e74c3c", "#3498db"),
         xlab = "Observé (0=absence, 1=présence)",
         ylab = "Probabilité prédite",
         main = "Observed vs Predicted")
    abline(h = 0.5, lty = 2, col = "gray")
  })

  # Greenup si disponible
  img_greenup <- ""
  if (!is.null(greenup_result)) {
    img_greenup <- plot_to_base64(function() {
      terra::plot(greenup_result$anomalie,
                  main = "Anomalie NDVI — Greenup",
                  col  = grDevices::colorRampPalette(
                    c("#d73027", "#ffffff", "#1a9850"))(100))
    })
  }

  # ── Tableaux HTML ─────────────────────────────────────────
  metriques_rows <- paste0(
    "<tr><td>", metriques$Metrique, "</td>",
    "<td><strong>", metriques$Valeur, "</strong></td></tr>",
    collapse = "\n"
  )

  top5_imp <- head(importance, 5)
  importance_rows <- paste0(
    "<tr><td>", top5_imp$variable, "</td>",
    "<td>", top5_imp$MeanDecreaseGini, "</td></tr>",
    collapse = "\n"
  )

  resume_rows <- paste0(
    "<tr><td>", resume$Niveau, "</td>",
    "<td>", resume$N_pixels, "</td>",
    "<td>", resume$Pourcentage, "%</td>",
    if ("Surface_km2" %in% names(resume))
      paste0("<td>", resume$Surface_km2, " km²</td>")
    else "",
    "<td>", resume$Prob_moyenne, "</td></tr>",
    collapse = "\n"
  )

  params_html <- paste0(
    "<tr><td>Nombre d'arbres</td><td>",
    params$ntree, "</td></tr>",
    "<tr><td>Variables par noeud</td><td>",
    params$mtry, "</td></tr>",
    "<tr><td>Proportion train</td><td>",
    params$prop_train * 100, "%</td></tr>",
    "<tr><td>Seed</td><td>", params$seed, "</td></tr>"
  )

  # ── Interprétation écologique ─────────────────────────────
  if (pct_eleve > 20) {
    interpretation <- paste0(
      "L'analyse révèle une situation préoccupante avec ",
      pct_eleve, "% de la zone à risque élevé (",
      surf_eleve, "). Les conditions climatiques et ",
      "végétatives sont très favorables au développement ",
      "de Schistocerca gregaria. Une intervention rapide ",
      "est fortement recommandée."
    )
  } else if (pct_eleve > 10) {
    interpretation <- paste0(
      "L'analyse indique un risque modéré avec ",
      pct_eleve, "% de la zone à risque élevé (",
      surf_eleve, "). Des foyers localisés sont possibles. ",
      "Une surveillance renforcée est conseillée."
    )
  } else {
    interpretation <- paste0(
      "L'analyse montre un risque globalement faible avec ",
      pct_eleve, "% de la zone à risque élevé (",
      surf_eleve, "). La surveillance de routine suffit."
    )
  }

  # ── Section greenup HTML ──────────────────────────────────
  greenup_section <- ""
  if (!is.null(greenup_result)) {
    greenup_section <- paste0('
    <div class="section">
      <h2>🌿 3. Analyse du Verdissement (Greenup)</h2>
      <table>
        <tr><th>Indicateur</th><th>Valeur</th></tr>',
                              paste0("<tr><td>",
                                     greenup_result$stats$indicateur,
                                     "</td><td>",
                                     greenup_result$stats$valeur,
                                     "</td></tr>", collapse = "\n"), '
      </table>
      <h3>Carte Anomalie NDVI</h3>
      <img src="', img_greenup, '"
           style="width:100%;border-radius:8px;">
    </div>')
  }

  # ── Générer le HTML complet ───────────────────────────────
  html_content <- paste0('
<!DOCTYPE html>
<html lang="fr">
<head>
  <meta charset="UTF-8">
  <title>Rapport locustTrack — ', annee, '</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 0;
           background: #f5f5f5; color: #333; }
    .header { background: linear-gradient(135deg,
              #2c3e50, #3498db); color: white;
              padding: 40px; text-align: center; }
    .header h1 { font-size: 2.2em; margin: 0; }
    .container { max-width: 1100px; margin: 0 auto;
                  padding: 20px; }
    .section { background: white; padding: 25px;
                margin: 20px 0; border-radius: 10px;
                box-shadow: 0 2px 6px rgba(0,0,0,0.1); }
    .badge { display: inline-block; padding: 8px 20px;
              border-radius: 20px; color: white;
              font-weight: bold; font-size: 1.1em;
              background: ', perf_col, '; }
    h2 { color: #2c3e50; border-left: 4px solid #3498db;
          padding-left: 12px; }
    h3 { color: #555; }
    table { width: 100%; border-collapse: collapse;
             margin-top: 15px; }
    th { background: #2c3e50; color: white;
          padding: 12px; text-align: left; }
    td { padding: 10px; border-bottom: 1px solid #eee; }
    tr:hover { background: #f8f9fa; }
    .grid-2 { display: grid;
               grid-template-columns: 1fr 1fr; gap: 20px; }
    .carte { width: 100%; border-radius: 8px;
              margin-top: 15px; }
    .interpretation { background: #eaf4fb;
                       border-left: 4px solid #3498db;
                       padding: 15px; border-radius: 5px;
                       font-style: italic; margin: 15px 0; }
    .footer { text-align: center; padding: 30px;
               color: #888; font-size: 0.9em; }
  </style>
</head>
<body>

<div class="header">
  <h1>🦗 Rapport d\'Analyse Acridienne</h1>
  <p>Package locustTrack — Prédiction du risque d\'invasion</p>
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
          <tr><td>Total occurrences</td>
              <td>', n_total_occ, '</td></tr>
          <tr><td>Points de présence</td>
              <td>', n_presences, '</td></tr>
          <tr><td>Source</td><td>GBIF / FAO / CSV</td></tr>
          <tr><td>Espèce</td>
              <td><em>Schistocerca gregaria</em></td></tr>
        </table>
      </div>
      <div>
        <h3>Données environnementales</h3>
        <table>
          <tr><th>Variable</th><th>Source</th></tr>
          <tr><td>Précipitations</td><td>WorldClim/CHIRPS</td></tr>
          <tr><td>Température</td><td>WorldClim</td></tr>
          <tr><td>Humidité</td><td>WorldClim (BIO15)</td></tr>
          <tr><td>NDVI</td><td>MODIS MOD13A3</td></tr>
        </table>
      </div>
    </div>
  </div>

  <!-- SECTION 2 : MODÈLE -->
  <div class="section">
    <h2>🤖 2. Modèle Random Forest</h2>
    <p>Performance globale :
      <span class="badge">',
                         perf_label, ' — AUC = ', auc_val, '
      </span>
    </p>
    <div class="grid-2">
      <div>
        <h3>Paramètres</h3>
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
    <h3>Top 5 Variables importantes</h3>
    <table>
      <tr><th>Variable</th><th>MeanDecreaseGini</th></tr>
      ', importance_rows, '
    </table>
    <div class="grid-2">
      <div>
        <h3>Courbe ROC</h3>
        <img src="', img_roc, '" class="carte">
      </div>
      <div>
        <h3>Observed vs Predicted</h3>
        <img src="', img_obs_pred, '" class="carte">
      </div>
    </div>
  </div>

  <!-- SECTION 3 : GREENUP -->
  ', greenup_section, '

  <!-- SECTION 4 : CARTE DE RISQUE -->
  <div class="section">
    <h2>🗺️ 4. Carte de Risque d\'Invasion</h2>
    <div class="grid-2">
      <div>
        <h3>Probabilité continue</h3>
        <img src="', img_risque, '" class="carte">
      </div>
      <div>
        <h3>Risque classé</h3>
        <img src="', img_classe, '" class="carte">
      </div>
    </div>
    <h3>Résumé par niveau</h3>
    <table>
      <tr>
        <th>Niveau</th>
        <th>Pixels</th>
        <th>Pourcentage</th>',
                         if ("Surface_km2" %in% names(resume))
                           "<th>Surface (km²)</th>"
                         else "", '
        <th>Prob. moyenne</th>
      </tr>
      ', resume_rows, '
    </table>
  </div>

  <!-- SECTION 5 : ANALYSE CLIMATIQUE -->
  <div class="section">
    <h2>🌡️ 5. Analyse Climatique</h2>
    <p>Les données climatiques utilisées proviennent de
    WorldClim (résolution 10 minutes) et couvrent la zone
    Afrique subsaharienne, Maghreb et Moyen-Orient
    (longitude -20° à 65°E, latitude -10° à 40°N).</p>
    <table>
      <tr><th>Variable</th><th>Source</th><th>Rôle</th></tr>
      <tr><td>Précipitations</td><td>WorldClim / CHIRPS</td>
          <td>Déclencheur verdissement</td></tr>
      <tr><td>Température max</td><td>WorldClim</td>
          <td>Conditions thermiques</td></tr>
      <tr><td>Température min</td><td>WorldClim</td>
          <td>Survie criquets</td></tr>
      <tr><td>Humidité relative</td><td>WorldClim BIO15</td>
          <td>Conditions hygrique</td></tr>
      <tr><td>NDVI</td><td>MODIS MOD13A3</td>
          <td>État végétation</td></tr>
    </table>
  </div>

  <!-- SECTION 6 : INTERPRÉTATION -->
  <div class="section">
    <h2>🔬 6. Interprétation Écologique</h2>
    <div class="interpretation">
      ', interpretation, '
    </div>
    <p>Le criquet pèlerin <em>Schistocerca gregaria</em>
    se développe principalement dans les zones semi-arides
    après des épisodes pluvieux favorisant la végétation.
    Le modèle Random Forest combine climat et NDVI pour
    prédire les zones à risque.</p>
  </div>

</div>

<div class="footer">
  <p>Rapport généré par <strong>locustTrack</strong>
  — ', Sys.time(), '</p>
  <p>Auteur : Salma Oubrayme</p>
</div>

</body>
</html>')

  # ── Sauvegarder HTML ──────────────────────────────────────
  nom_html    <- paste0("rapport_locusttrack_", annee, ".html")
  nom_pdf     <- paste0("rapport_locusttrack_", annee, ".pdf")
  chemin_html <- file.path(dossier, nom_html)
  chemin_pdf  <- file.path(dossier, nom_pdf)

  writeLines(html_content, chemin_html, useBytes = TRUE)
  message("Rapport HTML généré : ", chemin_html)

  # ── Export PDF ────────────────────────────────────────────
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop(paste0(
      "Le package 'pagedown' est requis pour l'export PDF.\n",
      "Installation : install.packages('pagedown')\n",
      "Ou ouvrez le HTML dans Chrome : File -> Print -> Save as PDF"
    ))
  }

  tryCatch({
    pagedown::chrome_print(
      input  = chemin_html,
      output = chemin_pdf
    )
    message("Rapport PDF genere : ", chemin_pdf)
  }, error = function(e) {
    message("PDF non genere : ", e$message)
    message("Conseil : ouvrez le HTML dans Chrome,")
    message("  puis File -> Print -> Save as PDF")
  })

  return(list(
    html = chemin_html,
    pdf  = chemin_pdf
  ))
}
