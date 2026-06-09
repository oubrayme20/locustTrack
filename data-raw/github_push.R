# ============================================================
# Publication du package locustTrack sur GitHub
# Fichier : data-raw/github_push.R
# Auteur  : Salma Oubrayme
# Objectif: Commandes git pour publier le package sur GitHub
#           A exécuter dans le Terminal Windows
# ============================================================

# ── Ces commandes sont à exécuter dans le Terminal Windows ──
# ── Une par une dans l'ordre indiqué ────────────────────────

# ÉTAPE 1 — Aller sur le bon disque
# D:

# ÉTAPE 2 — Aller dans le dossier du package
# cd "R/PROJET - R - OUBRAYME Salma/locustTrack"

# ÉTAPE 3 — Configurer l'identité git
# git config --global user.name "oubrayme20"
# git config --global user.email "salma.oubrayme@iav.ac.ma"

# ÉTAPE 4 — Ajouter tous les fichiers
# git add .

# ÉTAPE 5 — Premier commit
# git commit -m "Initial commit : package locustTrack v0.1.0"

# ÉTAPE 6 — Connecter au dépôt GitHub
# git remote add origin https://github.com/oubrayme20/locustTrack.git

# ÉTAPE 7 — Définir la branche principale
# git branch -M main

# ÉTAPE 8 — Publier sur GitHub
# git push -u origin main

# ── Lien du dépôt ────────────────────────────────────────────
# https://github.com/oubrayme20/locustTrack

