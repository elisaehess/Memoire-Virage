# Projet R — Mémoire Virage (Elisa Lemoine)

Ce projet reprend les enquêtes **Virage PG** (Ined, 2015) et **Virage
LGBT** (Ined, 2016) pour étudier les conjugalités bisexuelles. 

## Organisation des fichiers

```
projet_virage_memoire/
├── projet_virage_memoire.Rproj      ← ouvrir d'abord ce fichier dans RStudio
├── R/
│   ├── 01_PG_Import.R               ← import SAS, PG  
│   ├── 02_PG_Preparation.R          ← recodages PG 
│   ├── 03_LGBT_Import.R             ← import SAS, LGBT 
│   └── 04_LGBT_Preparation.R        ← recodages LGBT  
├── Memoire_Chapitre_I.qmd           ← tableaux et figures Chapitre I 
├── Memoire_Chapitre_II.qmd          ← tableaux et figures Chapitre II 
├── Memoire_Chapitre_III.qmd         ← tableaux et figures Chapitre III 
└── README.md
```

## Mode d'emploi

0. Faire une demande de données sur le site `progedo.fr`
1. Adapter le chemin vers les fichiers SAS dans `R/01_PG_Import.R` et
   `R/03_LGBT_Import.R` (variable `path`).
2. Ouvrir `projet_virage_memoire.Rproj` dans RStudio.
3. Ouvrir les fichiers `.qmd` et cliquer sur **Render** pour obtenir tous les tableaux et figures ou simplement run les chunks des tableaux et figures souhaitées. 

## Quelques choix méthodologiques 

- **Identification sexuelle** comme variable principale (et non attirance
  ni pratique)
- **Pondération `poids_cal`** appliquée à toutes les statistiques Virage
  PG via `survey::svydesign()`. Virage LGBT reste **non pondéré**.
- Tests : **Rao-Scott χ²** pour Virage PG (par défaut dans `tbl_svysummary`),
  **Fisher avec simulation Monte-Carlo (10 000 réplications)** pour
  Virage LGBT, conformément à ton choix face aux petits effectifs.
- Variables sociodémo **alignées entre PG et LGBT** (mêmes modalités
  pour âge, diplôme, statut d'activité, CSP, territoire), pour permettre
  la comparaison côte à côte.
