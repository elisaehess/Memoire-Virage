# Projet R — Mémoire Virage (Elisa Lemoine)

Ce projet reproduit, sur les enquêtes **Virage PG** (Ined, 2015) et **Virage
LGBT** (Ined, 2016) :

- le **tableau 1** de Bajos & Beltzer (chap. 12 de *Enquête CSF*, 2008) —
  expériences avec une personne du même sexe selon l'âge et le diplôme ;
- le **tableau 2a** de Trachman & Lejbowicz (chap. 10 de *Virage*, 2020) —
  caractéristiques sociodémographiques selon le sexe et l'identification.

## Organisation des fichiers

```
projet_virage_memoire/
├── projet_virage_memoire.Rproj      ← ouvrir d'abord ce fichier dans RStudio
├── R/
│   ├── 01_PG_Import.R               ← import SAS, PG  (= ton script existant)
│   ├── 02_PG_Preparation.R          ← recodages PG    (étendu : +statut_act, indicateurs Bajos)
│   ├── 03_LGBT_Import.R             ← import SAS, LGBT (= ton script existant)
│   └── 04_LGBT_Preparation.R        ← recodages LGBT  (étendu : +diplôme/CSP/territoire/statut_act)
├── tableaux_memoire.qmd             ← le document Quarto qui produit les deux tableaux
└── README.md
```

Les scripts d'import et de préparation sont **strictement issus de tes
fichiers existants**, complétés là où il manquait des variables pour les
tableaux finaux. Les ajouts sont signalés par un commentaire `# ↓` ou
`# ajouté pour …`.

## Prérequis

R ≥ 4.2, Quarto installé (<https://quarto.org>), et les packages :

```r
install.packages(c("tidyverse", "labelled", "haven",
                   "survey", "questionr",
                   "gtsummary", "flextable"))
```

## Mode d'emploi

1. Adapter le chemin vers les fichiers SAS dans `R/01_PG_Import.R` et
   `R/03_LGBT_Import.R` (variable `path`).
2. Ouvrir `projet_virage_memoire.Rproj` dans RStudio.
3. Ouvrir `tableaux_memoire.qmd` et cliquer sur **Render**. Cela produit
   `tableaux_memoire.docx` à côté du `.qmd`.

## Comment as-tu choisi entre script R, Quarto, Word et Excel ?

Tu m'as demandé quel format adopter pour obtenir une présentation comme
celle des articles Bajos et Trachman & Lejbowicz. Voici la réponse, point
par point.

**Comment sont faits les tableaux des articles INED ?** L'Ined publie ses
ouvrages via les *Cahiers de l'Ined* / *Grandes Enquêtes*. Les tableaux
sont **calculés en SAS ou en R** par les chercheur·es, puis **importés
dans Word ou InDesign** par l'éditeur·rice pour la mise en page finale
(filets fins, bandeaux, italiques pour les titres de section comme
*Groupe d'âges*, *Diplôme*, etc.). Tu ne peux pas reproduire le rendu
**InDesign** depuis R, mais tu peux t'en approcher **très près en Word**
grâce au package `flextable`.

**Mon choix pour ton mémoire : Quarto → Word (`.docx`)**, parce que :

1. Quarto mélange texte (ta rédaction), code R et tableaux dans un
   seul fichier reproductible.
2. La sortie est un **Word modifiable** : tu peux retoucher manuellement
   (encadrer en rouge les cellules saillantes, comme tu le fais dans ton
   mémoire actuel, griser les groupes p > 0.05, etc.).
3. `flextable` produit déjà du Word soigné (filets, bandeaux, gras pour
   les titres de variables, alignement). C'est le rendu le plus proche
   des tableaux INED accessible à partir de R.
4. Si tu veux **aller un cran plus loin**, tu crées un `template.docx`
   (Fichier Word vide avec tes polices, marges, styles "Titre 1",
   "Titre 2", etc.) et tu le déclares dans le YAML du `.qmd`
   (`reference-doc: template.docx`). Tout le mémoire adoptera alors ce
   style.

**Alternatives écartées et pourquoi :**

- **Tout dans des scripts `.R` puis copier-coller dans Word** : c'est ce
  que tu fais aujourd'hui. Ça marche, mais perd la reproductibilité et
  oblige à recommencer la mise en forme à chaque correction de données.
- **Excel** : pas adapté pour des tableaux à plusieurs niveaux de
  bandeaux comme Trachman & Lejbowicz, et déconseillé pour un mémoire de
  M2.
- **LaTeX / PDF** : permet de reproduire à l'identique le style Bajos
  (qui est composé en LaTeX-like), mais demande de tout convertir en
  LaTeX, ce qui complexifie la rédaction.

## Quelques choix méthodologiques retenus dans le code

Tu retrouves ces choix tels que tu les défends déjà dans ton mémoire
(partie II) :

- **Identification sexuelle** comme variable principale (et non attirance
  ni pratique), justifiée partie II.C.
- **Pondération `poids_cal`** appliquée à toutes les statistiques Virage
  PG via `survey::svydesign()`. Virage LGBT reste **non pondéré**, faute
  de plan d'échantillonnage.
- Tests : **Rao-Scott χ²** pour Virage PG (par défaut dans `tbl_svysummary`),
  **Fisher avec simulation Monte-Carlo (10 000 réplications)** pour
  Virage LGBT, conformément à ton choix face aux petits effectifs.
- Variables sociodémo **alignées entre PG et LGBT** (mêmes modalités
  pour âge, diplôme, statut d'activité, CSP, territoire), pour permettre
  la comparaison côte à côte.

## Différences avec le tableau 1 de Bajos & Beltzer

Bajos & Beltzer utilisent l'enquête **CSF (2006)**, pas Virage. Deux
limites en découlent pour la reproduction sur Virage PG :

1. **Pas de "pratique 12 mois"** : Virage n'a pas de variable directe
   donnant le sexe des partenaires des 12 derniers mois (seulement le
   sexe d'un éventuel *nouveau* partenaire, via `SEX12b1`). La colonne
   correspondante du tableau Bajos n'est donc pas reproduite, et cela
   est signalé dans le rendu.
2. **Tranches d'âge** : Virage commence à 20 ans (non 18 ans) et la
   variable recodée `Q19e_gragebis` propose 4 modalités (20-29, 30-39,
   40-49, 50-69) au lieu des 6 modalités de CSF (18-24, 25-34, 35-39,
   40-49, 50-59, 60-69). Le code utilise les 4 modalités Virage. Si tu
   veux refaire les 6 modalités CSF, il suffit de recoder `Q19E_age`
   (l'âge précis) au lieu de `Q19e_gragebis`.

Si jamais tu souhaites **quand même** une approximation de la pratique 12
mois homosexuelle, tu peux la reconstruire à partir de `SEX12b1` (sexe du
nouveau partenaire) croisé avec le genre — mais ça ne couvrira que les
personnes ayant eu un nouveau partenaire dans l'année. À discuter dans la
partie méthodo si tu le retiens.
