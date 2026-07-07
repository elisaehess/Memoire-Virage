###############################################################################
############################# VIRAGE LGBT #####################################
############################### IMPORT ########################################
###############################################################################

#### Objectif du script ----

# Importer les données de l'enquête Virage LGBT

#### Import des données ----

## Import des librairies
library(haven) # importer des données SAS

## Chemin vers le dossier contenant les bases (<!> à adapter <!>)

path = "Data/"

## Nom des fichiers

base = "viragelgbt_fpr.sas7bdat"
formats = "formats_lgbt.sas7bcat"

## Import du fichier

lgbt <- read_sas(
  data_file = paste0(path, base),
  catalog_file = paste0(path, formats)
)
