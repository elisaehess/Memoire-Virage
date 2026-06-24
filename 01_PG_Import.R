###############################################################################
############################### VIRAGE PG #####################################
################################ IMPORT #######################################
###############################################################################

#### Objectif du script ----

# Importer les données de l'enquête Virage PG

#### Import des données ----

## Import des librairies

library(haven) # importer des données SAS

## Chemin vers le dossier contenant les bases (<!> à adapter <!>)

path = "~/Documents/MASTER EHESS/DONNEES/PG/SAS/"

## Nom des fichiers

base = "base_virage.sas7bdat" # données
formats = "formats.sas7bcat" # catalogue des étiquettes

## Import du fichier

pg = read_sas( # ouvrir les fichiers sas
  data_file = paste0(path, base), # convertir données en vecteurs textuels
  catalog_file = paste0(path, formats) # idem pour catalogue des formats
)
