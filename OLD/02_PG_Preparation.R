###############################################################################
############################### VIRAGE PG #####################################
######################### PREPARATION DES DONNEES #############################
###############################################################################

#### Objectif du script ----

# Préparer les données pour l'analyse :
# - Sous-population principale (filtre sur ident_sexu, Homo/Bi/Hétéro)
# - Sous-population par genre (femmes / hommes)
# - Variables sociodémo recodées (pour tableau 2a Trachman & Lejbowicz)
# - Variables d'attirance/pratique/identification (pour tableau 1 Bajos & Beltzer)

#### Import des librairies ----

library(labelled) # manipuler des variables labellisées
library(tidyverse) # visualiser, manipuler, importer données
library(questionr) # fonctions utiles 

#### Sous-pop principale et recodage variables ----

pgn = pg %>%
  select(
    poids_cal,
    Q1,
    ident_sexu,
    attirance_sexu, # ajouté pour tableau Bajos
    pratique_sexu, # ajouté pour tableau Bajos
    Q19e_gragebis,
    Q29e_5gr,
    CS_E_NIV1,
    Q25E, # ajouté pour le statut d'activité (tableau 2a)
    TERRITOIRE_3MOD,
    Mig_e,
    Typecpl,
    Etatmat,
    Situmat,
    FCPL,
    C1,
    C1a,
    SEX14,
    LGBT1e
  ) %>%
  filter(!ident_sexu %in% c("NSP", "NVPD")) %>% # retire 46 observations
  mutate(
    genre = fct_drop(as_factor(Q1)),
    idsexu = fct_relevel(ident_sexu, "Homo", "Bi", "Hétéro"),
    age = as_factor(Q19e_gragebis),
    diplome = fct_recode(
      as_factor(Q29e_5gr),
      "Primaire" = "Aucun diplôme",
      "Secondaire" = "Baccalauréat",
      "Secondaire" = "BEPC/BEP/CAP",
      "Supérieur 1er niveau" = "Dipl. du supérieur 1er cycle",
      "Supérieur 2e niveau et plus" = "Dipl. du supérieur 2e et 3e cycle",
      "NVPD/NSP" = "Ne souhaite pas répondre",
      "NVPD/NSP" = "Ne sais pas"
    ) %>%
      fct_relevel(
        "Primaire",
        "Secondaire",
        "Supérieur 1er niveau",
        "Supérieur 2e niveau et plus",
        "NVPD/NSP"
      ),
    csp = fct_recode(
      as_factor(CS_E_NIV1),
      "Agriculteur·rice exploitant·e" = "Agriculteurs exploitants",
      "Artisan·e, commerçant·e, chef d'entreprise" = "Artisans, commerçants et chefs d'entreprise",
      "Cadre, profession intellect. sup." = "Cadres et professions intellectuelles supérieures",
      "Profession intermédiaire" = "Professions Intermédiaires",
      "Employé·e" = "Employés",
      "Ouvrier·e" = "Ouvriers",
      NULL = "Retraités",
      NULL = "Autres personnes sans activité professionnelle",
      NULL = "Indéterminé"
    ),
    # Statut d'activité en 5 modalités (Trachman & Lejbowicz)
    statut_act = fct_recode(
      as_factor(Q25E),
      "Actif·ve" = "En emploi (y compris intérim, congé de maternité/paternité, arrêt maladie, mais pas étudiant en stage rémunéré)",
      "Actif·ve" = "Au chômage avec indemnités",
      "Actif·ve" = "Au chômage sans indemnités",
      "Retraité·e" = "En retraite",
      "Étudiant·e" = "Etudiant-e, élève",
      "Étudiant·e" = "Etudiant-e, élève avec emploi, y compris financement doctoral, petit boulot ou stage rémunéré (y compris apprenti-e)",
      "Étudiant·e" = "Etudiant-e, élève avec stage non rémunéré",
      "Autre sans activité" = "Inactif-ve ou au foyer ayant déjà travaillé (ayant eu un contrat de travail, y compris congé maladie longue durée de 5 ans ou plus)",
      "Autre sans activité" = "Inactif-ve ou au foyer n'ayant jamais travaillé (n'ayant jamais eu de contrat de travail)",
      "Autre sans activité" = "En congé parental ou de solidarité familiale",
      "Autre sans activité" = "Autre congé de longue durée ( Année sabbatique, congé de création d'entreprise, congé LONGUE MALADIE de moins de 5 ans)",
      "Indéterminé" = "NSP",
      "Indéterminé" = "NVPD"
    ) %>%
      fct_relevel("Actif·ve", "Retraité·e", "Étudiant·e", "Autre sans activité", "Indéterminé"),
    territoire = fct_recode(
      as_factor(TERRITOIRE_3MOD),
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ),
    mig = fct_recode(
      as_factor(Mig_e),
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Né-e dans un DOM",
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Immigré-e",
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Descendant-e d'1 immigré-e",
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Descendant-e de 2 immigré-e-s", 
      "Indeterminé" = "Inclassable"
    ),
    etatmat = fct_recode(
      as_factor(Etatmat),
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ),
    couple12mois = fct_recode(
      as_factor(FCPL),
      "Oui" = "A une seule relation de couple au moment de l’enquête, depuis au moins 4 mois",
      "Oui" = "A au moins deux relations de couple au moment de l’enquête, la principale durant depuis au moins 4 mois",
      "Oui" = "A une relation de couple au moment de l’enquête (Q6=1,2) qui dure depuis moins de 4 mois, mais a eu auparavant une relation qui s’est terminée par une séparation (donc hors décès) dans les 12 mois et dont la durée a été de 4 mois ou plus dans les 12 mois",
      "Oui" = "Plus en couple, mais a vécu en couple, avec durée de 4 mois ou plus pendant ces 12 derniers mois et fin de relation due à une séparation (et non pas un décès)",
      "Non" = "Autres cas : a ou a eu au moins une relation de couple trop courte ou terminée depuis plus de 8 mois",
      "Non" = "Autres cas : jamais en couple ou non indiqué"
    ),
    statutcouple = fct_recode(
      as_factor(Situmat),
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ),
    comingoutconjoint = fct_recode(
      as_factor(LGBT1e),
      NULL = "",
      NULL = "Oui certains",
      NULL = "Oui tous",
      NULL = "Non concerné-e",
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    )
  ) 
## Ici on crée deux sous-populations : femmes et hommes

pgnf = pgn %>% # Création de la sous-population femmes
  filter(genre == "Une femme")

pgnh = pgn %>% # Création de la sous-population hommes
  filter(genre == "Un homme")

#### Sous-pop sans filtre sur l'identification (pour Bajos et id-attir-prat) ----

# Cette sous-pop conserve toutes les observations, y compris celles dont
# l'identification sexuelle est NSP/NVPD, et recode en parallèle les trois
# critères (identification, attirance, pratique).

pg_aip = pg %>%
  select(poids_cal, Q1, ident_sexu, attirance_sexu, pratique_sexu) %>%
  mutate(
    genre = fct_drop(as_factor(Q1)),
    
    # ↓ Identification avec NSP/NVPD conservés en modalité distincte
    idsexu = fct_recode(
      as_factor(ident_sexu),
      "NSP/NVPD" = "NSP",
      "NSP/NVPD" = "NVPD"
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "NSP/NVPD"),
    
    # ↓ Attirance (mêmes regroupements que pgattirancepratique)
    attirance = fct_recode(
      as_factor(attirance_sexu),
      "Hétéro" = "Uniquement sexe opposé",
      "Homo" = "Uniquement même sexe",
      "Bi" = "Autant les deux sexes",
      "Bi" = "Stt même sexe ms aussi sexe opposé",
      "Bi" = "Stt sexe opposé ms aussi même sexe",
      "Pas d'attirance/NSP/NVPD" = "N'a pas d'attirance",
      "Pas d'attirance/NSP/NVPD" = "NSP",
      "Pas d'attirance/NSP/NVPD" = "NVPD"
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "Pas d'attirance/NSP/NVPD"),
    
    # ↓ Pratique
    pratique = fct_recode(
      as_factor(pratique_sexu),
      "Pas de rapport/NSP/NVPD" = "Pas de rapport sexue",
      "Pas de rapport/NSP/NVPD" = "NVPD",
      "Pas de rapport/NSP/NVPD" = "NSP"
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "Pas de rapport/NSP/NVPD"),
    
    # ↓ Indicateurs binaires pour le tableau Bajos (construits sur l'ensemble)
    attire_ms = as.integer(attirance %in% c("Homo", "Bi")),
    pratique_vie = as.integer(pratique %in% c("Homo", "Bi")),
    pratique_vie_excl = as.integer(pratique == "Homo"),
    id_homo = as.integer(idsexu == "Homo"),
    id_bi = as.integer(idsexu == "Bi"),
    attire_jamais_prat = as.integer(attire_ms == 1 & pratique_vie == 0)
  )

pg_aip_f = pg_aip %>% filter(genre == "Une femme")
pg_aip_h = pg_aip %>% filter(genre == "Un homme")

#### Sous-pop personnes en couple (> 4 mois) au moment de l'enquête ----

pgn_couple = pgn %>%
  filter(FCPL %in% c("01", "02")) %>%
  mutate(
    satisfaction = fct_recode(
      as_factor(C1),
      "Oui" = "Très satisfaisante",
      "Oui" = "Satisfaisante",
      "Non" = "Peu satisfaisante",
      "Non" = "Pas du tout satisfaisante",
      NULL = "NVPD",
      NULL = "NSP"
    ),
    amoureux = fct_recode(
      as_factor(SEX14),
      "Oui" = "Vous êtes très amoureux-se",
      "Oui" = "Vous êtes amoureux-se",
      "Non" = "Vous n’êtes plus amoureux-se",
      "Non" = "Vous n’avez jamais été amoureux-se",
      NULL = "NVPD",
      NULL = "NSP"
    ),
    rupture = fct_recode(
      as_factor(C1a),
      "Oui" = "Oui vous-même",
      "Oui" = "Oui votre conjoint",
      "Oui" = "Oui les deux",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>%
      fct_relevel("Oui", "Non")
  ) %>%
  filter(!is.na(satisfaction), !is.na(amoureux), !is.na(rupture))

pgnf_couple = pgn_couple %>% # Création de la sous-population femmes
  filter(genre == "Une femme")

pgnh_couple = pgn_couple %>% # Création de la sous-population hommes
  filter(genre == "Un homme")
