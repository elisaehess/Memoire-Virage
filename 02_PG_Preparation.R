###############################################################################
############################### VIRAGE PG #####################################
######################### PREPARATION DES DONNEES #############################
###############################################################################

#### Objectif du script ----

# Préparer les données pour l'analyse :
# - Sous-population principale (filtre sur ident_sexu, Homo/Bi/Hétéro)
# - Sous-population par genre (femmes / hommes)
# - Variables sociodémo recodées (pour tableau Trachman & Lejbowicz)
# - Variables d'attirance/pratique/identification (pour tableau Bajos & Beltzer)
# - Variables de situation conjugale (pour tableau couple) :
#     * Sur l'ensemble (pgn) : etatmat, couple12mois, statutcouple, nb_enf_ego
#     * Sur les pers. en couple > 4 mois dans les 12 derniers mois (pgn_conjugal) :
#       cohabitation, nb_enf_couple

#### Import des librairies ----

library(labelled) # manipuler des variables labellisées
library(tidyverse) # visualiser, manipuler, importer données
library(questionr) # fonctions utiles 

#### Base de travail commune (tous les recodages, aucun filtre) ----

# pg_base contient l'ensemble des observations (y compris NSP/NVPD à ident_sexu)
# et tous les recodages. On en dérive ensuite pgn (filtrée) pour le tableau
# Trachman, et pg_aip (non filtrée) pour les tableaux id-attir-prat et Bajos.

pg_base = pg %>%
  select(
    poids_cal,
    Q1,
    ident_sexu,
    attirance_sexu, # ajouté pour tableau Bajos
    pratique_sexu, # ajouté pour tableau Bajos
    SEX10, # recodage présumé·es hétéro 
    SEX2H, # recodage présumé·es hétéros
    SEX2F, # recodage présumé·es hétéros
    SEX8_age, # recodage présumé·es hétéros
    SEX8a, # recodage présumé·es hétéros
    SEX9, # recodage présumé·es hétéros
    SEX9a, # recodage présumé·es hétéros
    Q19e_gragebis, #âge 
    Q29e_5gr, # niveau diplôme 
    CS_E_NIV1, # csp 
    REV2, # revenu individuel 
    REV4, # revenu subjectif
    Q25E, # statut activité
    Q3, #taille agglo 
    Mig_e, #statut migratoire
    # ↓ Variables brutes pour reconstruire FSEXCJT et TYPECPL
    FCPL, 
    Q12, 
    Q36, 
    Q17,
    Etatmat,
    Situmat,
    FCPL,
    FCOHAB, # cohabitation 
    Enf1,   # nb d'enfants d'ego 
    ENF2,   # nb d'enfants du couple
    Diffage_cjt, 
    Dur_relconj,
    C1,
    C1a,
    SEX14,
    LGBT1e
  ) %>%
  mutate(
    genre = fct_drop(as_factor(Q1)) %>% set_variable_labels("Genre"),
    
    # ↓ Identification avec NSP/NVPD conservés en modalité distincte
    idsexu = fct_recode(
      as_factor(ident_sexu),
      "NSP/NVPD" = "NSP",
      "NSP/NVPD" = "NVPD"
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "NSP/NVPD") %>% 
      set_variable_labels("Identification sexuelle"), 
    
    # ↓ Nouvelle variable idsexu présumé·es hétéros
    idsexu2 = case_when(
      SEX10 == "Homosexuel-le" ~ "Homo", 
      SEX10 == "Bisexuel-le" ~ "Bi",
      SEX10 %in% c("88", "99") ~ "NSP/NVPD",
      (SEX2F %in% c("77", "88", "99") & SEX8_age == 0) | 
      (SEX2F %in% c("77", "88", "99") & SEX8a %in% c("88", "99") & SEX9 %in% c("001", "888"))|
      (SEX2F %in% c("77", "88", "99") & SEX8a %in% c("88", "99") & SEX9a %in% c("88", "99")) ~ "Présumé·e hétéro", 
      (SEX2H %in% c("77", "88", "99") & SEX8_age == 0) | 
      (SEX2H %in% c("77", "88", "99") & SEX8a %in% c("88", "99") & SEX9 %in% c("001", "888"))|
      (SEX2H %in% c("77", "88", "99") & SEX8a %in% c("88", "99") & SEX9a %in% c("88", "99")) ~ "Présumé·e hétéro", 
      TRUE ~ as.character(ident_sexu)
    ) |> 
      fct_relevel("Homo", "Bi", "Hétéro", "Présumé·e hétéro", "NSP/NVPD"),
    
    # ↓ Attirance recodée
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
    
    # ↓ Pratique recodée
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
    attire_jamais_prat = as.integer(attire_ms == 1 & pratique_vie == 0), 
    
    # ↓ Recodage âge 
    age = as_factor(Q19e_gragebis) %>% 
      set_variable_labels("Groupe d'âge"),
    
    # ↓ Recodage diplôme 
    diplome = fct_recode(
      as_factor(Q29e_5gr),
      "Lycée ou inférieur" = "Aucun diplôme",
      "Lycée ou inférieur" = "BEPC/BEP/CAP",
      "Bac / Bac+3" = "Baccalauréat",
      "Bac / Bac+3" = "Dipl. du supérieur 1er cycle",
      "Supérieur à Bac+3" = "Dipl. du supérieur 2e et 3e cycle",
      "NVPD/NSP" = "Ne souhaite pas répondre",
      "NVPD/NSP" = "Ne sais pas"
    ) %>%
      fct_relevel(
        "Lycée ou inférieur",
        "Bac / Bac+3",
        "Supérieur à Bac+3",
        "NVPD/NSP"
      ) %>% 
      set_variable_labels("Diplôme"), 
    
    # ↓ Recodages classe 
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
    ) %>% 
      set_variable_labels("Catégorie socioprofessionnelle"),
    
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
    
    # Statut d'activité en 2 modalités : actif / inactif 
    statut_act2 = fct_recode(
      as_factor(Q25E),
      "Actif·ve" = "En emploi (y compris intérim, congé de maternité/paternité, arrêt maladie, mais pas étudiant en stage rémunéré)",
      "Actif·ve" = "Au chômage avec indemnités",
      "Actif·ve" = "Au chômage sans indemnités",
      "Inactif·ve" = "En retraite",
      "Inactif·ve" = "Etudiant-e, élève",
      "Inactif·ve" = "Etudiant-e, élève avec emploi, y compris financement doctoral, petit boulot ou stage rémunéré (y compris apprenti-e)",
      "Inactif·ve" = "Etudiant-e, élève avec stage non rémunéré",
      "Inactif·ve" = "Inactif-ve ou au foyer ayant déjà travaillé (ayant eu un contrat de travail, y compris congé maladie longue durée de 5 ans ou plus)",
      "Inactif·ve" = "Inactif-ve ou au foyer n'ayant jamais travaillé (n'ayant jamais eu de contrat de travail)",
      "Inactif·ve" = "En congé parental ou de solidarité familiale",
      "Inactif·ve" = "Autre congé de longue durée ( Année sabbatique, congé de création d'entreprise, congé LONGUE MALADIE de moins de 5 ans)",
      "NSP/NVPD" = "NSP",
      "NSP/NVPD" = "NVPD"
    ) %>%
      fct_relevel("Actif·ve", "Inactif·ve", "NSP/NVPD") %>% 
      set_variable_labels("En activité"), 
    
    # Revenu individuel 
    revenu = fct_recode(
      as_factor(REV2),
      "Moins de 1 000€" = "Aucun revenu", 
      "Moins de 1 000€" = "Moins de 700 euros",
      "Moins de 1 000€" = "De 700 à moins de 1000 euros", 
      "De 1 000 à moins de 2 000€" = "De 1000 à moins de 1300 euros",
      "De 1 000 à moins de 2 000€" = "De 1300 à moins de 1600 euros",
      "De 1 000 à moins de 2 000€" = "De 1600 à moins de 2000 euros",
      "2 000€ et plus" = "De 2000 à moins de 2500 euros",
      "2 000€ et plus" = "De 2500 à moins de 3000 euros",
      "2 000€ et plus" = "Plus de 3000 euros",
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ) %>% 
      set_variable_labels("Revenu individuel"), 
    
    # Revenu subjectif 
    revenu_sub = fct_recode(
      as_factor(REV4),
      "A l'aise/Ca va" = "Vous êtes-étiez très à l aise", 
      "A l'aise/Ca va" = "Ca va - ça allait", 
      "C'est juste" = "C est / c était juste",
      "Difficile/Dettes" = "Vous y arrivez/Vous y arriv(i)ez difficilement",
      "Difficile/Dettes" = "Vous ne pouvez /Vous ne pouv(i)ez pas y arriver sans faire de dettes",
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP",
      NULL = ""
    ) %>% 
      set_variable_labels("Situation financière"), 
    
    # ↓ territoire 
    taille_agglo = fct_recode(
      as_factor(Q3),
      "200 000 et plus" = "Paris ou la petite couronne (Hauts-de-Seine (92), Seine-Saint-Denis (93), Val-de-Marne (94))",  
      "200 000 et plus" = "Une agglomération de plus d'un million d' habitants",
      "200 000 et plus" = "Une agglomération de plus de 200 000 habitants",
      "De 20 000 à 200 000" = "Une agglomération de 100 000 à 200 000 habitants",
      "De 20 000 à 200 000" = "Une agglomération de 20 000 à 100 000 habitants",
      "Moins de 20 000" = "Une agglomération de moins de 20 000 habitants",
      "Moins de 20 000" = "Un village (moins de 2 000 habitants)",
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ) |> 
      fct_relevel("Moins de 20 000", "De 20 000 à 200 000", "200 000 et plus", "NVPD/NSP") %>% 
      set_variable_labels("Taille d'agglomération"), 
    
    # ↓ statut migratoire 
    mig = fct_recode(
      as_factor(Mig_e),
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Né-e dans un DOM",
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Immigré-e",
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Descendant-e d'1 immigré-e",
      "Immigré·e, descendant·e ou né·e dans un DOM" = "Descendant-e de 2 immigré-e-s", 
      "Indeterminé" = "Inclassable"
    ),
    # ↓ État matrimonial légal (sur l'ensemble)
    etatmat = fct_recode(
      as_factor(Etatmat),
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ),
    # ↓ Relation de couple > 4 mois au cours des 12 derniers mois (sur l'ensemble)
    couple12mois = fct_recode(
      as_factor(FCPL),
      "Oui" = "A une seule relation de couple au moment de l’enquête, depuis au moins 4 mois",
      "Oui" = "A au moins deux relations de couple au moment de l’enquête, la principale durant depuis au moins 4 mois",
      "Oui" = "A une relation de couple au moment de l’enquête (Q6=1,2) qui dure depuis moins de 4 mois, mais a eu auparavant une relation qui s’est terminée par une séparation (donc hors décès) dans les 12 mois et dont la durée a été de 4 mois ou plus dans les 12 mois",
      "Oui" = "Plus en couple, mais a vécu en couple, avec durée de 4 mois ou plus pendant ces 12 derniers mois et fin de relation due à une séparation (et non pas un décès)",
      "Non" = "Autres cas : a ou a eu au moins une relation de couple trop courte ou terminée depuis plus de 8 mois",
      "Non" = "Autres cas : jamais en couple ou non indiqué"
    ) %>% 
      fct_relevel("Oui", "Non") %>% 
      set_variable_labels("En couple au cours des 12 derniers mois (> 4 mois)"), 
    
    # ↓ Statut du couple au moment de l'enquête (sur l'ensemble)
    statutcouple = fct_recode(
      as_factor(Situmat),
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ),
    
    # ↓ Statut couple au moment de l'enquête : oui / non (ensemble)
    couple_enq = fct_recode(
      as_factor(Situmat),
      "Oui" = "Marié.e", 
      "Oui" = "Pacsé.e", 
      "Oui" = "Union libre", 
      "Non" = "Pas en couple", 
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ) |> 
      fct_relevel("Oui", "Non") %>% 
      set_variable_labels("En couple au moment de l'enquête"), 
    
    # ↓ SEXCONJOINT reconstruite (sexe conjoint-e)
    Sexcjt = case_when(
      Q1 == "01" & FCPL %in% c("05", "06") ~ "Pas en couple",
      
      Q1 == "01" & FCPL == "01" & Q12 == "01" | 
      Q1 == "01" & FCPL == "02" & Q12 == "01" |
      Q1 == "01" & FCPL == "03" & Q36 == "01" |
      Q1 == "01" & FCPL == "04" & Q17 == "01" ~ "Un homme",
      
      Q1 == "01" & FCPL == "01" & Q12 == "02" | 
      Q1 == "01" & FCPL == "02" & Q12 == "02" |
      Q1 == "01" & FCPL == "03" & Q36 == "02" |
      Q1 == "01" & FCPL == "04" & Q17 == "02" ~ "Une femme", 
      
      Q1 == "01" & FCPL == "01" & Q12 %in% c("88", "99") | 
      Q1 == "01" & FCPL == "02" & Q12 %in% c("88", "99") |
      Q1 == "01" & FCPL == "03" & Q36 %in% c("88", "99") |
      Q1 == "01" & FCPL == "04" & Q17 %in% c("88", "99") ~ "NSP/NVPD", 
      
      Q1 == "02" & FCPL %in% c("05", "06") ~ "Pas en couple",
      
      Q1 == "02" & FCPL == "01" & Q12 == "01" | 
      Q1 == "02" & FCPL == "02" & Q12 == "01" |
      Q1 == "02" & FCPL == "03" & Q36 == "01" |
      Q1 == "02" & FCPL == "04" & Q17 == "01" ~ "Un homme",
      
      Q1 == "02" & FCPL == "01" & Q12 == "02" | 
      Q1 == "02" & FCPL == "02" & Q12 == "02" |
      Q1 == "02" & FCPL == "03" & Q36 == "02" |
      Q1 == "02" & FCPL == "04" & Q17 == "02" ~ "Une femme",
      
      Q1 == "02" & FCPL == "01" & Q12 %in% c("88", "99") | 
      Q1 == "02" & FCPL == "02" & Q12 %in% c("88", "99") |
      Q1 == "02" & FCPL == "03" & Q36 %in% c("88", "99") |
      Q1 == "02" & FCPL == "04" & Q17 %in% c("88", "99") ~ "NSP/NVPD"
    ),
    
    # ↓ TYPECPL reconstruite (en couple homo ou hétéro, selon sexe ego)
    typecpl = case_when(
      Q1 == "01" & Sexcjt == "Un homme" ~ "En couple de même sexe", 
      Q1 == "01" & Sexcjt == "Une femme" ~ "En couple de sexe différent", 
      Q1 == "02" & Sexcjt == "Un homme" ~ "En couple de sexe différent", 
      Q1 == "02" & Sexcjt == "Une femme" ~ "En couple de même sexe", 
      Q1 == "01" & Sexcjt == "Pas en couple" |
      Q1 == "02" & Sexcjt == "Pas en couple"~ "Pas en couple",
      Q1 == "01" & Sexcjt == "NSP/NVPD" |
      Q1 == "02" & Sexcjt == "NSP/NVPD"~ "NSP/NVPD",
    ) %>% 
      factor(levels = c("En couple de même sexe",
                        "En couple de sexe différent",
                        "Pas en couple",
                        "NSP/NVPD")) %>%
      set_variable_labels("Type de couple"),
    
    # ↓ Nombre d'enfants d'ego (sur l'ensemble)
    enf_ego = fct_recode(
      as_factor(Enf1), 
      "Non" = "0", 
      "Oui" = "1", 
      "Oui" = "2",
      "Oui" = "3",
      "Oui" = "4",
      "Oui" = "5",
      "Oui" = "6",
      "Oui" = "7",
      "Oui" = "8",
      "Oui" = "9",
      "Oui" = "10",
      "Oui" = "11",
      "Oui" = "12"
    ) |> 
      fct_relevel("Oui", "Non") %>% 
      set_variable_labels("A des enfants"), 
    
    # ↓ Cohabitation (utile sur sous-pop en couple, mais on recode sur l'ensemble
    #   pour conserver la cohérence) — la modalité "Pas en couple" servira
    #   uniquement si on l'analyse sur l'ensemble ; vide une fois filtré sur
    #   pgn_conjugal.
    cohabitation = fct_recode(
      as_factor(FCOHAB),
      "En couple non cohabitant" = "",
      "En couple cohabitant" = "Couple 1 et cohabitant",
      "En couple cohabitant" = "Couple 2 et cohabitant",
      "En couple cohabitant" = "Couple 3 et cohabitant",
      "En couple cohabitant" = "Couple 4 et cohabitant",
      "Pas en couple" = "Autre cas"
    ) %>%
      fct_relevel("En couple cohabitant", "En couple non cohabitant", "Pas en couple"),
    # ↓ Nombre d'enfants du couple (filtré sur pers. en couple à l'enquête)
    enf_couple = fct_recode(
      as_factor(ENF2),
      NULL = "",
      "0" = "00",
      "1" = "01",
      "2" = "02",
      "3+" = "03",
      "3+" = "04",
      "3+" = "05",
      "3+" = "06",
      "3+" = "07",
      "3+" = "08",
      "3+" = "09",
      "3+" = "10",
      "3+" = "11",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>%
      fct_relevel("0", "1", "2", "3+"),
    # ↓ Durée de la relation actuelle (en années, catégorisée)
    # DUR_RELCONJ en mois ; 888=NVPD, 999=NSP → NA
    dur_rel_mois = if_else(
      as.numeric(Dur_relconj) %in% c(888, 999),
      NA_real_,
      as.numeric(Dur_relconj)
    ),
    duree_rel = case_when(
      is.na(dur_rel_mois)  ~ NA_character_,
      dur_rel_mois < 12    ~ "Moins de 1 an",
      dur_rel_mois < 60    ~ "1-4 ans",
      dur_rel_mois < 120   ~ "5-9 ans",
      dur_rel_mois < 240   ~ "10-19 ans",
      TRUE                 ~ "20 ans ou plus"
    ) %>%
      factor(levels = c("Moins de 1 an", "1-4 ans", "5-9 ans",
                        "10-19 ans", "20 ans ou plus")),
    
    # ↓ Écart d'âge avec le/la conjoint·e (5 catégories ego-centriques)
    # DIFFAGE_CJT : valeur numérique en années (codes 888=NVPD, 999=NSP → NA).
    # Convention INED : 
    #   - Si Q1=01 (homme) : DIFFAGE = age_ego - age_partner
    #   - Si Q1=02 (femme) : DIFFAGE = age_partner - age_ego
    # On normalise en "ego - partner" (positif = ego plus âgé·e)
    diff_age_num = if_else(
      as.numeric(Diffage_cjt) %in% c(888, 999),
      NA_real_,
      as.numeric(Diffage_cjt)
    ),
    ecart_signe = case_when(
      as.character(Q1) == "01" ~ diff_age_num,       # homme : pas de flip
      as.character(Q1) == "02" ~ -diff_age_num,      # femme : flip
      TRUE                     ~ NA_real_
    ),
    ecart_age = case_when(
      is.na(ecart_signe)         ~ NA_character_,
      ecart_signe >= 10          ~ "Conjoint·e plus jeune (10+ ans)",
      ecart_signe >= 5           ~ "Conjoint·e plus jeune (5-9 ans)",
      ecart_signe > -5           ~ "Du même âge (± 4 ans)",
      ecart_signe > -10          ~ "Conjoint·e plus âgé·e (5-9 ans)",
      TRUE                       ~ "Conjoint·e plus âgé·e (10+ ans)"
    ) %>%
      factor(levels = c("Conjoint·e plus âgé·e (10+ ans)",
                        "Conjoint·e plus âgé·e (5-9 ans)",
                        "Du même âge (± 4 ans)",
                        "Conjoint·e plus jeune (5-9 ans)",
                        "Conjoint·e plus jeune (10+ ans)")),
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


#### Sous-pop principale filtrée sur l'identification (Trachman & Lejbowicz) ----

# pgn est utilisée pour le tableau 2a Trachman et le tableau 3a (mémoire) :
# on exclut les NSP/NVPD ident (46 observations) et on supprime le niveau
# "NSP/NVPD" devenu vide.

pgn = pg_base %>%
  filter(!ident_sexu %in% c("NSP", "NVPD")) %>% # retire 46 observations
  mutate(idsexu = fct_drop(idsexu))

## Ici on crée deux sous-populations : femmes et hommes

pgnf = pgn %>% # Création de la sous-population femmes
  filter(genre == "Une femme")

pgnh = pgn %>% # Création de la sous-population hommes
  filter(genre == "Un homme")

#### Sous-pop homme bi / femme bie ----

pgnfbi = pgnf %>% 
  filter(idsexu == "Bi")

pgnhbi = pgnh %>% 
  filter(idsexu == "Bi")

#### Sous-pop sans filtre sur l'identification (Bajos et id-attir-prat) ----

# pg_aip est l'alias de pg_base, conservé pour la lisibilité des chunks qui
# l'appellent. Toutes les observations sont conservées, y compris celles dont
# l'identification est NSP/NVPD, ce qui évite les biais sur attirance/pratique.

# pg_aip = pg_base
# 
# pg_aip_f = pg_aip %>% filter(genre == "Une femme")
# pg_aip_h = pg_aip %>% filter(genre == "Un homme")

#### Sous-pop en couple > 4 mois 12 DERNIERS MOIS ----

# pgn_conjugal = pgn filtrée sur FCPL %in% c("01","02", "03", "04") 
# en couple >4 dans 12 MOIS C'est la base appropriée pour les variables filtrées
# (cohabitation, nb_enf_couple) qui ne sont posées qu'aux personnes dans ce cas.
# /!\ Distincte de pgn_couple : pgn_couple est plus restrictive ; on l'a gardée 
# intacte pour les analyses existantes sur la satisfaction relationnelle.

# pgn_conjugal = pgn %>%
#   filter(FCPL %in% c("01", "02", "03", "04")) %>%
#   mutate(
#     cohabitation = fct_drop(cohabitation), # supprime "Pas en couple" (vide ici)
#   )
# 
# pgnf_conjugal = pgn_conjugal %>% # sous-pop femmes en couple > 4 mois
#   filter(genre == "Une femme")
# 
# pgnh_conjugal = pgn_conjugal %>% # sous-pop hommes en couple > 4 mois
#   filter(genre == "Un homme")

#### Sous-pop personnes en couple (> 4 mois) AU MOMENT de l'enquête ----

# pgn_couple = pgn filtrée sur FCPL %in% c("01", "02") + 
# filtre NA satisfaction/amoureux/rupture. C'est la base appropriée pour les
# questions posées aux personnes en couple AU MOMENT de l'enquête. 
# /!\ On la conserve telle quelle pour ne pas casser les analyses existantes
#     sur la satisfaction. 

# pgn_couple = pgn %>%
#   filter(FCPL %in% c("01", "02")) %>%
#   mutate(
#     satisfaction = fct_recode(
#       as_factor(C1),
#       "Oui" = "Très satisfaisante",
#       "Oui" = "Satisfaisante",
#       "Non" = "Peu satisfaisante",
#       "Non" = "Pas du tout satisfaisante",
#       NULL = "NVPD",
#       NULL = "NSP"
#     ),
#     amoureux = fct_recode(
#       as_factor(SEX14),
#       "Oui" = "Vous êtes très amoureux-se",
#       "Oui" = "Vous êtes amoureux-se",
#       "Non" = "Vous n’êtes plus amoureux-se",
#       "Non" = "Vous n’avez jamais été amoureux-se",
#       NULL = "NVPD",
#       NULL = "NSP"
#     ),
#     rupture = fct_recode(
#       as_factor(C1a),
#       "Oui" = "Oui vous-même",
#       "Oui" = "Oui votre conjoint",
#       "Oui" = "Oui les deux",
#       NULL = "NVPD",
#       NULL = "NSP"
#     ) %>%
#       fct_relevel("Oui", "Non")
#   ) %>%
#   filter(!is.na(satisfaction), !is.na(amoureux), !is.na(rupture))
# 
# pgnf_couple = pgn_couple %>% # Création de la sous-population femmes
#   filter(genre == "Une femme")
# 
# pgnh_couple = pgn_couple %>% # Création de la sous-population hommes
#   filter(genre == "Un homme")

#### Sauvegarde des nouvelles bases ----

# save(pgn, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgn.Rdata")
# 
# save(pgnf, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgnf.Rdata")
# 
# save(pgnh, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgnh.Rdata")
# 
# save(pg_aip, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pg_aip.Rdata")
# 
# save(pg_aip_f, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pg_aip_f.Rdata")
# 
# save(pg_aip_h, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pg_aip_h.Rdata")
# 
# save(pgn_couple, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgn_couple.Rdata")
# 
# save(pgnf_couple, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgnf_couple.Rdata")
# 
# save(pgnh_couple, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgnh_couple.Rdata")
# 
# save(pgn_conjugal, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgn_conjugal.Rdata")
# 
# save(pgnf_conjugal, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgnf_conjugal.Rdata")
# 
# save(pgnh_conjugal, file = "~/Documents/MASTER EHESS/VIRAGE/2. Test claude/NOUVELLES BASES/pgnh_conjugal.Rdata")
# 
