###############################################################################
############################## VIRAGE LGBT ####################################
######################### PREPARATION DES DONNEES #############################
###############################################################################

#### Objectif du script ----

# Préparer les données pour l'analyse
# - Filtre sur les 20-69 ans (comparable à Virage PG)
# - Filtre sur identification Homo / Bi (les Hétéros et NSP/NVPD sont peu
#   nombreux dans LGBT et ne sont pas analysés dans le tableau 2a)
# - Recodage des variables sociodémo dans les mêmes catégories que PG
# - Variables de situation conjugale (pour tableau 3 mémoire) :
#     * Sur l'ensemble (lgbtn) : etatmat, couple12mois, statutcouple, nb_enf_ego
#     * Sur les pers. en couple > 4 mois dans les 12 derniers mois (lgbtn_conjugal) :
#       cohabitation, nb_enf_couple
# /!\ Les noms des variables conjugales (Etatmat, Situmat, FCPL, FCOHAB, Enf1,
#     ENF2) sont supposés identiques à PG (convention INED). Si l'un d'eux
#     n'existe pas dans le volet LGBT ou a un libellé différent, adapter le
#     recodage ci-dessous.

#### Notes sur la reconstruction des variables conjugales ---- 

# Le document INED "Variables Construites - Enquête Virage Principale"
# (24 mars 2017, sections 5.2.2 et 5.3.1) fournit les règles de construction
# que l'on applique ici en LGBT à partir des variables brutes.
#
# ETATMAT (état matrimonial légal) :
#   01 célibataire : q7a=01 ou q9a=01 ou q13a=01 ou q6=04
#                    ou (q6 in (88,99) et q6a=01)
#   02 marié-e    : q7a=02 ou q9a=02 ou q13a=02
#                    ou q7=01 ou q9=01 ou q13=01
#                    ou (q6 in (88,99) et q6a=02)
#   03 divorcé-e  : q7a=03 ou q9a=03 ou q13a=03
#                    ou (q6 in (88,99) et q6a=03)
#   04 veuf-ve    : q7a=04 ou q9a=04 ou q13a=04
#                    ou (q6 in (88,99) et q6a=04)
#   88 NVPD       : q7a=88 ou q9a=88 ou q13a=88
#   99 NSP        : q7a=99 ou q9a=99 ou q13a=99
#
# SITUMAT (situation conjugale au moment de l'enquête) :
#   01 pas en couple : q6 in (03, 04)
#   02 marié-e       : q7=01 ou q9=01 ou q13=01
#   03 pacsé-e       : q7=02 ou q9=02 ou q13=02
#   04 union libre   : q7=03 ou q9=03 ou q13=03
#   88 NVPD          : q7=88 ou q9=88 ou q13=88
#   99 NSP           : sinon
#
# Enf1 (nombre total d'enfants de l'ego) :
#   . NA            : si ENF1A ou ENF1A1_REC ou ENF1B_REC = NVPD/NSP
#   0               : si ENF1A = 00
#   ENF1A1_REC      : si ENF1A1_REC dans 0-87 (kids hors logement)
#   ENF1B_REC       : si ENF1B_REC dans 0-87 (kids dans logement)

#### Import des librairies ----

library(labelled) # manipuler des variables labellisées
library(tidyverse) # visualiser, manipuler, importer données
library(questionr) # fonctions utiles 

#### Sous-pop principale et recodage variables ----

## Sous-population qui repose sur l'identification sexuelle
## Pour le tableau 2a de Trachman & Lejbowicz on garde Homo + Bi
## (les effectifs Hétéro sont très faibles dans le volet LGBT).
lgbtn = lgbt %>%
  select(
    Q1,
    SEX10,
    Q19E_age_rec, # âge ego en tranches 
    Q29E_rec,
    CS_E_rec,
    Q25E,
    Q3_rec, 
    Q22E_01,
    Q22E_02,
    Q22E_03, 
    EA1, 
    EA2, 
    REV2, # Revenu individuel
    REV4, # Revenu subjectif
    # ↓ Variables brutes pour reconstruire ETATMAT et SITUMAT
    Q6, Q6a,
    Q7, Q7a,
    Q9, Q9a,
    Q13, Q13a,
    # ↓ Variables brutes pour reconstruire Enf1
    ENF1a, ENF1a1_rec, ENF1b_rec,
    # ↓ Variables construites déjà disponibles en LGBT
    FCPL,
    FCOHAB,
    Q11_duree_rec, # Durée relation 
    Q19C_rec, # âge conjoint en tranches 
    ENF2_rec
  ) %>%
  filter(
    !SEX10 %in% c("01", "88", "99", ""), # retire NSP/NVPD + hétéros
    !Q19E_age_rec %in% c("1", "12", "13", "14") # retire <20 ans et >=70 ans
  ) %>%
  mutate(
    genre = fct_drop(as_factor(Q1)),
    idsexu = fct_drop(as_factor(SEX10)) %>%
      fct_recode(
        "Homo" = "Homosexuel-le",
        "Bi" = "Bisexuel-le"
      ) %>%
      fct_relevel("Homo", "Bi"),
    # ↓ tranches d'âge alignées sur PG (4 modalités : 20-29, 30-39, 40-49, 50-69)
    age = as_factor(Q19E_age_rec),
    age = case_when(
      age %in% c("20-24", "25-29") ~ "20-29",
      age %in% c("30-34", "35-39") ~ "30-39",
      age %in% c("40-44", "45-49") ~ "40-49",
      age %in% c("50-54", "55-59", "60-64", "65-69") ~ "50-69"
    ) %>%
      fct_relevel("20-29", "30-39", "40-49", "50-69"),
    
    # ↓ Recodage classe
    
    # diplôme recodé en 5 modalités comme dans le tableau 2a
    # Q29E_rec est très détaillé, on regroupe selon le niveau
    diplome = as_factor(Q29E_rec),
    diplome = case_when(
      diplome %in% c(
        "0.Aucun diplôme",
        "10.Primaire :Certificat d’études primaires (CEP)", 
        "20.Secondaire :Brevet des collèges, BEPC",
        "21.Secondaire :CAP (Certificat d’aptitude professionnel)",
        "22.Secondaire :BEP (Brevet d’enseignement professionnel)",
        "23.Secondaire :Diplômes d'état d'assistant-e familial-e, d'aide soignant-e, d'auxiliaire de vie sociale, d'aide médico-psychologique",
        "24.Secondaire :Autre diplôme de niveau collège ou lycée"
      ) ~ "Lycée ou inférieur",
      diplome %in% c(
        "30.Bac :Baccalauréat général",
        "31.Bac :Baccalauréat technologique ou professionnel",
        "32.Bac :Capacité en droit, DAEU, ESEU, Brevet de Technicien, Brevet des métiers d'art…",
        "33.Bac :Autre diplôme équivalent au bac", 
        "40.Bac +2 :DEUG",
        "41.Bac +2 :BTS, DUT, DEUST, DSTS, DEIS (ingénirie sociale)",
        "42.Bac +2 :Diplôme des professions sociales et de la santé de niveau bac+2 (assistant-e social-e, éducateur-trice, infirmier-ère…)",
        "43.Bac +2 :Diplômes de 1er cycle du CNAM, Diplôme des métiers d'art,",
        "44.Bac +2 :Autre diplôme de niveau bac +2",
        "50.Bac +3 :Licence (L3)",
        "51.Bac +3 :Certificat d'aptitude pédagogique, Diplôme d'études supérieures d'instituteur",
        "52.Bac +3 :Bachelor en école de commerce",
        "53.Bac +3 :Diplôme d'état d'infirmier-e, puéricultrice, masseur-kinésithérapeute, psychomotricien-ne, capacité d'orthophonie…",
        "55.Bac +3 :Diplôme de formation générale en sciences médicales (DFGSM3), pharmaceutiques (DFGSP), d’odontologie (DFGSO)",
        "56.Bac +3 :Autre diplôme de niveau bac +3"
      ) ~ "Bac / Bac+3",
      diplome %in% c(
        "60.Bac +4 :Maîtrise, MST, licence en 4 ans",
        "61.Bac +4 :Diplôme d'une grande école de niveau bac +4 (ingénieur, commerce...)",
        "62.Bac +4 :Diplômes d'études supérieures du CNAM",
        "63.Bac +4 :Autre diplôme de niveau bac +4",
        "70.Bac +5 :CAPES, CAPA, Agrégation (pour enseigner au lycée)",
        "71.Bac +5 :Master enseignement (MEEF)",
        "72.Bac +5 :Master professionnel ou recherche (M2, y compris master IEP)",
        "73.Bac +5 :DESS, DEA, DESup",
        "74.Bac +5 :Diplôme d'une grande école de niveau master (ingénieur, commerce...)",
        "75.Bac +5 :Diplôme d’ingénieur universitaire",
        "78.Bac +5 :Autre diplôme de niveau bac +5",
        "80.Bac +6 et plus :Doctorat, HDR, Agrégation (pour enseigner dans le supérieur)",
        "81.Bac +6 et plus :Diplômes niveau bac +6 et plus dans le domaine médical (y compris DU et DIU de spécialités)",
        "82.Bac +6 et plus :Autre formation de niveau bac +6 et plus (architecte DPLG…)",
        "83.Bac +6 et plus :Autre diplôme de niveau bac +6 et plus"
      ) ~ "Supérieur à Bac+3",
      diplome %in% c("NVPD", "NSP") ~ "NVPD/NSP"
    ) %>%
      fct_relevel("Lycée ou inférieur", "Bac / Bac+3", "Supérieur à Bac+3",
                 "NVPD/NSP"),
    
    # CSP 
    csp = fct_recode(
      as_factor(CS_E_rec),
      "Agriculteur·rice exploitant·e" = "Agriculteurs exploitants",
      "Artisan·e, commerçant·e, chef d'entreprise" = "Artisans, commerçants et chefs d'entreprise",
      "Cadre, profession intellect. sup." = "Cadres et professions intellectuelles supérieures",
      "Profession intermédiaire" = "Professions Intermédiaires",
      "Employé·e" = "Employés",
      "Ouvrier·e" = "Ouvriers",
      NULL = "Retraités",
      NULL = "Autres personnes sans activité professionnelle",
      NULL = "0",
      NULL = "Activité ne souhaite pas répondre",
      NULL = "Activité ne sais pas"
    ),
    
    # Statut d'activité 5 modalités, même grille que PG
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
    
    # Statut d'activité 2 modalités : actif / inactif 
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
      fct_relevel("Actif·ve", "Inactif·ve", "NSP/NVPD"),
   
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
    ),
    
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
    ),
    
    # ↓ Territoire 
    taille_agglo = fct_recode(
      as_factor(Q3_rec),
      "200 000 et plus" = "",  #Paris et petite couronne sont les manquants 
      "200 000 et plus" = "1. Une agglomération de plus d'un million d' habitants",
      "200 000 et plus" = "2. Une agglomération de plus de 200 000 habitants",
      "De 20 000 à 200 000" = "3. Une agglomération de 100 000 à 200 000 habitants",
      "De 20 000 à 200 000" = "4. Une agglomération de 20 000 à 100 000 habitants",
      "Moins de 20 000" = "5. Une agglomération de moins de 20 000 habitants",
      "Moins de 20 000" = "6. Un village (moins de 2 000 habitants)",
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ) |> 
      fct_relevel("Moins de 20 000", "De 20 000 à 200 000", "200 000 et plus", "NVPD/NSP"),
    
    # migration 
    mig = case_when(
      # 1. Ego de nationalité étrangère à la naissance
      Q22E_02 == "01" | Q22E_03 == "01" 
      ~ "Immigré·e, descendant·e ou né·e dans un DOM",
      # 2. Au moins un parent immigré (français par acquisition ou autre nationalité)
      EA1 %in% c("04", "05") | EA2 %in% c("04", "05") 
      ~ "Immigré·e, descendant·e ou né·e dans un DOM",
      # 3. Au moins un parent né dans un DOM (proxy pour "lien avec un DOM")
      EA1 == "02" | EA2 == "02" 
      ~ "Immigré·e, descendant·e ou né·e dans un DOM",
      # 4. NSP / NVPD sur la nationalité d'ego ou du père et de la mère
      Q22E_01 %in% c("88", "99") | 
        (EA1 %in% c("88", "99") & EA2 %in% c("88", "99")) 
      ~ "Indeterminé",
      # 5. Tout le reste = population majoritaire
      TRUE 
      ~ "Majoritaire"
    ) %>%
      fct_relevel("Majoritaire",
                  "Immigré·e, descendant·e ou né·e dans un DOM",
                  "Indeterminé"), 
    
    #Reconstruction des variables conjugales absentes du LGBT
    
    # On s'assure d'avoir les valeurs en caractère pour pouvoir tester
    # explicitement les codes "01", "02", etc.
    Q6_chr   = as.character(Q6),
    Q6a_chr  = as.character(Q6a),
    Q7_chr   = as.character(Q7),
    Q7a_chr  = as.character(Q7a),
    Q9_chr   = as.character(Q9),
    Q9a_chr  = as.character(Q9a),
    Q13_chr  = as.character(Q13),
    Q13a_chr = as.character(Q13a),
    
    # ↓ ETATMAT reconstruite (état matrimonial légal)
    # Règle INED : on prend la 1re modalité non vide selon l'ordre du document
    Etatmat = case_when(
      Q7a_chr == "01" | Q9a_chr == "01" | Q13a_chr == "01" |
        Q6_chr == "04" |
        (Q6_chr %in% c("88", "99") & Q6a_chr == "01")             ~ "01",
      Q7a_chr == "02" | Q9a_chr == "02" | Q13a_chr == "02" |
        Q7_chr == "01" | Q9_chr == "01" | Q13_chr == "01" |
        (Q6_chr %in% c("88", "99") & Q6a_chr == "02")             ~ "02",
      Q7a_chr == "03" | Q9a_chr == "03" | Q13a_chr == "03" |
        (Q6_chr %in% c("88", "99") & Q6a_chr == "03")             ~ "03",
      Q7a_chr == "04" | Q9a_chr == "04" | Q13a_chr == "04" |
        (Q6_chr %in% c("88", "99") & Q6a_chr == "04")             ~ "04",
      Q7a_chr == "88" | Q9a_chr == "88" | Q13a_chr == "88"         ~ "88",
      Q7a_chr == "99" | Q9a_chr == "99" | Q13a_chr == "99"         ~ "99",
      TRUE                                                         ~ NA_character_
    ),
    # Libellés alignés sur PG
    etatmat = fct_recode(
      factor(Etatmat, levels = c("01", "02", "03", "04", "88", "99")),
      "Célibataire" = "01",
      "Marié.e"     = "02",
      "Divorcé.e"   = "03",
      "Veuf.ve"     = "04",
      "NVPD/NSP"    = "88",
      "NVPD/NSP"    = "99"
    ) %>%
      fct_relevel("Célibataire", "Marié.e", "Divorcé.e", "Veuf.ve", "NVPD/NSP"),
    
    # ↓ SITUMAT reconstruite (situation de couple au moment de l'enquête)
    Situmat = case_when(
      Q6_chr %in% c("03", "04")                                    ~ "01",
      Q7_chr == "01" | Q9_chr == "01" | Q13_chr == "01"            ~ "02",
      Q7_chr == "02" | Q9_chr == "02" | Q13_chr == "02"            ~ "03",
      Q7_chr == "03" | Q9_chr == "03" | Q13_chr == "03"            ~ "04",
      Q7_chr == "88" | Q9_chr == "88" | Q13_chr == "88"            ~ "88",
      TRUE                                                         ~ "99"
    ),
    
    # ↓ Statut couple au moment de l'enquête (ensemble)
    statutcouple = fct_recode(
      factor(Situmat, levels = c("01", "02", "03", "04", "88", "99")),
      "Pas en couple" = "01",
      "Marié.e"       = "02",
      "Pacsé.e"       = "03",
      "Union libre"   = "04",
      "NVPD/NSP"      = "88",
      "NVPD/NSP"      = "99"
    ) %>%
      fct_relevel("Pas en couple", "Marié.e", "Pacsé.e", "Union libre", "NVPD/NSP"),
    
    # ↓ Statut couple au moment de l'enquête : oui / non (ensemble)
    couple_enq = fct_recode(
      as_factor(Situmat),
      "Oui" = "02", 
      "Oui" = "03", 
      "Oui" = "04", 
      "Non" = "01", 
      "NVPD/NSP" = "88",
      "NVPD/NSP" = "99"
    ) |> 
      fct_relevel("Oui", "Non"), 

    # ↓ Couple > 4 mois au cours des 12 derniers mois (à partir de FCPL)
    couple12mois = case_when(
      as.character(FCPL) %in% c("01", "02", "03", "04") ~ "Oui",
      as.character(FCPL) %in% c("05", "06")             ~ "Non",
      TRUE                                              ~ NA_character_
    ) %>%
      factor(levels = c("Oui", "Non")),
    
    # ↓ Cohabitation (à partir de FCOHAB)
    # FCOHAB codes LGBT (identiques à PG) :
    # 01-04 = couple cohabitant ; 05 = autre cas (= pas en couple OU couple non cohabitant)
    # On utilise FCPL pour distinguer "couple non cohabitant" (FCPL=01..04) de
    # "pas en couple" (FCPL=05 ou 06) parmi les FCOHAB=05.
    cohabitation = case_when(
      as.character(FCOHAB) %in% c("01", "02", "03", "04") ~ "En couple cohabitant",
      as.character(FCOHAB) == "05" &
        as.character(FCPL) %in% c("01", "02", "03", "04") ~ "En couple non cohabitant",
      as.character(FCOHAB) == "05" &
        as.character(FCPL) %in% c("05", "06")             ~ "Pas en couple",
      TRUE                                                ~ NA_character_
    ) %>%
      fct_relevel("En couple cohabitant", "En couple non cohabitant", "Pas en couple"),
    
    #Reconstruction de Enf1 (nombre total d'enfants d'ego)
    
    # Règle INED : on prend la 1re info renseignée parmi
    # ENF1a, ENF1a1_rec, ENF1b_rec (priorité à ENF1a pour la valeur 0).
    # ENF1a codes : "00" Non, "01" Oui, "88" NVPD, "99" NSP
    # ENF1a1_rec et ENF1b_rec codes : "00","01","02","03","04" (=4+), "88" NVPD
    Enf1 = case_when(
      as.character(ENF1a) %in% c("88", "99") |
        as.character(ENF1a1_rec) == "88" |
        as.character(ENF1b_rec) == "88"               ~ NA_character_,
      as.character(ENF1a) == "00"                     ~ "Non",
      as.character(ENF1a1_rec) == "00"                ~ "Non",
      as.character(ENF1a1_rec) == "01"                ~ "Oui",
      as.character(ENF1a1_rec) == "02"                ~ "Oui",
      as.character(ENF1a1_rec) == "03"                ~ "Oui",
      as.character(ENF1a1_rec) == "04"                ~ "Oui",
      as.character(ENF1b_rec) == "00"                 ~ "Non",
      as.character(ENF1b_rec) == "01"                 ~ "Oui",
      as.character(ENF1b_rec) == "02"                 ~ "Oui",
      as.character(ENF1b_rec) == "03"                 ~ "Oui",
      as.character(ENF1b_rec) == "04"                 ~ "Oui",
      TRUE                                            ~ NA_character_
    ),
    enf_ego = factor(Enf1, levels = c("Oui", "Non")),
    
    # ↓ Nombre d'enfants du couple (à partir de ENF2_REC qui existe en LGBT)
    enf_couple = fct_recode(
      as_factor(ENF2_rec),
      NULL    = "",
      NULL = "NC", 
      "0"     = "00",
      "1"     = "01",
      "2"     = "02",
      "3+"     = "03",
      "3+"    = "4 et plus",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>%
      fct_relevel("0", "1", "2", "3+"), 
    
    # ↓ Durée de la relation actuelle (en mois → années, catégorisée)
    # Q11_DUREE_REC est en caractère pour LGBT. Pour FCPL=01,02 = relation
    # principale actuelle. Hors univers → NA naturellement.
    dur_rel_mois = as.numeric(as.character(Q11_duree_rec)),
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
    # Q19E_age_rec et Q19C_REC sont en tranches quinquennales (codes "1"-"14",
    # "88" pour NVPD). L'écart est calculé en nombre de tranches.
    # APPROXIMATION : 1 tranche ≈ 5 ans (en moyenne, peut osciller 0-9 ans).
    # À mentionner en note de tableau.
    age_ego_grp = case_when(
      as.character(Q19E_age_rec) %in% as.character(1:14)
      ~ as.integer(as.character(Q19E_age_rec)),
      TRUE ~ NA_integer_
    ),
    age_part_grp = case_when(
      as.character(Q19C_rec) %in% as.character(1:14)
      ~ as.integer(as.character(Q19C_rec)),
      TRUE ~ NA_integer_
    ),
    diff_age_grp = age_ego_grp - age_part_grp,
    ecart_age = case_when(
      is.na(diff_age_grp) ~ NA_character_,
      diff_age_grp >= 2   ~ "Conjoint·e plus jeune (10+ ans)",
      diff_age_grp == 1   ~ "Conjoint·e plus jeune (5-9 ans)",
      diff_age_grp == 0   ~ "Du même âge (± 4 ans)",
      diff_age_grp == -1  ~ "Conjoint·e plus âgé·e (5-9 ans)",
      diff_age_grp <= -2  ~ "Conjoint·e plus âgé·e (10+ ans)"
    ) %>%
      factor(levels = c("Conjoint·e plus âgé·e (10+ ans)",
                        "Conjoint·e plus âgé·e (5-9 ans)",
                        "Du même âge (± 4 ans)",
                        "Conjoint·e plus jeune (5-9 ans)",
                        "Conjoint·e plus jeune (10+ ans)")),
  ) %>%
  select(# variables temporaires
         -Q6_chr, -Q6a_chr, -Q7_chr, -Q7a_chr,
         -Q9_chr, -Q9a_chr, -Q13_chr, -Q13a_chr)

## Sous-populations femmes et hommes (Virage LGBT)

lgbtnf = lgbtn %>%
  filter(genre == "Une femme")

lgbtnh = lgbtn %>%
  filter(genre == "Un homme")

#### Sous-pop sans filtre sur l'identification (id-attir-prat) ----

lgbt_aip = lgbt %>%
  select(Q1, SEX10, Q19E_age_rec, SEX2F, SEX2H, SEX8a, SEX9, SEX9a) %>%
  filter(!Q19E_age_rec %in% c("1", "12", "13", "14")) %>%  # 20-69 ans
  mutate(
    genre = fct_drop(as_factor(Q1)),
    
    # ↓ Identification : libellés alignés sur pg_aip
    idsexu = fct_recode(
      as_factor(SEX10),
      "Homo" = "Homosexuel-le",
      "Bi" = "Bisexuel-le",
      "Hétéro" = "Hétérosexuel-le",
      "NSP/NVPD" = "NSP",
      "NSP/NVPD" = "NVPD", 
      "NSP/NVPD" = ""
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "NSP/NVPD"),
    
    # ↓ Attirance (logique de lgbtcritères, libellés alignés sur pg_aip)
    SEX2F = as_factor(SEX2F),
    SEX2H = as_factor(SEX2H),
    attirance = case_when(
      SEX2F == "Uniquement par des hommes" |
        SEX2H == "Uniquement par des femmes"
      ~ "Hétéro",
      SEX2F %in% c("Surtout par des hommes mais aussi par des femmes",
                   "Autant par des hommes que des femmes",
                   "Surtout par des femmes mais aussi par des hommes") |
        SEX2H %in% c("Surtout par des femmes mais aussi par des hommes",
                     "Autant par des femmes que des hommes",
                     "Surtout par des hommes mais aussi par des femmes")
      ~ "Bi",
      SEX2F == "Uniquement par des femmes" |
        SEX2H == "Uniquement par des hommes"
      ~ "Homo",
      TRUE ~ "Pas d'attirance/NSP/NVPD"
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "Pas d'attirance/NSP/NVPD"),
    
    # ↓ Pratique (logique de lgbtcritères, libellés alignés sur pg_aip)
    SEX8a = as_factor(SEX8a),
    SEX9a = as_factor(SEX9a),
    pratique = case_when(
      genre == "Une femme" & SEX8a == "Une femme" & SEX9 == "001" |
        genre == "Une femme" & SEX9 > "001" & SEX9a == "Uniquement des femmes" |
        genre == "Un homme" & SEX8a == "Un homme" & SEX9 == "001" |
        genre == "Un homme" & SEX9 > "001" & SEX9a == "Uniquement des hommes"
      ~ "Homo",
      genre == "Une femme" & SEX8a == "Un homme" & SEX9 == "001" |
        genre == "Une femme" & SEX9 == "000" & SEX9a == "Uniquement des hommes" |
        genre == "Une femme" & SEX9 > "001" & SEX9a == "Uniquement des hommes" |
        genre == "Un homme" & SEX8a == "Une femme" & SEX9 == "001" |
        genre == "Un homme" & SEX9 == "000" & SEX9a == "Uniquement des femmes" |
        genre == "Un homme" & SEX9 > "001" & SEX9a == "Uniquement des femmes"
      ~ "Hétéro",
      SEX9 > "001" & SEX9a == "Des hommes et des femmes"
      ~ "Bi",
      TRUE ~ "Pas de rapport/NSP/NVPD"
    ) %>%
      fct_relevel("Homo", "Bi", "Hétéro", "Pas de rapport/NSP/NVPD") %>% 
      factor(levels = c("Homo", "Bi", "Hétéro", "Pas de rapport/NSP/NVPD"))
  )

lgbt_aip_f = lgbt_aip %>% filter(genre == "Une femme")
lgbt_aip_h = lgbt_aip %>% filter(genre == "Un homme")

#### Sous-pop en couple > 4 mois dans 12 DERNIERS MOIS (tableau 3b) ----

# lgbtn_conjugal = lgbtn filtrée sur FCPL %in% c("01","02", "03", "04") 
# en couple > 4 mois dans les 12 DERNIERS MOIS.

lgbtn_conjugal = lgbtn %>%
  filter(FCPL %in% c("01", "02", "03", "04")) %>%
  mutate(
    cohabitation  = fct_drop(cohabitation)
  )

lgbtnf_conjugal = lgbtn_conjugal %>%
  filter(genre == "Une femme")

lgbtnh_conjugal = lgbtn_conjugal %>%
  filter(genre == "Un homme")