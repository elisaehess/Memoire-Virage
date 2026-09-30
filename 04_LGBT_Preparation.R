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
    ID, 
    Q1,
    SEX10,
    Q19E_age_rec, # âge ego en tranches 
    Q29E_rec,
    CS_E_rec,
    Q25E,
    Q25C, 
    Q3_rec, 
    Q22E_01,
    Q22E_02,
    Q22E_03, 
    EA1, 
    EA2, 
    REV2, # Revenu individuel
    REV3, 
    REV4, # Revenu subjectif
    Q2_rec, # Reconstruire territoire
    Q3_rec,  # Reconstruire territoire
    # ↓ Variables brutes pour reconstruire ETATMAT et SITUMAT
    Q6, Q6a,
    Q7, Q7a,
    Q9, Q9a,
    Q13, Q13a,
    # ↓ Variables brutes pour reconstruire Enf1
    ENF1a, ENF1a1_rec, ENF1b_rec,
    # ↓ Variables construites déjà disponibles en LGBT
    FCOHAB,
    Q11_duree_rec, # Durée relation 
    Q19C_rec, # âge conjoint en tranches 
    ENF2_rec,
    # ↓ Variables brutes pour reconstruire FSEXCJT et TYPECPL
    FCPL, 
    Q12, 
    Q36, 
    Q17,
    # ↓ Styles de conjugalité 
    CF1a, # tout faire ensemble
    CF1b, # compromis 
    SOC1a, 
    SOC1b, 
    SOC1c, 
    SOC1d, 
    SOC2a, 
    SOC2b, 
    SOC2c, 
    SOC2d, 
    CF2, # tâches ménagères
    CF3, # s'occuper des enfants
    
    # ↓ Satisfaction relation  
    C1,
    C1a,
    C1a1,
    SEX14,
    S2a_02, 
    LGBT1e, 
    
    # ↓ Sujets de conflits 
    CF5a, 
    CF5b,
    CF5c1,
    CF5c2, 
    CF5d, 
    CF5e, 
    CF5f, 
    CF5g, 
    CF5h, 
    CF5i, 
    CF5j, 
    CF5k, 
    CF5l, 
    
    # ↓ Situation avec parents 
    EA9a, 
    EA9b, 
    EA9c, 
    EA9d, 
    EA9e, 
    EA9f, 
    EA9g,
    
    # ↓ Violences
    C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,C12, C13, 
    C15,C16,C14a,C14b,
    C18a,C18b, C18c,C18d,C18e,C18f,C18g,
    C24,C26, 
    C20,C21,C22, C23,
    C30,C31, C34
  ) %>%
  filter(
    !SEX10 %in% c("01", "88", "99", ""), # retire NSP/NVPD + hétéros
    !Q19E_age_rec %in% c("1", "12", "13", "14") # retire <20 ans et >=70 ans
  ) %>%
  mutate(
    genre = fct_drop(as_factor(Q1)) %>% set_variable_labels("Genre"),
    idsexu = fct_drop(as_factor(SEX10)) %>%
      fct_recode(
        "Homo" = "Homosexuel-le",
        "Bi" = "Bisexuel-le"
      ) %>%
      fct_relevel("Homo", "Bi") %>% set_variable_labels("Identification sexuelle"),
    
    # ↓ Nouvelle variable croisée genre et identification sexuelle 
    genre_idsexu = case_when(
      genre == "Un homme" & idsexu == "Bi" ~ "Bi", 
      genre == "Un homme" & idsexu == "Homo" ~ "Gay",
      genre == "Une femme" & idsexu == "Bi" ~ "Bie", 
      genre == "Une femme" & idsexu == "Homo" ~ "Lesbienne"
    ) %>% 
      fct_relevel ("Bi", "Bie", "Gay", "Lesbienne"),
    
    # ↓ tranches d'âge alignées sur PG (4 modalités : 20-29, 30-39, 40-49, 50-69)
    age = as_factor(Q19E_age_rec),
    age = case_when(
      age %in% c("20-24", "25-29") ~ "20-29",
      age %in% c("30-34", "35-39") ~ "30-39",
      age %in% c("40-44", "45-49") ~ "40-49",
      age %in% c("50-54", "55-59", "60-64", "65-69") ~ "50-69"
    ) %>%
      fct_relevel("20-29", "30-39", "40-49", "50-69") %>%
      set_variable_labels("Groupe d'âge"),
    
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
                  "NVPD/NSP") %>% 
      set_variable_labels("Diplôme"), 
    
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
    ) %>% 
      set_variable_labels("Catégorie socioprofessionnelle"),
    
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
      fct_relevel("Actif·ve", "Inactif·ve", "NSP/NVPD") %>% 
      set_variable_labels("En activité"),
    
    statut_act2_conjoint = fct_recode( #filtré FCPL 01, 02, 03, 04
      as_factor(Q25C),
      "Actif·ve" = "En emploi (y compris intérim, congé de maternité/paternité, arrêt maladie, mais pas étudiant en stage rémunéré)",
      "Actif·ve" = "Au chômage avec indemnités",
      "Actif·ve" = "Au chômage sans indemnités",
      "Inactif·ve" = "A la retraite",
      "Inactif·ve" = "Etudiant-e, élève",
      "Inactif·ve" = "Etudiant-e, élève avec emploi y compris petit boulot ou stage rémunéré",
      "Inactif·ve" = "Etudiant-e, élève avec stage non rémunéré",
      "Inactif·ve" = "Inactif-ve ou au foyer ayant déjà travaillé (ayant eu un contrat de travail y compris congé maladie longue durée de 5 ans ou plus)",
      "Inactif·ve" = "Inactif-ve ou au foyer n'ayant jamais travaillé (n'ayant jamais eu de contrat de travail)",
      "Inactif·ve" = "En congé parental, de solidarité familiale",
      "Inactif·ve" = "Autre congé de longue durée (Anné sabbatique, congé de création d'entreprise, congé LONGUE MALADIE de moins de 5 ans)",
      NULL = "NSP",
      NULL = "NVPD", 
      NULL = ""
    ) %>%
      fct_relevel("Actif·ve", "Inactif·ve") %>% 
      set_variable_labels("Conjoint-e en activité"), 
    
    # Statut d'activité comparé couple 
    statut_act_compare = case_when(
      statut_act2 == "Actif·ve" & statut_act2_conjoint == "Actif·ve" ~ "Couple actif", 
      statut_act2 == "Actif·ve" & statut_act2_conjoint == "Inactif·ve" ~ "Ego actif / Partenaire inactif", 
      statut_act2 == "Inactif·ve" & statut_act2_conjoint == "Actif·ve" ~ "Ego inactif / Partenaire actif", 
      statut_act2 == "Inactif·ve" & statut_act2_conjoint == "Inactif·ve" ~ "Couple inactif"
    ),
   
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
    
    # ↓ Revenu du ménage (REV3), en tranches. Champ : ménage cohabitant
    #   (FCOHAB=01..04 ou FCOHAB=05 avec un autre membre du ménage).
    revenu_menage = case_when(
      REV3 %in% c("00", "01", "02", "03") ~ "Moins de 1 800€",
      REV3 %in% c("04", "05")       ~ "De 1 800 à moins de 3 000€",
      REV3 %in% c("06")             ~ "De 3 000 à moins de 4 000€", 
      REV3 %in% c("07", "08")       ~ "4 000€ et plus",
      REV3 %in% c("10", "88", "99") ~ "Pas de budget commun/NVPD/NSP",
      TRUE                          ~ NA_character_
    ) %>%
      factor(levels = c("Moins de 1 800€", "De 1 800 à moins de 3 000€",
                        "De 3 000 à moins de 4 000€", "4 000€ et plus",
                        "Pas de budget commun/NVPD/NSP")) %>%
      set_variable_labels("Revenu du ménage"),
    
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
      fct_relevel("Moins de 20 000", "De 20 000 à 200 000", "200 000 et plus", "NVPD/NSP") %>% 
      set_variable_labels("Taille d'agglomération"),
    
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
      set_variable_labels("Type de couple"),
    
    # ↓ TYPECPL bi
    typecplbi = case_when(
      idsexu == "Bi" & typecpl == "En couple de même sexe" ~ "Bi en couple de même sexe",
      idsexu == "Bi" & typecpl == "En couple de sexe différent" ~ "Bi en couple de sexe différent",
      idsexu == "Bi" & typecpl == "Pas en couple" ~ "Bi célibataire", 
      idsexu == "Homo" & typecpl == "En couple de même sexe" ~ "Homo en couple", 
      idsexu == "Homo" & typecpl == "Pas en couple" ~ "Homo célibataire", 
      TRUE ~ NA_character_ # on perd 37 homo en couple de sexe différent et 5 NVPD/NSP et 11 Bi NVPD/NSP
    ) %>% 
      set_variable_labels("Type de couple"),
    
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
      fct_relevel("Oui", "Non") %>% 
      set_variable_labels("En couple au moment de l'enquête"), 

    # ↓ Couple > 4 mois au cours des 12 derniers mois (à partir de FCPL)
    couple12mois = case_when(
      as.character(FCPL) %in% c("01", "02", "03", "04") ~ "Oui",
      as.character(FCPL) %in% c("05", "06")             ~ "Non",
      TRUE                                              ~ NA_character_
    ) %>%
      factor(levels = c("Oui", "Non")) %>% 
      set_variable_labels("En couple au cours des 12 derniers mois (> 4 mois)"), 
    
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
    enf_ego = factor(Enf1, levels = c("Oui", "Non")) %>% 
      set_variable_labels("A des enfants"), 
    
    # (filtré sur pers. en couple à l'enquête et avec au moins 1 enfant)
    enf_couple = fct_recode(
      as_factor(ENF2_rec),
      "Sans enfant" = "",
      "Enfant(s) d'une précédente union" = "00",
      "Enfant(s) dans le couple" = "01",
      "Enfant(s) dans le couple" = "02",
      "Enfant(s) dans le couple" = "03",
      "Enfant(s) dans le couple" = "4 et plus",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>%
      fct_relevel("Sans enfant", "Enfant(s) dans le couple", "Enfant(s) d'une précédente union") %>% 
      set_variable_labels("Situation parentale"),
    
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
    
    # ↓ Styles de conjugalité
    fusion = fct_recode( # non filtrée
      as_factor(CF1a), 
      "D'accord" = "Tout à fait d'accord", 
      "D'accord" = "Plutôt d'accord", 
      "Pas d'accord" = "Plutôt pas d'accord", 
      "Pas d'accord" = "Pas du tout d'accord", 
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Dans un couple, il faut tout faire ensemble"),  
    compromis = fct_recode( # non filtrée
      as_factor(CF1b), 
      "D'accord" = "Tout à fait d'accord", 
      "D'accord" = "Plutôt d'accord", 
      "Pas d'accord" = "Plutôt pas d'accord", 
      "Pas d'accord" = "Pas du tout d'accord", 
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Dans un couple, il faut faire des compromis pour ne pas contrarier l'autre"), 
    confidence_cjt = fct_recode( # filtré FCPL 01 et 02
      as_factor(SOC1a),
      NULL = "",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% 
      fct_relevel("Oui", "Non", "Ca dépend du problème") %>% 
      set_variable_labels("Se confie au partenaire lors d'un problème personnel"), 
    confidence_famille = fct_recode( 
      as_factor(SOC1b),
      NULL = "",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% 
      fct_relevel("Oui", "Non", "Ca dépend du problème") %>% 
      set_variable_labels("Se confie à un membre de la famille lors d'un problème personnel"),
    confidence_amis = fct_recode( 
      as_factor(SOC1c),
      NULL = "",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% 
      fct_relevel("Oui", "Non", "Ca dépend du problème") %>% 
      set_variable_labels("Se confie à un-e ami-e lors d'un problème personnel"),
    contact_famille = fct_recode( # non filtrée
      as_factor(SOC2a), 
      "Peu ou pas de contact" = "Non", 
      "Peu ou pas de contact" = "De temps en temps", 
      "Contacts réguliers" = "Souvent", 
      "Contacts réguliers" = "Très souvent",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Contacts avec la famille (12 derniers mois)"), 
    contact_amis = fct_recode( # non filtrée
      as_factor(SOC2b), 
      "Peu ou pas de contact" = "Non", 
      "Peu ou pas de contact" = "De temps en temps", 
      "Contacts réguliers" = "Souvent", 
      "Contacts réguliers" = "Très souvent",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Contacts avec des ami-es (12 derniers mois)"), 
    activites = fct_recode( # non filtrée
      as_factor(SOC2c), 
      "Peu ou pas d'activité" = "Non", 
      "Peu ou pas d'activité" = "De temps en temps", 
      "Activités régulières" = "Souvent", 
      "Activités régulières" = "Très souvent",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Activités sportives ou de loisirs (12 derniers mois)"), 
    assos = fct_recode( # non filtrée
      as_factor(SOC2d), 
      "Peu ou pas d'activité" = "Non", 
      "Peu ou pas d'activité" = "De temps en temps", 
      "Activités régulières" = "Souvent", 
      "Activités régulières" = "Très souvent",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Activités associatives, politiques ou syndicales (12 derniers mois)"), 
    taches_men = fct_recode( # filtré FCOHAB 01, 02, 03, 04
      as_factor(CF2),
      NULL = "",
      "Une autre personne s'occupe des tâches ménagères" = "Une autre personne vivant au foyer s'occupe de l'essentiel des tâches ménagères", 
      "Une autre personne s'occupe des tâches ménagères" = "Une autre personne rémunérée ou non s'occupe de l'essentiel des tâches",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Répartition tâches ménagères"), 
    taches_enf = fct_recode( # filtré FCOHAB 01, 02, 03, 04 et Q5b>00 (personnes de moins de 16 ans dans foyer)
      as_factor(CF3),
      NULL = "",
      "Une autre personne s'occupe des enfants" = "Une autre personne vivant au foyer s'en occupe", 
      "Une autre personne s'occupe des enfants" = "Une personne hors du foyer s'en occupe",
      NULL = "77",
      NULL = "NVPD", 
      NULL = "NSP"
    ) %>% set_variable_labels("Répartition tâches enfants foyer"),
    
    
    # ↓ Satisfaction conjugale 
    satisfaction = fct_recode(
      as_factor(C1), # filtrée FCPL = 01, 02
      NULL = "", 
      "Oui" = "Très satisfaisante",
      "Oui" = "Satisfaisante",
      "Non" = "Peu satisfaisante",
      "Non" = "Pas du tout satisfaisante",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% set_variable_labels("Satisfait de la relation de couple actuelle"),
    amoureux = fct_recode(
      as_factor(SEX14), # filtrée FCPL = 01, 02
      NULL = "", 
      "Oui" = "Vous êtes très amoureux-se",
      "Oui" = "Vous êtes amoureux-se",
      "Non" = "Vous n’êtes plus amoureux-se",
      "Non" = "Vous n’avez jamais été amoureux-se",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% set_variable_labels("Sentiment amoureux vis-à-vis partenaire"),
    rupture = fct_recode(
      as_factor(C1a), # filtrée FCPL = 01, 02
      NULL = "", 
      "Oui" = "Oui vous-même",
      "Oui" = "Oui votre conjoint",
      "Oui" = "Oui les deux",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>%
      fct_relevel("Oui", "Non") %>% 
      set_variable_labels("Intention de rompre (12 derniers mois)"),
    rupture_suite = fct_recode( 
      as_factor(C1a1), # filtrée FCPL = 01, 02
      NULL = "", 
      "Oui" = "Une demande de divorce",
      "Oui" = "Une séparation   avec décohabitation",
      "Oui" = "Une séparation  sans décohabitation",
      "Non" = "Vous êtes toujours ensemble",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>%
      fct_relevel("Oui", "Non") %>% 
      set_variable_labels("Aboutissement intention de rupture"), 
    depression = case_when( #décalage code / label dans la base source 
      haven::zap_labels(S2a_02) == "01" ~ "Oui",
      haven::zap_labels(S2a_02) == "00" ~ "Non",
      haven::zap_labels(S2a_02) %in% c("77", "88", "99") ~ NA_character_
    ) %>%
      factor(levels = c("Oui", "Non")) %>%
      set_variable_labels("Dépression ou anxiété au moment de l'enquête"), 
    comingoutconjoint = fct_recode(
      as_factor(LGBT1e), # filtrée Q6 = 01, 02
      NULL = "",
      NULL = "Oui certains", #vide
      NULL = "Oui tous", #vide
      NULL = "Non concerné-e", #vide
      "NVPD/NSP" = "NVPD",
      "NVPD/NSP" = "NSP"
    ) %>% 
      set_variable_labels("Le/la partenaire a connaissance de votre bisexualité"),
    
    # ↓ Sujets de conflits
    conflit_taches = fct_recode(
      as_factor(CF5a), # filtrée FCOHAB 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Tâches vie quotidienne"),
    conflit_enf = fct_recode(
      as_factor(CF5b), # filtrée FCPL 01, 02, 03, 04 et ENF > 01 
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Enfants"),
    conflit_argent = fct_recode(
      as_factor(CF5d), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Argent"),
    conflit_opinion = fct_recode(
      as_factor(CF5e), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Politique, religion ou opinions"),
    conflit_vacances = fct_recode(
      as_factor(CF5f), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Sorties, loisirs ou vacances"),
    conflit_famille = fct_recode(
      as_factor(CF5g), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Relations famille"),
    conflit_famille_cjt = fct_recode(
      as_factor(CF5h), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Relations famille conjoint"),
    conflit_amis = fct_recode(
      as_factor(CF5i), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Relations amis"),
    conflit_travail = fct_recode(
      as_factor(CF5j), # filtrée FCPL 01, 02, 03, 04 et en emploi 
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Travail"),
    conflit_travail_cjt = fct_recode(
      as_factor(CF5k), # filtrée FCPL 01, 02, 03, 04 et en emploi 
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Travail conjoint"),
    conflit_sexualite = fct_recode(
      as_factor(CF5l), # filtrée FCPL 01, 02, 03, 04
      NULL = "",
      "Oui" = "Parfois", 
      "Oui" = "Souvent",
      NULL = "tout le temps ou presque", #vide
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Sexualité de couple"), 
    
    # ↓ Situation avec parents 
    privation = fct_recode(
      as_factor(EA9a), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Privations matérielles ou négligences graves"), 
    conflit_pere = fct_recode(
      as_factor(EA9b), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Conflit très grave avec le père"), 
    conflit_mere = fct_recode(
      as_factor(EA9c), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Conflit très grave avec la mère"),
    fugue = fct_recode(
      as_factor(EA9d), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Fugue ou mise à la porte"),
    conflit_parents = fct_recode(
      as_factor(EA9e), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Tensions graves ou climat de violence entre parents"),
    aide_educ = fct_recode(
      as_factor(EA9f), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Aide éducative à la maison"),
    foyer = fct_recode(
      as_factor(EA9g), 
      NULL = "Non concerné-e",
      NULL = "NVPD",
      NULL = "NSP"
    ) %>% 
      set_variable_labels("Placement en foyer ou famille d'accueil"),
  ) %>%
  select(# variables temporaires
         -Q6_chr, -Q6a_chr, -Q7_chr, -Q7a_chr,
         -Q9_chr, -Q9a_chr, -Q13_chr, -Q13a_chr)

## recodage violence 

# Fonction générique : binarise un item Virage (00=Non, 01-05=Oui, 77/88/99/NA -> NA)
recode_item_virage <- function(x) {
  case_when(
    x == "00" ~ 0L,
    x %in% c("01","02","03","04","05") ~ 1L,
    x %in% c("77","88","99") ~ NA_integer_,
    is.na(x) ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

items_psy <- c("C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
               "C15","C16","C14a","C14b",
               "C18a","C18b","C18c","C18d","C18e","C18f","C18g",
               "C24","C26")

items_phy <- c("C20","C21","C22","C23")
items_sex <- c("C30","C31")

lgbt_violence <- lgbtn %>%
  mutate(across(all_of(c(items_psy, items_phy, items_sex)),
                recode_item_virage, .names = "{.col}_bin"))

lgbt_violence <- lgbt_violence %>%
  mutate(
    psy_oui  = case_when(
      rowSums(across(all_of(paste0(items_psy, "_bin"))), na.rm = TRUE) > 0 ~ "Oui",
      rowSums(!is.na(across(all_of(paste0(items_psy, "_bin"))))) == 0 ~ NA_character_,
      TRUE ~ "Non"
    ), 
    phy_oui  = case_when(
      rowSums(across(all_of(paste0(items_phy, "_bin"))), na.rm = TRUE) > 0 ~ "Oui",
      rowSums(!is.na(across(all_of(paste0(items_phy, "_bin"))))) == 0 ~ NA_character_,
      TRUE ~ "Non"
    ), 
    sex_oui  = case_when(
      rowSums(across(all_of(paste0(items_sex, "_bin"))), na.rm = TRUE) > 0 ~ "Oui",
      rowSums(!is.na(across(all_of(paste0(items_sex, "_bin"))))) == 0 ~ NA_character_,
      TRUE ~ "Non"
    )
  )

lgbtf_violence = lgbt_violence %>% 
  filter(genre == "Une femme")

lgbth_violence = lgbt_violence %>% 
  filter(genre == "Un homme")

lgbt_violence_conjugal = lgbt_violence %>%
  filter(FCPL %in% c("01", "02", "03", "04")) 

lgbtf_violence_conjugal = lgbtf_violence %>%
  filter(FCPL %in% c("01", "02", "03", "04"))

lgbth_violence_conjugal = lgbth_violence %>%
  filter(FCPL %in% c("01", "02", "03", "04"))

## Sous-populations femmes et hommes (Virage LGBT)

lgbtnf = lgbtn %>%
  filter(genre == "Une femme")

lgbtnh = lgbtn %>%
  filter(genre == "Un homme")

# #### Sous-pop sans filtre sur l'identification (id-attir-prat) ----
# 
# lgbt_aip = lgbt %>%
#   select(Q1, SEX10, Q19E_age_rec, SEX2F, SEX2H, SEX8a, SEX9, SEX9a) %>%
#   filter(!Q19E_age_rec %in% c("1", "12", "13", "14")) %>%  # 20-69 ans
#   mutate(
#     genre = fct_drop(as_factor(Q1)),
#     
#     # ↓ Identification : libellés alignés sur pg_aip
#     idsexu = fct_recode(
#       as_factor(SEX10),
#       "Homo" = "Homosexuel-le",
#       "Bi" = "Bisexuel-le",
#       "Hétéro" = "Hétérosexuel-le",
#       "NSP/NVPD" = "NSP",
#       "NSP/NVPD" = "NVPD", 
#       "NSP/NVPD" = ""
#     ) %>%
#       fct_relevel("Homo", "Bi", "Hétéro", "NSP/NVPD"),
#     
#     # ↓ Attirance (logique de lgbtcritères, libellés alignés sur pg_aip)
#     SEX2F = as_factor(SEX2F),
#     SEX2H = as_factor(SEX2H),
#     attirance = case_when(
#       SEX2F == "Uniquement par des hommes" |
#         SEX2H == "Uniquement par des femmes"
#       ~ "Hétéro",
#       SEX2F %in% c("Surtout par des hommes mais aussi par des femmes",
#                    "Autant par des hommes que des femmes",
#                    "Surtout par des femmes mais aussi par des hommes") |
#         SEX2H %in% c("Surtout par des femmes mais aussi par des hommes",
#                      "Autant par des femmes que des hommes",
#                      "Surtout par des hommes mais aussi par des femmes")
#       ~ "Bi",
#       SEX2F == "Uniquement par des femmes" |
#         SEX2H == "Uniquement par des hommes"
#       ~ "Homo",
#       TRUE ~ "Pas d'attirance/NSP/NVPD"
#     ) %>%
#       fct_relevel("Homo", "Bi", "Hétéro", "Pas d'attirance/NSP/NVPD"),
#     
#     # ↓ Pratique (logique de lgbtcritères, libellés alignés sur pg_aip)
#     SEX8a = as_factor(SEX8a),
#     SEX9a = as_factor(SEX9a),
#     pratique = case_when(
#       genre == "Une femme" & SEX8a == "Une femme" & SEX9 == "001" |
#         genre == "Une femme" & SEX9 > "001" & SEX9a == "Uniquement des femmes" |
#         genre == "Un homme" & SEX8a == "Un homme" & SEX9 == "001" |
#         genre == "Un homme" & SEX9 > "001" & SEX9a == "Uniquement des hommes"
#       ~ "Homo",
#       genre == "Une femme" & SEX8a == "Un homme" & SEX9 == "001" |
#         genre == "Une femme" & SEX9 == "000" & SEX9a == "Uniquement des hommes" |
#         genre == "Une femme" & SEX9 > "001" & SEX9a == "Uniquement des hommes" |
#         genre == "Un homme" & SEX8a == "Une femme" & SEX9 == "001" |
#         genre == "Un homme" & SEX9 == "000" & SEX9a == "Uniquement des femmes" |
#         genre == "Un homme" & SEX9 > "001" & SEX9a == "Uniquement des femmes"
#       ~ "Hétéro",
#       SEX9 > "001" & SEX9a == "Des hommes et des femmes"
#       ~ "Bi",
#       TRUE ~ "Pas de rapport/NSP/NVPD"
#     ) %>%
#       fct_relevel("Homo", "Bi", "Hétéro", "Pas de rapport/NSP/NVPD") %>% 
#       factor(levels = c("Homo", "Bi", "Hétéro", "Pas de rapport/NSP/NVPD"))
#   )
# 
# lgbt_aip_f = lgbt_aip %>% filter(genre == "Une femme")
# lgbt_aip_h = lgbt_aip %>% filter(genre == "Un homme")
# 
# #### Sous-pop en couple > 4 mois dans 12 DERNIERS MOIS (tableau 3b) ----
# lgbtn_conjugal = lgbtn filtrée sur FCPL %in% c("01","02", "03", "04") # en couple > 4 mois dans les 12 DERNIERS MOIS.

lgbtn_conjugal = lgbtn %>%
  filter(FCPL %in% c("01", "02", "03", "04")) %>%
  mutate(
    cohabitation  = fct_drop(cohabitation)
  )

lgbtnf_conjugal = lgbtn_conjugal %>%
  filter(genre == "Une femme")

lgbtnh_conjugal = lgbtn_conjugal %>%
  filter(genre == "Un homme")

#### Sous-pop personnes en couple (> 4 mois) AU MOMENT de l'enquête ----

# lgbtn_couple = lgbtn filtrée sur FCPL %in% c("01", "02") + 
# filtre NA satisfaction/amoureux/rupture. C'est la base appropriée pour les
# questions posées aux personnes en couple AU MOMENT de l'enquête. 
# /!\ On la conserve telle quelle pour ne pas casser les analyses existantes
#     sur la satisfaction. 

lgbtn_couple = lgbtn %>%
  filter(FCPL %in% c("01", "02"))

lgbtnf_couple = lgbtn_couple %>% # Création de la sous-population femmes
  filter(genre == "Une femme")

lgbtnh_couple = lgbtn_couple %>% # Création de la sous-population hommes
  filter(genre == "Un homme")