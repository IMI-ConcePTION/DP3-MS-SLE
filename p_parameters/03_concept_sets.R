#-----------------------------------------
# ASSIGN NAMES AND DOMAINS OF CONCEPTSETS
#
# in this file the names of the conceptsets are listed. 
# as a default they are divided in 6 lists, based on the domain (diagnosis, medicines etc). there are two lists for diagnostic conseptsets: one is the list of those with tag 'narrow' and 'possible', and the other is the list of 'plain' conceptsets
# according to the needs of the project the lists can be further subdivided, or some of them may be missing
# those lists are used to feed CreateConceptsetDatasets in step 01_1, and then may be further used in the next steps
# at the bottom of this file there is a call to a second file that assigns codes to each conceptsets. such file may either be assigned directly or be retrieved from a csv
# the codes assigned in this file are preliminary (concept_set_codes_our_study_pre) and may be further refined, possibly in  a datasource-specific manner, in the parameter step 06_algorithms 
#----------------------------------------- 

concept_set_domains<- vector(mode="list")

#-------------
# assign names of diagnosis conceptsets that have a narrow or possible tag 
NARROW_POSSIBLE_conceptsets <- c("GBS_narrow","GBS_possible","ADEM_narrow","ADEM_possible","NARCOLEPSY_narrow","NARCOLEPSY_possible","ACUASEARTHRITIS_narrow","ACUASEARTHRITIS_possible","DM1_narrow","DM1_possible","MICROANGIO_narrow","MICROANGIO_possible","HF_narrow","HF_possible","STRCARD_narrow","STRCARD_possible","CAD_narrow","CAD_possible","ARR_narrow","ARR_possible","MYOCARD_narrow","MYOCARD_possible","SOCV_narrow","SOCV_possible","ALI_narrow","ALI_possible","AKI_narrow","AKI_possible","GENCONV_narrow","GENCONV_possible","MENINGOENC_narrow","MENINGOENC_possible","ARD_narrow","ARD_possible","ERYTH_narrow","ERYTH_possible","CHILBLAIN_narrow","CHILBLAIN_possible","ANOSMIA_narrow","ANOSMIA_possible","ANAPHYL_narrow","ANAPHYL_possible","KD_narrow","KD_possible","MISCC_narrow","MISCC_possible","MIS_narrow","MIS_possible","SUDDENDEAT_narrow","SUDDENDEAT_possible","TRANSMYELITIS_narrow","TRANSMYELITIS_possible","DIC_narrow","DIC_possible","Hemostroke_narrow","Hemostroke_possible","Ischstroke_narrow","Ischstroke_possible","Sinusthrom_narrow","Sinusthrom_possible","VTE_narrow","VTE_possible","TP_narrow","TP_possible","TMA_narrow","TMA_possible","COVID_narrow","COVID_possible","Myocardalone_narrow","Myocardalone_possible","BP_narrow","BP_possible","PERICARD_narrow","PERICARD_possible")


# for (concept in c(NARROW_POSSIBLE_conceptsets)) {
#   concept_set_domains[[paste0(concept,"_narrow")]] = "Diagnosis"
#   concept_set_domains[[paste0(concept,"_possible")]] = "Diagnosis"
# }

for (concept in c(NARROW_POSSIBLE_conceptsets)) {
  concept_set_domains[[concept]] = "Diagnosis"
  concept_set_domains[[concept]] = "Diagnosis"
}


#-------------
# assign names of diagnosis conceptsets that don't have a narrow or possible tag 
DIAGNOSIS_conceptsets <- c("MISCC","MIS","SUDDENDEAT","TRANSMYELITIS","DIC","Hemostroke","Ischstroke","Sinusthrom","VTE","TP","TMA","COVID","Myocardalone","BP")



for (concept in c(DIAGNOSIS_conceptsets)) {
  concept_set_domains[[concept]] = "Diagnosis"
}

#-------------
# assign names of drug conceptsets
DRUGS_conceptsets <- c("DP_COVCANCER","DP_COVDIAB","DP_CVD","DP_COVHIV","DP_COVCKD","DP_COVCOPD","DP_COVOBES","DP_COVSICKLE","IMMUNOSUPPR","DP_CONTRHYPERT")
for (concept in c(DRUGS_conceptsets)) {
  concept_set_domains[[concept]] = "Medicines"
}

#-------------
# assign names of procedure conceptsets
PROC_conceptsets <- c()
for (concept in c(PROC_conceptsets)) {
  concept_set_domains[[concept]] = "Procedures"
}

#-------------
# assign names of results conceptsets
RESULTS_conceptsets <- c()
for (concept in c(RESULTS_conceptsets)) {
  concept_set_domains[[concept]] = "Results"
}

#-------------
# assign names of vaccine conceptsets
vaccine_conceptssets <- c("Covid_vaccine")
concept_set_domains[["Covid_vaccine"]] = "VaccineATC"


#-------------
# list all available conceptsets (do not change, to be used in CreateConceptsetDatasets)

concept_sets_of_our_study <- unique(c(DIAGNOSIS_conceptsets, NARROW_POSSIBLE_conceptsets, DRUGS_conceptsets, RESULTS_conceptsets,PROC_conceptsets,vaccine_conceptssets))

COV_conceptssets <- c("COVCANCER","COVCOPD","COVHIV","COVCKD","COVDIAB","COVOBES","COVSICKLE","CONTRDIVERTIC","CONTRHYPERT")

SEVERCOVID_conceptsets <- c("COVIDSYMPTOM","MechanicalVent")

#-------------
# START assignment of codes to conceptsets (mostly to be done)
concept_set_codes_our_study_pre <- vector(mode="list")
concept_set_codes_our_study_pre_excl <- vector(mode="list")

source(paste0(thisdir,"/p_parameters/archive_parameters/parameters_raw_test.R"))
