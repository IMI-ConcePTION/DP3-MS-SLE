# we need to create several groups of meanings: one referring to hospitals HOSP (excluding emergency care), one referring to primary care PC, etc

# meanings_of_this_study <- vector(mode="list")
# 
# meanings_of_this_study[["INPATIENT"]]=c("hospitalisation_primary","hospitalisation_secondary","hospital_diagnosis","hopitalisation_diagnosis_unspecified","episode_primary_diagnosis","episode_secondary_diagnosis","diagnosis_procedure","procedure_during_hospitalisation","hospitalisation_associated","hospitalisation_linked","diag_hospitalisation_automatically_referred_to_PC","hospitalisation_ICU_primary","hospitalisation_ICU_secondary","hospitalisation_ICU_unspecified","emergency_room_diagnosis","emergency_room_presentation","hospitalisation_not_overnight_primary", "hospitalisation_not_overnight_secondary", "radiation_hospitalised_primary", "radiation_hospitalised_secondary", "meaning_of_procedure", "hp", "CRITICAL_CARE_STAY", "sd", "pd", "ed")
# meanings_of_this_study[["PC"]]=c("primary_care_event","primary_care_diagnosis","primary_care_events_BIFAP","primary_care_antecedents_BIFAP","primary_care_condicionants_BIFAP","primary_care_main_diagnosis", "primary_care_secondary_diagnosis", "primary_care_midwife_main_diagnosis", "primary_care_midwife_secondary_diagnosis","primary_care_emergency_event", "gpe")
# meanings_of_this_study[["OUTPATIENT_NO_PC"]] = c("specialist_diagnosis","outpatient_hospital_planned_primary","outpatient_hospital_planned_secondary","outpatient_contact_primary","outpatient_contact_secondary","access_to_mental_health_service_primary", "access_to_mental_health_service_comorbidity","reason_for_specialist_encounter", "op")
# meanings_of_this_study[["LONGTERM"]] = c("exemption","long_term_diagnosis")

meanings_of_this_study <- list()

meanings_of_this_study[["UOSL"]][["meaning_of_event"]][["PC"]] <- "primary_care_diagnosis"

meanings_of_this_study[["UOSL"]][["meaning_of_visit"]][["PC"]] <- "primary_care"
meanings_of_this_study[["UOSL"]][["meaning_of_visit"]][["INPATIENT"]] <- c("hospitalisation",
                                                                           "hospitalisation_not_overnight", 
                                                                           "radiation_hospitalised")
meanings_of_this_study[["UOSL"]][["meaning_of_visit"]][["OUTPATIENT_NO_PC"]] <- c("hospital_encounter", "outpatient_contact")


meanings_of_this_study[["THL"]][["meaning_of_event"]][["PC"]] <- c("primary_care_diagnosis", "birth_registry")
meanings_of_this_study[["THL"]][["meaning_of_event"]][["LONGTERM"]] <- "long_term_diagnosis"

meanings_of_this_study[["THL"]][["meaning_of_visit"]][["PC"]] <- "primary_care"
meanings_of_this_study[["THL"]][["meaning_of_visit"]][["INPATIENT"]] <- c("hospitalisation",
                                                                          "hospitalisation_not_overnight")
meanings_of_this_study[["THL"]][["meaning_of_visit"]][["OUTPATIENT_NO_PC"]] <- "outpatient_specialist_visit"


meanings_of_this_study[["SAIL Databank"]][["meaning_of_event"]][["PC"]] <- "gpe"
meanings_of_this_study[["SAIL Databank"]][["meaning_of_event"]][["INPATIENT"]] <- c("pd", "hp", "CRITICAL_CARE_STAY", "sd")

meanings_of_this_study[["SAIL Databank"]][["meaning_of_visit"]] <- list()


meanings_of_this_study[["RDRU_FISABIO"]][["meaning_of_event"]][["INPATIENT"]] <- c("hospitalisation_primary",
                                                                                   "hospitalisation_secondary",
                                                                                   "death_registry")

meanings_of_this_study[["RDRU_FISABIO"]][["meaning_of_visit"]] <- list()


meanings_of_this_study[["FERR"]][["meaning_of_event"]][["OUTPATIENT_NO_PC"]] <- c("emergency_room_diagnosis",
                                                                                            "access_to_mental_health_service",
                                                                                            "access_to_mental_health_service_primary baby and teenager")
meanings_of_this_study[["FERR"]][["meaning_of_event"]][["LONGTERM"]] <- "exemption"

meanings_of_this_study[["FERR"]][["meaning_of_visit"]][["INPATIENT"]] <- c("hospitalisation",
                                                                                     "hospitalisation_not_overnight")
meanings_of_this_study[["FERR"]][["meaning_of_visit"]][["OUTPATIENT_NO_PC"]] <- c("outpatient_specialist_visit")


meanings_of_this_study[["EFEMERIS"]][["meaning_of_event"]][["INPATIENT"]] <- c("hospitalisation_primary",
                                                                               "hospitalisation_associated",
                                                                               "hospitalisation_linked")

meanings_of_this_study[["EFEMERIS"]][["meaning_of_visit"]] <- list()


meanings_of_this_study[["TEST"]][["meaning_of_event"]][["PC"]] <- c("primary_care_diagnosis", "birth_registry")
meanings_of_this_study[["TEST"]][["meaning_of_event"]][["OUTPATIENT_NO_PC"]] <- c("emergency_room_diagnosis", 
                                                                                  "emergency_room_presentation")
meanings_of_this_study[["TEST"]][["meaning_of_event"]][["LONGTERM"]] <- "exemption"

meanings_of_this_study[["TEST"]][["meaning_of_visit"]][["INPATIENT"]] <- c("hospitalisation",
                                                                                     "hospitalisation_not_overnight")
meanings_of_this_study[["TEST"]][["meaning_of_visit"]][["OUTPATIENT_NO_PC"]] <- c("oupatient_specialist_visit")

meanings_of_this_study_dap <- meanings_of_this_study[[thisdatasource]]
rm(meanings_of_this_study)




medicinal_products_date <- list()
medicinal_products_date[["SAIL Databank"]][["DMT-MS_SPEC"]][["start"]][["L04AA34"]] <- ymd(20130101)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_SPEC"]][["start"]][["L04AG06"]] <- ymd(20130101)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_SPEC"]][["start"]][["L04AC01"]] <- ymd(20160101)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_SPEC"]][["start"]][["L04AA08"]] <- ymd(20160101)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_SPEC"]][["end"]][["N07XX09"]] <- ymd(20170531)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_SPEC"]][["end"]][["L04AX07"]] <- ymd(20170531)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_UNSPEC"]][["start"]][["N07XX09"]] <- ymd(20170601)
medicinal_products_date[["SAIL Databank"]][["DMT-MS_UNSPEC"]][["start"]][["L04AX07"]] <- ymd(20170601)

medicinal_products_date[["UOSL"]][["DMT-MS_SPEC"]][["start"]][["L04AA34"]] <- ymd(20130101)
medicinal_products_date[["UOSL"]][["DMT-MS_SPEC"]][["start"]][["L04AG06"]] <- ymd(20130101)
medicinal_products_date[["UOSL"]][["DMT-MS_SPEC"]][["start"]][["L04AC01"]] <- ymd(20160101)
medicinal_products_date[["UOSL"]][["DMT-MS_SPEC"]][["start"]][["L04AA08"]] <- ymd(20160101)
medicinal_products_date[["UOSL"]][["DMT-MS_SPEC"]][["end"]][["N07XX09"]] <- ymd(20170531)
medicinal_products_date[["UOSL"]][["DMT-MS_SPEC"]][["end"]][["L04AX07"]] <- ymd(20170531)
medicinal_products_date[["UOSL"]][["DMT-MS_UNSPEC"]][["start"]][["N07XX09"]] <- ymd(20170601)
medicinal_products_date[["UOSL"]][["DMT-MS_UNSPEC"]][["start"]][["L04AX07"]] <- ymd(20170601)

medicinal_products_date[["THL"]][["DMT-MS_SPEC"]][["start"]][["L04AA34"]] <- ymd(20130101)
medicinal_products_date[["THL"]][["DMT-MS_SPEC"]][["start"]][["L04AG06"]] <- ymd(20130101)
medicinal_products_date[["THL"]][["DMT-MS_SPEC"]][["start"]][["L04AC01"]] <- ymd(20160101)
medicinal_products_date[["THL"]][["DMT-MS_SPEC"]][["start"]][["L04AA08"]] <- ymd(20160101)

medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_SPEC"]][["start"]][["L04AA34"]] <- ymd(20130101)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_SPEC"]][["start"]][["L04AG06"]] <- ymd(20130101)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_SPEC"]][["start"]][["L04AC01"]] <- ymd(20160101)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_SPEC"]][["start"]][["L04AA08"]] <- ymd(20160101)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_SPEC"]][["end"]][["N07XX09"]] <- ymd(20170731)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_SPEC"]][["end"]][["L04AX07"]] <- ymd(20170731)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_UNSPEC"]][["start"]][["N07XX09"]] <- ymd(20170801)
medicinal_products_date[["RDRU_FISABIO"]][["DMT-MS_UNSPEC"]][["start"]][["L04AX07"]] <- ymd(20170801)

medicinal_products_date[["FERR"]][["DMT-MS_SPEC"]][["start"]][["L04AA34"]] <- ymd(20130101)
medicinal_products_date[["FERR"]][["DMT-MS_SPEC"]][["start"]][["L04AG06"]] <- ymd(20130101)
medicinal_products_date[["FERR"]][["DMT-MS_SPEC"]][["start"]][["L04AC01"]] <- ymd(20160101)
medicinal_products_date[["FERR"]][["DMT-MS_SPEC"]][["start"]][["L04AA08"]] <- ymd(20160101)

medicinal_products_date[["EFEMERIS"]][["DMT-MS_SPEC"]][["start"]][["L04AA34"]] <- ymd(20130101)
medicinal_products_date[["EFEMERIS"]][["DMT-MS_SPEC"]][["start"]][["L04AG06"]] <- ymd(20130101)
medicinal_products_date[["EFEMERIS"]][["DMT-MS_SPEC"]][["start"]][["L04AC01"]] <- ymd(20160101)
medicinal_products_date[["EFEMERIS"]][["DMT-MS_SPEC"]][["start"]][["L04AA08"]] <- ymd(20160101)

chosen_MS_algorithm <- list()
chosen_MS_algorithm[["UOSL"]] <- "MS_1"
chosen_MS_algorithm[["THL"]] <- "MS_1"
chosen_MS_algorithm[["SAIL Databank"]] <- "MS_1"
chosen_MS_algorithm[["RDRU_FISABIO"]] <- "MS_1"
chosen_MS_algorithm[["FERR"]] <- "MS_1"
chosen_MS_algorithm[["EFEMERIS"]] <- "MS_1"
chosen_MS_algorithm[["TEST"]] <- "MS_1"













# # create two conditions on the meaning_of_event variable, associated to HOSP, to PC... as listed above
# 
# 
# condmeaning <- list()
# for (level1 in names(meanings_of_this_study)) {
#   for (meaning in meanings_of_this_study[[level1]]) {
#     if (length(condmeaning[[level1]])==0) {condmeaning[[level1]]=paste0("meaning_renamed == '",meanings_of_this_study[[level1]][[1]],"'")
#     }else{
#       condmeaning[[level1]]=paste0(condmeaning[[level1]], " | meaning_renamed == '",meaning,"'")
#     }
#   }
# }

# #----------------------------
# # SECONDARY COMPONENTS
# 
# # SECCOMPONENTS <- c("ArterialNoTP", "ArterialTP", "VTENoTP", "VTETP", "ArterialVTENoTP", "ArterialVTETP", "CVSTNoTP", "CVSTTP")
# SECCOMPONENTS <- NULL
# 
# concept_set_seccomp <- vector(mode="list")
# rule_seccomp <- vector(mode="list")
# distance_seccomp <- vector(mode="list")
# direction_seccomp <- vector(mode="list")
# 
# for (SECCOMP in SECCOMPONENTS) {
#   concept_set_seccomp[[SECCOMP]][['B']] <- c("TP_narrow","TP_possible")
#   distance_seccomp[[SECCOMP]] = '10'
#   direction_seccomp[[SECCOMP]] = "Either direction"
#   
#   selectionrule_direction_seccomp <- vector(mode="list")
#   selectionrule_direction_seccomp["A before B"] <- paste0("dateA <= dateB  & dateB <= dateA + ",distance_seccomp[[SECCOMP]])
#   selectionrule_direction_seccomp["B before A"] <- paste0("dateB <= dateA  & dateA <= dateB + ",distance_seccomp[[SECCOMP]])
#   selectionrule_direction_seccomp["Either direction"] <- paste0('((',selectionrule_direction_seccomp["A before B"],') | (',selectionrule_direction_seccomp["B before A"],'))')
#   
# }
# 
# 
# # ArterialNoTP
# concept_set_seccomp[["ArterialNoTP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow")
# rule_seccomp[["ArterialNoTP"]] <- "AND NOT"
# 
# # ArterialTP
# concept_set_seccomp[["ArterialTP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow")
# rule_seccomp[["ArterialTP"]] <- "AND"
# 
# # VTENoTP
# concept_set_seccomp[["VTENoTP"]][['A']] <- c("VTE_narrow","VTE_possible")
# rule_seccomp[["VTENoTP"]] <- "AND NOT"
# 
# # VTETP
# concept_set_seccomp[["VTETP"]][['A']] <- c("VTE_narrow","VTE_possible")
# rule_seccomp[["VTETP"]] <- "AND"
# 
# # ArterialVTENoTP
# concept_set_seccomp[["ArterialVTENoTP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow","VTE_narrow","VTE_possible")
# rule_seccomp[["ArterialVTENoTP"]] <- "AND NOT"
# 
# # ArterialVTETP
# concept_set_seccomp[["ArterialVTETP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow","VTE_narrow","VTE_possible")
# rule_seccomp[["ArterialVTETP"]] <- "AND"
# 
# # CVSTNoTP
# concept_set_seccomp[["CVSTNoTP"]][['A']] <- c("Sinusthrom_narrow","Sinusthrom_possible")
# rule_seccomp[["CVSTNoTP"]] <- "AND NOT"
# 
# # CVSTTP
# concept_set_seccomp[["CVSTTP"]][['A']] <- c("Sinusthrom_narrow","Sinusthrom_possible")
# rule_seccomp[["CVSTTP"]] <- "AND"
# 
# # XXXX
# concept_set_seccomp[["XXXX"]][['A']] <- c()
# rule_seccomp[["XXXX"]] <- ""

# # DATASOURCE-SPECIFIC ALGORITHMS
# 
# datasources_with_specific_algorithms <- c('GePaRD')
# 
# exclude_meanings_from_OUTCOME <- vector(mode="list")
# OUTCOME_events<-c('ABS','ADEM','GENCONV','MENINGOENC','ARD','ANAPHYL','MISCC')
# # HH HA NH NA NE NN NO NS NU A G V Z
# for (OUTCOME in OUTCOME_events){
#   exclude_meanings_from_OUTCOME[["GePaRD"]][[OUTCOME]]=c("HA", "NA", "NE", "NN", "NO", "NS", "NU", "A", "G", "V", "Z")
# }
# for (OUTCOME in c('ABS','ADEM','GENCONV','MENINGOENC','ARD','ANAPHYL','MISCC')) {
#         exclude_meanings_from_OUTCOME[["GePaRD"]][[OUTCOME]]=c(exclude_meanings_from_OUTCOME[["GePaRD"]][[OUTCOME]],"G")
# }
# 
# # create the rule that eliminates the meanings that are not appropriate for each outcome
# selection_meanings_from_OUTCOME <- vector(mode="list")
# if (thisdatasource %in% datasources_with_specific_algorithms){ 
#   for (OUTCOME in OUTCOME_events){
#     select <- "!is.na(person_id) "
#     for (meaningevent in exclude_meanings_from_OUTCOME[[thisdatasource]][[OUTCOME]]){
#       select <- paste0(select," & meaning_of_event!= '",meaningevent,"'")
#     }
#     selection_meanings_from_OUTCOME[[thisdatasource]][[OUTCOME]] <- select
#   }
# }

# concept sets specific for datasources

# if (thisdatasource == 'ARS'){
#   #concept_set_codes_our_study_pre[["COVID_narrow"]][["ICD9"]] <- c(concept_set_codes_our_study_pre[["COVID_narrow"]][["ICD9"]],'043','48041','51891','51971')
#   concept_set_codes_our_study_pre[["ARD_narrow"]][["ICD9"]] <- c(concept_set_codes_our_study_pre[["ARD_narrow"]][["ICD9"]],'5189')
# }

#-------------------------------------
# set concept sets

concept_set_codes_our_study <- concept_set_codes_our_study_pre
concept_set_codes_our_study_DU <- concept_set_codes_our_study_pre_DU
# concept_set_codes_our_study_excl <- concept_set_codes_our_study_pre_excl

# # augment ICPC codes
# for (outcome in OUTCOME_events){
#   outnarrow <- paste0(outcome,'_narrow')
#   outpossible <- paste0(outcome,'_possible')
#   if (length(concept_set_codes_our_study_pre[[outnarrow]][["ICPC"]]) == 0 & length(concept_set_codes_our_study_pre[[outnarrow]][["ICPC2P"]]) >0 ){
#   concept_set_codes_our_study[[outpossible]][["ICPC"]] <- unique(c(concept_set_codes_our_study_pre[[outpossible]][["ICPC"]],substr(concept_set_codes_our_study_pre[[outnarrow]][["ICPC2P"]],1,3)))
#   }
# }
# 
# for (conceptset in c(COV_conceptssets,SEVERCOVID_conceptsets)){
#   if (length(concept_set_codes_our_study_pre[[conceptset]][["ICPC2P"]]) >0 ){
#     concept_set_codes_our_study[[conceptset]][["ICPC"]] <- unique(c(concept_set_codes_our_study_pre[[conceptset]][["ICPC"]],substr(concept_set_codes_our_study_pre[[conceptset]][["ICPC2P"]],1,3)))
#   }
# }

#-------------------------------------
# fix for ICD10GM

for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD10GM"]] <- concept_set_codes_our_study[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD10CM
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD10CM"]] <- concept_set_codes_our_study[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD10ES
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD10ES"]] <- concept_set_codes_our_study[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD9CM
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD9"]] <- concept_set_codes_our_study[[conceptset]][["ICD9CM"]]
  }
}


for (conceptset in concept_sets_of_our_study_DU){
  if (concept_set_domains_DU[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study_DU[[conceptset]][["ICD10GM"]] <- concept_set_codes_our_study_DU[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD10CM
for (conceptset in concept_sets_of_our_study_DU){
  if (concept_set_domains_DU[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study_DU[[conceptset]][["ICD10CM"]] <- concept_set_codes_our_study_DU[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD10ES
for (conceptset in concept_sets_of_our_study_DU){
  if (concept_set_domains_DU[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study_DU[[conceptset]][["ICD10ES"]] <- concept_set_codes_our_study_DU[[conceptset]][["ICD10"]]
  }
}

#-------------------------------------
# fix for ICD9CM
for (conceptset in concept_sets_of_our_study_DU){
  if (concept_set_domains_DU[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study_DU[[conceptset]][["ICD9"]] <- concept_set_codes_our_study_DU[[conceptset]][["ICD9CM"]]
  }
}


save(concept_set_codes_our_study,file = paste0(direxp, "concept_set_codes_our_study.RData"))
save(concept_set_codes_our_study,file = paste0(direxpmask, "concept_set_codes_our_study.RData"))
save(concept_set_codes_our_study,file = paste0(direxp, "concept_set_codes_our_study_DU.RData"))
save(concept_set_codes_our_study,file = paste0(direxpmask, "concept_set_codes_our_study_DU.RData"))
# save(concept_set_codes_our_study_excl,file=paste0(direxp,"concept_set_codes_our_study_excl.RData"))
# save(concept_set_codes_our_study,file = paste0(dirsmallcountsremoved, "concept_set_codes_our_study.RData"))
# save(concept_set_codes_our_study_excl,file=paste0(dirsmallcountsremoved,"concept_set_codes_our_study_excl.RData"))

rm(conceptset, concept_set_codes_our_study_pre)
