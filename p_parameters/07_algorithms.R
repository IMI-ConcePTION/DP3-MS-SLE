# we need to create two groups of meanings: one referring to hospitals HOSP (excluding emergency care) and one referring to primary care PC

meanings_of_this_study<-vector(mode="list")
meanings_of_this_study[["HOSP"]]=c("hospitalisation_primary","hospitalisation_secondary","hospital_diagnosis","hopitalisation_diagnosis_unspecified","episode_primary_diagnosis","episode_secondary_diagnosis","diagnosis_procedure","hospitalisation_associated","hospitalisation_linked","HH","NH","hospitalisation_ICU_primary","hospitalisation_ICU_secondary","hospitalisation_ICU_unspecified")
meanings_of_this_study[["PC"]]=c("primary_care_event","primary_care_diagnosis","primary_care_events_BIFAP","primary_care_antecedents_BIFAP","primary_care_condicionants_BIFAP")

# create two conditions on the meaning_of_event variable, associated to HOSP and to PC as listed above

condmeaning <- list()
for (level1 in c("HOSP","PC")) {
  for (meaning in meanings_of_this_study[[level1]]) {
    if (length(condmeaning[[level1]])==0) {condmeaning[[level1]]=paste0("meaning_renamed == '",meanings_of_this_study[[level1]][[1]],"'")
    }else{
      condmeaning[[level1]]=paste0(condmeaning[[level1]], " | meaning_renamed == '",meaning,"'")
    }
  }
}

#----------------------------
# SECONDARY COMPONENTS

# SECCOMPONENTS <- c("ArterialNoTP", "ArterialTP", "VTENoTP", "VTETP", "ArterialVTENoTP", "ArterialVTETP", "CVSTNoTP", "CVSTTP")
SECCOMPONENTS <- NULL

concept_set_seccomp <- vector(mode="list")
rule_seccomp <- vector(mode="list")
distance_seccomp <- vector(mode="list")
direction_seccomp <- vector(mode="list")

for (SECCOMP in SECCOMPONENTS) {
  concept_set_seccomp[[SECCOMP]][['B']] <- c("TP_narrow","TP_possible")
  distance_seccomp[[SECCOMP]] = '10'
  direction_seccomp[[SECCOMP]] = "Either direction"
  
  selectionrule_direction_seccomp <- vector(mode="list")
  selectionrule_direction_seccomp["A before B"] <- paste0("dateA <= dateB  & dateB <= dateA + ",distance_seccomp[[SECCOMP]])
  selectionrule_direction_seccomp["B before A"] <- paste0("dateB <= dateA  & dateA <= dateB + ",distance_seccomp[[SECCOMP]])
  selectionrule_direction_seccomp["Either direction"] <- paste0('((',selectionrule_direction_seccomp["A before B"],') | (',selectionrule_direction_seccomp["B before A"],'))')
  
}


# ArterialNoTP
concept_set_seccomp[["ArterialNoTP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow")
rule_seccomp[["ArterialNoTP"]] <- "AND NOT"

# ArterialTP
concept_set_seccomp[["ArterialTP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow")
rule_seccomp[["ArterialTP"]] <- "AND"

# VTENoTP
concept_set_seccomp[["VTENoTP"]][['A']] <- c("VTE_narrow","VTE_possible")
rule_seccomp[["VTENoTP"]] <- "AND NOT"

# VTETP
concept_set_seccomp[["VTETP"]][['A']] <- c("VTE_narrow","VTE_possible")
rule_seccomp[["VTETP"]] <- "AND"

# ArterialVTENoTP
concept_set_seccomp[["ArterialVTENoTP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow","VTE_narrow","VTE_possible")
rule_seccomp[["ArterialVTENoTP"]] <- "AND NOT"

# ArterialVTETP
concept_set_seccomp[["ArterialVTETP"]][['A']] <- c("CAD_narrow","Ischstroke_narrow","VTE_narrow","VTE_possible")
rule_seccomp[["ArterialVTETP"]] <- "AND"

# CVSTNoTP
concept_set_seccomp[["CVSTNoTP"]][['A']] <- c("Sinusthrom_narrow","Sinusthrom_possible")
rule_seccomp[["CVSTNoTP"]] <- "AND NOT"

# CVSTTP
concept_set_seccomp[["CVSTTP"]][['A']] <- c("Sinusthrom_narrow","Sinusthrom_possible")
rule_seccomp[["CVSTTP"]] <- "AND"

# XXXX
concept_set_seccomp[["XXXX"]][['A']] <- c()
rule_seccomp[["XXXX"]] <- ""

# DATASOURCE-SPECIFIC ALGORITHMS

datasources_with_specific_algorithms <- c('GePaRD')

exclude_meanings_from_OUTCOME <- vector(mode="list")
OUTCOME_events<-c('ABS','ADEM','GENCONV','MENINGOENC','ARD','ANAPHYL','MISCC')
# HH HA NH NA NE NN NO NS NU A G V Z
for (OUTCOME in OUTCOME_events){
  exclude_meanings_from_OUTCOME[["GePaRD"]][[OUTCOME]]=c("HA", "NA", "NE", "NN", "NO", "NS", "NU", "A", "G", "V", "Z")
}
for (OUTCOME in c('ABS','ADEM','GENCONV','MENINGOENC','ARD','ANAPHYL','MISCC')) {
        exclude_meanings_from_OUTCOME[["GePaRD"]][[OUTCOME]]=c(exclude_meanings_from_OUTCOME[["GePaRD"]][[OUTCOME]],"G")
}

# create the rule that eliminates the meanings that are not appropriate for each outcome
selection_meanings_from_OUTCOME <- vector(mode="list")
if (thisdatasource %in% datasources_with_specific_algorithms){ 
  for (OUTCOME in OUTCOME_events){
    select <- "!is.na(person_id) "
    for (meaningevent in exclude_meanings_from_OUTCOME[[thisdatasource]][[OUTCOME]]){
      select <- paste0(select," & meaning_of_event!= '",meaningevent,"'")
    }
    selection_meanings_from_OUTCOME[[thisdatasource]][[OUTCOME]] <- select
  }
}

# concept sets specific for datasources

if (thisdatasource == 'ARS'){
  #concept_set_codes_our_study_pre[["COVID_narrow"]][["ICD9"]] <- c(concept_set_codes_our_study_pre[["COVID_narrow"]][["ICD9"]],'043','48041','51891','51971')
  concept_set_codes_our_study_pre[["ARD_narrow"]][["ICD9"]] <- c(concept_set_codes_our_study_pre[["ARD_narrow"]][["ICD9"]],'5189')
}

#-------------------------------------
# set concept sets

concept_set_codes_our_study <- concept_set_codes_our_study_pre
concept_set_codes_our_study_excl <- concept_set_codes_our_study_pre_excl

# augment ICPC codes
for (outcome in OUTCOME_events){
  outnarrow <- paste0(outcome,'_narrow')
  outpossible <- paste0(outcome,'_possible')
  if (length(concept_set_codes_our_study_pre[[outnarrow]][["ICPC"]]) == 0 & length(concept_set_codes_our_study_pre[[outnarrow]][["ICPC2P"]]) >0 ){
  concept_set_codes_our_study[[outpossible]][["ICPC"]] <- unique(c(concept_set_codes_our_study_pre[[outpossible]][["ICPC"]],substr(concept_set_codes_our_study_pre[[outnarrow]][["ICPC2P"]],1,3)))
  }
}

for (conceptset in c(COV_conceptssets,SEVERCOVID_conceptsets)){
  if (length(concept_set_codes_our_study_pre[[conceptset]][["ICPC2P"]]) >0 ){
    concept_set_codes_our_study[[conceptset]][["ICPC"]] <- unique(c(concept_set_codes_our_study_pre[[conceptset]][["ICPC"]],substr(concept_set_codes_our_study_pre[[conceptset]][["ICPC2P"]],1,3)))
  }
}

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
# fix for ICD9CM
for (conceptset in concept_sets_of_our_study){
  if (concept_set_domains[[conceptset]] == "Diagnosis"){
    concept_set_codes_our_study[[conceptset]][["ICD9CM"]] <- concept_set_codes_our_study[[conceptset]][["ICD9"]]
  }
}


save(concept_set_codes_our_study,file=paste0(direxp,"concept_set_codes_our_study.RData"))
save(concept_set_codes_our_study_excl,file=paste0(direxp,"concept_set_codes_our_study_excl.RData"))
save(concept_set_codes_our_study,file=paste0(dirsmallcountsremoved,"concept_set_codes_our_study.RData"))
save(concept_set_codes_our_study_excl,file=paste0(dirsmallcountsremoved,"concept_set_codes_our_study_excl.RData"))

if (this_datasource_has_subpopulations == TRUE){ 
  for (subpop in subpopulations[[thisdatasource]]){
    save(concept_set_codes_our_study,file=paste0(direxpsubpop[[subpop]],"concept_set_codes_our_study.RData"))
    save(concept_set_codes_our_study_excl,file=paste0(direxpsubpop[[subpop]],"concept_set_codes_our_study_excl.RData"))
    save(concept_set_codes_our_study_excl,file=paste0(dirsmallcountsremovedsubpop[[subpop]],"concept_set_codes_our_study_excl.RData"))
    
  }
}
