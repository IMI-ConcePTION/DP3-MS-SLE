#-----------------------------------------
# ASSIGN NAMES AND DOMAINS OF CONCEPTSETS
#
# in this file the names of the conceptsets are listed. 
# as a default they are divided in 6 lists, based on the domain (diagnosis, medicines etc). there are two lists for diagnostic conseptsets: one is the list of those with tag 'narrow' and 'possible', and the other is the list of 'plain' conceptsets
# according to the needs of the project the lists can be further subdivided, or some of them may be missing
# those lists are used to feed CreateConceptsetDatasets in step 01_1, and then may be further used in the next steps
# at the bottom of this file there is a call to a second file that assigns codes to each conceptsets. such file may either be assigned directly or be retrieved from a csv
# the codes assigned in this file are preliminary (concept_set_codes_our_study_pre) and may be further refined, possibly in  a datasource-specific manner, in the parameter step 06_algorithms 

source(paste0(thisdir,"/p_parameters/archive_parameters/parameters_raw.R"))
#----------------------------------------- 

concept_set_domains<- vector(mode="list")
for (concept in names(concept_set_codes_our_study_pre)) {
  if (all(names(concept_set_codes_our_study_pre[[concept]]) == "ATC")) {
    concept_set_domains[[concept]] <- "Medicines"
  } else {
    concept_set_domains[[concept]] <- "Diagnosis"
  }
}

concept_sets_of_our_study <- names(concept_set_codes_our_study_pre)

rm(concept)
