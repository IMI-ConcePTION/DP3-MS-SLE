# -----------------------------------------------------------------------
# RETRIEVE PROMPT DATASETS

# input: SURVEY_ID, SURVEY_OBSERVATIONS
# output: prompt datasets

print("RETRIEVE PROMPTSETS")

# RETRIEVE FROM SURVEY_ID ALL prompt datasets corresponding to "covid_registry" 

SURVEY_ID_COVID <- data.table(person_id = character(), survey_date  = character(), survey_meaning = character())

for (file in files_ConcePTION_CDM_tables[["SURVEY_ID"]]) {
  SURVEY_ID_COVID <-rbind(SURVEY_ID_COVID,fread(paste0(dirinput,file,".csv"), colClasses = list( character="person_id"))[survey_meaning =="covid_registry" | survey_meaning == "covid19_registry",], fill = TRUE)  
}
covid_registry <- SURVEY_ID_COVID[,date:=ymd(survey_date)]
covid_registry <- covid_registry[,-"survey_date"]



# RETRIEVE FROM VISIT_OCCURRENCE ALL prompt datasets corresponding to "hospitalisation_automatically_referred_to_PC" (data source PEDIANET)

hospitalisation_automatically_referred_to_PC <- data.table(person_id = character(), visit_start_date  = character(), meaning_of_visit = character())

for (file in files_ConcePTION_CDM_tables[["VISIT_OCCURRENCE"]]) {
  hospitalisation_automatically_referred_to_PC <- rbind(hospitalisation_automatically_referred_to_PC,fread(paste0(dirinput,file,".csv"), colClasses = list( character=c("person_id")))[meaning_of_visit == "hospitalisation_automatically_referred_to_PC",], fill = TRUE)  
}

hospitalisation_automatically_referred_to_PC <- hospitalisation_automatically_referred_to_PC[, date:=ymd(visit_start_date)]
hospitalisation_automatically_referred_to_PC <- hospitalisation_automatically_referred_to_PC[, -"visit_start_date"]

save(covid_registry,file = paste0(dirtemp,"covid_registry.RData"))
save(hospitalisation_automatically_referred_to_PC,file = paste0(dirtemp,"hospitalisation_automatically_referred_to_PC.RData"))
