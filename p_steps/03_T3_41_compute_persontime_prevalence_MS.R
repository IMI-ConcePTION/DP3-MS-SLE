# Create D4_prevalence_persontime_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"

for (outcome in OUTCOME_variables) {
  
  # Load algorithms
  algo_df <- smart_load(paste("D3_algorithms", outcome, sep = "_"), dirtemp, return = T)
  
  # Clean name of algorithm
  algo_df[, algorithm := gsub("_", "", algorithm)]
  
  # Add birth date from study_population
  smart_load("D3_study_population_SAP1", dirtemp)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                           "birth_date")]
  
  type_algo_vect <- paste0("MS", seq_len(5))
  
  rows_to_add <- data.table(person_id = D3_study_population_SAP1[1, person_id], date = ymd(99991231),
                            algorithm = type_algo_vect)
  algo_df <- rbind(algo_df, rows_to_add)
  
  persontime_prevalence <- CountPersonTime(
    Dataset_events = algo_df,
    Dataset = D3_study_population_SAP1,
    Person_id = "person_id",
    Start_study_time = as.character(recommended_start_date, "%Y%m%d"),
    End_study_time = as.character(study_end, "%Y%m%d"),
    Start_date = "cohort_entry_date",
    End_date = "cohort_exit_date",
    Birth_date = "birth_date",
    Name_event = "algorithm",
    Date_event = "date",
    Age_bands = ageband_definition,
    Increment = "year",
    Outcomes_nrec = type_algo_vect,
    Unit_of_age = "year",
    include_remaning_ages = F,
    Aggregate = T
  )
  
  smart_save(persontime_prevalence, diroutput, override_name = paste("D4_prevalence_persontime", outcome, sep = "_"))
}
