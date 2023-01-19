# Create D4_prevalence_period_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"

for (outcome in OUTCOME_variables) {
  
  # Load components
  components <- smart_load(paste("D3_components", outcome, "SAP1", sep = "_"), dirtemp, return = T)
  
  # Select algorithms columns
  algo_cols <- colnames(components)[grepl("_[0-9]_date", colnames(components))]
  algorithms_dates <- components[, c("person_id", "cohort_entry_date", "cohort_exit_date", algo_cols), with = F]
  
  algorithms_long <- data.table::melt(algorithms_dates, id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date"),
                                       measure.vars = algo_cols, variable.name = "algorithm", variable.factor = F,
                                       value.name = "date_algo")
  
  algorithms_long[, algorithm := gsub("_(?<=_)(?!.*_).*", "", algorithm, perl = T)]
  algorithms_long[, algorithm := gsub("_", "", algorithm)]
  
  # load D3_persons and merged it to the components to retrieve the birth date
  smart_load("D3_PERSONS", dirtemp)
  D3_PERSONS <- D3_PERSONS[, c("person_id", "birth_date")]
  algorithms_long <- merge(algorithms_long, D3_PERSONS, all.x = T, by = "person_id")
  
  period_prevalence <- CountPrevalence(algorithms_long[, .(person_id, cohort_entry_date, cohort_exit_date, birth_date)],
                                       algorithms_long, c("person_id"),
                                       Start_date = "cohort_entry_date",
                                       End_date = "cohort_exit_date", Birth_date = "birth_date",
                                       Name_condition = "algorithm", Date_condition = "date_algo",
                                       Type_prevalence = "period", Increment_period = "year",
                                       Start_study_time = recommended_start_date, End_study_time = study_end,
                                       Conditions = unique(algorithms_long[, algorithm]),
                                       include_remaning_ages = F,
                                       Age_bands = ageband_definition)
  
  smart_save(period_prevalence, diroutput, override_name = paste("D4_prevalence_period", outcome, sep = "_"))
}
