# Create D4_prevalence_average_point_MS
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
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, start_observation_period = entry_spell_category,
                                                           cohort_entry_date, cohort_exit_date, birth_date)]
  
  type_algo_vect <- paste0("MS", seq_len(5))
  
  period_prevalence <- CountPrevalence(D3_study_population_SAP1,
                                       algo_df, c("person_id"),
                                       Start_date = "cohort_entry_date",
                                       End_date = "cohort_exit_date", Birth_date = "birth_date",
                                       Name_condition = "algorithm", Date_condition = "date",
                                       Type_prevalence = "period", Increment_period = "month",
                                       Start_study_time = recommended_start_date, End_study_time = study_end,
                                       Conditions = type_algo_vect, Strata = "start_observation_period",
                                       include_remaning_ages = F,
                                       Age_bands = ageband_definition,
                                       Aggregate = F)
  
  # Remove when person is not in population
  period_prevalence <- period_prevalence[in_population != 0, ][, in_population := NULL]
  
  # Extract year/month from timeframe
  period_prevalence[, start_timeframe := as.Date(substr(timeframe, 1, 10))]
  period_prevalence[, timeframe := NULL]
  
  # Melt algorithms columns
  period_prevalence_long <- data.table::melt(period_prevalence,
                                             id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                         "start_observation_period", "Ageband", "start_timeframe"),
                                             measure.vars = paste0("prev_MS", seq_len(5)), variable.name = "algorithm",
                                             variable.factor = F, value.name = "numerator")
  
  # Fix algorithms names
  period_prevalence_long[, algorithm := gsub("prev_", "", algorithm)]
  
  # Create column denominator (all 1 since everyone is in population for previous filter)
  period_prevalence_long[, in_population := 1]
  
  smart_save(period_prevalence, diroutput, override_name = paste("D4_prevalence_average_point", outcome, sep = "_"))
}
