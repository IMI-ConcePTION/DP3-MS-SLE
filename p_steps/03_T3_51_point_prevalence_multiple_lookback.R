# Point_prevalence_multiple_lookback
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"

# s = "all" is to be excluded for now
s <- c(1, 2, 3, 5, 8)

for (outcome in OUTCOME_variables) {
  
  # Load components
  algo_look <- smart_load(paste("D3_algorithms_multiple_lookback", outcome, sep = "_"), dirtemp, return = T)
  
  # Recode all 1 to the length of lokkback
  algo_look[at_least_5_years_of_lookback_at_20191231 == 1, at_least_5_years_of_lookback_at_20191231 := 5]
  algo_look[at_least_10_years_of_lookback_at_20191231 == 1, at_least_10_years_of_lookback_at_20191231 := 10]
  
  # Select algorithms columns
  algo_cols <- colnames(algo_look)[grepl("M[0-9]_([0-9]|all)_date", colnames(algo_look))]
  
  # Lookback to a single column
  algo_look <- data.table::melt(algo_look, id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date", algo_cols),
                                measure.vars = c("at_least_5_years_of_lookback_at_20191231",
                                                 "at_least_10_years_of_lookback_at_20191231"),
                                variable.name = "original_var", variable.factor = F,
                                value.name = "years_of_lookback_at_20191231")
  algo_look[, original_var := NULL]
  
  # Algorithm to a single column
  algo_look <- data.table::melt(algo_look, id.vars = c("person_id", "years_of_lookback_at_20191231"),
                                measure.vars = algo_cols,
                                variable.name = "algorithm", variable.factor = F,
                                value.name = "date_algo")
  
  # Add birth date from study_population
  smart_load("D3_study_population_SAP1", dirtemp)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, start_observation_period = entry_spell_category,
                                                           cohort_entry_date, cohort_exit_date, birth_date)]
  
  algo_cols <- gsub("_date", "", algo_cols)
  algo_look[, algorithm := gsub("_date", "", algorithm)]
  
  period_prevalence <- CountPrevalence(D3_study_population_SAP1,
                                       algo_look, "person_id",
                                       Start_date = "cohort_entry_date",
                                       End_date = "cohort_exit_date", Birth_date = "birth_date",
                                       Name_condition = "algorithm", Date_condition = "date_algo",
                                       Type_prevalence = "period", Increment_period = "year",
                                       Start_study_time = recommended_start_date, End_study_time = study_end,
                                       Conditions = algo_cols,
                                       include_remaning_ages = F,
                                       Age_bands = ageband_definition)
}





