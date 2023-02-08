# Create D4_prevalence_period_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Load algorithms
  algo_df <- smart_load(paste("D3_algorithms", outcome, sep = "_"), dirtemp, return = T, extension = extension)
  
  # Clean name of algorithm
  algo_df[, algorithm := gsub("_", "", algorithm)]
  
  # Add birth date from study_population
  smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, start_observation_period = entry_spell_category,
                                                           cohort_entry_date, cohort_exit_date, birth_date)]
  
  period_prevalence <- CountPrevalence(D3_study_population_SAP1,
                                       algo_df, c("person_id"),
                                       Start_date = "cohort_entry_date",
                                       End_date = "cohort_exit_date", Birth_date = "birth_date",
                                       Name_condition = "algorithm", Date_condition = "date",
                                       Type_prevalence = "period", Increment_period = "year",
                                       Start_study_time = recommended_start_date, End_study_time = study_end,
                                       Conditions = unique(algo_df[, algorithm]),
                                       include_remaning_ages = F,
                                       Age_bands = ageband_definition)
  
  # Extract year from timeframe
  period_prevalence[, timeframe := as.Date(substr(timeframe, 1, 10))]
  period_prevalence[, timeframe := year(timeframe)]
  
  # Select algorithms columns
  algo_cols <- colnames(period_prevalence)[grepl("[MS|SLE][0-9]", colnames(period_prevalence))]
  
  # Melt algorithms columns
  period_prevalence <- data.table::melt(period_prevalence, id.vars = c("timeframe", "Ageband", "in_population"),
                                        measure.vars = algo_cols, variable.name = "algorithm", variable.factor = F,
                                        value.name = "numerator")
  
  # Clean algorithms values from prev_
  period_prevalence[, algorithm := gsub("prev_", "", algorithm)]
  
  # Add a column to define the type of prevalence
  period_prevalence[, type_of_prevalence := "period_prevalence"]
  
  # Clean and reorder the columns
  period_prevalence <- period_prevalence[!is.na(Ageband), ]
  period_prevalence[, timeframe := as.character(timeframe)]
  setnames(period_prevalence, c("Ageband", "in_population"), c("ageband", "denominator"))
  setcolorder(period_prevalence, c("type_of_prevalence", "timeframe", "ageband", "numerator", "denominator",
                                   "algorithm"))
  
  # Calculate the aggregated dataset by timeframe and add it to the original
  aggregated_timeframe <- copy(period_prevalence)[.(timeframe = as.character(seq(2005, 2019)),
                                                    to = c(rep(c("2005-2009", "2010-2014", "2015-2019"), each = 5))),
                                                  on = "timeframe", timeframe := i.to]
  aggregated_timeframe <- aggregated_timeframe[, lapply(.SD, sum), by = c("type_of_prevalence", "ageband", "timeframe",
                                                                          "algorithm"),
                                               .SDcols = c("numerator", "denominator")]
  period_prevalence <- rbindlist(list(period_prevalence, aggregated_timeframe), use.names = T)
  
  # Calculate the aggregated dataset by ageband
  aggregated_ageband <- copy(period_prevalence)[, lapply(.SD, sum), by = c("type_of_prevalence", "timeframe",
                                                                             "algorithm"),
                                               .SDcols = c("numerator", "denominator")]
  aggregated_ageband[, ageband := "all"]
  period_prevalence <- rbindlist(list(period_prevalence, aggregated_ageband), use.names = T)
  
  
  smart_save(period_prevalence, diroutput, override_name = paste("D4_prevalence_period", outcome, sep = "_"), extension = extension)
}
