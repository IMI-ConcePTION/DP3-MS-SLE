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
  
  period_prevalence <- CountPrevalence(D3_study_population_SAP1, algo_df, c("person_id"),
                                       Start_date = "cohort_entry_date",
                                       End_date = "cohort_exit_date", Birth_date = "birth_date",
                                       Name_condition = "algorithm", Date_condition = "date",
                                       Type_prevalence = "period", Increment_period = "year",
                                       Start_study_time = recommended_start_date, End_study_time = study_end,
                                       Conditions = unique(algo_df[, algorithm]),
                                       include_remaning_ages = F,
                                       Age_bands = ageband_definition, Aggregate = F)
  
  period_prevalence_period <- CountPrevalence(D3_study_population_SAP1, algo_df, c("person_id"),
                                                  Start_date = "cohort_entry_date",
                                                  End_date = "cohort_exit_date", Birth_date = "birth_date",
                                                  Name_condition = "algorithm", Date_condition = "date",
                                                  Type_prevalence = "period",
                                                  Periods_of_time = list(
                                                    list("20050101", "20091231"),
                                                    list("20100101", "20141231"),
                                                    list("20150101", "20191231"),
                                                    list("20050101", "20191231")
                                                  ),
                                                  Start_study_time = recommended_start_date, End_study_time = study_end,
                                                  Conditions = unique(algo_df[, algorithm]),
                                                  include_remaning_ages = F,
                                                  Age_bands = ageband_definition, Aggregate = F)
  
  # Extract year from timeframe
  period_prevalence[, timeframe := year(as.Date(substr(timeframe, 1, 10)))]
  period_prevalence_period[, timeframe := paste(year(as.Date(substr(timeframe, 1, 10))),
                                                year(as.Date(substr(timeframe, 12, 21))), sep = "-")]
  
  # Combine datasets and remove unnecessary columns
  period_prevalence <- rbindlist(list(period_prevalence, period_prevalence_period))
  period_prevalence[, c("cohort_entry_date", "cohort_exit_date") := NULL]
  
  # Select algorithms columns
  algo_cols <- colnames(period_prevalence)[grepl("[MS|SLE][0-9]", colnames(period_prevalence))]
  
  # Aggregate to get only one row per person/timeframe and then add it to the original dataset
  period_prevalence_period_no_ageband <- copy(period_prevalence)[, lapply(.SD, max),
                                                                        .SDcols = c(algo_cols, "in_population"),
                                                                        by = c("timeframe", "person_id")]
  period_prevalence_period_no_ageband[, Ageband := "all"]
  period_prevalence <- rbindlist(list(period_prevalence, period_prevalence_period_no_ageband), use.names = T)
  
  # Aggregate to get only one row per person/timeframe 
  period_prevalence <- period_prevalence[, lapply(.SD, max),
                                         .SDcols = c(algo_cols, "in_population"),
                                         by = c("timeframe", "Ageband", "person_id")]
  
  # Original aggregate inside countprevalence
  period_prevalence <- period_prevalence[, lapply(.SD, sum),
                                         .SDcols = c(algo_cols, "in_population"),
                                         by = c("timeframe", "Ageband")]
  
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
  
  # Remove unnened values
  period_prevalence <- period_prevalence[grepl("-", timeframe) | ageband == "all", ]
  
  smart_save(period_prevalence, diroutput, override_name = paste("D4_prevalence_period", outcome, sep = "_"), extension = extension)
}
