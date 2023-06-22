# Create D4_prevalence_persontime_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Define missing value, present value and algo_cols
  miss_value <- 0
  pres_value <- 1
  algo_cols <- paste0(outcome, seq_len(5))
  
  # Load algorithms and study_population
  algo_df <- smart_load(paste("D3_algorithms", outcome, sep = "_"), dirtemp, return = T, extension = extension)
  smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, start_observation_period = entry_spell_category,
                                                           cohort_entry_date, cohort_exit_date, birth_date)]
  
  # Clean name of algorithm
  algo_df[, algorithm := gsub("_", "", algorithm)]
  
  # Dcast to wide algorithm
  algo_df[, flag := pres_value]
  algo_df <- data.table::dcast(algo_df, person_id + date ~ algorithm, fill = miss_value, drop = T, value.var = "flag")
  
  # Find if there columns/algorithms not present
  missing_cols <- setdiff(algo_cols, colnames(algo_df))
  if (length(missing_cols) > 0) algo_df[, (missing_cols) := miss_value]
  
  # Add observation period when persons are not positive
  algo_df_negative <- copy(D3_study_population_SAP1)[, .(person_id, date = start_observation_period)]
  algo_df_negative[, (algo_cols) := miss_value]
  
  # Combine the datasets
  algo_df <- rbindlist(list(algo_df, algo_df_negative))
  
  # Set keys and then foverlaps to find the events inside each spell
  setkey(D3_study_population_SAP1, person_id, start_observation_period, cohort_exit_date)
  setkey(algo_df, person_id, date, date)
  algo_df_pop <- better_foverlaps(D3_study_population_SAP1, algo_df, by.x = key(D3_study_population_SAP1))
  temp <- copy(algo_df_pop)
  # Remove additional row when event happen at the start of period
  
  setorder(algo_df_pop, person_id, date)
  algo_df_pop[, cohort_entry_date := pmax(date, cohort_entry_date, na.rm = T)]
  algo_df_pop[, cohort_exit_date := pmin(cohort_exit_date, shift(cohort_entry_date, type = "lead") - 1, na.rm = T),
              by = "person_id"]
  
  algo_df_pop <- algo_df_pop[cohort_exit_date >= cohort_entry_date, ]
  
  algo_df_pop[, (algo_cols) := lapply(.SD, cummax), by = "person_id", .SDcols = algo_cols]
  algo_df_pop[, date := NULL]
  
  persontime_prevalence <- CountPersonTime(
    Dataset = algo_df_pop,
    Person_id = "person_id",
    Start_study_time = recommended_start_date,
    End_study_time = study_end,
    Start_date = "cohort_entry_date",
    End_date = "cohort_exit_date",
    Birth_date = "birth_date",
    Strata = algo_cols,
    Age_bands = ageband_definition_level_1,
    Increment = "year",
    Unit_of_age = "year",
    include_remaning_ages = F,
    Aggregate = T
  )
  
  # Extract year from timeframe
  setnames(persontime_prevalence, "year", "timeframe")
  
  # Melt algorithms columns
  persontime_prevalence <- melt(persontime_prevalence, measure = algo_cols,
                                value.name = "num_dem", variable.name = "algorithm",
                                variable.factor = F)
  
  # Calculate numerator and denominator
  persontime_prevalence <- persontime_prevalence[, .(numerator = sum(Persontime * num_dem),
                                                     denominator = sum(Persontime)), by = c("timeframe", "Ageband", "algorithm")]
  
  base_agebands <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  
  df_recode_Ageband_2 <- data.table::data.table(Ageband_2 = c("15-24", "15-24", "25-29", "30-34", "35-39", "40-49", "40-49"),
                                                base_agebands = base_agebands)
  df_recode_Ageband_3 <- data.table::data.table(Ageband_3 = c("15-24", "15-24", "25-34", "25-34", "35-49", "35-49", "35-49"),
                                                base_agebands = base_agebands)
  
  persontime_prevalence[, timeframe := as.integer(timeframe)]
  
  setnames(persontime_prevalence, "Ageband", "Ageband_1")
  persontime_prevalence[df_recode_Ageband_2, on = .(Ageband_1 = base_agebands), Ageband_2 := i.Ageband_2]
  persontime_prevalence[df_recode_Ageband_3, on = .(Ageband_1 = base_agebands), Ageband_3 := i.Ageband_3]
  
  assigned_levels <- vector(mode = "list")
  assigned_levels[["Ageband"]] <- c("Ageband_1", "Ageband_2", "Ageband_3")
  assigned_levels[["timeframe"]] <- c("timeframe", "timeframe_2")
  assigned_levels[["algorithm"]] <- c("algorithm")
  
  assigned_rule <- vector(mode = "list")
  assigned_rule[["timeframe"]][["timeframe_2"]] <- list("split_in_bands","timeframe", c(2005, 2010, 2015, 2020))
  
  persontime_prevalence <- Cube(input = persontime_prevalence,
                  dimensions = c("Ageband","timeframe","algorithm"),
                  levels = assigned_levels,
                  computetotal = c("timeframe", "Ageband"),
                  measures = c("numerator", "denominator"),
                  rule_from_numeric_to_categorical = assigned_rule
  )
  
  persontime_prevalence[get("timeframe_LabelValue") == "Alltimeframe", timeframe_LabelValue := "2005-2019"]
  persontime_prevalence[get("Ageband_LabelValue") == "AllAgeband", Ageband_LabelValue := "all"]
  
  setnames(persontime_prevalence, paste(c("numerator", "denominator"), "sum", sep = "_"), c("numerator", "denominator"))
  setnames(persontime_prevalence, paste(c("timeframe", "Ageband", "algorithm"), "LabelValue", sep = "_"),
           c("timeframe", "Ageband", "algorithm"))
  
  persontime_prevalence[, paste(c("algorithm"), "LevelOrder", sep = "_") := NULL]
  
  # Add a column to define the type of prevalence
  persontime_prevalence[, type_of_prevalence := "persontime_prevalence"]
  
  # Clean and reorder the columns
  setnames(persontime_prevalence, "Ageband", "ageband")
  setcolorder(persontime_prevalence, c("type_of_prevalence", "timeframe", "ageband", "algorithm"))
  
  # Find if a level contains at least a value to censor
  summary_threshold <- 5 * 365
  tmp <- copy(persontime_prevalence)
  
  for(measure in c("numerator", "denominator")) {
    tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
  }
  
  tmp <- tmp[, lapply(.SD, all), by = c("Ageband_LevelOrder", "timeframe_LevelOrder", "algorithm"), .SDcols = c("numerator", "denominator")]
  
  setorder(tmp, "algorithm")
  
  smart_save(tmp, direxp, override_name = paste("D4_prevalence_persontime", outcome, "summary_levels", sep = "_"), extension = extension)
  
  smart_save(persontime_prevalence, diroutput, override_name = paste("D4_prevalence_persontime", outcome, sep = "_"),
             extension = extension)
}
