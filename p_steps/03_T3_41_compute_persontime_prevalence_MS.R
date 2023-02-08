# Create D4_prevalence_persontime_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Load algorithms and study_population
  algo_df <- smart_load(paste("D3_algorithms", outcome, sep = "_"), dirtemp, return = T, extension = extension)
  smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, start_observation_period = entry_spell_category,
                                                           cohort_entry_date, cohort_exit_date, birth_date)]
  # TODO remove for release
  D3_study_population_SAP1 <- D3_study_population_SAP1[person_id == "ConCDM_SIM_200421_00628",
                                                       birth_date := ymd(20001107)]
  D3_study_population_SAP1 <- D3_study_population_SAP1[person_id == "ConCDM_SIM_200421_00628",
                                                       person_id := "ConCDM_SIM_200421_00629"]
  
  # Clean name of algorithm
  algo_df[, algorithm := gsub("_", "", algorithm)]
  algo_df <- algo_df[person_id == "ConCDM_SIM_200421_00629", date := ymd(20051107)]
  
  # Dcast to wide algorithm
  
  
  # TODO remove 
  # algo_df[, flag := 1]
  # algo_df <- data.table::dcast(algo_df, person_id + date ~ algorithm, fill = 0, drop = T, value.var = "flag")
  algo_df <- algo_df[algorithm == "MS1", ]
  
  # Add observation period when persons are not positive
  algo_df_negative <- copy(D3_study_population_SAP1)[, .(person_id, date = start_observation_period, algorithm = NA)]
  
  # Combine the datasets
  algo_df <- rbindlist(list(algo_df, algo_df_negative))
  
  # Set keys and then foverlaps to find the events inside each spell
  setkey(D3_study_population_SAP1, person_id, start_observation_period, cohort_exit_date)
  setkey(algo_df, person_id, date, date)
  algo_df_pop <- better_foverlaps(D3_study_population_SAP1, algo_df, by.x = key(D3_study_population_SAP1))
  temp <- copy(algo_df_pop)
  # Remove additional row when event happen at the start of period
  
  algo_df_pop[, cohort_entry_date := pmax(date, cohort_entry_date, na.rm = T)]
  algo_df_pop[, cohort_exit_date := pmin(cohort_exit_date, shift(cohort_entry_date, type = "lead") - 1, na.rm = T),
              by = "person_id"]
  
  algo_df_pop <- algo_df_pop[cohort_exit_date >= cohort_entry_date, ]
  
  algo_df_pop[, algorithm := fifelse(is.na(algorithm), shift(algorithm, fill = NA), algorithm), by = "person_id"]
  algo_df_pop[, date := NULL]
  
  algo_df_pop[, flag := 1]
  algo_df_pop <- data.table::dcast(algo_df_pop,
                                   person_id + cohort_entry_date + cohort_exit_date + birth_date ~ algorithm, fill = 0, drop = T, value.var = "flag")
  
  
  
  
  
  
  algo_df_pop[, (paste0(outcome, seq_len(5))) := nafill(.SD, fill = 0), .SDcols = paste0(outcome, seq_len(5))]
  
# temp <- data.table::dcast(algo_df_pop, person_id + date + ~ algorithm, drop = T)
# algo_df_pop <- merge(D3_study_population_SAP1, algo_df, by = "person_id", all.x = T)
# rows_to_add <- data.table(person_id = D3_study_population_SAP1[1, person_id], date = ymd(99991231),
#                           algorithm = paste0(outcome, seq_len(5)))
# algo_df <- rbind(algo_df, rows_to_add)
# 
# algo_df <- data.table::dcast(algo_df, person_id ~ algorithm, drop = T)
# 
#   # # Add missing algorithm columns in case there are no observation
#   cols_to_add <- setdiff(paste0(outcome, seq_len(5)), colnames(algo_df))
#   if (length(cols_to_add) > 0) algo_df[, (cols_to_add) := as.Date(NA_integer_)]
  
  persontime_prevalence <- CountPersonTime(
    Dataset = algo_df_pop,
    Person_id = "person_id",
    Start_study_time = as.character(recommended_start_date, "%Y%m%d"),
    End_study_time = as.character(study_end, "%Y%m%d"),
    Start_date = "cohort_entry_date",
    End_date = "cohort_exit_date",
    Birth_date = "birth_date",
    Strata = paste0(outcome, seq_len(5)),
    Age_bands = ageband_definition,
    Increment = "year",
    Unit_of_age = "year",
    include_remaning_ages = F,
    Aggregate = T
  )
  
  # Extract year from timeframe
  setnames(persontime_prevalence, "year", "timeframe")
  
  # Select algorithms columns
  algo_cols <- colnames(persontime_prevalence)[grepl("[MS|SLE][0-9]", colnames(persontime_prevalence))]
  
  # Melt algorithms columns
  persontime_prevalence <- melt(persontime_prevalence, measure = algo_cols,
                                value.name = "num_dem", variable.name = "algorithm",
                                variable.factor = F)
  
  # Recode num_dem 0/1 to denominator/numerator
  persontime_prevalence[, num_dem := fifelse(num_dem == 1, "numerator", "denominator")]
  
  # Add a column to define the type of prevalence
  persontime_prevalence[, type_of_prevalence := "persontime_prevalence"]
  
  # Clean and reorder the columns
  setnames(persontime_prevalence, "Ageband", "ageband")
  setcolorder(persontime_prevalence, c("type_of_prevalence", "timeframe", "ageband", "Persontime",
                                       "algorithm", "num_dem"))
  
  # Calculate the aggregated dataset by timeframe and add it to the original
  persontime_prevalence <- persontime_prevalence[, .(Persontime = sum(Persontime)),
                                                 by = c("type_of_prevalence", "timeframe", "ageband", "algorithm",
                                                        "num_dem")]
  
  # Numerator/denominato to wide
  persontime_prevalence <- data.table::dcast(persontime_prevalence, type_of_prevalence + timeframe + ageband + algorithm ~ num_dem,
                                             value.var = "Persontime", fill = 0)
  
  # Calculate the aggregated dataset by timeframe and add it to the original
  aggregated_timeframe <- copy(persontime_prevalence)[.(timeframe = as.character(seq(2005, 2019)),
                                                    to = c(rep(c("2005-2009", "2010-2014", "2015-2019"), each = 5))),
                                                  on = "timeframe", timeframe := i.to]
  aggregated_timeframe <- aggregated_timeframe[, lapply(.SD, sum), by = c("type_of_prevalence", "ageband", "timeframe",
                                                                          "algorithm", "Persontime"),
                                               .SDcols = c("numerator", "denominator")]
  persontime_prevalence <- rbindlist(list(persontime_prevalence, aggregated_timeframe), use.names = T)
  
  # Calculate the aggregated dataset by ageband
  aggregated_ageband <- copy(persontime_prevalence)[, lapply(.SD, sum), by = c("type_of_prevalence", "timeframe",
                                                                           "algorithm"),
                                                .SDcols = c("numerator", "denominator", "Persontime")]
  aggregated_ageband[, ageband := "all"]
  persontime_prevalence <- rbindlist(list(persontime_prevalence, aggregated_ageband), use.names = T)
  
  smart_save(persontime_prevalence, diroutput, override_name = paste("D4_prevalence_persontime", outcome, sep = "_"),
             extension = extension)
}
