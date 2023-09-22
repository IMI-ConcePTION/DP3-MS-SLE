# Create D4_prevalence_average_point_MS
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
  
  # D3_study_population_SAP1[person_id == "ConCDM_SIM_200421_00628", person_id := "ConCDM_SIM_200421_00629"]
  # D3_study_population_SAP1[person_id == "ConCDM_SIM_200421_00629", birth_date := ymd(19840201)]
  # 
  # algo_df[person_id == "ConCDM_SIM_200421_00629" & algorithm == "MS1", date := ymd(20050201)]
  
  sequence_start_end_year <- seq(max(year(recommended_start_date), year(study_start)), year(study_end))
  sequence_start_end_year <- split(sequence_start_end_year, ceiling(seq_along(sequence_start_end_year) / 5))
  sequence_start_year <- lapply(sequence_start_end_year, function(x) ymd(paste0(first(x), "0101")))
  sequence_end_year <- lapply(sequence_start_end_year, function(x) ymd(paste0(last(x), "1231")))
  
  period_prevalence_list <- lapply(seq_len(length(sequence_start_year)), function(x) {
    CountPrevalence(D3_study_population_SAP1,
                    algo_df, c("person_id"),
                    Start_date = "cohort_entry_date",
                    End_date = "cohort_exit_date", Birth_date = "birth_date",
                    Name_condition = "algorithm", Date_condition = "date",
                    Type_prevalence = "point", Increment = "month",
                    Start_study_time = sequence_start_year[[x]],
                    End_study_time = sequence_end_year[[x]],
                    Conditions = unique(algo_df[, algorithm]),
                    include_remaning_ages = F,
                    Age_bands = ageband_definition_level_1,
                    Aggregate = T,
                    drop_not_in_population = T)
  })
  rm(D3_study_population_SAP1)
  
  period_prevalence <- rbindlist(period_prevalence_list)
  
  # Extract year/month from timeframe
  period_prevalence[, timeframe := as.Date(timeframe)]
  period_prevalence[, n_month := month(timeframe)]
  period_prevalence[, timeframe := as.character(year(timeframe))]
  
  # Select algorithms columns
  algo_cols <- colnames(period_prevalence)[grepl("[MS|SLE][0-9]", colnames(period_prevalence))]
  
  # # Aggregate to get only one row per person/timeframe and then add it to the original dataset
  # period_prevalence_period_no_ageband <- copy(period_prevalence)[, lapply(.SD, sum),
  #                                                                .SDcols = c(algo_cols, "in_population"),
  #                                                                by = c("timeframe", "n_month")]
  # period_prevalence_period_no_ageband[, Ageband := "all"]
  # period_prevalence <- rbindlist(list(period_prevalence, period_prevalence_period_no_ageband), use.names = T)
  # rm(period_prevalence_period_no_ageband)
  
  # Melt algorithms columns
  period_prevalence <- data.table::melt(period_prevalence,
                                        id.vars = c("timeframe", "Ageband", "in_population", "n_month"),
                                        measure.vars = algo_cols, variable.name = "algorithm",
                                        variable.factor = F, value.name = "numerator")
  
  # Fix algorithms names
  period_prevalence[, algorithm := gsub("prev_", "", algorithm)]
  
  # Create column denominator (all 1 since everyone is in_population for previous filter)
  setnames(period_prevalence, "in_population", "denominator")
  
  
  base_agebands <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  
  df_recode_Ageband_2 <- data.table::data.table(Ageband_2 = c("15-24", "15-24", "25-29", "30-34", "35-39", "40-49", "40-49"),
                                                base_agebands = base_agebands)
  df_recode_Ageband_3 <- data.table::data.table(Ageband_3 = c("15-24", "15-24", "25-34", "25-34", "35-49", "35-49", "35-49"),
                                                base_agebands = base_agebands)
  
  setnames(period_prevalence, "Ageband", "Ageband_1")
  period_prevalence[df_recode_Ageband_2, on = .(Ageband_1 = base_agebands), Ageband_2 := i.Ageband_2]
  period_prevalence[df_recode_Ageband_3, on = .(Ageband_1 = base_agebands), Ageband_3 := i.Ageband_3]
  
  assigned_levels <- vector(mode = "list")
  assigned_levels[["Ageband"]] <- c("Ageband_1", "Ageband_2", "Ageband_3")
  assigned_levels[["timeframe"]] <- c("timeframe")
  assigned_levels[["algorithm"]] <- c("algorithm")
  assigned_levels[["n_month"]] <- c("n_month")
  
  period_prevalence <- Cube(input = period_prevalence,
                            dimensions = c("Ageband","timeframe","algorithm", "n_month"),
                            levels = assigned_levels,
                            computetotal = c("Ageband"),
                            measures = c("numerator", "denominator")
  )
  
  period_prevalence[get("Ageband_LabelValue") == "AllAgeband", Ageband_LabelValue := "all"]
  
  setnames(period_prevalence, paste(c("numerator", "denominator"), "sum", sep = "_"), c("numerator", "denominator"))
  setnames(period_prevalence, paste(c("timeframe", "Ageband", "algorithm", "n_month"), "LabelValue", sep = "_"),
           c("timeframe", "Ageband", "algorithm", "n_month"))
  
  period_prevalence[, paste(c("algorithm", "timeframe", "n_month"), "LevelOrder", sep = "_") := NULL]
  period_prevalence[, timeframe := as.character(timeframe)]
  
  # Recode the timeframe and add it to the original
  recoded_timeframe <- copy(period_prevalence)[, n_month := fcase(
    timeframe == "2005" | timeframe == "2010" | timeframe == "2015", n_month + 12 * 0,
    timeframe == "2006" | timeframe == "2011" | timeframe == "2016", n_month + 12 * 1,
    timeframe == "2007" | timeframe == "2012" | timeframe == "2017", n_month + 12 * 2,
    timeframe == "2008" | timeframe == "2013" | timeframe == "2018", n_month + 12 * 3,
    timeframe == "2009" | timeframe == "2014" | timeframe == "2019", n_month + 12 * 4
  )]
  recoded_timeframe <- recoded_timeframe[.(timeframe = as.character(seq(2005, 2019)),
                                           to = c(rep(c("2005-2009", "2010-2014", "2015-2019"), each = 5))),
                                         on = "timeframe", timeframe := i.to]
  recoded_timeframe[, timeframe_LevelOrder := 3]
  period_prevalence <- rbindlist(list(period_prevalence[, timeframe_LevelOrder := 1], recoded_timeframe), use.names = T)
  
  # Recode the timeframe and add it to the original
  period_prevalence[, n_month := sprintf("%02d", n_month)]
  
  # Month to wide as columns
  period_prevalence <- data.table::dcast(period_prevalence,
                                         timeframe + timeframe_LevelOrder + Ageband + Ageband_LevelOrder + algorithm ~ n_month, fill = 0,
                                         drop = T, value.var = c("numerator", "denominator"))
  
  # # Change column names
  # setnames(period_prevalence, "Ageband", "ageband")
  # setnames(period_prevalence, "Ageband_LevelOrder", "ageband_LevelOrder")
  
  # Add a column to define the type of prevalence
  period_prevalence[, type_of_prevalence := "average_monthly_prevalence"]
  
  # Find if a level contains at least a value to censor
  summary_threshold <- 5
  tmp <- copy(period_prevalence)
  
  numerator_to_censor <- paste("numerator", sprintf("%02d", seq_len(60)), sep="_")
  
  for(measure in c(numerator_to_censor)) {
    tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
  }
  
  
  
  tmp_1 <- copy(tmp)[, lapply(.SD, all), by = c("timeframe", "Ageband_LevelOrder", "timeframe_LevelOrder", "algorithm"),
             .SDcols = c(numerator_to_censor)]
  
  tmp <- tmp[, lapply(.SD, all), by = c("Ageband_LevelOrder", "timeframe_LevelOrder", "algorithm"),
             .SDcols = c(numerator_to_censor)]
  
  tmp[, numerator := as.logical(do.call(pmin, .SD)), .SDcols = numerator_to_censor]
  tmp[, c(numerator_to_censor) := NULL]
  
  tmp_1[, numerator := as.logical(do.call(pmin, .SD)), .SDcols = numerator_to_censor]
  tmp_1[, c(numerator_to_censor) := NULL]
  
  
  setorder(tmp, "algorithm")
  
  if (enable_summary_levels) {
    smart_save(tmp, direxpcheck, override_name = paste("D4_prevalence_average_point", outcome, "summary_levels", sep = "_"), extension = "csv")
    smart_save(tmp_1, direxpcheck, override_name = paste("D4_prevalence_average_point", outcome, "summary_levels_yearly", sep = "_"), extension = "csv")
  }
  
  smart_save(period_prevalence, diroutput, override_name = paste("D4_prevalence_average_point", outcome, sep = "_"), extension = extension, save_copy = "csv")
  
  # update_vector("datasets_to_censor", dirpargen, paste("D4_prevalence_average_point", outcome, sep = "_"))
  # update_vector("variables_to_censor", dirpargen, c("numerator" = 5))
}
