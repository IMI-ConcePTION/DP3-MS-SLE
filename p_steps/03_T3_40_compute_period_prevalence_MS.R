# Create D4_prevalence_period_MS
# input: D3_study_population_SAP1, conceptset
# period_prevalence: D3_clean_spells

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
  
  level_2_periods <- list(list("20050101", "20071231"),
                          list("20080101", "20101231"),
                          list("20110101", "20131231"),
                          list("20140101", "20161231"),
                          list("20170101", "20191231"))
  
  level_3_periods <- list(list("20050101", "20091231"),
                          list("20100101", "20141231"),
                          list("20150101", "20191231"))
  
  level_4_periods <- list(list("20050101", "20191231"))
  
  period_prevalence_period <- CountPrevalence(D3_study_population_SAP1, algo_df, c("person_id"),
                                                  Start_date = "cohort_entry_date",
                                                  End_date = "cohort_exit_date", Birth_date = "birth_date",
                                                  Name_condition = "algorithm", Date_condition = "date",
                                                  Type_prevalence = "period",
                                                  Periods_of_time = c(level_2_periods, level_3_periods, level_4_periods),
                                                  Start_study_time = recommended_start_date, End_study_time = study_end,
                                                  Conditions = unique(algo_df[, algorithm]),
                                                  include_remaning_ages = F,
                                                  Age_bands = ageband_definition, Aggregate = F)
  
  period_prevalence <- period_prevalence[!is.na(Ageband), ]
  period_prevalence_period <- period_prevalence_period[!is.na(Ageband), ]
  
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
  setnames(period_prevalence, "Ageband", "Ageband_1")
  
  base_agebands <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  
  df_recode_Ageband_2 <- data.table::data.table(Ageband_2 = c("15-24", "15-24", "25-29", "30-34", "35-39", "40-49", "40-49"),
                                                base_agebands = base_agebands)
  df_recode_Ageband_3 <- data.table::data.table(Ageband_3 = c("15-24", "15-24", "25-34", "25-34", "35-49", "35-49", "35-49"),
                                                base_agebands = base_agebands)
  df_recode_Ageband_4 <- data.table::data.table(Ageband_4 = c("all", "all", "all", "all", "all", "all", "all"),
                                                base_agebands = base_agebands)
  period_prevalence[df_recode_Ageband_2, on = .(Ageband_1 = base_agebands), Ageband_2 := i.Ageband_2]
  period_prevalence[df_recode_Ageband_3, on = .(Ageband_1 = base_agebands), Ageband_3 := i.Ageband_3]
  period_prevalence[df_recode_Ageband_4, on = .(Ageband_1 = base_agebands), Ageband_4 := i.Ageband_4]
  
  assigned_levels <- vector(mode="list")
  assigned_levels[["Ageband"]] <- c("Ageband_1", "Ageband_2", "Ageband_3", "Ageband_4")
  assigned_levels[["person_id"]] <- c("person_id")
  assigned_levels[["timeframe"]] <- c("timeframe")
  
  assigned_statistics <- vector(mode="list")
  for (col_name in c(algo_cols, "in_population")) {
    assigned_statistics[[col_name]] <- c("max")
  }
  
  suppressWarnings(period_prevalence <- Cube(input = period_prevalence,
                 dimensions = c("Ageband","person_id","timeframe"),
                 levels = assigned_levels,
                 measures = c(algo_cols, "in_population"),
                 statistics = assigned_statistics,
  ))
  
  setnames(period_prevalence, paste(c(algo_cols, "in_population"), "max", sep = "_"), c(algo_cols, "in_population"))
  setnames(period_prevalence, paste(c("timeframe", "Ageband", "person_id"), "label_value", sep = "-"),
           c("timeframe", "Ageband", "person_id"))
  
  period_prevalence[, paste(c("timeframe", "person_id"), "level_order", sep = "-") := NULL]
  
  # Aggregate to get only one row per person/timeframe 
  period_prevalence <- period_prevalence[, lapply(.SD, max),
                                         .SDcols = c(algo_cols, "in_population"),
                                         by = c("timeframe", "Ageband", "person_id", "Ageband-level_order")]
  
  # Original aggregate inside countprevalence
  period_prevalence <- period_prevalence[, lapply(.SD, sum),
                                         .SDcols = c(algo_cols, "in_population"),
                                         by = c("timeframe", "Ageband", "Ageband-level_order")]
  
  # Melt algorithms columns
  period_prevalence <- data.table::melt(period_prevalence, id.vars = c("timeframe", "Ageband", "Ageband-level_order", "in_population"),
                                        measure.vars = algo_cols, variable.name = "algorithm", variable.factor = F,
                                        value.name = "numerator")
  
  # Clean algorithms values from prev_
  period_prevalence[, algorithm := gsub("prev_", "", algorithm)]
  
  # Add a column to define the type of prevalence
  period_prevalence[, type_of_prevalence := "period_prevalence"]
  
  # Clean and reorder the columns
  period_prevalence[, timeframe := as.character(timeframe)]
  setnames(period_prevalence, c("Ageband", "in_population"), c("ageband", "denominator"))
  setcolorder(period_prevalence, c("type_of_prevalence", "timeframe", "ageband", "numerator", "denominator",
                                   "algorithm"))
  
  df_recode_year_level_1 <- data.table::data.table(timeframe = as.character(seq(year(recommended_start_date), year(study_end))),
                                                   "timeframe-level_order" = 1)
  df_recode_year_level_2 <- data.table::data.table(timeframe = c("2005-2007", "2008-2010", "2011-2013", "2014-2016", "2017-2019"),
                                                   "timeframe-level_order" = 2)
  df_recode_year_level_3 <- data.table::data.table(timeframe = c("2005-2009", "2010-2014", "2015-2019"),
                                                   "timeframe-level_order" = 3)
  df_recode_year_level_4 <- data.table::data.table(timeframe = c("2005-2019"),
                                                   "timeframe-level_order" = 4)
  df_recode_year_level <- data.table::rbindlist(list(df_recode_year_level_1, df_recode_year_level_2,
                                                     df_recode_year_level_3, df_recode_year_level_4))
  
  period_prevalence <- merge(period_prevalence, df_recode_year_level, all.x = T, by = "timeframe")
  
  # Remove unnened values
  period_prevalence <- period_prevalence[grepl("-", timeframe) | ageband == "all", ]
  
  smart_save(period_prevalence, dirperiod_prevalence, override_name = paste("D4_prevalence_period", outcome, sep = "_"), extension = extension)
}
