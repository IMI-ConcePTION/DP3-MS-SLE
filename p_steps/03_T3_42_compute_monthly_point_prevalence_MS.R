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
  period_prevalence[, timeframe := as.Date(substr(timeframe, 1, 10))]
  period_prevalence[, n_month := month(timeframe)]
  period_prevalence[, timeframe := as.character(year(timeframe))]
  
  # Melt algorithms columns
  period_prevalence_long <- data.table::melt(period_prevalence,
                                             id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                         "start_observation_period", "Ageband", "timeframe", "n_month"),
                                             measure.vars = paste0("prev_MS", seq_len(5)), variable.name = "algorithm",
                                             variable.factor = F, value.name = "numerator")
  
  # Fix algorithms names
  period_prevalence_long[, algorithm := gsub("prev_", "", algorithm)]
  
  # Create column denominator (all 1 since everyone is in_population for previous filter)
  period_prevalence_long[, denominator := 1]
  
  # Add a column to define the type of prevalence
  period_prevalence_long[, type_of_prevalence := "average_monthly_prevalence"]
  
  # Recode the timeframe and add it to the original
  recoded_timeframe <- copy(period_prevalence_long)[, n_month := fcase(
    timeframe == "2005" | timeframe == "2010" | timeframe == "2015", n_month + 12 * 0,
    timeframe == "2006" | timeframe == "2011" | timeframe == "2016", n_month + 12 * 1,
    timeframe == "2007" | timeframe == "2012" | timeframe == "2017", n_month + 12 * 2,
    timeframe == "2008" | timeframe == "2013" | timeframe == "2018", n_month + 12 * 3,
    timeframe == "2009" | timeframe == "2014" | timeframe == "2019", n_month + 12 * 4
  )]
  recoded_timeframe <- recoded_timeframe[.(timeframe = as.character(seq(2005, 2019)),
                                           to = c(rep(c("2005-2009", "2010-2014", "2015-2019"), each = 5))),
                                         on = "timeframe", timeframe := i.to]
  period_prevalence_long <- rbindlist(list(period_prevalence_long, recoded_timeframe), use.names = T)
  
  # Month to wide as columns
  period_prevalence <- data.table::dcast(period_prevalence_long,
                                         person_id + start_observation_period + cohort_entry_date + cohort_exit_date + type_of_prevalence + timeframe + Ageband + algorithm ~ n_month,
                                         drop = T, value.var = c("numerator", "denominator"))
  
  # Calculating additional variables
  period_prevalence[, start_timeframe := ymd(paste0(substr(timeframe, 1, 4),"0101"))]
  period_prevalence[, lookback_time_at_start_timeframe := correct_difftime(start_timeframe, cohort_entry_date)]
  period_prevalence[, start_timeframe := NULL]
  
  period_prevalence[, in_study_at_start_timeframe := fifelse(lookback_time_at_start_timeframe >= 0, 1, 0)]
  
  period_prevalence[, years_since_in_study := floor(lookback_time_at_start_timeframe / 365.25)]
  period_prevalence[, paste("in_study_since", seq_len(10), "years", sep = "_") := lapply(
    seq_len(10), function(x) fifelse(years_since_in_study >= x, 1, 0))]
  period_prevalence[, years_since_in_study := NULL]
  
  # Change column names
  setnames(period_prevalence, "Ageband", "ageband")
  
  smart_save(period_prevalence, diroutput, override_name = paste("D4_prevalence_average_point", outcome, sep = "_"))
}
