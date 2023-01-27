# Create_algorithms_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"

# s = "all" is to be excluded for now
s <- c(1, 2, 3, 5, 8, "all")

for (outcome in OUTCOME_variables) {
  
  # Load components
  components <- smart_load(paste("D3_components", outcome, "SAP1", sep = "_"), dirtemp, return = T)
  
  # Select algorithms columns
  algo_cols <- colnames(components)[grepl("_[0-9]_date", colnames(components))]
  algorithms_dates <- components[, c("person_id", algo_cols), with = F]
  
  # Keep only persons with at least one algorithm positive
  algorithms_dates <- algorithms_dates[rowSums(!is.na(algorithms_dates[, ..algo_cols])) > 0, ]
  
  D3_algorithms_MS <- data.table::melt(algorithms_dates,
                                       id.vars = c("person_id"),
                                       measure.vars = algo_cols, variable.name = "algorithm",
                                       variable.factor = F, value.name = "date")
  
  D3_algorithms_MS <- D3_algorithms_MS[, algorithm := gsub("_(?<=_)(?!.*_).*", "", algorithm, perl = T)]
  setcolorder(D3_algorithms_MS, c("person_id", "date", "algorithm"))
  
  # Remove row without a date
  D3_algorithms_MS <- D3_algorithms_MS[!is.na(date), ]
  
  smart_save(D3_algorithms_MS, dirtemp)
  
  if (thisdatasource %in% c("EFEMERIS", "THL")) {
    print(paste("D3_algorithms_multiple_lookback_", outcome, " can't be calculated in datasource EFEMERIS and THL"))
    next
  }
  
  # Load components
  components_lookback <- smart_load(paste("D3_components_multiple_lookback", outcome, sep = "_"), dirtemp, return = T)
  
  # remove at_least_n_years_of_lookback_at_20191231
  components_lookback[, c("at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231") := NULL]
  
  # Select algorithms columns
  algo_cols <- colnames(components_lookback)[grepl("M[0-9]_([0-9]|all)_date", colnames(components_lookback))]
  
  # Algorithm to a single column
  components_lookback <- data.table::melt(components_lookback, id.vars = c("person_id"),
                                measure.vars = algo_cols,
                                variable.name = "algorithm", variable.factor = F,
                                value.name = "date_algo")
  
  # Clean the algorithm columns and add the years off lookback
  components_lookback[, algorithm := gsub("_date", "", algorithm)]
  algo_cols <- gsub("_date", "", algo_cols)
  
  # Get study_population as dataset cohort
  smart_load("D3_study_population_SAP1", dirtemp)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date,
                                                           cohort_exit_date)]
  
  # Keep only women who exit the study at 31st December 2019 and have any events
  D3_study_population_SAP1 <- D3_study_population_SAP1[cohort_exit_date == study_end, ]
  
  algorithm_lookback <- CountPrevalence(D3_study_population_SAP1, components_lookback, "person_id",
                          Start_date = "cohort_entry_date", End_date = "cohort_exit_date", 
                          Name_condition = "algorithm", Date_condition = "date_algo",
                          Strata = "entry_spell_category",
                          Type_prevalence = "period", Periods_of_time = list(c("cohort_entry_date", "cohort_exit_date")),
                          Start_study_time = recommended_start_date, End_study_time = study_end,
                          Conditions = unique(components_lookback[, algorithm]),
                          Aggregate = F)
  algorithm_lookback[, c("timeframe", "in_population") := NULL]
  
  # TODO: remove when countprevalence is fixed
  # Select algorithms columns that need to be recodede
  cols_to_add <- colnames(algorithm_lookback)[grepl("^M[0-9]_([0-9]|all)", colnames(algorithm_lookback))]
  algorithm_lookback[, (cols_to_add) := lapply(.SD, as.integer), .SDcols = cols_to_add]
  
  cols_to_change <- colnames(algorithm_lookback)[grepl("^prev_", colnames(algorithm_lookback))]
  new_col_names <- sapply(strsplit(cols_to_change, "_"), function(x) paste(x[2], x[3], sep = "_"))
  setnames(algorithm_lookback, cols_to_change, new_col_names)
  
  # Calculate length of spell and then remove entry_spell_category
  algorithm_lookback[, length_spell := age_fast(entry_spell_category, cohort_exit_date)]
  algorithm_lookback[, entry_spell_category := NULL]
  
  # Find if spells are longer than 5 and 10 years and then remove length_spell
  algorithm_lookback[, at_least_5_years_of_lookback_at_20191231 := as.integer(length_spell >= 5)]
  algorithm_lookback[, at_least_10_years_of_lookback_at_20191231 := as.integer(length_spell >= 10)]
  algorithm_lookback[, length_spell := NULL]
  
  smart_save(algorithm_lookback, dirtemp, override_name = paste("D3_algorithms_multiple_lookback", outcome, sep = "_"))
}





