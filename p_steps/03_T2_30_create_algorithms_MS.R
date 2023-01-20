# Create_algorithms_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"

# s = "all" is to be excluded for now
s <- c(1, 2, 3, 5, 8)

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
  
  # Select algorithms columns and spells
  algorithms_dates_spells <- components[, c("person_id", "cohort_entry_date", "cohort_exit_date", "entry_spell_category", algo_cols), with = F]
  
  # Keep only women who exit the study at 31st december 2019
  algorithms_dates_spells <- algorithms_dates_spells[cohort_exit_date == ymd(20191231), ]
  
  # Calculate length of spell and then remove entry_spell_category
  algorithms_dates_spells[, length_spell := age_fast(entry_spell_category, cohort_exit_date)]
  algorithms_dates_spells[, entry_spell_category := NULL]
  
  # Keep only women with at least 5 years of lookback
  algorithms_dates_spells <- algorithms_dates_spells[length_spell >= 5, ]
  
  # Keep only persons with at least one algorithm positive
  algorithms_dates_spells_short <- algorithms_dates_spells[rowSums(!is.na(algorithms_dates_spells[, ..algo_cols])) > 0, ]
  
  # Wide to long dataset
  algorithms_dates_spells_short <- data.table::melt(algorithms_dates_spells_short,
                                              id.vars = c("person_id", "cohort_entry_date",
                                                          "cohort_exit_date", "length_spell"),
                                              measure.vars = algo_cols, value.name = "date_algo",
                                              variable.name = "algorithm", variable.factor = F)
  
  # Remove row without algorithm positivity, transform algorithm to integer
  algorithms_dates_spells_short <- algorithms_dates_spells_short[!is.na(date_algo), ]
  algorithms_dates_spells_short[, algorithm := as.integer(gsub("[a-zA-Z_]", "", algorithm, perl = T))]
  
  # Calculate years from positivity to end of study
  algorithms_dates_spells_short[, flag_algo := age_fast(date_algo, cohort_exit_date)]
  
  # Find if spells are longer than 5 and 10 years and then remove length_spell
  algorithms_dates_spells_short[, at_least_5_years_of_lookback_at_20191231 := as.integer(length_spell >= 5)]
  algorithms_dates_spells_short[, at_least_10_years_of_lookback_at_20191231 := as.integer(length_spell >= 10)]
  algorithms_dates_spells_short[, length_spell := NULL]
  
  # # Calculate distance from positivity of algorithm to end of study
  # algorithms_dates_spells[, (algo_cols) := lapply(algo_cols, function(x) age_fast(get(x), cohort_exit_date))]
  
  # Calculate distance from positivity of algorithm to end of study
  algorithms_dates_spells_short[, (paste("s", s, sep = "_")) := lapply(s, function(x) as.integer(flag_algo >= x))]
  algorithms_dates_spells_short[, s_all := as.integer(1)]
  algorithms_dates_spells_short[, (paste("t", s, sep = "_")) := lapply(s, function(x) fifelse(flag_algo >= x, date_algo, NA_Date_))]
  algorithms_dates_spells_short[, t_all := date_algo]
  
  # Remove impossible values
  algorithms_dates_spells_short[at_least_10_years_of_lookback_at_20191231 == 0, s_8 := NA_integer_]
  algorithms_dates_spells_short[at_least_10_years_of_lookback_at_20191231 == 0, t_8 := NA_Date_]
  
  
  algorithms_dates_spells_short <- data.table::dcast(algorithms_dates_spells_short,
                                               person_id + cohort_entry_date + cohort_exit_date + at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ algorithm,
                                               drop = T, value.var = c(paste("s", s, sep = "_"), "s_all",
                                                                       paste("t", s, sep = "_"), "t_all"))
  
  cols_to_change <- colnames(algorithms_dates_spells_short)[grepl("^s_", colnames(algorithms_dates_spells_short))]
  new_col_names <- sapply(strsplit(cols_to_change, "_"), function(x) paste0("M", paste(x[3], x[2], sep = "_")))
  setnames(algorithms_dates_spells_short, cols_to_change, new_col_names)
  
  cols_to_change <- colnames(algorithms_dates_spells_short)[grepl("^t_", colnames(algorithms_dates_spells_short))]
  new_col_names <- sapply(strsplit(cols_to_change, "_"), function(x) paste0("M", paste(x[3], x[2], "date", sep = "_")))
  setnames(algorithms_dates_spells_short, cols_to_change, new_col_names)
  
  smart_save(algorithms_dates_spells_short,
             dirtemp, override_name = paste("D3_algorithms_multiple_lookback", outcome, sep = "_"))
}





