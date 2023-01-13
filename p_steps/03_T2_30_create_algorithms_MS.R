# Create_algorithms_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"
s <- c(999, 8, 5, 3, 2, 1)

for (outcome in OUTCOME_variables) {
  
  # Load components
  components <- smart_load(paste("D3_components", outcome, "SAP1", sep = "_"), dirtemp, return = T)
  
  # Select algorithms columns
  algo_cols <- colnames(components)[grepl("_[0-9]_date", colnames(components))]
  algorithms_dates <- components[, c("person_id", algo_cols), with = F]
  
  # Keep only persons with at least one algorithm positive
  algorithms_dates <- algorithms_dates[rowSums(!is.na(algorithms_dates[, ..algo_cols])) > 0, ]
  
  D3_algorithms_MS <- data.table::melt(algorithms_dates, id.vars = "person_id", measure.vars = algo_cols,
                                       variable.name = "algorithm", variable.factor = F, value.name = "date")
  
  D3_algorithms_MS <- D3_algorithms_MS[, algorithm := gsub("_(?<=_)(?!.*_).*", "", algorithm, perl = T)]
  setcolorder(D3_algorithms_MS, c("person_id", "date", "algorithm"))
  
  smart_save(D3_algorithms_MS, dirtemp)
  
  # Select algorithms columns and spells
  algorithms_dates_spells <- components[, c("person_id", "cohort_entry_date", "cohort_exit_date", "entry_spell_category", algo_cols), with = F]

  # Keep only persons with at least one algorithm positive
  algorithms_dates_spells <- algorithms_dates_spells[rowSums(!is.na(algorithms_dates_spells[, ..algo_cols])) > 0, ]
  
  # Keep only women who exit the study at 31st december 2019
  algorithms_dates_spells <- algorithms_dates_spells[cohort_exit_date == ymd(20191231), ]
  
  # Calculate length of spell
  algorithms_dates_spells[, length_spell := age_fast(entry_spell_category, cohort_exit_date)]
  
  # Find if spells are longer than 5 and 10 years
  algorithms_dates_spells[, at_least_5_years_of_lookback_at_20191231 := as.integer(length_spell >= 5)]
  algorithms_dates_spells[, at_least_10_years_of_lookback_at_20191231 := as.integer(length_spell >= 10)]
}









