# Create_algorithms_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

#TODO remove when activating SLE
OUTCOME_variables <- "MS"

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
}
