# Create D4_prevalence_aggregated_all_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Load period prevalence
  D4_prevalence_period <- smart_load(paste("D4_prevalence_period", outcome, sep = "_"), diroutput, return = T, extension = extension)
  D4_prevalence_period <- D4_prevalence_period[grepl("-", timeframe) | ageband == "all", ]
  
  # Load persontime prevalence
  D4_prevalence_persontime <- smart_load(paste("D4_prevalence_persontime", outcome, sep = "_"), diroutput, return = T, extension = extension)
  D4_prevalence_persontime <- D4_prevalence_persontime[grepl("-", timeframe) | ageband == "all", ]
  
  # Load average point prevalence
  D4_prevalence_average_point <- smart_load(paste("D4_prevalence_average_point", outcome, sep = "_"), diroutput, return = T, extension = extension)
  
  D4_prevalence_aggregated_all <- rbindlist(list(D4_prevalence_period, D4_prevalence_persontime, D4_prevalence_average_point),
                                            use.names = T, fill = T)
  
  D4_prevalence_aggregated_all[, datasource := thisdatasource][, algorithm := gsub("^MS", "M", algorithm)]
  
  export_name <- paste("D4_prevalence_aggregated_all", outcome, sep = "_")
  smart_save(D4_prevalence_aggregated_all, diroutput,
             override_name = export_name,
             extension = "csv")
  
  names_vars_to_censor <- c("numerator", paste("numerator", sprintf("%02d", seq_len(60)), sep="_"))
  value_vars_to_censor <- rep(5, length(names_vars_to_censor))
  names(value_vars_to_censor) <- names_vars_to_censor
  
  # update_vector("datasets_to_censor", dirpargen, export_name)
  # update_vector("variables_to_censor", dirpargen, value_vars_to_censor)
}
