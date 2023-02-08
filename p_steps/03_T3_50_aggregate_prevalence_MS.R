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
  D4_prevalence_persontime[, Persontime := NULL]
  D4_prevalence_persontime <- D4_prevalence_persontime[grepl("-", timeframe) | ageband == "all", ]
  
  # Load average point prevalence
  D4_prevalence_average_point <- smart_load(paste("D4_prevalence_average_point", outcome, sep = "_"), diroutput, return = T, extension = extension)
  D4_prevalence_average_point <- D4_prevalence_average_point[, lapply(.SD, sum, na.rm = T),
                                                             by = c("type_of_prevalence", "ageband",
                                                                    "timeframe", "algorithm"),
                                                             .SDcols = c(paste("numerator", seq_len(60), sep="_"),
                                                                         paste("denominator", seq_len(60), sep="_"))]
  
  D4_prevalence_aggregated_all <- rbindlist(list(D4_prevalence_period, D4_prevalence_persontime, D4_prevalence_average_point),
                                            use.names = T, fill = T)
  
  D4_prevalence_aggregated_all[, datasource := thisdatasource][, algorithm := gsub("^MS", "M", algorithm)]
  
  smart_save(D4_prevalence_aggregated_all, direxp,
             override_name = paste("D4_prevalence_aggregated_all", outcome, sep = "_"),
             extension = "csv")
}
