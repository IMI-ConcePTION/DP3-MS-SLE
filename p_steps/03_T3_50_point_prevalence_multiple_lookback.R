##%######################################################%##
#                                                          #
####            COMPUTE LOOKBACK PREVALENCE             ####
#                                                          #
##%######################################################%##

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  if (thisdatasource %in% datasources_only_preg) {
    print(paste("D4_prevalence_aggregated_multiple_lookback", outcome, " can't be calculated in datasource with only pregnancies"))
    next
  }
  
  # Load components
  algo_look <- smart_load(paste("D3_algorithms_multiple_lookback", outcome, sep = "_"), dirtemp, return = T, extension = extension)
  
  # Recode all 1 to the length of lookback
  algo_look[at_least_5_years_of_lookback_at_20191231 == 1, at_least_5_years_of_lookback_at_20191231 := 5]
  algo_look[at_least_10_years_of_lookback_at_20191231 == 1, at_least_10_years_of_lookback_at_20191231 := 10]
  
  # Select algorithms columns
  algo_cols <- colnames(algo_look)[grepl("M[0-9]_([0-9]|all)", colnames(algo_look))]
  
  # Lookback to a single column
  algo_look <- data.table::melt(algo_look, id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date", algo_cols),
                                measure.vars = c("at_least_5_years_of_lookback_at_20191231",
                                                 "at_least_10_years_of_lookback_at_20191231"),
                                variable.name = "original_var", variable.factor = F,
                                value.name = "years_of_lookback_at_20191231")
  algo_look[, original_var := NULL]
  
  # Remove all at_least_10_years_of_lookback_at_20191231 == 0 that where there before
  algo_look <- algo_look[years_of_lookback_at_20191231 != 0, ]
  
  # Algorithm to a single column
  algo_look <- data.table::melt(algo_look, id.vars = c("person_id", "years_of_lookback_at_20191231"),
                                measure.vars = algo_cols,
                                variable.name = "algorithm", variable.factor = F,
                                value.name = "flag_algo")
  
  # Remove impossible values
  algo_look <- algo_look[!(years_of_lookback_at_20191231 == 5 & grepl("M[0-9]_[5-9]", algorithm)), ]
  
  algo_look <- algo_look[, .(numerator = sum(flag_algo, na.rm = T), denominator = .N),
                    by = c("algorithm", "years_of_lookback_at_20191231")]
  
  # # Extract years of lookback from algorithm
  # algo_look[, algo_lookback := sapply(strsplit(algorithm, "_"), function(x) x[2])]
  # 
  # # Recode all
  # algo_look <- algo_look[algo_lookback == "all", algo_lookback := "99"][, algo_lookback := as.integer(algo_lookback)]
  # 
  # # Keep correct denominator
  # algo_look <- algo_look[(algo_lookback <= 5 & years_of_lookback_at_20191231 == 5) |
  #                          (algo_lookback > 5 & years_of_lookback_at_20191231 == 10), ]
  # algo_look[, algo_lookback := NULL]
  
  # algo_look <- MergeFilterAndCollapse(list(algo_look),
  #                                condition = "!is.na(person_id)",
  #                                strata = c("algorithm", "years_of_lookback_at_20191231"),
  #                                summarystat = list(c("sum", "flag_algo", "numerator"),
  #                                                   c("count", "person_id", "denominator")))

  algo_look <- algo_look[, datasource := thisdatasource]
  
  tmp <- copy(algo_look)
  summary_threshold <- 5
  
  for(measure in c("numerator")) {
    tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
  }
  
  tmp <- tmp[, lapply(.SD, all), by = "algorithm", .SDcols = "numerator"]
  
  setorder(tmp, "algorithm")
  
  if (enable_summary_levels) {
    smart_save(tmp, direxpcheck, override_name = paste("D4_prevalence_aggregated_multiple_lookback", outcome, "summary_levels", sep = "_"), extension = "csv")
  }
  
  export_name <- paste("D4_prevalence_aggregated_multiple_lookback", outcome, sep = "_")
  smart_save(algo_look, diroutput, override_name = export_name, extension = extension, save_copy = "csv")
  
  # update_vector("datasets_to_censor", dirpargen, export_name)
  # update_vector("variables_to_censor", dirpargen, c("N" = 5))
}





