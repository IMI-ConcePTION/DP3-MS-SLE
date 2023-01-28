# Create D4_prevalence_aggregated_multiple_lookback
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

for (outcome in OUTCOME_variables) {
  
  if (thisdatasource %in% c("EFEMERIS", "THL")) {
    print(paste("D4_prevalence_aggregated_multiple_lookback", outcome, " can't be calculated in datasource EFEMERIS and THL"))
    next
  }
  
  # Load components
  algo_look <- smart_load(paste("D3_algorithms_multiple_lookback", outcome, sep = "_"), dirtemp, return = T)
  
  # Recode all 1 to the length of lokkback
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
  
  algo_look <- MergeFilterAndCollapse(list(algo_look),
                                 condition = "!is.na(person_id)",
                                 strata = c("algorithm", "years_of_lookback_at_20191231"),
                                 summarystat = list(c("sum", "flag_algo", "numerator"),
                                                    c("count", "person_id", "denominator")))
  
  smart_save(algo_look, direxp, override_name = paste("D4_prevalence_aggregated_multiple_lookback", outcome, sep = "_"),
             extension = "RDS")
}





