if (thisdatasource %in% datasources_only_preg) {
  print(paste("D3_components_multiple_lookback_MS can't be calculated in datasource with only pregnancies"))
} else {
  
  smart_load("D3_outcomes_cleaned", dirtemp, extension = extension)
  
  lookback <- c(1, 2, 3, 5, 8, "all")
  
  better_sum <- function(...) {
    if (all(is.na(list(...)))) {
      return(NA_integer_)
    }
    sum(..., na.rm = T)
  }
  
  # Cumulative sum with treshold
  sum_reset_at <- function(thresh) {
    function(x) {
      accumulate(x, ~if_else(better_sum(.x, .y) >= thresh, as.integer(0), sum(.x, .y, na.rm = T), missing = NA_integer_))
    }  
  }
  
  # Transform date to integer and order the dataset
  D3_outcomes_cleaned[, date := as.integer(date)]
  setorder(D3_outcomes_cleaned, person_id, concept, meaning_renamed, date)
  
  # For each lookback period find the eligible event taking account recurrencies
  combined_df_outcome <- list()
  for (lb in lookback) {
    
    years_lookback <- if (lb == "all") 99 else lb
    date_lookback <- as.integer(ymd(20191231) - years(years_lookback))
    
    D3_outcomes_cleaned <- as.data.table(D3_outcomes_cleaned)
    D3_outcomes_cleaned <- D3_outcomes_cleaned[, lookback := as.character(lb)]
    D3_outcomes_cleaned[date > date_lookback, diff_date := date - shift(date, 1, first(date), "lag"),
                        by = c("person_id", "concept", "meaning_renamed")]
    
    D3_outcomes_cleaned <- D3_outcomes_cleaned %>%
      group_by(person_id) %>%
      mutate(distance := case_when(
        concept == "MS" ~ sum_reset_at(30)(diff_date),
        concept == "SLE" ~ sum_reset_at(28)(diff_date)
      )) %>%
      select(-diff_date)
    
    # Save the dataset in a list
    combined_df_outcome <- append(combined_df_outcome, list(filter(D3_outcomes_cleaned, !is.na(distance))))
  }
  rm(D3_outcomes_cleaned)
  
  # Collapse the list and revert date to a date format
  combined_df_outcome <- rbindlist(combined_df_outcome)
  combined_df_outcome[, date := as.Date(date, "1970-01-01")]
  
  # keep only value with cumulate == 0 (so first row or after threshold) or LONGTERM
  combined_df_outcome <- combined_df_outcome[distance == 0 | meaning_renamed == "LONGTERM", ][, distance := NULL]
  
  smart_load("D3_drug_proxies_cleaned", dirtemp, extension = extension)
  
  # Simplied version of the cycle above. No check for recurrencies
  combined_df_drug <- list()
  for (lb in lookback) {
    
    years_lookback <- if (lb == "all") 99 else lb
    date_lookback <- ymd(20191231) - years(years_lookback)
    
    D3_drug_proxies_cleaned <- D3_drug_proxies_cleaned[, lookback := as.character(lb)]
    
    # Save the dataset in a list
    combined_df_drug <- append(combined_df_drug, list(D3_drug_proxies_cleaned[date > date_lookback, ]))
  }
  rm(D3_drug_proxies_cleaned)
  
  # Collapse the list and revert date to a date format
  combined_df_drug <- rbindlist(combined_df_drug)
  
  # Combine outcomes and drugs
  combined_df <- rbindlist(list(combined_df_outcome, combined_df_drug))
  
  combined_df <- combined_df[, .N, by = c("person_id", "concept", "meaning_renamed", "lookback")]
  
  combined_df_MS <- combined_df[concept == "MS", ]
  combined_df_SLE <- combined_df[concept == "SLE", ]
  rm(combined_df)
  
  # Count number of occurences of each meaning in each lookback window
  combined_df_MS <- dcast(combined_df_MS, person_id + lookback ~ meaning_renamed, value.var = "N", fill = 0)
  
  # Add missing columns if necessary
  MS_cols <- setdiff(c("DMT", "DMT_SPEC", "DMT_UNSPEC", "INPATIENT", "LONGTERM", "OUTPATIENT_NO_PC", "PC", "UNSPECIFIED"),
                     colnames(combined_df_MS))
  if (length(MS_cols) > 0) combined_df_MS[, (MS_cols) := 0]
  
  # first combination for MS
  combined_df_MS[, combination_diag_spec_MS := rowSums(.SD),
                 .SDcols = c("INPATIENT", "PC", "OUTPATIENT_NO_PC", "LONGTERM", "UNSPECIFIED", "DMT_SPEC")]
  
  # second combination for MS
  combined_df_MS[, combination_diag_outpatient_no_pc_PC_unspec_MS := rowSums(.SD),
                 .SDcols = c("PC", "OUTPATIENT_NO_PC", "LONGTERM", "UNSPECIFIED")]
  
  # Create the algorithms (For full specification see codebook)
  combined_df_MS[, M1 := as.integer(combination_diag_spec_MS >= 1)]
  combined_df_MS[, M2 := as.integer(pmax(combined_df_MS$combination_diag_spec_MS >= 2,
                                         pmin(combined_df_MS$combination_diag_spec_MS >= 1, combined_df_MS$DMT_UNSPEC >= 1)))]
  combined_df_MS[, M3 := as.integer(pmax(combination_diag_spec_MS >= 3,
                                         pmin(combination_diag_spec_MS >= 2, DMT_UNSPEC >= 1),
                                         pmin(combination_diag_spec_MS >= 1, DMT_UNSPEC >= 2)))]
  combined_df_MS[, M4 := as.integer(pmax(INPATIENT >= 1,
                                         combination_diag_outpatient_no_pc_PC_unspec_MS >= 2))]
  combined_df_MS[, M5 := as.integer(pmax(combination_diag_outpatient_no_pc_PC_unspec_MS >= 2,
                                         INPATIENT >= 2,
                                         pmin(combination_diag_outpatient_no_pc_PC_unspec_MS >= 1, INPATIENT >= 1)))]
  
  # Long format to add lookback variable to algorithms
  combined_df_MS<- melt(combined_df_MS, id.vars = c("person_id", "lookback"),
                        measure.vars = c("M1", "M2", "M3", "M4", "M5"))
  combined_df_MS[, variable := paste(variable, lookback, sep = "_")]
  
  # Calculate total lookback for each person
  smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date, cohort_exit_date)]
  D3_study_population_SAP1 <- D3_study_population_SAP1[cohort_exit_date == study_end, ]
  D3_study_population_SAP1[, lookback := floor(interval(entry_spell_category, cohort_exit_date) / years(1))]
  D3_study_population_SAP1 <- D3_study_population_SAP1[lookback >= 5, ]
  D3_study_population_SAP1[, at_least_5_years_of_lookback_at_20191231 := 1]
  D3_study_population_SAP1 <- D3_study_population_SAP1[, at_least_10_years_of_lookback_at_20191231 := fifelse(lookback >= 10, 1, 0)]
  D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, cohort_entry_date, cohort_exit_date,
                                                           at_least_5_years_of_lookback_at_20191231,
                                                           at_least_10_years_of_lookback_at_20191231)]
  
  # Wide format by algorithm
  combined_df_MS <- dcast(combined_df_MS, person_id ~ variable, value.var = "value", fill = 0)
  
  # Add total lookback to persons with MS
  combined_df_MS <- merge(D3_study_population_SAP1, combined_df_MS, by = "person_id", all.x = T)
  combined_df_MS[is.na(combined_df_MS)] <- 0
  
  # Select algorithms columns and delete impossible values
  algo_cols <- colnames(combined_df_MS)[grepl("M[0-9]_[5-9]", colnames(combined_df_MS))]
  combined_df_MS[at_least_10_years_of_lookback_at_20191231 == 0, (algo_cols) := NA]
  
  
  smart_save(combined_df_MS, dirtemp, override_name = "D3_algorithms_multiple_lookback_MS", extension = extension, save_copy = "csv")
  
  # Count number of occurences of each meaning in each lookback window
  combined_df_SLE <- dcast(combined_df_SLE, person_id + lookback ~ meaning_renamed, value.var = "N", fill = 0)
  
  # Add missing columns if necessary
  SLE_cols <- setdiff(c("DMT", "INPATIENT", "LONGTERM", "OUTPATIENT_NO_PC", "PC", "UNSPECIFIED"),
                      colnames(combined_df_SLE))
  if (length(SLE_cols) > 0) combined_df_SLE[, (SLE_cols) := 0]
  
  # first combination for SLE
  combined_df_SLE[, combination_diag_spec_SLE := rowSums(.SD),
                  .SDcols = c("INPATIENT", "PC", "OUTPATIENT_NO_PC", "LONGTERM", "UNSPECIFIED")]
  
  # Create the algorithSLE (For full specification see codebook)
  combined_df_SLE[, M1 := as.integer(pmax(INPATIENT >= 1,
                                          OUTPATIENT_NO_PC >= 2))]
  combined_df_SLE[, M2 := as.integer(combination_diag_spec_SLE >= 1)]
  combined_df_SLE[, M3 := as.integer(pmax(combination_diag_spec_SLE >= 2,
                                          pmin(combination_diag_spec_SLE >= 1, DMT >= 1)))]
  combined_df_SLE[, M4 := as.integer(pmax(combination_diag_spec_SLE >= 3,
                                          pmin(combination_diag_spec_SLE >= 2, DMT >= 1)))]
  combined_df_SLE[, M5 := as.integer(combination_diag_spec_SLE >= 4)]
  
  # Long format to add lookback variable to algorithms
  combined_df_SLE<- melt(combined_df_SLE, id.vars = c("person_id", "lookback"),
                         measure.vars = c("M1", "M2", "M3", "M4", "M5"))
  combined_df_SLE[, variable := paste(variable, lookback, sep = "_")]
  
  # Wide format by algorithm
  combined_df_SLE <- dcast(combined_df_SLE, person_id ~ variable, value.var = "value", fill = 0)
  
  # Add total lookback to persons with MS
  combined_df_SLE <- merge(D3_study_population_SAP1, combined_df_SLE, by = "person_id", all.x = T)
  combined_df_SLE[is.na(combined_df_SLE)] <- 0
  
  # Select algorithms columns and delete impossible values
  algo_cols <- colnames(combined_df_SLE)[grepl("M[0-9]_[5-9]", colnames(combined_df_SLE))]
  combined_df_SLE[at_least_10_years_of_lookback_at_20191231 == 0, (algo_cols) := NA]
  
  smart_save(combined_df_SLE, dirtemp, override_name = "D3_algorithms_multiple_lookback_SLE", extension = extension, save_copy = "csv")
}
