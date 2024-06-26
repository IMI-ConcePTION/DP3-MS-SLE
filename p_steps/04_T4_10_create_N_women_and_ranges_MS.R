##%######################################################%##
#                                                          #
####   CREATE DESCRIPTIVE TABLE D5_N_women_and_ranges   ####
#                                                          #
##%######################################################%##


for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Load components
  components <- smart_load(paste("D3_components", outcome, "SAP1", sep = "_"), dirtemp, return = T, extension = extension)
  
  # Select algorithms and components columns
  algo_cols <- colnames(components)[grepl("_[0-9]_date$", colnames(components))]
  component_cols <- colnames(components)[grepl("^component_", colnames(components))]
  components <- components[, c("person_id", "entry_spell_category", "cohort_exit_date",component_cols, algo_cols),
                           with = F]
  
  # Algorithms to single column
  algorithms <- data.table::melt(components, id.vars = "person_id", measure.vars = algo_cols,
                                 value.name = "date_component", variable.name = "component_name")
  
  # Components to single column
  components <- data.table::melt(components, id.vars = "person_id", measure.vars = component_cols,
                                 value.name = "date_component", variable.name = "component_name")
  
  # Remove _date from name
  algorithms[, component_name := gsub("_date", "", component_name)]
  
  # Remove component_ from name
  components[, component_name := gsub("component_", "", component_name)]
  
  # Combine algorithms and components
  components <- rbindlist(list(components, algorithms))
  
  # Remove length_spell if the row does not contains a component
  components <- components[!is.na(date_component)]
  
  # Load D3_PERSONS and select on id and birth date
  smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
  D3_study_population_SAP1 <- D3_study_population_SAP1[, c("person_id", "birth_date", "cohort_entry_date",
                                                           "cohort_exit_date", "entry_spell_category")]
  
  # Calculate length of spell and then remove entry_spell_category
  if (thisdatasource == "SAIL Databank") {
    D3_study_population_SAP1[, length_spell := difftime(cohort_exit_date, entry_spell_category, units = "days")]
    D3_study_population_SAP1[, min_max_spell := difftime(max(cohort_exit_date), min(entry_spell_category), units = "days"),
                             by = "person_id"]
  } else {
    D3_study_population_SAP1[, length_spell := difftime(cohort_exit_date, cohort_entry_date, units = "days")]
    D3_study_population_SAP1[, min_max_spell := difftime(max(cohort_exit_date), min(cohort_entry_date), units = "days"),
                             by = "person_id"]
  }
  
  D3_study_population_SAP1[, n_spell := .N, by = "person_id"]
  D3_study_population_SAP1[, coverage := as.numeric(sum(length_spell) / as.numeric(min_max_spell) * 100),
                           by = "person_id"]
  D3_study_population_SAP1[, cohort_entry_date := min(cohort_entry_date), by = "person_id"]
  D3_study_population_SAP1[, age_at_entry_spell := age_fast(birth_date, cohort_entry_date)]
  D3_study_population_SAP1[, c("cohort_exit_date", "cohort_entry_date", "entry_spell_category", "length_spell") := NULL]
  D3_study_population_SAP1 <- unique(D3_study_population_SAP1)
  
  # D3_study_population_SAP1 <- D3_study_population_SAP1[, lapply(.SD, sum),
  #                                                      by = c("person_id", "birth_date", "age_at_entry_spell"),
  #                                                      .SDcols = "length_spell"]
  D3_study_population_SAP1[, min_max_spell := as.numeric(min_max_spell) / 365.25]
  
  D5_N_women_and_ranges <- MergeFilterAndCollapse(list(components),
                                                  D3_study_population_SAP1,
                                                  key = "person_id",
                                                  condition = "!is.na(person_id)",
                                                  additionalvar = list(
                                                    list(c("age"), "age_fast(birth_date, date_component)")
                                                  ),
                                                  strata = c("component_name"),
                                                  summarystat = list(c("count", "date_component", "N"),
                                                                     c("median", "n_spell", "n_spell_median"),
                                                                     c("25p", "n_spell", "n_spell_25p"),
                                                                     c("75p", "n_spell", "n_spell_75p"),
                                                                     c("median", "min_max_spell", "lookback_median"),
                                                                     c("25p", "min_max_spell", "lookback_25p"),
                                                                     c("75p", "min_max_spell", "lookback_75p"),
                                                                     c("mean", "coverage", "coverage_mean"),
                                                                     c("25p", "coverage", "coverage_25p"),
                                                                     c("75p", "coverage", "coverage_75p"),
                                                                     c("median", "age", "age_median"),
                                                                     c("25p", "age", "age_25p"),
                                                                     c("75p", "age", "age_75p")))
  
  setnames(D3_study_population_SAP1, "age_at_entry_spell", "age")
  D3_study_population_SAP1[, component_name := "Study population"]
  
  D5_N_women_and_ranges_tot <- MergeFilterAndCollapse(list(D3_study_population_SAP1),
                                                  condition = "!is.na(person_id)",
                                                  strata = c("component_name"),
                                                  summarystat = list(c("count", "person_id", "N"),
                                                                     c("median", "n_spell", "n_spell_median"),
                                                                     c("25p", "n_spell", "n_spell_25p"),
                                                                     c("75p", "n_spell", "n_spell_75p"),
                                                                     c("median", "min_max_spell", "lookback_median"),
                                                                     c("25p", "min_max_spell", "lookback_25p"),
                                                                     c("75p", "min_max_spell", "lookback_75p"),
                                                                     c("mean", "coverage", "coverage_mean"),
                                                                     c("25p", "coverage", "coverage_25p"),
                                                                     c("75p", "coverage", "coverage_75p"),
                                                                     c("median", "age", "age_median"),
                                                                     c("25p", "age", "age_25p"),
                                                                     c("75p", "age", "age_75p")))
  
  D5_N_women_and_ranges <- rbindlist(list(D5_N_women_and_ranges, D5_N_women_and_ranges_tot))
  
  D5_N_women_and_ranges <- D5_N_women_and_ranges[!is.na(component_name)]
  D5_N_women_and_ranges <- D5_N_women_and_ranges[, lapply(.SD, round, 1), by = c("component_name", "N", "n_spell_median",
                                                                                 "n_spell_25p", "n_spell_75p",
                                                                                 "age_median", "age_25p", "age_75p"),
                                                 .SDcols = c("lookback_median", "lookback_25p", "lookback_75p",
                                                             "coverage_mean", "coverage_25p", "coverage_75p")]
  D5_N_women_and_ranges <- D5_N_women_and_ranges[, lapply(.SD, round, 0), by = c("component_name", "N",
                                                                                 "lookback_median", "lookback_25p",
                                                                                 "lookback_75p", "coverage_mean",
                                                                                 "coverage_25p", "coverage_75p"),
                                                 .SDcols = c("n_spell_median", "n_spell_25p", "n_spell_75p",
                                                             "age_median", "age_25p", "age_75p")]
  
  if (thisdatasource == "SAIL Databank") {
    export_name <- paste("D5_N_women_and_ranges", outcome, "2", sep = "_")
  } else {
    export_name <- paste("D5_N_women_and_ranges", outcome, sep = "_")
  }
  
  smart_save(D5_N_women_and_ranges, direxp, override_name = export_name, extension = "csv")
  
  # Create a filtered version of the prevalence excluding the row with at least a small count
  D5_N_women_and_ranges_masked <- copy(D5_N_women_and_ranges)[, N := lapply(.SD,
                                                                          function(x) fifelse(as.integer(x) < 5  & as.integer(x) > 0,
                                                                                              paste0("<5"), as.character(x))),
                                        .SDcols = "N"]
  
  smart_save(D5_N_women_and_ranges_masked, direxpmask, override_name = paste(export_name, "masked", sep = "_"),
             extension = "csv")
  
  # update_vector("datasets_to_censor", dirpargen, export_name)
  # update_vector("variables_to_censor", dirpargen, c("N" = 5))
  # 
  # update_vector("datasets_to_censor_check", dirpargen, "D5_N_women_and_ranges")
}





