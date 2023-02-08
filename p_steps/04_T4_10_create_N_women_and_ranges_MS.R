# Create N_women_and_ranges_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

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
  D3_study_population_SAP1 <- D3_study_population_SAP1[, c("person_id", "birth_date", "entry_spell_category",
                                                           "cohort_exit_date")]
  
  # Calculate length of spell and then remove entry_spell_category
  D3_study_population_SAP1[, length_spell := difftime(cohort_exit_date, entry_spell_category, "days")]
  D3_study_population_SAP1[, c("entry_spell_category", "cohort_exit_date") := NULL]
  
  D3_study_population_SAP1 <- D3_study_population_SAP1[, lapply(.SD, sum), by = c("person_id", "birth_date"),
                                                       .SDcols = "length_spell"]
  D3_study_population_SAP1[, length_spell := as.numeric(length_spell) / 365.25]
  
  D5_N_women_and_ranges <- MergeFilterAndCollapse(list(components),
                                                  D3_study_population_SAP1,
                                                  key = "person_id",
                                                  condition = "!is.na(person_id)",
                                                  additionalvar = list(
                                                    list(c("age"), "age_fast(birth_date, date_component)")
                                                  ),
                                                  strata = c("component_name"),
                                                  summarystat = list(c("count", "date_component", "N"),
                                                                     c("median", "length_spell", "lookback_median"),
                                                                     c("25p", "length_spell", "lookback_25p"),
                                                                     c("75p", "length_spell", "lookback_75p"),
                                                                     c("median", "age", "age_median"),
                                                                     c("25p", "age", "age_25p"),
                                                                     c("75p", "age", "age_75p")))
  
  D5_N_women_and_ranges <- D5_N_women_and_ranges[!is.na(component_name)]
  D5_N_women_and_ranges <- D5_N_women_and_ranges[, lapply(.SD, round, 0), by = c("component_name", "N"),
                                                 .SDcols = c("lookback_median", "lookback_25p", "lookback_75p",
                                                             "age_median", "age_25p", "age_75p")]
  
  smart_save(D5_N_women_and_ranges, direxp, override_name = paste("D5_N_women_and_ranges", outcome, sep = "_"),
             extension = "csv")
}





