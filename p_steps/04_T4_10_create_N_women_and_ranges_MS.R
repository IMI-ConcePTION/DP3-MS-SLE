# Create N_women_and_ranges_MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Load components
  components <- smart_load(paste("D3_components", outcome, "SAP1", sep = "_"), dirtemp, return = T)
  
  # Select components columns
  component_cols <- colnames(components)[grepl("^component_", colnames(components))]
  components <- components[, c("person_id", "entry_spell_category", "cohort_exit_date", component_cols), with = F]
  
  # Calculate length of spell and then remove entry_spell_category
  components[, length_spell := age_fast(entry_spell_category, cohort_exit_date)]
  components[, c("entry_spell_category", "cohort_exit_date") := NULL]
  
  # Components to single column
  components <- data.table::melt(components, id.vars = c("person_id", "length_spell"), measure.vars = component_cols,
                                 value.name = "date_component", variable.name = "component_name")
  
  # Remove component_ from name
  components[, component_name := gsub("component_", "", component_name)]
  
  # Remove length_spell if the row does not contains a component
  components[is.na(date_component), length_spell := NA]
  
  # Load D3_PERSONS and select on id and birth date
  smart_load("D3_PERSONS", dirtemp)
  D3_PERSONS <- D3_PERSONS[, c("person_id", "birth_date")]
  
  D5_N_women_and_ranges <- MergeFilterAndCollapse(list(components),
                                                  D3_PERSONS,
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
  
  smart_save(D5_N_women_and_ranges, direxp, override_name = paste("D5_N_women_and_ranges", outcome, sep = "_"),
             extension = "RDS")
}





