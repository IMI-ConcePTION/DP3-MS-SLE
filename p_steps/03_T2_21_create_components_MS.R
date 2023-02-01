# Create components
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells
# Load person_id in study_populations

component_algo <- smart_load("D3_main_components", dirtemp, return = T)

# Meaning to wide as columns
main_components_MS <- data.table::dcast(component_algo[concept == "MS", ],
                                        person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                          at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept + meaning_renamed,
                                        drop = T, value.var = c("component_1", "component_2", "component_3", "component_4"))

# Changing columns names to what specified in the codebook
main_components_MS <- set_names_components(main_components_MS)

### Create the combinations
# Remove unneded components and transform the dataset back to long format
combination_algo <- component_algo[, c("component_3", "component_4") := NULL]
rm(component_algo)

combination_algo <- data.table::melt(combination_algo,
                                     id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                 "meaning_renamed", "concept", "length_lookback",
                                                 "at_least_5_years_of_lookback_at_20191231",
                                                 "at_least_10_years_of_lookback_at_20191231"),
                                     measure.vars = c("component_1", "component_2"),
                                     value.name = "date", na.rm = T)[, variable := NULL]

# first combination for MS
first_comb <- MergeFilterAndCollapse(list(combination_algo),
                                     condition = "meaning_renamed %in% c('INPATIENT', 'PC', 'OUTPATIENT_NO_PC', 'LONGTERM',
                                     'DMT_SPEC', 'UNSPECIFIED') & concept =='MS'",
                                     sorting = c("person_id", "date"),
                                     strata = c("person_id", "cohort_entry_date", "cohort_exit_date", "concept", "length_lookback",
                                                "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"),
                                     summarystat = list(c("first", "date"), c("second", "date"), c("third", "date")))
if (nrow(first_comb) != 0) {
  first_comb <- data.table::dcast(first_comb, person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                    at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept,
                                  drop = T, value.var = c("first_date", "second_date", "third_date"))
  setnames(first_comb, colnames(first_comb)[grepl("MS", colnames(first_comb))],
           paste("combination_diag_spec_MS", c(1, 2, 3), sep = "_"))
  main_components_MS <- merge(main_components_MS, first_comb, all = T,
                              by = c("person_id", "cohort_entry_date", "cohort_exit_date", "length_lookback",
                                     "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"))
}

# second combination for MS
second_comb <- MergeFilterAndCollapse(list(combination_algo),
                                      condition = "meaning_renamed %in% c('PC', 'OUTPATIENT_NO_PC', 'UNSPECIFIED') & concept =='MS'",
                                      sorting = c("person_id", "date"),
                                      strata = c("person_id", "cohort_entry_date", "cohort_exit_date", "concept", "length_lookback",
                                                 "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"),
                                      summarystat = list(c("first", "date"), c("second", "date")))
if (nrow(second_comb) != 0) {
  second_comb <- data.table::dcast(second_comb, person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                     at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept,
                                   drop = T, value.var = c("second_date"))
  setnames(second_comb, colnames(second_comb)[grepl("MS", colnames(second_comb))],
           paste("combination_diag_outpatient_no_pc_PC_unspec_MS", c(1, 2), sep = "_"))
  main_components_MS <- merge(main_components_MS, second_comb, all = T,
                              by = c("person_id", "cohort_entry_date", "cohort_exit_date", "length_lookback",
                                     "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"))
}

# Add any missing variable as a missing date and create the correct order
full_var_names <- c(paste("component_MS_INPATIENT", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_OUTPATIENT_NO_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_UNSPECIFIED", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_LONGTERM", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_SPEC_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_UNSPEC_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("combination_diag_spec_MS", c(1, 2, 3), sep = "_"),
                    paste("combination_diag_outpatient_no_pc_PC_unspec_MS", c(1, 2), sep = "_"))
cols_to_add <- setdiff(full_var_names, colnames(main_components_MS))
main_components_MS[, (cols_to_add) := as.Date(as.double(NA_integer_))]
setcolorder(main_components_MS, c("person_id", "cohort_entry_date", "cohort_exit_date", "length_lookback",
                                  "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231",
                                  full_var_names))

# Create the algorithms (For full specification see codebook)
main_components_MS[, MS_1_date := combination_diag_spec_MS_1]
main_components_MS[, MS_2_date := pmin(combination_diag_spec_MS_2,
                                       pmax(combination_diag_spec_MS_1, component_MS_UNSPEC_DMT_1), na.rm = T)]
main_components_MS[, MS_3_date := pmin(combination_diag_spec_MS_3,
                                       pmax(combination_diag_spec_MS_2, component_MS_UNSPEC_DMT_1),
                                       pmax(combination_diag_spec_MS_1, component_MS_UNSPEC_DMT_2), na.rm = T)]
main_components_MS[, MS_4_date := pmin(component_MS_INPATIENT_1,
                                       combination_diag_outpatient_no_pc_PC_unspec_MS_2, na.rm = T)]
main_components_MS[, MS_5_date := pmin(combination_diag_outpatient_no_pc_PC_unspec_MS_2,
                                       component_MS_INPATIENT_2,
                                       pmax(combination_diag_outpatient_no_pc_PC_unspec_MS_1, component_MS_INPATIENT_1), na.rm = T)]

# Select the components calculated on the whole dataset
main_components_MS_whole <- copy(main_components_MS)[length_lookback == "whole", ]
main_components_MS_whole <- main_components_MS_whole[, c("length_lookback", "at_least_5_years_of_lookback_at_20191231",
                                                         "at_least_10_years_of_lookback_at_20191231") := NULL]

# Load population and keep one line per person
smart_load("D3_study_population_SAP1", dirtemp)
D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date,
                                                         cohort_exit_date)]
D3_study_population_SAP1[, c("entry_spell_category", "cohort_entry_date", "cohort_exit_date") :=
                           list(min(entry_spell_category, na.rm = T), min(cohort_entry_date, na.rm = T),
                                max(cohort_exit_date, na.rm = T)), by = "person_id"]
D3_study_population_SAP1 <- unique(D3_study_population_SAP1)

smart_save(merge(D3_study_population_SAP1, main_components_MS_whole, all.x = T,
                 by = c("person_id", "cohort_entry_date", "cohort_exit_date")),
           dirtemp, override_name = "D3_components_MS_SAP1")

if (thisdatasource %in% c("EFEMERIS", "THL")) {
  print(paste("D3_components_multiple_lookback_MS can't be calculated in datasource EFEMERIS and THL"))
} else {
  
  # Select the components calculated on the whole dataset
  main_components_MS <- main_components_MS[length_lookback != "whole", ]
  
  # Select algorithms columns and remove components
  algo_cols <- colnames(main_components_MS)[grepl("_[0-9]_date", colnames(main_components_MS))]
  not_algo_cols <- colnames(main_components_MS)[grepl("component_|combination_", colnames(main_components_MS))]
  main_components_MS <- main_components_MS[, (not_algo_cols) := NULL]
  
  # Remove impossible values
  main_components_MS <- main_components_MS[!(at_least_10_years_of_lookback_at_20191231 == 0 & length_lookback %in% c("8", "all")), ]
  
  # Lookback period to wide
  main_components_MS <- data.table::dcast(main_components_MS,
                                          person_id + cohort_entry_date + cohort_exit_date + at_least_5_years_of_lookback_at_20191231 +
                                            at_least_10_years_of_lookback_at_20191231 ~ length_lookback,
                                          drop = T, value.var = algo_cols)
  
  cols_to_change <- colnames(main_components_MS)[grepl("^MS_[0-9]_", colnames(main_components_MS))]
  new_col_names <- sapply(strsplit(cols_to_change, "_"), function(x) paste0("M", paste(x[2], x[4], x[3], sep = "_")))
  setnames(main_components_MS, cols_to_change, new_col_names)
  
  smart_save(main_components_MS, dirtemp, override_name = "D3_components_multiple_lookback_MS")
  
}
