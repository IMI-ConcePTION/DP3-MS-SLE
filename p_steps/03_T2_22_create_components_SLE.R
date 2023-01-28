# Create components
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells
# Load person_id in study_populations

component_algo <- smart_load("D3_main_components", dirtemp, return = T)

# Meaning to wide as columns
main_components_SLE <- data.table::dcast(component_algo[concept == "SLE", ],
                                         person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                           at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept + meaning_renamed,
                                         drop = T, value.var = c("component_1", "component_2", "component_3", "component_4"))

# Changing columns names to what specified in the codebook
main_components_SLE <- set_names_components(main_components_SLE)

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

# first combination for SLE
first_comb <- MergeFilterAndCollapse(list(combination_algo),
                                     condition = "meaning_renamed %in% c('HOSP', 'PC', 'OUTPATIENT_NO_PC', 'LONGTERM',
                                     'UNSPECIFIED') & concept =='SLE'",
                                     sorting = c("person_id", "date"),
                                     strata = c("person_id", "cohort_entry_date", "cohort_exit_date", "concept", "length_lookback",
                                                "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"),
                                     summarystat = list(c("first", "date"), c("second", "date"), c("third", "date"), c("fourth", "date")))
if (nrow(first_comb) != 0) {
  first_comb <- data.table::dcast(first_comb, person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                    at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept,
                                  drop = T, value.var = c("first_date", "second_date", "third_date", "fourth_date"))
  setnames(first_comb, colnames(first_comb)[grepl("SLE", colnames(first_comb))],
           paste("combination_diag_spec_SLE", c(1, 2, 3, 4), sep = "_"))
  main_components_SLE <- merge(main_components_SLE, first_comb, all = T,
                              by = c("person_id", "cohort_entry_date", "cohort_exit_date", "length_lookback",
                                     "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"))
}

# Add any missing variable as a missing date and create the correct order
full_var_names <- c(paste("component_SLE_HOSP", c(1, 2, 3, 4), sep = "_"),
                    paste("component_SLE_OUTPATIENT_NO_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_SLE_UNSPECIFIED", c(1, 2, 3, 4), sep = "_"),
                    paste("component_SLE_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_SLE_LONGTERM", c(1, 2, 3, 4), sep = "_"),
                    paste("component_SLE_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("combination_diag_spec_SLE", c(1, 2, 3, 4), sep = "_"))
cols_to_add <- setdiff(full_var_names, colnames(main_components_SLE))
main_components_SLE[, (cols_to_add) := as.Date(as.double(NA_integer_))]
setcolorder(main_components_SLE, c("person_id", "cohort_entry_date", "cohort_exit_date", "length_lookback",
                                  "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231",
                                  full_var_names))

# Create the algorithSLE (For full specification see codebook)
main_components_SLE[, SLE_1_date := pmin(component_SLE_HOSP_1,
                                         component_SLE_OUTPATIENT_NO_PC_2, na.rm = T)]
main_components_SLE[, SLE_2_date := combination_diag_spec_SLE_1]
main_components_SLE[, SLE_3_date := pmin(combination_diag_spec_SLE_2,
                                         pmax(combination_diag_spec_SLE_1, component_SLE_DMT_1), na.rm = T)]
main_components_SLE[, SLE_4_date := pmin(combination_diag_spec_SLE_3,
                                         pmax(combination_diag_spec_SLE_2, component_SLE_DMT_1), na.rm = T)]
main_components_SLE[, SLE_5_date := combination_diag_spec_SLE_4]

# Select the components calculated on the whole dataset
main_components_SLE_whole <- copy(main_components_SLE)[length_lookback == "whole", ]
main_components_SLE_whole <- main_components_SLE_whole[, c("length_lookback", "at_least_5_years_of_lookback_at_20191231",
                                                         "at_least_10_years_of_lookback_at_20191231") := NULL]

smart_load("D3_study_population_SAP1", dirtemp)
D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date,
                                                         cohort_exit_date)]

smart_save(merge(D3_study_population_SAP1, main_components_SLE_whole, all.x = T,
                 by = c("person_id", "cohort_entry_date", "cohort_exit_date")),
           dirtemp, override_name = "D3_components_SLE_SAP1")

if (thisdatasource %in% c("EFEMERIS", "THL")) {
  print(paste("D3_components_multiple_lookback_", outcome, " can't be calculated in datasource EFEMERIS and THL"))
  next
}

# Select the components calculated on the whole dataset
main_components_SLE <- main_components_SLE[length_lookback != "whole", ]

# Select algorithSLE columns and remove components
algo_cols <- colnames(main_components_SLE)[grepl("_[0-9]_date", colnames(main_components_SLE))]
not_algo_cols <- colnames(main_components_SLE)[grepl("component_|combination_", colnames(main_components_SLE))]
main_components_SLE <- main_components_SLE[, (not_algo_cols) := NULL]

# Remove impossible values (not 10 year with lookback 8 or all)
main_components_SLE <- main_components_SLE[!(at_least_10_years_of_lookback_at_20191231 == 0 & length_lookback %in% c("8", "all")), ]

# Lookback period to wide
main_components_SLE <- data.table::dcast(main_components_SLE,
                                        person_id + cohort_entry_date + cohort_exit_date + at_least_5_years_of_lookback_at_20191231 +
                                          at_least_10_years_of_lookback_at_20191231 ~ length_lookback,
                                        drop = T, value.var = algo_cols)

cols_to_change <- colnames(main_components_SLE)[grepl("^SLE_[0-9]_", colnames(main_components_SLE))]
new_col_names <- sapply(strsplit(cols_to_change, "_"), function(x) paste0("M", paste(x[2], x[4], x[3], sep = "_")))
setnames(main_components_SLE, cols_to_change, new_col_names)

smart_save(main_components_SLE, dirtemp, override_name = "D3_components_multiple_lookback_SLE")
