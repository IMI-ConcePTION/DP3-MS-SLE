# Create components MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells
# Load person_id in study_populations

smart_load("D3_study_population_SAP1", dirtemp)
D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date,
                                                         cohort_exit_date)]

# Load corresponding outcome conceptsets
outcome_df <- rbindlist(lapply(OUTCOME_variables, function(x) {
  return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]]
  )[, .(person_id, date, meaning_renamed)][, concept := x])}
))

# Recode meaning with names used in the algorithms
out <- lapply(names(meanings_of_this_study),
              function(x) data.table(meaning_renamed = meanings_of_this_study[[x]], new = x))
out <- data.table::rbindlist(out)
outcome_df[out, on = "meaning_renamed", meaning_renamed := i.new]
outcome_df[is.na(meaning_renamed) | meaning_renamed %not in% names(meanings_of_this_study), meaning_renamed := "UNSPECIFIED"]

# Load corresponding drug_proxy conceptsets
dp_df <- rbindlist(lapply(DP_variables, function(x) {
  meaning_components <- strsplit(x, "_|-")[[1]]
  meaning_renamed <- fifelse(!is.na(meaning_components[3]), paste0(meaning_components[1], "_", meaning_components[3]),
                             meaning_components[1])
  return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]]
  )[, .(person_id, date)][, meaning_renamed := meaning_renamed][, concept := meaning_components[2]])}
))

# Recode meaning with names used in the algorithms
DP_variables_recoded <- unlist(lapply(strsplit(DP_variables, "_|-"),
                                      function(x) {fifelse(!is.na(x[3]), paste0(x[1], "_", x[3]), x[1])}))
concept_df <- rbindlist(list(outcome_df, dp_df))
rm(outcome_df, dp_df)

# Set keys and then foverlaps to find the events inside each spell
setkey(D3_study_population_SAP1, person_id, entry_spell_category, cohort_exit_date)
setkey(concept_df, person_id, date, date)
concept_in_pop <- better_foverlaps(D3_study_population_SAP1, concept_df, by.x = key(D3_study_population_SAP1))
rm(concept_df)

# find 1/2/3/4th occurrences for each meaning only for people with the events (MS/SLE)
setorder(concept_in_pop, person_id, date)
concept_in_pop <- unique(concept_in_pop)
component_algo <- MergeFilterAndCollapse(list(concept_in_pop),
                                         condition = "!is.na(meaning_renamed)",
                                         strata = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                    "concept", "meaning_renamed"),
                                         summarystat = list(c("first", "date", "component_1"),
                                                            c("second", "date", "component_2"),
                                                            c("third", "date", "component_3"),
                                                            c("fourth", "date", "component_4")))
rm(concept_in_pop)

# Meaning to wide as columns
main_components_MS <- data.table::dcast(component_algo[concept == "MS", ],
                                        person_id + cohort_entry_date + cohort_exit_date ~ concept + meaning_renamed,
                                        drop = T, value.var = c("component_1", "component_2", "component_3", "component_4"))

main_components_SLE <- data.table::dcast(component_algo[concept == "SLE", ],
                                        person_id + cohort_entry_date + cohort_exit_date ~ concept + meaning_renamed,
                                        drop = T, value.var = c("component_1", "component_2", "component_3", "component_4"))

# Changing columns names to what specified in the codebook
set_names_components <- function(x) {
  cols_to_change <- names(x)[grepl("component", names(x))]
  new_cols_names <- lapply(strsplit(cols_to_change, "_"), function (y) {
    fifelse(is.na(y[5]), paste(y[1], y[3], y[4], y[2], sep = "_"), paste(y[1], y[3], y[5], y[4], y[2], sep = "_"))
  })
  setnames(x, cols_to_change, unlist(new_cols_names))
  return(x)
}

main_components_MS <- set_names_components(main_components_MS)
main_components_SLE <- set_names_components(main_components_SLE)

### Create the combinations
# Remove unneded components and transform the dataset back to long format
combination_algo <- component_algo[, c("component_3", "component_4") := NULL]
rm(component_algo)

combination_algo <- data.table::melt(combination_algo,
                                     id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date", "meaning_renamed", "concept"),
                                     measure.vars = c("component_1", "component_2"), value.name = "date")[, variable := NULL]

# first combination for MS
first_comb <- MergeFilterAndCollapse(list(combination_algo),
                                     condition = "meaning_renamed %in% c('HOSP', 'PC', 'SPECIALIST', 'LONGTERM',
                                     'DMT_SPEC', 'UNSPECIFIED') & concept =='MS'",
                                     strata = c("person_id", "cohort_entry_date", "cohort_exit_date", "concept"),
                                     summarystat = list(c("first", "date"), c("second", "date"), c("third", "date")))
if (nrow(first_comb) != 0) {
  first_comb <- data.table::dcast(first_comb, person_id + cohort_entry_date + cohort_exit_date ~ concept,
                                  drop = T, value.var = c("first_date", "second_date", "third_date"))
  setnames(first_comb, colnames(first_comb)[grepl("MS", colnames(first_comb))],
           paste("combination_diag_spec_MS", c(1, 2, 3), sep = "_"))
  main_components_MS <- merge(main_components_MS, first_comb, all = T,
                           by = c("person_id", "cohort_entry_date", "cohort_exit_date"))
}

# second combination for MS
second_comb <- MergeFilterAndCollapse(list(combination_algo),
                                      condition = "meaning_renamed %in% c('PC', 'SPECIALIST', 'UNSPECIFIED') & concept =='MS'",
                                      strata = c("person_id", "cohort_entry_date", "cohort_exit_date", "concept"),
                                      summarystat = list(c("second", "date")))
if (nrow(second_comb) != 0) {
  second_comb <- data.table::dcast(second_comb, person_id + cohort_entry_date + cohort_exit_date ~ concept,
                                   drop = T, value.var = c("second_date"))
  setnames(second_comb, colnames(second_comb)[grepl("MS", colnames(second_comb))],
           paste("combination_diag_specialist_PC_unspec_MS", 2, sep = "_"))
  main_components_MS <- merge(main_components_MS, second_comb, all = T,
                           by = c("person_id", "cohort_entry_date", "cohort_exit_date"))
}

# Add any missing variable as a missing date and create the correct order
full_var_names <- c(paste("component_MS_HOSP", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_SPECIALIST", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_UNSPECIFIED", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_LONGTERM", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_SPEC_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_UNSPEC_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("combination_diag_spec_MS", c(1, 2, 3), sep = "_"),
                    paste("combination_diag_specialist_PC_unspec_MS", 2, sep = "_"))
cols_to_add <- setdiff(full_var_names, colnames(main_components_MS))
main_components_MS[, (cols_to_add) := as.Date(as.double(NA_integer_))]
setcolorder(main_components_MS, c("person_id", "cohort_entry_date", "cohort_exit_date", full_var_names))

# Create the algorithms (For full specification see codebook)
main_components_MS[, MS_1_date := combination_diag_spec_MS_1]
main_components_MS[, MS_2_date := pmin(combination_diag_spec_MS_2,
                                    pmax(combination_diag_spec_MS_1, component_MS_UNSPEC_DMT_1), na.rm = T)]
main_components_MS[, MS_3_date := pmin(combination_diag_spec_MS_3,
                                    pmax(combination_diag_spec_MS_2, component_MS_UNSPEC_DMT_1),
                                    pmax(combination_diag_spec_MS_1, component_MS_UNSPEC_DMT_2), na.rm = T)]
main_components_MS[, MS_4_date := pmin(component_MS_HOSP_1,
                                    combination_diag_spec_MS_2, na.rm = T)]
main_components_MS[, MS_5_date := pmin(combination_diag_spec_MS_2,
                                    component_MS_HOSP_2,
                                    pmax(combination_diag_spec_MS_1, component_MS_HOSP_1), na.rm = T)]

smart_save(merge(D3_study_population_SAP1, main_components_MS, all.x = T,
                 by = c("person_id", "cohort_entry_date", "cohort_exit_date")),
           dirtemp, override_name = "D3_components_MS_SAP1")
