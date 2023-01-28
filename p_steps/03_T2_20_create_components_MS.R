# Create components MS
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells
# Load person_id in study_populations

smart_load("D3_study_population_SAP1", dirtemp)
D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date,
                                                         cohort_exit_date)]

# s = "all" is to be excluded for now
s <- c(1, 2, 3, 5, 8, "all")

# Load corresponding outcome conceptsets
outcome_df <- rbindlist(lapply(OUTCOME_variables, function(x) {
  return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]]
  )[, .(person_id, date, meaning_renamed)][, concept := x])}
))

# Create dataset with meaning to recode
out <- lapply(names(meanings_of_this_study),
              function(x) data.table(meaning_renamed = meanings_of_this_study[[x]], new = x))
out <- data.table::rbindlist(out)

# Count meaning occurences and save it in direxp
meaning_occurences <- copy(outcome_df)[out, on = "meaning_renamed", meaning_recoded := i.new]
setnames(meaning_occurences, "meaning_renamed", "original_meaning")
meaning_occurences[is.na(meaning_recoded) | meaning_recoded %not in% names(meanings_of_this_study), meaning_recoded := "UNSPECIFIED"]

meaning_occurences <- MergeFilterAndCollapse(list(meaning_occurences),
                                             condition = "!is.na(person_id)",
                                             strata = c("concept", "original_meaning", "meaning_recoded"),
                                             summarystat = list(c("count", "person_id", "count")))
smart_save(meaning_occurences, direxp, override_name = "D5_meaning_occurences", extension = "RDS")


# Recode meaning with names used in the algorithms
outcome_df[out, on = "meaning_renamed", meaning_renamed := i.new]
outcome_df[is.na(meaning_renamed) | meaning_renamed %not in% names(meanings_of_this_study), meaning_renamed := "UNSPECIFIED"]

# Load corresponding drug_proxy conceptsets
dp_df <- rbindlist(lapply(DP_variables, function(x) {
  meaning_components <- strsplit(x, "_|-")[[1]]
  
  # Recode meaning with names used in the algorithms
  meaning_renamed <- fifelse(!is.na(meaning_components[3]), paste0(meaning_components[1], "_", meaning_components[3]),
                             meaning_components[1])
  return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]]
  )[, .(person_id, date)][, meaning_renamed := meaning_renamed][, concept := meaning_components[2]])}
))

# Combine outcomes and drug proxies
concept_df <- rbindlist(list(outcome_df, dp_df))
rm(outcome_df, dp_df)

# Set keys and then foverlaps to find the events inside each spell
setkey(D3_study_population_SAP1, person_id, entry_spell_category, cohort_exit_date)
setkey(concept_df, person_id, date, date)
concept_in_pop <- better_foverlaps(D3_study_population_SAP1, concept_df, by.x = key(D3_study_population_SAP1))
rm(concept_df)

### Calculate components for lookback
# Restrict population
# Keep only women who exit the study at 31st December 2019 and have any events
concept_in_pop_restricted <- copy(concept_in_pop)[!is.na(date) & cohort_exit_date == study_end, ]

# Calculate length of spell and then remove entry_spell_category
concept_in_pop_restricted[, length_spell := age_fast(entry_spell_category, cohort_exit_date)]
concept_in_pop_restricted[, entry_spell_category := NULL]

# Find if spells are longer than 5 and 10 years and then remove length_spell
concept_in_pop_restricted[, at_least_5_years_of_lookback_at_20191231 := as.integer(length_spell >= 5)]
concept_in_pop_restricted[, at_least_10_years_of_lookback_at_20191231 := as.integer(length_spell >= 10)]
concept_in_pop_restricted[, length_spell := NULL]

# Keep only women with at least 5 years of lookback
concept_in_pop_restricted <- concept_in_pop_restricted[at_least_5_years_of_lookback_at_20191231 == 1, ]

# Clean the dataset
concept_in_pop_restricted <- unique(concept_in_pop_restricted)

# find 1/2/3/4th occurrences for each meaning only for people with the events (MS/SLE)
component_lookback_algo <- lapply(s, function(x){
  int_x <- if (x == "all") 99 else as.integer(x)
  MergeFilterAndCollapse(list(concept_in_pop_restricted),
                         condition = paste0("!is.na(meaning_renamed) & date > study_end - years(", int_x, ")"),
                         additionalvar = list(c("length_lookback", paste0("'", x, "'"))),
                         strata = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                    "concept", "meaning_renamed", "length_lookback", "at_least_5_years_of_lookback_at_20191231",
                                    "at_least_10_years_of_lookback_at_20191231"),
                         sorting = c("person_id", "date"),
                         summarystat = list(c("first", "date", "component_1"),
                                            c("second", "date", "component_2"),
                                            c("third", "date", "component_3"),
                                            c("fourth", "date", "component_4")))
})

### Repeat on the whole dataset
concept_in_pop <- unique(concept_in_pop)
component_algo <- MergeFilterAndCollapse(list(concept_in_pop),
                                         condition = "!is.na(meaning_renamed)",
                                         additionalvar = list(c("length_lookback", paste0("'", 'whole', "'"))),
                                         strata = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                    "concept", "meaning_renamed", "length_lookback"),
                                         sorting = c("person_id", "date"),
                                         summarystat = list(c("first", "date", "component_1"),
                                                            c("second", "date", "component_2"),
                                                            c("third", "date", "component_3"),
                                                            c("fourth", "date", "component_4")))
rm(concept_in_pop)

component_algo <- rbindlist(c(component_lookback_algo, list(component_algo)), fill = T)

# Meaning to wide as columns
main_components_MS <- data.table::dcast(component_algo[concept == "MS", ],
                                        person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                          at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept + meaning_renamed,
                                        drop = T, value.var = c("component_1", "component_2", "component_3", "component_4"))

main_components_SLE <- data.table::dcast(component_algo[concept == "SLE", ],
                                         person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                           at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept + meaning_renamed,
                                         drop = T, value.var = c("component_1", "component_2", "component_3", "component_4"))


main_components <- data.table::dcast(component_algo,
                                         person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                           at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept + meaning_renamed,
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
                                     id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                 "meaning_renamed", "concept", "length_lookback",
                                                 "at_least_5_years_of_lookback_at_20191231",
                                                 "at_least_10_years_of_lookback_at_20191231"),
                                     measure.vars = c("component_1", "component_2"),
                                     value.name = "date", na.rm = T)[, variable := NULL]

# first combination for MS
first_comb <- MergeFilterAndCollapse(list(combination_algo),
                                     condition = "meaning_renamed %in% c('HOSP', 'PC', 'OUTPATIENT_NO_PC', 'LONGTERM',
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
                                      summarystat = list(c("second", "date")))
if (nrow(second_comb) != 0) {
  second_comb <- data.table::dcast(second_comb, person_id + cohort_entry_date + cohort_exit_date + length_lookback +
                                     at_least_5_years_of_lookback_at_20191231 + at_least_10_years_of_lookback_at_20191231 ~ concept,
                                   drop = T, value.var = c("second_date"))
  setnames(second_comb, colnames(second_comb)[grepl("MS", colnames(second_comb))],
           paste("combination_diag_outpatient_no_pc_PC_unspec_MS", 2, sep = "_"))
  main_components_MS <- merge(main_components_MS, second_comb, all = T,
                           by = c("person_id", "cohort_entry_date", "cohort_exit_date", "length_lookback",
                                  "at_least_5_years_of_lookback_at_20191231", "at_least_10_years_of_lookback_at_20191231"))
}

# Add any missing variable as a missing date and create the correct order
full_var_names <- c(paste("component_MS_HOSP", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_OUTPATIENT_NO_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_UNSPECIFIED", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_PC", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_LONGTERM", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_SPEC_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("component_MS_UNSPEC_DMT", c(1, 2, 3, 4), sep = "_"),
                    paste("combination_diag_spec_MS", c(1, 2, 3), sep = "_"),
                    paste("combination_diag_outpatient_no_pc_PC_unspec_MS", 2, sep = "_"))
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
main_components_MS[, MS_4_date := pmin(component_MS_HOSP_1,
                                       combination_diag_specialist_PC_unspec_MS_2, na.rm = T)]
main_components_MS[, MS_5_date := pmin(combination_diag_spec_MS_2,
                                    component_MS_HOSP_2,
                                    pmax(combination_diag_spec_MS_1, component_MS_HOSP_1), na.rm = T)]

# Select the components calculated on the whole dataset
main_components_MS_whole <- copy(main_components_MS)[length_lookback == "whole", ]
main_components_MS_whole <- main_components_MS_whole[, c("length_lookback", "at_least_5_years_of_lookback_at_20191231",
                                                         "at_least_10_years_of_lookback_at_20191231") := NULL]

smart_save(merge(D3_study_population_SAP1, main_components_MS_whole, all.x = T,
                 by = c("person_id", "cohort_entry_date", "cohort_exit_date")),
           dirtemp, override_name = "D3_components_MS_SAP1")

if (thisdatasource %in% c("EFEMERIS", "THL")) {
  print(paste("D3_components_multiple_lookback_", outcome, " can't be calculated in datasource EFEMERIS and THL"))
  next
}

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
