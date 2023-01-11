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

setorder(concept_in_pop, person_id, date)
concept_in_pop <- unique(concept_in_pop)
component_algo <- MergeFilterAndCollapse(list(concept_in_pop),
                                         condition = "meaning_renamed %in% c(names(meanings_of_this_study), DP_variables_recoded) & concept == 'MS'",
                                         strata = c("person_id", "cohort_entry_date", "cohort_exit_date",
                                                    "concept", "meaning_renamed"),
                                         summarystat = list(c("first", "date", "component_1"),
                                                            c("second", "date", "component_2"),
                                                            c("third", "date", "component_3"),
                                                            c("fourth", "date", "component_4")))

# Create the combinations
component_algo_simpl <- component_algo[, c("concept", "component_3", "component_4") := NULL]
component_algo_simpl <- data.table::melt(component_algo_simpl,
                                         id.vars = c("person_id", "cohort_entry_date", "cohort_exit_date", "meaning_renamed"),
                                         measure.vars = c("component_1", "component_2"))
component_algo_simpl <- component_algo_simpl[, variable := NULL]

component_algo <- data.table::dcast(component_algo, person_id + cohort_entry_date + cohort_exit_date ~ concept + meaning_renamed,
                                    drop = F, value.var = c("component_1", "component_2", "component_3", "component_4"))

cols_to_change <- names(component_algo)[grepl("component", names(component_algo))]
new_cols_names <- lapply(strsplit(cols_to_change, "_"), function (x) {
  fifelse(is.na(x[5]), paste(x[1], x[3], x[4], x[2], sep = "_"), paste(x[1], x[3], x[5], x[4], x[2], sep = "_"))
})
setnames(component_algo, cols_to_change, unlist(new_cols_names))

full_var_names <- c("component_MS_HOSP_1", "component_MS_HOSP_2", "component_MS_HOSP_3", "component_MS_HOSP_4",
                    "component_MS_SPECIALIST_1", "component_MS_SPECIALIST_2", "component_MS_SPECIALIST_3",
                    "component_MS_SPECIALIST_4", "component_MS_UNSPECIFIED_1", "component_MS_UNSPECIFIED_12",
                    "component_MS_UNSPECIFIED_3", "component_MS_UNSPECIFIED_4", "component_MS_PC_1", "component_MS_PC_2",
                    "component_MS_PC_3", "component_MS_PC_4", "component_MS_LONGTERM_1", "component_MS_LONGTERM_2",
                    "component_MS_LONGTERM_3", "component_MS_LONGTERM_4", "component_MS_SPEC_DMT_1",
                    "component_MS_SPEC_DMT_2", "component_MS_SPEC_DMT_3", "component_MS_SPEC_DMT_4",
                    "component_MS_UNSPEC_DMT_1", "component_MS_UNSPEC_DMT_2", "component_MS_UNSPEC_DMT_3",
                    "component_MS_UNSPEC_DMT_4")
cols_to_add <- setdiff(full_var_names, colnames(component_algo))
component_algo[, (cols_to_add) := as.Date(NA_integer_)]
setcolorder(component_algo, c("person_id", "cohort_entry_date", "cohort_exit_date", full_var_names))

colnames(component_algo)[grepl("^component_MS_.*(1|2)$", colnames(component_algo)) & !grepl("UNSPEC", colnames(component_algo))]
