# Create components MS

# input: D3_output_spells_category
# output: D3_clean_spells

# Load person_id in study_populations
smart_load("D3_study_population_SAP1", dirtemp)
D3_study_population_SAP1 <- D3_study_population_SAP1[, .(person_id, entry_spell_category, cohort_entry_date,
                                                         cohort_exit_date)]

# Load corresponding conceptsets
concept_df <- rbindlist(lapply(OUTCOME_variables, function(x) {
  return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]]
             )[, .(person_id, date, meaning_renamed)][, concept := x])}
))

setkey(D3_study_population_SAP1, person_id, entry_spell_category, cohort_exit_date)
setkey(concept_df, person_id, date, date)
concept_in_pop <- better_foverlaps(D3_study_population_SAP1, concept_df, by.x = key(D3_study_population_SAP1))

setorder(concept_in_pop, person_id, date)

out <- list()
for (i in names(meanings_of_this_study)) {
  out <- append(out, list(data.table(meaning_renamed = meanings_of_this_study[[i]], new = i)))
}
out <- data.table::rbindlist(out)

concept_in_pop[out, on = "meaning_renamed", meaning_renamed := i.new]

component_MS_long_term <- MergeFilterAndCollapse(list(concept_in_pop),
                                                 condition = "!is.na(person_id)",
                                                 strata = c("person_id", "concept", "meaning_renamed"),
                                                 summarystat = list(c("first", "date"), c("second", "date"),
                                                                    c("third", "date"), c("fourth", "date")))

