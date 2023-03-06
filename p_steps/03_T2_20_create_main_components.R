# Create D3_main_components
# input: D3_study_population_SAP1, conceptset
# output: D3_clean_spells
# Load person_id in study_populations

smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
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
smart_save(meaning_occurences, direxp, override_name = "D5_meaning_occurences", extension = "csv")

update_vector("datasets_to_censor", dirpargen, "D5_meaning_occurences")
update_vector("variables_to_censor", dirpargen, c("count" = 5))


# Recode meaning with names used in the algorithms
outcome_df[out, on = "meaning_renamed", meaning_renamed := i.new]
outcome_df[is.na(meaning_renamed) | meaning_renamed %not in% names(meanings_of_this_study), meaning_renamed := "UNSPECIFIED"]

### IMPORTANT recurrent event for MS and SLE
# Create lag date
outcome_df[, date := as.integer(date)]
setorder(outcome_df, person_id, concept, meaning_renamed, date)
outcome_df[, diff_date := date - shift(date, 1, first(date), "lag"), by = c("person_id", "concept", "meaning_renamed")]
outcome_df[, date := as.Date(date, "1970-01-01")]

# Cumulative sum with treshold
sum_reset_at <- function(thresh) {
  function(x) {
    accumulate(x, ~if_else(.x + .y >= thresh, as.integer(0), .x + .y))
  }  
}

outcome_df <- outcome_df %>%
  tibble() %>% 
  group_by(person_id) %>% 
  mutate(diff_date = case_when(
    concept == "MS" ~ sum_reset_at(30)(diff_date),
    concept == "SLE" ~ sum_reset_at(28)(diff_date)
  )) %>%
  as.data.table()

# keep only value with cumulate == 0 (so first row or after threshold) or LONGTERM
outcome_df <- unique(outcome_df[diff_date == 0 | meaning_renamed == "LONGTERM", ][, diff_date := NULL])

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
# from multiple spell per person to single row
concept_in_pop[, c("cohort_entry_date", "cohort_exit_date") := list(min(cohort_entry_date, na.rm = T),
                                                                    max(cohort_exit_date, na.rm = T)), by = "person_id"]
concept_in_pop[, entry_spell_category := NULL]

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

smart_save(component_algo, dirtemp, override_name = "D3_main_components", extension = extension)
