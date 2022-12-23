# Create D3_study_pop

# input: D3_output_spells_category
# output: D3_clean_spells

# Load person_id in study_populations
smart_load("D4_study_population_SAP1", diroutput)

# Load persons information
smart_load("D3_PERSONS", dirtemp)
D3_PERSONS <- D3_PERSONS[, .(person_id, birth_date)]

# Import the spells and clean
smart_load("D3_clean_spells", dirtemp)
D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, cohort_exit_date = exit_spell_category,
                                       is_the_study_spell)]
D3_clean_spells <- D3_clean_spells[is_the_study_spell == 1, ][, is_the_study_spell := NULL]
D3_clean_spells[, cohort_entry_date := pmax(entry_spell_category, study_start)]

# Merge all datasets
D3_study_population_SAP1 <- merge(D4_study_population_SAP1, D3_PERSONS, all.x = T, by = "person_id")
D3_study_population_SAP1 <- merge(D3_study_population_SAP1, D3_clean_spells, all.x = T, by = "person_id")

# Calculate age at start of spells
D3_study_population_SAP1[, age_at_entry_spell_category := age_fast(birth_date, cohort_entry_date)][, birth_date := NULL]

# Calculate the lenght of the spell in years and cut it in categories
D3_study_population_SAP1[, for_n_years_in_study := age_fast(cohort_entry_date, cohort_exit_date + 1)]

# Save the dataset
smart_save(D3_study_population_SAP1, dirtemp)
