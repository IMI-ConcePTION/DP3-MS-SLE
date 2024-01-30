##%######################################################%##
#                                                          #
####    CREATE BASIC VARIABLES FOR STUDY POPULATION     ####
#                                                          #
##%######################################################%##

# Load person_id in study_populations
smart_load("D4_study_population_SAP1", diroutput, extension = extension)

# Import the spells and clean
smart_load("D3_clean_spells", dirtemp, extension = extension)
D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, birth_date,
                                       is_the_study_spell)]
D3_clean_spells <- D3_clean_spells[is_the_study_spell == 1, ][, is_the_study_spell := NULL]

# Merge all datasets
D3_study_population_SAP1 <- merge(D4_study_population_SAP1, D3_clean_spells, all.x = T, by = "person_id")

# Calculate cohort entry and exit date (censor for age and study_start)
D3_study_population_SAP1[, cohort_entry_date := pmax(entry_spell_category, study_start, birth_date + ceiling(15 * 365.25))]
D3_study_population_SAP1[, cohort_exit_date := pmin(exit_spell_category, birth_date + floor(50 * 365.25) - 1)]
D3_study_population_SAP1[, exit_spell_category := NULL]

# Calculate age at start of spells
D3_study_population_SAP1[, age_at_entry_spell_category := age_fast(birth_date, cohort_entry_date)]

# Calculate the length of the spell in years
D3_study_population_SAP1[, for_n_years_in_study := age_fast(cohort_entry_date, cohort_exit_date + 1)]

# Save the dataset
smart_save(D3_study_population_SAP1, dirtemp, extension = extension, save_copy = "csv")
