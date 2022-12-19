# CLEAN THE SPELLS

# input: D3_output_spells_category
# output: D3_clean_spells

smart_load("D3_PERSONS", dirtemp)

smart_load("D3_output_spells_category", dirtemp)

person_spell <- merge(D3_output_spells_category, D3_PERSONS, all.x = T, by = "person_id")

person_spell <- person_spell[, .(person_id, birth_date, death_date, entry_spell_category_crude = entry_spell_category,
                                 exit_spell_category_crude = exit_spell_category, op_meaning, num_spell)]

person_spell[, entry_spell_category := entry_spell_category_crude]
person_spell[, exit_spell_category := pmin(exit_spell_category_crude, death_date, na.rm = T)]

person_spell[, op_start_date_cleaned := data.table::fifelse(entry_spell_category != entry_spell_category_crude, 0, 1)]
person_spell[, op_end_date_cleaned := data.table::fifelse(exit_spell_category != exit_spell_category_crude, 0, 1)]

person_spell[, starts_after_ending := data.table::fifelse(entry_spell_category < exit_spell_category, 0, 1)]

# find spells that do not overlap the study period
person_spell[, no_overlap_study_period := fifelse(
  entry_spell_category > study_end | exit_spell_category < study_start, 1, 0)]

# find spells that are shorter than x days
person_spell[, spell_less_than_12_months_fup := fifelse(
  correct_difftime(pmin(exit_spell_category, study_end),
                   pmax(study_start, entry_spell_category)) <= min_spell_lenght, 1, 0)]


#TODO
#add additional criteria specific for the study (for example keep only the first spell for vaccinated)

# add a criteria that identify the specific spell of interest
person_spell[starts_after_ending == 0 & no_overlap_study_period == 0 & spell_less_than_12_months_fup == 0,
             is_the_study_spell := 1]

#alternative for dinamic cohort (example: if a subject enter after the study start it will be included with this alternative way)--------------------

# On the spells with still flag equal to 0 take the one with the minimum exit_spell_category after study_start
# person_spell[flag == 0 & exit_spell_category >= study_start,
#              min_exit_spell_category := min(exit_spell_category), by = person_id]
# 
# person_spell[exit_spell_category == min_exit_spell_category, is_the_study_spell := 1]
# person_spell[, c("min_exit_spell_category") := NULL]
# 
# person_spell[is.na(is_the_study_spell), is_the_study_spell := 0]
#----------------------------------

##add criteria to evaluate lookback 

smart_save(person_spell, dirtemp, override_name = "D3_clean_spells")
