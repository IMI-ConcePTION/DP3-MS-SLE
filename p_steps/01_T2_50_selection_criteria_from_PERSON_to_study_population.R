# -----------------------------------------------------
# CREATE EXCLUSION CRITERIA for persons/spells

# input: D3_PERSONS, OBSERVATION_PERIODS, output_spells_category
# output: D3_sel_cri

print('CREATE EXCLUSION CRITERIA FOR STUDY POPULATION')

smart_load("D3_PERSONS", dirtemp)

### Create the criteria based on D3_PERSONS. They are the same for adults and children populations.
# Remove persons with sex or birth day missing (recoded to year 9999)
D3_sel_cri <- D3_PERSONS[, sex_or_birth_date_is_not_defined := fifelse(
  is.na(sex_at_instance_creation) | sex_at_instance_creation == "U" | year(birth_date) == 9999, 1, 0)]

# Remove persons with  partial date of death
D3_sel_cri[, partial_date_of_death := fifelse(!is.na(death_date) & year(death_date) == 9999, 1, 0)]

# Remove persons with absurd date of birth
D3_sel_cri[, birth_date_absurd := fifelse(year(birth_date) < 1900 & birth_date > instance_creation, 1, 0)]

# Clean dataset
D3_sel_cri <- D3_sel_cri[, .(person_id, sex_or_birth_date_is_not_defined, partial_date_of_death, birth_date_absurd)]

smart_load("D3_clean_spells", dirtemp)

D3_clean_spells <- D3_clean_spells[, .(person_id, entry_spell_category, exit_spell_category, starts_after_ending,
                                       no_overlap_study_period, less_than_x_days_and_not_starts_at_birth,
                                       is_the_study_spell)]

# Creation of no_spells criteria
D3_sel_cri <- D3_sel_cri[, no_spells := fifelse(person_id %in% unlist(unique(D3_clean_spells[, .(person_id)])), 0, 1)]

# Creation of all_spells_start_after_ending criteria
D3_clean_spells[, tot_spell_num := .N, by = person_id]
D3_clean_spells[, tot_starts_after_ending := sum(starts_after_ending), by = person_id]
D3_clean_spells[, all_spells_start_after_ending := fifelse(tot_starts_after_ending == tot_spell_num, 1, 0)]
D3_clean_spells[, removed_row := starts_after_ending]
D3_clean_spells[, c("starts_after_ending", "tot_starts_after_ending", "tot_spell_num") := NULL]

# Creation of no_spell_overlapping_the_study_period criteria
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_no_overlap_study_period := sum(no_overlap_study_period), by = person_id]
D3_clean_spells[removed_row == 0, no_spell_overlapping_the_study_period := fifelse(
  tot_no_overlap_study_period == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "no_overlap_study_period")]
D3_clean_spells[, c("no_overlap_study_period", "tot_no_overlap_study_period", "tot_spell_num") := NULL]

# Creation of no_spell_longer_than_x_days. Keep other spells even if they are less than 365 days long
D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
D3_clean_spells[removed_row == 0, tot_x_days := sum(less_than_x_days_and_not_starts_at_birth), by = person_id]
D3_clean_spells[removed_row == 0, no_spell_longer_than_x_days := fifelse(tot_x_days == tot_spell_num, 1, 0)]
D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD),
                .SDcols = c("removed_row", "less_than_x_days_and_not_starts_at_birth")]
D3_clean_spells[, c("less_than_x_days_and_not_starts_at_birth", "tot_x_days", "tot_spell_num") := NULL]

# Creation of all_spells_include_vax1_but_less than_365_days_from_it
# D3_clean_spells[removed_row == 0, tot_spell_num := .N, by = person_id]
# D3_clean_spells[removed_row == 0, tot_less_than_365_days := sum(has_vax1_before_365_days), by = person_id]
# D3_clean_spells[removed_row == 0, all_spells_include_vax1_but_less_than_365_days_from_it := fifelse(
#   tot_less_than_365_days == tot_spell_num, 1, 0)]
# D3_clean_spells[removed_row == 0, removed_row := rowSums(.SD),
#                 .SDcols = c("removed_row", "has_vax1_before_365_days")]
# D3_clean_spells[, c("has_vax1_before_365_days", "tot_less_than_365_days", "tot_spell_num", "removed_row") := NULL]

# Keep only study spells chosen in 01_T2_043_clean_spells
study_spells <- D3_clean_spells[is_the_study_spell == 1, ][, .(person_id, entry_spell_category, exit_spell_category)]
study_spells <- unique(study_spells)
D3_sel_cri_temp<-merge(study_spells,D3_sel_cri,all.y = T,by="person_id")

# Keep only one row for each spell which syntethize the previously defined exclusion criteria
D3_clean_spells <- unique(D3_clean_spells[, c("entry_spell_category", "exit_spell_category",
                                              "is_the_study_spell") := NULL])
for (i in names(D3_clean_spells)) D3_clean_spells[is.na(get(i)), (i) := 0]
D3_clean_spells <- D3_clean_spells[, lapply(.SD, max), by = person_id]

# Add spells exclusion criteria to the one for person. Keep only persons which have a spell
D3_sel_cri_spells <- merge(D3_sel_cri_temp, D3_clean_spells,
                           all.y = T, by = "person_id")

# ### Create the criteria based on D3_vaccines_curated
# # Import doses dataset and create doses criteria
# load(paste0(dirtemp,"D3_vaccines_curated.RData"))
# 
# # Merge spells and vaccines
# spells_vaccines <- merge(study_spells, D3_vaccines_curated, all.x = T, by = "person_id")
# setorder(spells_vaccines, person_id, date_curated)

# # Find if each vaccination is inside the choosen spell for each person
# spells_vaccines[, vax_in_spell := fifelse(between(
#   date_curated, entry_spell_category, exit_spell_category), 1, 0, na = 0)]
# 
# # calculate order of vaccines inside the spell and compare whith the original dose number
# spells_vaccines[vax_in_spell == 1, seq_vax_in_spell := seq(.N), by = person_id]
# spells_vaccines[, higher_doses_included_but_lower_doses_missing := fifelse(
#   !is.na(seq_vax_in_spell) & dose_curated != seq_vax_in_spell, 1, 0), by = person_id]
# spells_vaccines[, c("date_curated", "dose_curated", "manufacturer_curated", "vax_in_spell", "seq_vax_in_spell") := NULL]
# spells_vaccines <- unique(spells_vaccines)
# 
# D3_sel_cri_spells_vaccines <- merge(D3_sel_cri_spells, spells_vaccines, all = T, by = "person_id")
D3_sel_cri_spells[, study_entry_date := pmax(entry_spell_category, start_lookback)]
D3_sel_cri_spells[, study_exit_date := pmin(exit_spell_category, study_end)]
D3_sel_cri_spells[, c("entry_spell_category", "exit_spell_category") := NULL]

# Saving exclusion criteria for populations
smart_save(D3_sel_cri_spells, dirtemp, override_name = "D3_selection_criteria_from_PERSONS_to_study_population")
