#---------------------------------------------------------------
# Apply exclusion criteria to create study population 

# input: D3_selection_criteria_from_PERSONS_to_study_population, D3_selection_criteria_from_PERSONS_to_children_study_population
# output: Flowchart_exclusion_criteria_children, Flowchart_exclusion_criteria, D4_study_population, D4_children_study_population

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

# Create flowchart for adults and save D4_study_population
smart_load("D3_selection_criteria_from_PERSONS_to_study_population", dirtemp, extension = extension)
selection_criteria <- get("D3_selection_criteria_from_PERSONS_to_study_population")

selected_population <- CreateFlowChart(
  dataset = selection_criteria,
  listcriteria = c("sex_or_birth_date_is_not_defined", "not_female", "too_young_female", "too_old_female", "no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period",
                   "less_than_12_months_fup", "all_entry_spell_category_cleaned", "all_exit_spell_category_cleaned"),
  flowchartname = "Flowchart_exclusion_criteria")

fwrite(get("Flowchart_exclusion_criteria"),
       paste0(direxp, "Flowchart_exclusion_criteria.csv"))

smart_save(selected_population[, .(person_id)], diroutput, override_name = "D4_study_population_SAP1", extension = extension, save_copy = "csv")

update_vector("datasets_to_censor", dirpargen, "Flowchart_exclusion_criteria")
update_vector("variables_to_censor", dirpargen, c("N" = 5))

update_vector("datasets_to_censor_check", dirpargen, "Flowchart_exclusion_criteria")
