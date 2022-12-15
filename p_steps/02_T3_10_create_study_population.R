#---------------------------------------------------------------
# Apply exclusion criteria to create study population 

# input: D3_selection_criteria_from_PERSONS_to_study_population, D3_selection_criteria_from_PERSONS_to_children_study_population
# output: Flowchart_exclusion_criteria_children, Flowchart_exclusion_criteria, D4_study_population, D4_children_study_population

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

# Create flowchart for adults and save D4_study_population
smart_load("D3_selection_criteria_from_PERSONS_to_study_population", dirtemp)
selection_criteria <- get("D3_selection_criteria_from_PERSONS_to_study_population")

selected_population <- CreateFlowChart(
  dataset = selection_criteria,
  listcriteria = c("sex_or_birth_date_is_not_defined", "birth_date_absurd", "partial_date_of_death", "no_spells",
                   "all_spells_start_after_ending", "no_spell_overlapping_the_study_period",
                   "no_spell_longer_than_x_days"),
  flowchartname = "Flowchart_exclusion_criteria")

fwrite(get("Flowchart_exclusion_criteria"),
       paste0(direxp, "Flowchart_exclusion_criteria"))

selected_population <- selected_population[, .(person_id, study_entry_date, study_exit_date)]

smart_save(selected_population, diroutput, override_name = "D4_study_population")
