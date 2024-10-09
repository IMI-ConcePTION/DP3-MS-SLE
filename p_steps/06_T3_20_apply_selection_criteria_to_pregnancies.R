##%######################################################%##
#                                                          #
####            APPLY EXCLUSION CRITERIA TO             ####
####      CREATE STUDY POPULATION FOR PREGNANCIES       ####
#                                                          #
##%######################################################%##

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

# Create flowchart for adults and save D4_DU_PREGNANCY_COHORT
smart_load("D3_DU_selection_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT", dirtemp, extension = extension)
selection_criteria <- D3_DU_selection_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT

selected_population <- CreateFlowChart(
  dataset = selection_criteria,
  listcriteria = c("EXCLUSION_1_pregnancy_in_persons_of_non_female_gender", 
                   "EXCLUSION_2_pregnancy_with_inappropriate_quality", "EXCLUSION_3_pregnancy_not_in_fertile_age",
                   "EXCLUSION_4_pregnancy_not_in_study_period", "EXCLUSION_5_pregnancy_outside_period_with_medicines"),
  flowchartname = "Flowchart_exclusion_criteria")

# Find if a level contains at least a value to censor
tmp <- copy(Flowchart_exclusion_criteria)[, N := as.character(N)]
tmp <- tmp[as.integer(N) < summary_threshold & as.integer(N) > 0, N := "<5"]

# Save flowcharts
smart_save(tmp, direxpmask,
           override_name = "D5_DU_flowchart_exclusion_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT_masked",
           extension = "csv")
smart_save(tmp, direxpred,
           override_name = "D5_DU_flowchart_exclusion_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT_masked",
           extension = "csv")
smart_save(Flowchart_exclusion_criteria, direxp,
           override_name = "D5_DU_flowchart_exclusion_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT", extension = "csv")

# Save population
smart_save(selected_population[, .(pregnancy_id, person_id, entry_spell_category, birth_date, pregnancy_start_date,
                                   pregnancy_end_date, type_of_pregnancy_end, cohort_entry_date, cohort_exit_date,
                                   DU_pregnancy_study_entry_date, DU_pregnancy_study_exit_date)],
           diroutput, override_name = "D4_DU_PREGNANCY_COHORT", extension = extension, save_copy = "csv")


