##%######################################################%##
#                                                          #
####            APPLY EXCLUSION CRITERIA TO             ####
####       CREATE STUDY POPULATION FOR MS-COHORT        ####
#                                                          #
##%######################################################%##

print('FLOWCHART')

# USE THE FUNCTION CREATEFLOWCHART TO SELECT THE SUBJECTS IN POPULATION

# Create flowchart for adults and save D4_DU_MS_COHORT
smart_load("D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort", dirtemp, extension = extension)
selection_criteria <- D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort

selected_population <- CreateFlowChart(
  dataset = selection_criteria,
  listcriteria = c("never_positive_for_MS_chosen", "women_diagnosed_after_childbearing_age", "women_with_less_than_1_year_fup"),
  flowchartname = "Flowchart_exclusion_criteria")

# Find if a level contains at least a value to censor
tmp <- copy(Flowchart_exclusion_criteria)[, N := as.character(N)]
tmp <- tmp[as.integer(N) < summary_threshold & as.integer(N) > 0, N := "<5"]

# Save flowcharts
smart_save(tmp, direxpmask,
           override_name = "D5_DU_flowchart_exclusion_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort_masked",
           extension = "csv")
smart_save(tmp, direxpred,
           override_name = "D5_DU_flowchart_exclusion_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort_masked",
           extension = "csv")
smart_save(Flowchart_exclusion_criteria, direxp,
           override_name = "D5_DU_flowchart_exclusion_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort", extension = "csv")

# Save population
smart_save(selected_population[, .(person_id, date_MS, entry_spell_category, birth_date, cohort_entry_date, cohort_exit_date)],
           diroutput, override_name = "D4_DU_MS_COHORT", extension = extension, save_copy = "csv")


