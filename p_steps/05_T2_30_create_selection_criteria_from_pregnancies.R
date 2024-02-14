##%######################################################%##
#                                                          #
####   CREATE EXCLUSION CRITERIA FOR PREGNANCY COHORT   ####
#                                                          #
##%######################################################%##

load(paste0(dirpregnancy, "D3_pregnancy_final.Rdata"))

# Keep only necessary columns
pregnancy_df <- D3_pregnancy_final[, c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date",
                                       "type_of_pregnancy_end", "PROMPT")]

smart_load("D3_study_population_SAP1", dirtemp, extension = extension)

# Keep only necessary columns
sap1_pop <- D3_study_population_SAP1[, c("person_id", "entry_spell_category", "birth_date",
                                         "cohort_entry_date", "cohort_exit_date")]

# INNER JOIN between SAP1 population and pregnancies
selection_criteria <- merge(sap1_pop, pregnancy_df, all.x = F, all.y = F, by = "person_id")
setcolorder(selection_criteria, c("pregnancy_id", "person_id", "entry_spell_category", "birth_date",
                                  "pregnancy_start_date", "pregnancy_end_date", "type_of_pregnancy_end", "PROMPT",
                                  "cohort_entry_date", "cohort_exit_date"))

# Create DU entry and exit date
prior_data_avalaibility <- data.table::fcase(thisdatasource == "EFEMERIS", days(78),
                                             thisdatasource %in% c("THL", "FISABIO"), months(3),
                                             default, years(1))
selection_criteria[, DU_pregnancy_study_entry_date := pregnancy_start_date %m-% prior_data_avalaibility]
if (thisdatasource == "EFEMERIS") {
  selection_criteria[, DU_pregnancy_study_exit_date := pregnancy_end_date]
} else {
  selection_criteria[, DU_pregnancy_study_exit_date := pregnancy_start_date %m+% months(3)]
}


# TODO add here filter for pregnancies quality
selection_criteria[, EXCLUSION_1_pregnancy_with_inappropriate_quality := 0]

# Criteria for pregnancies before 15th and after 50th birthday
selection_criteria[, EXCLUSION_2_pregnancy_not_in_fertile_age := fifelse((DU_pregnancy_study_entry_date < birth_date %m+% years(15)) |
                                                                           (DU_pregnancy_study_exit_date >= birth_date %m+% years(50)), 1, 0)]

# Criteria for pregnancies outside study period
selection_criteria[, EXCLUSION_3_pregnancy_not_in_study_period := fifelse(DU_pregnancy_study_entry_date < study_start |
                                                                            DU_pregnancy_study_exit_date > study_end, 1, 0)]

# Criteria for pregnancies outside study period
selection_criteria[, EXCLUSION_4_pregnancy_outside_period_with_medicines := fifelse(DU_pregnancy_study_entry_date < cohort_entry_date |
                                                                                       DU_pregnancy_study_exit_date > cohort_exit_date, 1, 0)]

smart_save(selection_criteria, dirtemp, override_name = "D3_DU_selection_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT",
           extension = extension, save_copy = "csv")
