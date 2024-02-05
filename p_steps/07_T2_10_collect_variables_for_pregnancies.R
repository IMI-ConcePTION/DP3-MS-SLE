##%######################################################%##
#                                                          #
####              CREATES COHORT PREGNANCY              ####
####            WITH ALL NECESSARY VARIABLES            ####
#                                                          #
##%######################################################%##

# TODO ask Rosa if D3_study_population_SAP1 is necessary
smart_load("D4_DU_MS_COHORT", diroutput, extension = extension)
smart_load("D4_DU_PREGNANCY_COHORT", diroutput, extension = extension)

# Keep only variables needed to be added and primary key
D4_DU_MS_COHORT <- D4_DU_MS_COHORT[, .(person_id, date_MS)]

# Add information of MS to pregnancies
pregnancy_variables <- merge(D4_DU_PREGNANCY_COHORT, D4_DU_MS_COHORT, all.x = T, by = "person_id")
pregnancy_variables[, has_MS_ever := data.table::fifelse(is.na(date_MS), 0, 1)]

# Variable to store when the MS diagnosis happened wrt pregnancy
pregnancy_variables[, pregnancy_with_MS_detail := data.table::fcase(
  date_MS > DU_pregnancy_study_exit_date, "long after pregnancy",
  date_MS > pregnancy_end_date & date_MS <= DU_pregnancy_study_exit_date, "right after pregnancy",
  date_MS >= pregnancy_start_date & date_MS <= pregnancy_end_date, "during pregnancy",
  date_MS >= pregnancy_start_date %m-% months(3) & date_MS < pregnancy_start_date, "right before pregnancy",
  date_MS >= DU_pregnancy_study_entry_date & date_MS < pregnancy_start_date %m-% months(3), "recently before pregnancy",
  date_MS < DU_pregnancy_study_entry_date, "long before pregnancy",
  default = "no")]

# Check if pregnancy is entirely exposed to MS
# TODO check with Marie
pregnancy_variables[, pregnancy_with_MS := fifelse(pregnancy_with_MS_detail == "long before pregnancy" |
                                                     (pregnancy_with_MS_detail == "recently before pregnancy" &
                                                        thisdatasource %in% datasources_only_preg), 1, 0)]

# Check if MS diagnosed during pregnancy
# TODO check with Rosa, not sure about this criteria
pregnancy_variables[, MS_developed_during_pregnancy := fifelse(pregnancy_with_MS_detail != "no"  &
                                                                 pregnancy_with_MS == 0, 1, 0)]

# Number of pregnancy in study
# TODO tell Rosa description is codebook is not optimal
pregnancy_variables[, number_of_pregnancies_in_the_study := .N, by = "person_id"]

# Number of pregnancy with MS in study
pregnancy_variables[, number_of_pregnancies_with_MS_in_the_study := sum(pregnancy_with_MS), by = "person_id"]

# Definition of start and end of periods
# TODO check if Marie has left notes for start_preg_period_during_2 ... end_preg_period_during_3
pregnancy_variables[, start_preg_period_pre_4 := pregnancy_start_date %m-% days(365)]
pregnancy_variables[, end_preg_period_pre_4 := pregnancy_start_date %m-% days(275)]
pregnancy_variables[, start_preg_period_pre_3 := pregnancy_start_date %m-% days(274)]
pregnancy_variables[, end_preg_period_pre_3 := pregnancy_start_date %m-% days(183)]
pregnancy_variables[, start_preg_period_pre_2 := pregnancy_start_date %m-% days(182)]
pregnancy_variables[, end_preg_period_pre_2 := pregnancy_start_date %m-% days(91)]
pregnancy_variables[, start_preg_period_pre_1 := pregnancy_start_date %m-% days(90)]
pregnancy_variables[, end_preg_period_pre_1 := pregnancy_start_date %m-% days(1)]
pregnancy_variables[, start_preg_period_during_1 := pregnancy_start_date]
pregnancy_variables[, end_preg_period_during_1 := min(pregnancy_start_date %m+% days(97), pregnancy_end_date)]
pregnancy_variables[, start_preg_period_during_2 := min(pregnancy_start_date %m+% days(98), pregnancy_end_date)]
pregnancy_variables[, end_preg_period_during_2 := min(pregnancy_start_date %m+% days(195), pregnancy_end_date)]
pregnancy_variables[, start_preg_period_during_3 := min(pregnancy_start_date %m+% days(196), pregnancy_end_date)]
pregnancy_variables[, end_preg_period_during_3 := pregnancy_end_date]
pregnancy_variables[, start_preg_period_after_1 := pregnancy_end_date %m+% days(1)]
pregnancy_variables[, end_preg_period_after_1 := pregnancy_end_date %m+% days(90)]
pregnancy_variables[, start_preg_period_pre_all := start_preg_period_pre_4]
pregnancy_variables[, end_preg_period_pre_all := end_preg_period_pre_1]

# Save the file
smart_save(pregnancy_variables, dirtemp, override_name = "D3_DU_PREGNANCY_COHORT_variables",
           extension = extension, save_copy = "csv")
