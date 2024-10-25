##%######################################################%##
#                                                          #
####              CREATES COHORT PREGNANCY              ####
####            WITH ALL NECESSARY VARIABLES            ####
#                                                          #
##%######################################################%##

# ask Rosa if D3_study_population_SAP1 is necessary
smart_load("D4_DU_MS_COHORT", diroutput, extension = extension)
smart_load("D4_DU_PREGNANCY_COHORT", diroutput, extension = extension)

# Keep only variables needed to be added and primary key
D4_DU_MS_COHORT <- unique(D4_DU_MS_COHORT[, .(person_id, date_MS)])

# Add information of MS to pregnancies
pregnancy_variables <- merge(D4_DU_PREGNANCY_COHORT, D4_DU_MS_COHORT, all.x = T, by = "person_id")
pregnancy_variables[, has_MS_ever := data.table::fifelse(!is.na(date_MS), 1, 0)]

# Variable to store when the MS diagnosis happened wrt pregnancy
# TODO change using specific numbers

pregnancy_variables[, pregnancy_with_MS_detail := data.table::fcase(
  date_MS > DU_pregnancy_study_exit_date, "long after pregnancy",
  date_MS > pregnancy_end_date, "right after pregnancy",
  date_MS >= pregnancy_start_date, "during pregnancy",
  date_MS >= pregnancy_start_date %m-% days(90), "right before pregnancy",
  date_MS >= pregnancy_start_date %m-% days(365), "recently before pregnancy",
  date_MS < pregnancy_start_date %m-% days(365), "long before pregnancy",
  default = "no")]

# Check if pregnancy is exposed to MS
if (thisdatasource %in% datasources_only_preg) {
  pregnancy_variables[, pregnancy_with_MS := fifelse(
    pregnancy_with_MS_detail %in% c("long before pregnancy", "recently before pregnancy",
                                    "right before pregnancy", "during pregnancy"), 1, 0)]
} else {
  pregnancy_variables[, pregnancy_with_MS := fifelse(
    pregnancy_with_MS_detail %in% c("long before pregnancy", "recently before pregnancy",
                                    "right before pregnancy"), 1, 0)]
  pregnancy_variables[pregnancy_with_MS_detail %in% c("recently before pregnancy", "right before pregnancy"),
                      DU_pregnancy_study_entry_date := date_MS]
}

pregnancy_variables[, pregnancy_with_MS_extended := fifelse(
  pregnancy_with_MS_detail %in% c("long before pregnancy", "recently before pregnancy",
                                  "right before pregnancy", "during pregnancy"), 1, 0)]


# Check if MS diagnosed during pregnancy
# TODO check with Rosa, not sure about this criteria
# pregnancy_variables[, MS_developed_during_pregnancy := fifelse(pregnancy_with_MS_detail %not in% c("no", "long after pregnancy")  &
#                                                                  pregnancy_with_MS == 0, 1, 0)]

# Number of pregnancy in study
# TODO tell Rosa description is codebook is not optimal
pregnancy_variables[, number_of_pregnancies_in_the_study := .N, by = "person_id"]

# Number of pregnancy with MS in study
pregnancy_variables[, number_of_pregnancies_with_MS_in_the_study := sum(pregnancy_with_MS), by = "person_id"]


# Order data.table to identify not first pregnancies
setorder(pregnancy_variables, person_id, pregnancy_start_date)
pregnancy_variables[, has_previous_pregnancy := as.integer(rowid(person_id) != 1)]

# TODO ask Marie if ok (lower or upper inclusion?)
# Calculate time between pregnancies in months
# TODO change with specific numbers
pregnancy_variables[, time_since_previous_pregnancy := as.integer((pregnancy_start_date - shift(pregnancy_end_date))), by = "person_id"]

# Divide time between pregnancies in categories
pregnancy_variables[, categories_time_since_previous_pregnancy := data.table::fcase(
  time_since_previous_pregnancy <= 90, "Less than 3 months",
  time_since_previous_pregnancy <= 182, "Between 3 and 6 months",
  time_since_previous_pregnancy <= 365, "Between 6 and 12 months",
  time_since_previous_pregnancy <= 455, "Between 12 and 15 months",
  time_since_previous_pregnancy > 455, "More than 15 months")]

# Definition of start and end of periods
# TODO check if Marie has left notes for start_preg_period_during_2 ... end_preg_period_during_3

if (thisdatasource %in% datasources_only_preg) {
  pregnancy_variables[, c("start_preg_period_pre_4", "end_preg_period_pre_4", "start_preg_period_pre_3",
                          "end_preg_period_pre_3", "start_preg_period_pre_2", "end_preg_period_pre_2") := NA_Date_]
  if (thisdatasource == "EFEMERIS") {
    pregnancy_variables[, start_preg_period_pre_1 := pregnancy_start_date %m-% days(78)]
  } else{
    pregnancy_variables[, start_preg_period_pre_1 := pregnancy_start_date %m-% days(90)]
  }
  pregnancy_variables[, start_preg_period_pre_all := start_preg_period_pre_1]
} else {
  
  pregnancy_variables[, start_preg_period_pre_1 := fifelse(DU_pregnancy_study_entry_date != pregnancy_start_date,
                                                           pmax(pregnancy_start_date %m-% days(90),  DU_pregnancy_study_entry_date),
                                                           NA_Date_)]
  pregnancy_variables[, end_preg_period_pre_2 := data.table::fifelse(start_preg_period_pre_1 != DU_pregnancy_study_entry_date,
                                                                          pregnancy_start_date %m-% days(91), NA_Date_)]
  pregnancy_variables[, start_preg_period_pre_2 := fifelse(start_preg_period_pre_1 != DU_pregnancy_study_entry_date,
                                                           pmax(pregnancy_start_date %m-% days(182), DU_pregnancy_study_entry_date),
                                                           NA_Date_)]
  pregnancy_variables[, end_preg_period_pre_3 := data.table::fifelse(start_preg_period_pre_2 != DU_pregnancy_study_entry_date,
                                                                     pregnancy_start_date %m-% days(183), NA_Date_)]
  pregnancy_variables[, start_preg_period_pre_3 := fifelse(start_preg_period_pre_2 != DU_pregnancy_study_entry_date,
                                                           pmax(pregnancy_start_date %m-% days(274), DU_pregnancy_study_entry_date),
                                                           NA_Date_)]
  pregnancy_variables[, end_preg_period_pre_4 := data.table::fifelse(start_preg_period_pre_3 != DU_pregnancy_study_entry_date,
                                                                     pregnancy_start_date %m-% days(275), NA_Date_)]
  pregnancy_variables[, start_preg_period_pre_4 := fifelse(start_preg_period_pre_3 != DU_pregnancy_study_entry_date,
                                                           pmax(pregnancy_start_date %m-% days(365), DU_pregnancy_study_entry_date),
                                                           NA_Date_)]
  pregnancy_variables[, start_preg_period_pre_all := pmin(start_preg_period_pre_1, start_preg_period_pre_2,
                                                          start_preg_period_pre_3, start_preg_period_pre_4, na.rm = T)]
}

pregnancy_variables[, end_preg_period_pre_1 := data.table::fifelse(DU_pregnancy_study_entry_date != pregnancy_start_date,
                                                                   pregnancy_start_date %m-% days(1), NA_Date_)]
pregnancy_variables[, start_preg_period_during_1 := pregnancy_start_date]
pregnancy_variables[, end_preg_period_during_1 := pmin(pregnancy_start_date %m+% days(97), pregnancy_end_date)]
pregnancy_variables[, start_preg_period_during_2 := data.table::fifelse(end_preg_period_during_1 != pregnancy_end_date,
                                                            pregnancy_start_date %m+% days(98), NA_Date_)]
pregnancy_variables[, end_preg_period_during_2 := fifelse(end_preg_period_during_1 != pregnancy_end_date,
                                                          pmin(pregnancy_start_date %m+% days(195), pregnancy_end_date), NA_Date_)]
pregnancy_variables[, start_preg_period_during_3 := fifelse(!is.na(end_preg_period_during_2) & end_preg_period_during_2 != pregnancy_end_date,
                                                            pregnancy_start_date %m+% days(196), NA_Date_)]
pregnancy_variables[, end_preg_period_during_3 := fifelse(!is.na(end_preg_period_during_2) & end_preg_period_during_2 != pregnancy_end_date,
                                                          pregnancy_end_date, NA_Date_)]
pregnancy_variables[, start_preg_period_after_1 := fifelse(DU_pregnancy_study_exit_date != pregnancy_end_date,
                                                           pregnancy_end_date %m+% days(1), NA_Date_)]
pregnancy_variables[, end_preg_period_after_1 := fifelse(!is.na(start_preg_period_after_1), pmin(pregnancy_end_date %m+% days(90),
                                                                                                 DU_pregnancy_study_exit_date), NA_Date_)]
pregnancy_variables[, end_preg_period_pre_all := end_preg_period_pre_1]

# Save when pregnancies ended
pregnancy_variables[, trimester_when_pregnancy_ended := fcase(
  pregnancy_start_date %m+% days(97) >= pregnancy_end_date, "t1",
  pregnancy_start_date %m+% days(195) >= pregnancy_end_date, "t2",
  default = "t3"
)]

# Save the file
smart_save(pregnancy_variables, dirtemp, override_name = "D3_DU_PREGNANCY_COHORT_variables",
           extension = extension, save_copy = "csv")
