##%######################################################%##
#                                                          #
####   CREATE EXCLUSION CRITERIA FOR PREGNANCY COHORT   ####
#                                                          #
##%######################################################%##

load(paste0(dirpregnancy, "D3_pregnancy_final.Rdata"))

# Keep only necessary columns
pregnancy_df <- D3_pregnancy_final[, c("person_id", "pregnancy_id", "pregnancy_start_date", "pregnancy_end_date",
                                       "type_of_pregnancy_end", "PROMPT", "highest_quality", "EUROCAT")]

smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
smart_load("D3_persons", dirtemp, extension = extension)

# Keep only necessary columns
sap1_pop <- D3_study_population_SAP1[, c("person_id", "entry_spell_category", "birth_date",
                                         "cohort_entry_date", "cohort_exit_date")]
sex_pop <- D3_persons[, c("person_id", "sex_at_instance_creation")]

# INNER JOIN between SAP1 population and pregnancies
selection_criteria <- merge(sap1_pop, pregnancy_df, all.x = F, all.y = F, by = "person_id")
selection_criteria <- merge(selection_criteria, sex_pop, all.x = F, all.y = F, by = "person_id")
setcolorder(selection_criteria, c("pregnancy_id", "person_id", "entry_spell_category", "birth_date",
                                  "pregnancy_start_date", "pregnancy_end_date", "type_of_pregnancy_end", "PROMPT",
                                  "cohort_entry_date", "cohort_exit_date"))

# Create DU entry and exit date (DO NOT USE FCASE!!)
if (thisdatasource == "EFEMERIS") {
  prior_data_avalaibility <- lubridate::days(78)
} else if (thisdatasource %in% c("THL", "RDRU_FISABIO")) {
  prior_data_avalaibility <- lubridate::months(3)
} else {
  prior_data_avalaibility <- lubridate::years(1)
}

setorder(selection_criteria, person_id, cohort_entry_date, pregnancy_start_date)

selection_criteria[, lag_pregnancy_end_date := shift(pregnancy_end_date) + 1, by = c("person_id", "cohort_entry_date")]
selection_criteria[, DU_pregnancy_study_entry_date := pmax(pregnancy_start_date %m-% prior_data_avalaibility,
                                                           lag_pregnancy_end_date, na.rm = T)]
selection_criteria[, lag_pregnancy_end_date := NULL]

if (thisdatasource == "EFEMERIS") {
  selection_criteria[, DU_pregnancy_study_exit_date := pregnancy_end_date]
} else {
  # After Marie comment take always the three months after
  selection_criteria[, DU_pregnancy_study_exit_date := pregnancy_end_date %m+% months(3)]
  # selection_criteria[, lead_pregnancy_start_date := shift(pregnancy_start_date, type = "lead") - 1, by = "person_id"]
  # selection_criteria[, DU_pregnancy_study_exit_date := pmin(pregnancy_end_date %m+% months(3),
  #                                                           lead_pregnancy_start_date, na.rm = T)]
  # selection_criteria[, lead_pregnancy_start_date := NULL]
}

# Some pregnancies may comes from Males (gender)
selection_criteria[, EXCLUSION_1_pregnancy_in_persons_of_non_female_gender := fifelse(sex_at_instance_creation != "F", 1, 0)]
selection_criteria[, removed_row := EXCLUSION_1_pregnancy_in_persons_of_non_female_gender]

# TODO add here filter for pregnancies quality
selection_criteria[removed_row == 0, tot_preg_num := .N, by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0,
                   not_quality_preg := fcase(thisdatasource == "UOSL" & !((type_of_pregnancy_end %in% c("LB", "SB") & PROMPT == "yes") | type_of_pregnancy_end %in% c("SA", "T")), 1,
                                             thisdatasource == "FERR" & is.na(pregnancy_id), 1,
                                             thisdatasource == "RDRU_FISABIO" & !(PROMPT == "yes" & highest_quality %in% c("green", "yellow")), 1,
                                             thisdatasource == "UOSL" & !(EUROCAT == "yes" | (PROMPT == "yes" & highest_quality %in% c("green", "yellow"))), 1,
                                             default = 0)]
selection_criteria[removed_row == 0, tot_not_quality_preg := sum(not_quality_preg), by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, EXCLUSION_2_pregnancy_with_inappropriate_quality := fifelse(tot_not_quality_preg == tot_preg_num, 1, 0)]
selection_criteria[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "not_quality_preg")]
selection_criteria[, c("not_quality_preg", "tot_not_quality_preg", "tot_preg_num") := NULL]

# Criteria for pregnancies before 15th and after 50th birthday
# selection_criteria[removed_row == 0, EXCLUSION_3_pregnancy_not_in_fertile_age := fifelse((DU_pregnancy_study_entry_date < birth_date %m+% years(15)) |
#                                                                            (DU_pregnancy_study_exit_date >= birth_date %m+% years(50)), 1, 0)]
# selection_criteria[removed_row == 0, removed_row := rowSums(.SD),
#                    .SDcols = c("removed_row", "EXCLUSION_3_pregnancy_not_in_fertile_age")]
selection_criteria[removed_row == 0, tot_preg_num := .N, by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, preg_not_in_fertile_age := fifelse((DU_pregnancy_study_entry_date < birth_date %m+% years(15)) |
                                                                          (DU_pregnancy_study_exit_date >= birth_date %m+% years(50)), 1, 0)]
selection_criteria[removed_row == 0, tot_preg_not_in_fertile_age := sum(preg_not_in_fertile_age), by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, EXCLUSION_3_pregnancy_not_in_fertile_age := fifelse(tot_preg_not_in_fertile_age == tot_preg_num, 1, 0)]
selection_criteria[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "preg_not_in_fertile_age")]
selection_criteria[, c("preg_not_in_fertile_age", "tot_preg_not_in_fertile_age", "tot_preg_num") := NULL]

# Criteria for pregnancies outside study period
# selection_criteria[removed_row == 0, EXCLUSION_4_pregnancy_not_in_study_period := fifelse(DU_pregnancy_study_entry_date < study_start |
#                                                                             DU_pregnancy_study_exit_date > study_end, 1, 0)]
# selection_criteria[removed_row == 0, removed_row := rowSums(.SD),
#                    .SDcols = c("removed_row", "EXCLUSION_4_pregnancy_not_in_study_period")]
selection_criteria[removed_row == 0, tot_preg_num := .N, by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, preg_not_in_study_period := fifelse(DU_pregnancy_study_entry_date < study_start |
                                                                           DU_pregnancy_study_exit_date > study_end, 1, 0)]
selection_criteria[removed_row == 0, tot_preg_not_in_study_period := sum(preg_not_in_study_period), by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, EXCLUSION_4_pregnancy_not_in_study_period := fifelse(tot_preg_not_in_study_period == tot_preg_num, 1, 0)]
selection_criteria[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "preg_not_in_study_period")]
selection_criteria[, c("preg_not_in_study_period", "tot_preg_not_in_study_period", "tot_preg_num") := NULL]

# Criteria for pregnancies outside study period
# selection_criteria[removed_row == 0, EXCLUSION_5_pregnancy_outside_period_with_medicines := fifelse(DU_pregnancy_study_entry_date < cohort_entry_date |
#                                                                                       DU_pregnancy_study_exit_date > cohort_exit_date, 1, 0)]
# selection_criteria[removed_row == 0, removed_row := rowSums(.SD),
#                    .SDcols = c("removed_row", "EXCLUSION_5_pregnancy_outside_period_with_medicines")]
selection_criteria[removed_row == 0, tot_preg_num := .N, by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, preg_not_in_obs_period := fifelse(DU_pregnancy_study_entry_date < cohort_entry_date |
                                                                         DU_pregnancy_study_exit_date > cohort_exit_date, 1, 0)]
selection_criteria[removed_row == 0, tot_preg_not_in_obs_period := sum(preg_not_in_obs_period), by = c("person_id", "pregnancy_id")]
selection_criteria[removed_row == 0, EXCLUSION_5_pregnancy_outside_period_with_medicines := fifelse(tot_preg_not_in_obs_period == tot_preg_num, 1, 0)]
selection_criteria[removed_row == 0, removed_row := rowSums(.SD), .SDcols = c("removed_row", "preg_not_in_obs_period")]
selection_criteria[, c("preg_not_in_obs_period", "tot_preg_not_in_obs_period", "tot_preg_num") := NULL]

for (i in names(selection_criteria)) selection_criteria[is.na(get(i)), (i) := 0]

selection_criteria[, EXCLUSION_1_pregnancy_in_persons_of_non_female_gender := max(EXCLUSION_1_pregnancy_in_persons_of_non_female_gender),
                   by = c("person_id", "pregnancy_id")]
selection_criteria[, EXCLUSION_2_pregnancy_with_inappropriate_quality := max(EXCLUSION_2_pregnancy_with_inappropriate_quality),
                   by = c("person_id", "pregnancy_id")]
selection_criteria[, EXCLUSION_3_pregnancy_not_in_fertile_age := max(EXCLUSION_3_pregnancy_not_in_fertile_age),
                   by = c("person_id", "pregnancy_id")]
selection_criteria[, EXCLUSION_4_pregnancy_not_in_study_period := max(EXCLUSION_4_pregnancy_not_in_study_period),
                   by = c("person_id", "pregnancy_id")]
selection_criteria[, EXCLUSION_5_pregnancy_outside_period_with_medicines := max(EXCLUSION_5_pregnancy_outside_period_with_medicines),
                   by = c("person_id", "pregnancy_id")]

selection_criteria[, filter_helper := pmax(EXCLUSION_1_pregnancy_in_persons_of_non_female_gender,
                                           EXCLUSION_2_pregnancy_with_inappropriate_quality,
                                           EXCLUSION_3_pregnancy_not_in_fertile_age,
                                           EXCLUSION_4_pregnancy_not_in_study_period,
                                           EXCLUSION_5_pregnancy_outside_period_with_medicines,
                                           removed_row)]
selection_criteria <- selection_criteria[selection_criteria[, .I[filter_helper == max(filter_helper)], by = c("person_id", "pregnancy_id")]$V1]
selection_criteria <- selection_criteria[selection_criteria[, .I[cohort_entry_date == max(cohort_entry_date)], by = c("person_id", "pregnancy_id")]$V1]

smart_save(selection_criteria, dirtemp, override_name = "D3_DU_selection_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT",
           extension = extension, save_copy = "csv")
