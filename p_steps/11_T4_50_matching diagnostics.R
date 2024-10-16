
smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)
smart_load("D4_DU_matched_MS_PREGNANCY_COHORT_to_MS_COHORT", diroutput, extension = extension)

full_preg_cohort <- D3_DU_PREGNANCY_COHORT_variables[pregnancy_with_MS == 1, ]
full_preg_cohort <- full_preg_cohort[, .(person_id, age_at_lmp = age_fast(birth_date, pregnancy_start_date),
                                         followup_length = correct_difftime(cohort_exit_date, cohort_entry_date),
                                         days_from_MS = correct_difftime(pregnancy_start_date, date_MS),
                                         pregnancy_start_date)]

match_equi_pregs <- D4_DU_matched_MS_PREGNANCY_COHORT_to_MS_COHORT[is_pregnancy == 1, ]
match_equi_pregs <- match_equi_pregs[, .(person_id, age_at_lmp = age_fast(birth_date, start_preg_period_during_1_MS_pregnancy_id),
                                         followup_length = correct_difftime(cohort_exit_date, cohort_entry_date),
                                         days_from_MS = correct_difftime(start_preg_period_during_1_MS_pregnancy_id, date_MS),
                                         pregnancy_start_date = start_preg_period_during_1_MS_pregnancy_id)]

full_df <- rbindlist(list(full_preg_cohort[, cohort := "MS_pregnancy"], match_equi_pregs[, cohort := "matched"]))
full_df[, followup_length := as.integer(followup_length)]
full_df[, days_from_MS := as.integer(days_from_MS)]
full_df[, year_at_LMP := cut(year(pregnancy_start_date), breaks = c(2005, 2010, 2015, 2020),
                             labels = c("2005-2009", "2010-2014", "2015-2019"), right = F)]
full_df[, pregnancy_start_date := NULL]
full_df <- rbindlist(list(full_df, copy(full_df)[, year_at_LMP := "2005-2019"]))

match_diagnostics <- data.table()
full_df <- full_df[, .(N_preg = .N, median_age_lmp_p25 = round(quantile(age_at_lmp, 0.25), 0),
                       median_age_lmp = round(quantile(age_at_lmp, 0.50), 0),
                       median_age_lmp_p75 = round(quantile(age_at_lmp, 0.75), 0),
                       median_followup_p25 = round(quantile(followup_length, 0.25), 0),
                       median_followup = round(quantile(followup_length, 0.50), 0),
                       median_followup_p75 = round(quantile(followup_length, 0.75), 0),
                       median_distance_MS_lmp_p25 = round(quantile(days_from_MS, 0.25), 0),
                       median_distance_MS_lmp = round(quantile(days_from_MS, 0.50), 0),
                       median_distance_MS_lmp_p75 = round(quantile(days_from_MS, 0.75), 0)),
                   by = c("year_at_LMP", "cohort")]

# Save the file
smart_save(full_df, direxp, override_name = "Description_matched_preg_DU_MS",
           extension = extension, save_copy = "csv")
# Save the file
smart_save(full_df, direxpmask, override_name = "Description_matched_preg_DU_MS",
           extension = extension, save_copy = "csv")
# Save the file
smart_save(full_df, direxpred, override_name = "Description_matched_preg_DU_MS",
           extension = extension, save_copy = "csv")
