# Load dataset of pregnancies and medicines conceptsets
smart_load("D4_DU_prevalence_of_use_MSmeds_in_MSpregnancy_cohort", diroutput, extension = extension)

prev_MS_preg_cohort <- D4_DU_prevalence_of_use_MSmeds_in_MSpregnancy_cohort

prev_MS_preg_cohort <- prev_MS_preg_cohort[!(Calendartime_level_order == 1 & Age_level_order == 1), ]

# Change names of columns
prev_MS_preg_cohort <- prev_MS_preg_cohort[, .(is_pregnancy, row_identifier_1 = medication_label,
                                               row_identifier_2 = Calendartime_label, row_identifier_3 = Ageband_label,
                                               column_identifier = pregnancy_period_label,
                                               n1 = denom_preg_use, n2 = num_preg_use,
                                               row_identifier_1_order = medication_level_order,
                                               row_identifier_2_order = Calendartime_level_order,
                                               row_identifier_3_order = Age_level_order)]

# Calculate prevalence and CI
prev_MS_preg_cohort[, c("n3", "n4", "n5") := lapply(Hmisc::binconf(n2, n1, return.df = T) * 1000, round, 10)]

# Reorder columns
setcolorder(prev_MS_preg_cohort, c("is_pregnancy", "row_identifier_1", "row_identifier_2", "row_identifier_3",
                                   "column_identifier", "n1", "n2", "n3", "n4", "n5", "row_identifier_1_order",
                                   "row_identifier_2_order", "row_identifier_3_order"))

# TODO
stop()
# Following the meeting this morning, here are the rules for sorting the D5_DU_for_Templates_8_11 : The order of the sorting :
#   
#   is_pregnancy (0,1)
# row_identifier_1 (alphabetical order)
# row_identifier_2 (2005-2009,2010-2014,2015-2019,2005-2019)
# row_identifier_3 (15-24,25-29,30-34,35-39,40-49, AllAge)
# column_identifier (pre_1, pre_2, pre_3, pre_4, all_pre, during_1, during_2, during_3, all_during, after_1) you can delete the all-after category as it is exactly the same as after_1

# Generate masked dataset
prev_MS_preg_cohort_mask <- copy(prev_MS_preg_cohort)[, n1 := as.character(n1)][, n2 := as.character(n2)]
prev_MS_preg_cohort_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
prev_MS_preg_cohort_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(prev_MS_preg_cohort, direxp, override_name = "D5_DU_for_Templates_8_11", extension = extension, save_copy = "csv")
smart_save(prev_MS_preg_cohort_mask, direxpmask, override_name = "D5_DU_for_Templates_8_11",
           extension = extension, save_copy = "csv")
