# Load dataset of pregnancies and medicines conceptsets
smart_load("D4_DU_prevalence_of_use_MSmeds_in_MSpregnancy_cohort", diroutput, extension = extension)

prev_MS_preg_cohort <- D4_DU_prevalence_of_use_MSmeds_in_MSpregnancy_cohort

prev_MS_preg_cohort <- prev_MS_preg_cohort[!(Calendartime_level_order == 1 & Age_level_order == 1), ]
prev_MS_preg_cohort <- prev_MS_preg_cohort[pregnancy_period_label != "all_after", ]

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

prev_MS_preg_cohort[, column_identifier := factor(column_identifier,
                                                  levels = c("pre_1", "pre_2", "pre_3", "pre_4", "all_pre", "during_1",
                                                             "during_2", "during_3", "all_during", "after_1"))]

setorder(prev_MS_preg_cohort, is_pregnancy, row_identifier_1, row_identifier_2_order, row_identifier_2,
         row_identifier_3_order, row_identifier_3, column_identifier)

# Generate masked dataset
prev_MS_preg_cohort_mask <- copy(prev_MS_preg_cohort)[, n1 := as.character(n1)][, n2 := as.character(n2)]
prev_MS_preg_cohort_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
prev_MS_preg_cohort_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(prev_MS_preg_cohort, direxp, override_name = "D5_DU_for_Templates_8_11", extension = extension, save_copy = "csv")
smart_save(prev_MS_preg_cohort_mask, direxpmask, override_name = "D5_DU_for_Templates_8_11_masked",
           extension = extension, save_copy = "csv")

prev_MS_preg_cohort_mask <- prev_MS_preg_cohort_mask[!(n2 == "0" | row_identifier_2_order == 1), ]

# 0<x<15 in all age -> Do not stratify by ageband
# 5Y period small number -> not stratify by 5y periods and ageband
# 0<x<5 in one ageband -> Do not stratify by ageband

prev_MS_preg_cohort_to_remove_1 <- prev_MS_preg_cohort[row_identifier_2_order != 1 & row_identifier_3_order == 99 &
                                                         n2 < 15 & n2 >= 0, ]
prev_MS_preg_cohort_to_remove_2 <- prev_MS_preg_cohort[row_identifier_3_order == 1 & n2 < 5 & n2 >= 0, ]
prev_MS_preg_cohort_to_remove_3 <- prev_MS_preg_cohort[row_identifier_2_order == 2 & n2 < 5 & n2 >= 0, ]
prev_MS_preg_cohort_to_remove <- rbindlist(list(prev_MS_preg_cohort_to_remove_1, prev_MS_preg_cohort_to_remove_2,
                                                prev_MS_preg_cohort_to_remove_3))
prev_MS_preg_cohort_to_remove <- prev_MS_preg_cohort_to_remove[, .(is_pregnancy, row_identifier_1, row_identifier_2,
                                                                   row_identifier_3,column_identifier)]
prev_MS_preg_cohort_to_remove[, flag := 1]

prev_MS_preg_cohort_mask <- prev_MS_preg_cohort_to_remove[prev_MS_preg_cohort_mask,
                                                on = c("is_pregnancy", "row_identifier_1", "row_identifier_2",
                                                       "row_identifier_3", "column_identifier")]
prev_MS_preg_cohort_mask[is.na(flag), flag := 0]
prev_MS_preg_cohort_mask[, flag_max := max(flag), by = c("is_pregnancy", "row_identifier_1", "row_identifier_2",
                                                     "column_identifier")]
prev_MS_preg_cohort_mask <- prev_MS_preg_cohort_mask[row_identifier_3_order == 1 & flag_max == 1, flag_total := 1]

prev_MS_preg_cohort_mask[, flag_max := max(flag), by = c("is_pregnancy", "row_identifier_1", "row_identifier_3",
                                                         "column_identifier")]
prev_MS_preg_cohort_mask <- prev_MS_preg_cohort_mask[, flag_total := fifelse(row_identifier_2_order == 2 & flag_max == 1, 1, flag_total)]

prev_MS_preg_cohort_mask[, flag_max := max(flag), by = c("is_pregnancy", "row_identifier_1", "row_identifier_2",
                                                         "column_identifier")]
prev_MS_preg_cohort_mask <- prev_MS_preg_cohort_mask[, flag_total := fifelse(row_identifier_2_order != 1 & flag_max == 1, 1, flag_total)]

prev_MS_preg_cohort_mask[is.na(flag_total), flag := 0]
prev_MS_preg_cohort_mask <- prev_MS_preg_cohort_mask[flag == 0, ]
prev_MS_preg_cohort_mask[, c("flag", "flag_max", "flag_total") := NULL]

smart_save(prev_MS_preg_cohort_mask, direxpred, override_name = "D5_DU_for_Templates_8_11_masked_simplified",
           extension = extension, save_copy = "csv")
