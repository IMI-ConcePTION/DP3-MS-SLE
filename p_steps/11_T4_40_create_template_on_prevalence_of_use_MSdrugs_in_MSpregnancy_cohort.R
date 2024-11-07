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

setorder(prev_MS_preg_cohort, is_pregnancy, row_identifier_1_order, row_identifier_1, row_identifier_2_order, row_identifier_2,
         row_identifier_3_order, row_identifier_3, column_identifier)

# Generate masked dataset
prev_MS_preg_cohort_mask <- copy(prev_MS_preg_cohort)[, n1 := as.character(n1)][, n2 := as.character(n2)]
prev_MS_preg_cohort_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
prev_MS_preg_cohort_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(prev_MS_preg_cohort, direxp, override_name = "D5_DU_for_Templates_8_11", extension = extension, save_copy = "csv")
smart_save(prev_MS_preg_cohort_mask, direxpmask, override_name = "D5_DU_for_Templates_8_11_masked",
           extension = extension, save_copy = "csv")

prev_MS_preg_cohort <- prev_MS_preg_cohort[!(n2 == 0 | row_identifier_2_order == 1), ]

# 0<x<15 in all age -> Do not stratify by ageband
prev_MS_preg_cohort_flag <- copy(prev_MS_preg_cohort)[row_identifier_3_order == 99 & row_identifier_2_order != 1, ]
prev_MS_preg_cohort_flag[, flag := fifelse(n2 < 15 & n2 >= 0, 1, 0)]
prev_MS_preg_cohort_flag <- unique(prev_MS_preg_cohort_flag[, c("row_identifier_3", "row_identifier_3_order") := NULL])
prev_MS_preg_cohort_flag <- prev_MS_preg_cohort_flag[, .(flag = max(flag)),
                                                     by = c("is_pregnancy", "row_identifier_1", "row_identifier_2", "column_identifier",
                                        "row_identifier_1_order", "row_identifier_2_order")]
prev_MS_preg_cohort_to_retain_1 <- prev_MS_preg_cohort_flag[copy(prev_MS_preg_cohort),
                                 on = c("is_pregnancy", "row_identifier_1", "row_identifier_2", "column_identifier",
                                        "row_identifier_1_order", "row_identifier_2_order")]
prev_MS_preg_cohort_to_retain_1 <- prev_MS_preg_cohort_to_retain_1[row_identifier_3_order == 99, flag := 0][is.na(flag) | flag == 0, ][, flag := NULL]

# 0<x<5 in one ageband -> Do not stratify by ageband
prev_MS_preg_cohort_flag <- copy(prev_MS_preg_cohort)[row_identifier_3_order == 1, ]
prev_MS_preg_cohort_flag[, flag := fifelse(n2 < 5 & n2 >= 0, 1, 0)]
prev_MS_preg_cohort_flag <- unique(prev_MS_preg_cohort_flag[, c("row_identifier_3") := NULL])
prev_MS_preg_cohort_flag <- prev_MS_preg_cohort_flag[, .(flag = max(flag)),
                                                     by = c("is_pregnancy", "row_identifier_1", "row_identifier_2",
                                                            "column_identifier", "row_identifier_1_order", "row_identifier_2_order", "row_identifier_3_order")]
prev_MS_preg_cohort_to_retain_2 <- prev_MS_preg_cohort_flag[copy(prev_MS_preg_cohort),
                                                         on = c("is_pregnancy", "row_identifier_1", "row_identifier_2", "column_identifier",
                                                                "row_identifier_1_order", "row_identifier_2_order", "row_identifier_3_order")]
prev_MS_preg_cohort_to_retain_2 <- prev_MS_preg_cohort_to_retain_2[is.na(flag) | flag == 0, ][, flag := NULL]

# 5Y period small number -> not stratify by 5y periods and ageband
prev_MS_preg_cohort_flag <- copy(prev_MS_preg_cohort)[row_identifier_3_order == 99 & row_identifier_2_order == 2, ]
prev_MS_preg_cohort_flag[, flag := fifelse(n2 < 5 & n2 >= 0, 1, 0)]
prev_MS_preg_cohort_flag <- unique(prev_MS_preg_cohort_flag[, "row_identifier_2" := NULL])
prev_MS_preg_cohort_flag <- prev_MS_preg_cohort_flag[, .(flag = max(flag)),
                                                     by = c("is_pregnancy", "row_identifier_1", "column_identifier",
                                                            "row_identifier_1_order", "row_identifier_2_order",
                                                            "row_identifier_3", "row_identifier_3_order")]
prev_MS_preg_cohort_to_retain_3 <- prev_MS_preg_cohort_flag[copy(prev_MS_preg_cohort),
                                                         on = c("is_pregnancy", "row_identifier_1", "column_identifier",
                                                                "row_identifier_1_order", "row_identifier_2_order",
                                                                "row_identifier_3", "row_identifier_3_order")]
prev_MS_preg_cohort_to_retain_3 <- prev_MS_preg_cohort_to_retain_3[is.na(flag) | flag == 0, ][, flag := NULL]

# 5Y period small number -> not stratify by 5y periods and ageband
prev_MS_preg_cohort_flag <- copy(prev_MS_preg_cohort)[row_identifier_3_order == 1 & row_identifier_2_order == 2, ]
prev_MS_preg_cohort_flag[, flag := fifelse(n2 < 5 & n2 >= 0, 1, 0)]
prev_MS_preg_cohort_flag <- unique(prev_MS_preg_cohort_flag[, c("row_identifier_2", "row_identifier_3") := NULL])
prev_MS_preg_cohort_flag <- prev_MS_preg_cohort_flag[, .(flag = max(flag)),
                                                     by = c("is_pregnancy", "row_identifier_1", "column_identifier",
                                                            "row_identifier_1_order", "row_identifier_2_order", "row_identifier_3_order")]
prev_MS_preg_cohort_to_retain_4 <- prev_MS_preg_cohort_flag[copy(prev_MS_preg_cohort),
                                                            on = c("is_pregnancy", "row_identifier_1", "column_identifier",
                                                                   "row_identifier_1_order", "row_identifier_2_order", "row_identifier_3_order")]
prev_MS_preg_cohort_to_retain_4 <- prev_MS_preg_cohort_to_retain_4[is.na(flag) | flag == 0, ][, flag := NULL]

prev_MS_preg_cohort_filtered <- prev_MS_preg_cohort_to_retain_1[copy(prev_MS_preg_cohort), on = colnames(prev_MS_preg_cohort), nomatch = NULL]
prev_MS_preg_cohort_filtered <- prev_MS_preg_cohort_to_retain_2[prev_MS_preg_cohort_filtered, on = colnames(prev_MS_preg_cohort), nomatch = NULL]
prev_MS_preg_cohort_filtered <- prev_MS_preg_cohort_to_retain_3[prev_MS_preg_cohort_filtered, on = colnames(prev_MS_preg_cohort), nomatch = NULL]
prev_MS_preg_cohort_filtered <- prev_MS_preg_cohort_to_retain_4[prev_MS_preg_cohort_filtered, on = colnames(prev_MS_preg_cohort), nomatch = NULL]


setcolorder(prev_MS_preg_cohort_filtered, c("is_pregnancy", "row_identifier_1", "row_identifier_2", "row_identifier_3",
                                   "column_identifier", "n1", "n2", "n3", "n4", "n5", "row_identifier_1_order",
                                   "row_identifier_2_order", "row_identifier_3_order"))

# Generate masked dataset
prev_MS_preg_cohort_filtered[, n1 := as.character(n1)][, n2 := as.character(n2)]
prev_MS_preg_cohort_filtered[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
prev_MS_preg_cohort_filtered[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

smart_save(prev_MS_preg_cohort_filtered, direxpred, override_name = "D5_DU_for_Templates_8_11_masked_simplified",
           extension = extension, save_copy = "csv")
