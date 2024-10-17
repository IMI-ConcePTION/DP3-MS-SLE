##%######################################################%##
#                                                          #
####                GENERATE TEMPLATE 3                 ####
#                                                          #
##%######################################################%##

preg_med_ind <- smart_load("D4_DU_prevalence_of_exclusive_use_MSmeds_in_MSpregnancy_trimesters", diroutput,
                           extension = extension, return = T)

# Calculate the total number of pregnancies in MS cohort, then join the result to the original dataset
preg_med_ind_total <- preg_med_ind[medication_label %in% c("missing", "anydrug") & before_pregnancy == "any" &
                                     during_pregnancy %in% c("notri", "anytri"), .(n0 = sum(numerator_preg_use)),
                                   by = c("trimester_when_pregnancy_ended")]
preg_med_ind <- preg_med_ind[preg_med_ind_total, on = "trimester_when_pregnancy_ended"]

# After calculating the denominator pregnancies without medications are not needed anymore
preg_med_ind <- preg_med_ind[medication_label != "missing", ]

# Cleaning for final table
preg_med_ind <- preg_med_ind[, .(row_identifier_1 = medication_label,
                                 row_identifier_2 = before_pregnancy,
                                 row_identifier_3 = during_pregnancy,
                                 row_identifier_4 = trimester_when_pregnancy_ended,
                                 n0, 
                                 n1 = as.character(numerator_preg_use),
                                 n2 = numerator_preg_use / n0 * 100,
                                 n3 = median_number_medications,
                                 row_identifier_1_order = medication_level_order,
                                 row_identifier_2_order = fifelse(before_pregnancy == "any", 99, 1),
                                 row_identifier_3_order = fifelse(during_pregnancy %in% c("anytri", "notri"), 99, 1),
                                 row_identifier_4_order = fcase(trimester_when_pregnancy_ended == "t1+t2", 2,
                                                                trimester_when_pregnancy_ended == "t1+t2+t3", 99,
                                                                default = 1))]
  
# Save the file
smart_save(preg_med_ind, direxp, override_name = "D5_DU_for_Template_3", extension = extension, save_copy = "csv")

preg_med_ind_mask <- copy(preg_med_ind)[between(as.numeric(n1), 1, 4), c("n1", "n2") := list("<5", NA)]
preg_med_ind_mask_simplified <- copy(preg_med_ind_mask)[!(row_identifier_2_order == 1 & row_identifier_3_order == 1), ]
preg_med_ind_mask_simplified <- preg_med_ind_mask_simplified[!(row_identifier_2_order == 99 & row_identifier_3_order == 99), ]
preg_med_ind_mask_simplified <- preg_med_ind_mask_simplified[row_identifier_4 == "t3", ]

smart_save(preg_med_ind_mask, direxpmask, override_name = "D5_DU_for_Template_3_masked",
           extension = extension, save_copy = "csv")

smart_save(preg_med_ind_mask_simplified, direxpred, override_name = "D5_DU_for_Template_3_masked_simplified",
           extension = extension, save_copy = "csv")

