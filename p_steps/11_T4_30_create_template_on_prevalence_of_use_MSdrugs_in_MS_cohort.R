##%######################################################%##
#                                                          #
####             GENERATE TEMPLATE 6 AND 7              ####
#                                                          #
##%######################################################%##

# Load dataset of pregnancies and medicines conceptsets
medication_preg <- smart_load("D4_DU_prevalence_of_use_MSdrugs_in_MS_cohort",
                              diroutput, extension = extension, return = T)

# Select necessary levels
medication_preg <- medication_preg[CalendarTime_level_order %in% c(1, 99) & Ageband_level_order == 99, ]

# Recode 2005-2019 to "all"
medication_preg <- medication_preg[CalendarTime_level_order == 99, CalendarTime_label := "all"]

# Change names of columns
medication_preg <- medication_preg[, .(row_identifier_1 = medication_label, row_identifier_2 = CalendarTime_label,
                                       n1 = numerator, n2 = denominator, row_identifier_1_order = medication_level_order,
                                       row_identifier_2_order = CalendarTime_level_order)]

# Calculate prevalence and CI
medication_preg[, c("n3", "n4", "n5") := lapply(Hmisc::binconf(n1, n2, return.df = T) * 1000, round, 10)]

# Reorder columns
setcolorder(medication_preg, c("row_identifier_1", "row_identifier_2", "n1", "n2", "n3", "n4", "n5",
                               "row_identifier_1_order", "row_identifier_2_order"))

# Generate masked dataset
medication_preg_mask <- copy(medication_preg)[, n1 := as.character(n1)]
medication_preg_mask[, n2 := as.character(n2)]
medication_preg_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
medication_preg_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(medication_preg, direxp, override_name = "D5_DU_for_Template_6", extension = extension, save_copy = "csv")
smart_save(medication_preg_mask, direxpmask, override_name = "D5_DU_for_Template_6_masked",
           extension = extension, save_copy = "csv")
smart_save(medication_preg_mask, direxpred, override_name = "D5_DU_for_Template_6_masked",
           extension = extension, save_copy = "csv")

# Load dataset of pregnancies and medicines conceptsets
medication_preg <- smart_load("D4_DU_prevalence_of_use_MSdrugs_in_MS_cohort",
                              diroutput, extension = extension, return = T)

# Select necessary levels
medication_preg <- medication_preg[CalendarTime_level_order %in% c(2, 99), ]

# Change names of columns
medication_preg <- medication_preg[, .(row_identifier_1 = medication_label, row_identifier_2 = CalendarTime_label,
                                       row_identifier_3 = Ageband_label, n1 = numerator, n2 = denominator,
                                       row_identifier_1_order = medication_level_order,
                                       row_identifier_2_order = CalendarTime_level_order,
                                       row_identifier_3_order = Ageband_level_order)]

# Calculate prevalence and CI
medication_preg[, c("n3", "n4", "n5") := lapply(Hmisc::binconf(n1, n2, return.df = T) * 1000, round, 10)]

# Reorder columns
setcolorder(medication_preg, c("row_identifier_1", "row_identifier_2", "row_identifier_3", "n1", "n2", "n3", "n4", "n5",
                               "row_identifier_1_order", "row_identifier_2_order", "row_identifier_3_order"))

# Generate masked dataset
medication_preg_mask <- copy(medication_preg)[, n1 := as.character(n1)]
medication_preg_mask[, n2 := as.character(n2)]
medication_preg_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
medication_preg_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(medication_preg, direxp, override_name = "D5_DU_for_Template_7", extension = extension, save_copy = "csv")
smart_save(medication_preg_mask, direxpmask, override_name = "D5_DU_for_Template_7_masked",
           extension = extension, save_copy = "csv")
smart_save(medication_preg_mask, direxpred, override_name = "D5_DU_for_Template_7_masked",
           extension = extension, save_copy = "csv")
