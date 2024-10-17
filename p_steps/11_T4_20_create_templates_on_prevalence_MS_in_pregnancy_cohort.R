##%######################################################%##
#                                                          #
####             GENERATE TEMPLATE 4 AND 5              ####
#                                                          #
##%######################################################%##

# Load dataset of pregnancies and medicines conceptsets
smart_load("D4_DU_prevalence_MS_in_pregnancy_cohort", diroutput, extension = extension)

# Rename dataset
prevalence_preg <- D4_DU_prevalence_MS_in_pregnancy_cohort

# Select necessary levels
prevalence_preg <- prevalence_preg[CalendarTime_level_order %in% c(1, 99) & Ageband_level_order == 99, ]

# Recode 2005-2019 to "all"
prevalence_preg <- prevalence_preg[CalendarTime_level_order == 99, CalendarTime_label := "all"]

# Change names of columns
prevalence_preg <- prevalence_preg[, .(row_identifier = CalendarTime_label, n1 = numerator, n2 = denominator)]

# Calculate prevalence and CI
prevalence_preg[, c("n3", "n4", "n5") := lapply(Hmisc::binconf(n1, n2, return.df = T) * 1000, round, 10)]

# Generate masked dataset
prevalence_preg_mask <- copy(prevalence_preg)[, n1 := as.character(n1)]
prevalence_preg_mask[, n2 := as.character(n2)]
prevalence_preg_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
prevalence_preg_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(prevalence_preg, direxp, override_name = "D5_DU_for_Template_4", extension = extension, save_copy = "csv")
smart_save(prevalence_preg_mask, direxpmask, override_name = "D5_DU_for_Template_4_masked",
           extension = extension, save_copy = "csv")
smart_save(prevalence_preg_mask, direxpred, override_name = "D5_DU_for_Template_4_masked_simplified",
           extension = extension, save_copy = "csv")

# Load dataset of pregnancies and medicines conceptsets
smart_load("D4_DU_prevalence_MS_in_pregnancy_cohort", diroutput, extension = extension)

# Rename dataset
prevalence_preg <- D4_DU_prevalence_MS_in_pregnancy_cohort

# Select necessary levels
prevalence_preg <- prevalence_preg[CalendarTime_level_order %in% c(2, 99), ]

# Change names of columns
prevalence_preg <- prevalence_preg[, .(row_identifier_1 = CalendarTime_label, row_identifier_2 = Ageband_label,
                                       n1 = numerator, n2 = denominator)]

# Calculate prevalence and CI
prevalence_preg[, c("n3", "n4", "n5") := lapply(Hmisc::binconf(n1, n2, return.df = T) * 1000, round, 10)]

# Generate masked dataset
prevalence_preg_mask <- copy(prevalence_preg)[, n1 := as.character(n1)]
prevalence_preg_mask[, n2 := as.character(n2)]
prevalence_preg_mask[as.integer(n1) > 0 & as.integer(n1) < 5, c("n1", "n3", "n4", "n5") := list("<5", NA, NA, NA)]
prevalence_preg_mask[as.integer(n2) > 0 & as.integer(n2) < 5, c("n2", "n3", "n4", "n5") := list("<5", NA, NA, NA)]

# Save the file
smart_save(prevalence_preg, direxp, override_name = "D5_DU_for_Template_5", extension = extension, save_copy = "csv")
smart_save(prevalence_preg_mask, direxpmask, override_name = "D5_DU_for_Template_5_masked",
           extension = extension, save_copy = "csv")
smart_save(prevalence_preg_mask, direxpred, override_name = "D5_DU_for_Template_5_masked_simplified",
           extension = extension, save_copy = "csv")
