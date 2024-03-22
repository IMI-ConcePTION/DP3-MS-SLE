# Load dataset of pregnancies and medicines conceptsets
smart_load("D4_DU_prevalence_MS_in_pregnancy_cohort", diroutput, extension = extension)

prevalence_preg <- D4_DU_prevalence_MS_in_pregnancy_cohort

prevalence_preg <- prevalence_preg[CalendarTime_level_order %in% c(1, 99) & Ageband_level_order == 99, ]
prevalence_preg <- prevalence_preg[, .(row_identifier = CalendarTime_label, n1 = numerator, n2 = denominator)]

prevalence_preg[, c("n3", "n4", "n5") := lapply(Hmisc::binconf(n1, n2, return.df = T) * 1000, round, 10)]


