##%######################################################%##
#                                                          #
####      CREATE EXCLUSION CRITERIA FOR MS COHORT       ####
#                                                          #
##%######################################################%##

smart_load("D3_SAP1_MS_COHORT", dirtemp, extension = extension)

# Remove persons without MS
selection_criteria <- D3_SAP1_MS_COHORT[, never_positive_for_MS_chosen := data.table::fifelse(is.na(date_MS), 1, 0)]

# Remove persons with diagnosed MS after their 50th birthday
selection_criteria[, women_diagnosed_outside_childbearing_age := data.table::fifelse(
  date_MS >= (lubridate::ymd(birth_date) %m+% years(50)), 1, 0)]

# Remove persons with less than 1 year of followup after diagnosis
selection_criteria[, women_with_less_than_1_year_fup := data.table::fifelse(
  difftime(cohort_exit_date, date_MS, units = "days") < 365, 1, 0)]

smart_save(selection_criteria, dirtemp, override_name = "D3_DU_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort",
           extension = extension, save_copy = "csv")
