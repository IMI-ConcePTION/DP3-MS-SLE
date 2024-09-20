smart_load("D4_DU_MS_COHORT", diroutput, extension = extension)

ms_cohort <- unique(D4_DU_MS_COHORT[, .(person_id, date_MS)])

smart_load("D3_DU_selection_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT", dirtemp, extension = extension)

preg_cohort_sel_crit <- D3_DU_selection_criteria_from_pregnancies_to_DU_PREGNANCY_COHORT
preg_cohort_sel_crit <- preg_cohort_sel_crit[, .(person_id, entry_spell_category, birth_date, DU_pregnancy_study_entry_date,
                                                 DU_pregnancy_study_exit_date, cohort_entry_date, cohort_exit_date)]

preg_cohort <- preg_cohort_sel_crit[ms_cohort, on = "person_id"]
preg_cohort <- preg_cohort[date_MS <= DU_pregnancy_study_exit_date, ]
preg_cohort <- preg_cohort[date_MS > DU_pregnancy_study_entry_date, DU_pregnancy_study_entry_date := date_MS]

ms_cohort_not_preg <- unique(D4_DU_MS_COHORT[person_id %not in% unique(preg_cohort[, person_id]),
                                             .(person_id, date_MS, entry_spell_category, birth_date, cohort_entry_date,
                                               cohort_exit_date, start_candidate_period = pmax(cohort_entry_date, date_MS),
                                               end_candidate_period = cohort_exit_date)])

# preg_cohort[, c("cohort_entry_date", "cohort_exit_date") := list(min(cohort_entry_date), min(cohort_exit_date)),
#             by = "person_id"]

setorder(preg_cohort, person_id, DU_pregnancy_study_entry_date)

#lagged end_date
preg_cohort[, lag_end_date := data.table::shift(DU_pregnancy_study_exit_date, fill = DU_pregnancy_study_exit_date[1L]),
            by = "person_id"]

# cumulative max for dates
preg_cohort[, lag_end_date := as.integer(lag_end_date)]
preg_cohort[, lag_end_date := cummax(lag_end_date), by = "person_id"]
preg_cohort[, lag_end_date := as.Date(lag_end_date, "1970-01-01")]

#compute the number of spell
preg_cohort[, num_spell := data.table::fifelse(data.table::rowid(person_id) > 1 & DU_pregnancy_study_entry_date <= lag_end_date + 1,
                                               0, 1)]
preg_cohort[, num_spell := cumsum(num_spell), by = "person_id"]
preg_cohort[, num_spell := as.integer(num_spell)]

#group by num spell and compute min and max date for each one
preg_cohort <- preg_cohort[, .(
  entry_spell_category = min(entry_spell_category), birth_date = birth_date[1L],
  date_MS = date_MS[1L], DU_pregnancy_study_entry_date = min(DU_pregnancy_study_entry_date),
  DU_pregnancy_study_exit_date = max(DU_pregnancy_study_exit_date),
  cohort_entry_date = min(cohort_entry_date),
  cohort_exit_date = max(cohort_exit_date)
),
by = c("person_id", "num_spell")]
preg_cohort[, num_spell := NULL]

# Keep only the first pregnancy to check if there is a need to add an initial period
initial_preg_cohort <- preg_cohort[data.table::rowid(person_id) == 1, ]
initial_preg_cohort[, start_candidate_period := pmax(date_MS, cohort_entry_date)]
initial_preg_cohort <- initial_preg_cohort[start_candidate_period < DU_pregnancy_study_entry_date, ]
initial_preg_cohort[, end_candidate_period := DU_pregnancy_study_entry_date - 1]

#lead start_date
preg_cohort[, DU_pregnancy_study_entry_date_lead := min(shift(DU_pregnancy_study_entry_date, type = "lead",
                                                              fill = cohort_exit_date[.N] + 1),
                                                        cohort_exit_date + 1), by = "person_id"]

# Calculate start_candidate_period and end_candidate_period
# KEEP <= since we may have periods of only 1 day
preg_cohort[DU_pregnancy_study_exit_date + 1 <= DU_pregnancy_study_entry_date_lead - 1,
            c("start_candidate_period", "end_candidate_period") := list(DU_pregnancy_study_exit_date + 1,
                                                                        DU_pregnancy_study_entry_date_lead - 1)]
preg_cohort[, DU_pregnancy_study_entry_date_lead := NULL]
preg_cohort <- preg_cohort[!is.na(start_candidate_period) & !is.na(end_candidate_period)]

complete_preg_cohort <- rbindlist(list(initial_preg_cohort, preg_cohort))
complete_preg_cohort[, c("DU_pregnancy_study_entry_date", "DU_pregnancy_study_exit_date") := NULL]
setcolorder(complete_preg_cohort, c("person_id", "date_MS", "entry_spell_category", "birth_date", "cohort_entry_date",
                                    "cohort_exit_date", "start_candidate_period", "end_candidate_period"))

complete_preg_cohort <- rbindlist(list(complete_preg_cohort, ms_cohort_not_preg))

# Save the file
smart_save(complete_preg_cohort, dirtemp, override_name = "D4_candidate_matches_MS_non_pregnant",
           extension = extension, save_copy = "csv")
