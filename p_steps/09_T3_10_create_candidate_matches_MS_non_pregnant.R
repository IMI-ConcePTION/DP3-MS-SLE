smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

preg_cohort <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, entry_spell_category, birth_date, date_MS,
                                                    DU_pregnancy_study_entry_date, DU_pregnancy_study_exit_date, 
                                                    cohort_entry_date, cohort_exit_date, pregnancy_with_MS_detail)]
preg_cohort <- preg_cohort[pregnancy_with_MS_detail %in% c("long before pregnancy", "recently before pregnancy",
                                                           "right before pregnancy", "during pregnancy"), ]

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
  # entry_spell_category = min(entry_spell_category), birth_date = birth_date[1L],
                               date_MS = date_MS[1L], DU_pregnancy_study_entry_date = min(DU_pregnancy_study_entry_date),
                               DU_pregnancy_study_exit_date = max(DU_pregnancy_study_exit_date),
                               cohort_entry_date = min(cohort_entry_date),
                               cohort_exit_date = max(cohort_exit_date),
                               pregnancy_with_MS_detail = pregnancy_with_MS_detail[1L]
                               ),
                   by = c("person_id", "num_spell")]
preg_cohort[, num_spell := NULL]

# Keep only the first pregnancy to check if there is a need to add an initial period
initial_preg_cohort <- preg_cohort[data.table::rowid(person_id) == 1, ]
initial_preg_cohort[, start_candidate_period := pmax(date_MS, cohort_entry_date)]
initial_preg_cohort <- initial_preg_cohort[start_candidate_period < DU_pregnancy_study_entry_date, ]
initial_preg_cohort[, end_candidate_period := DU_pregnancy_study_entry_date - 1]

#lead start_date
preg_cohort[, DU_pregnancy_study_entry_date_lead := shift(DU_pregnancy_study_entry_date, type = "lead",
                                                          fill = cohort_exit_date[.N] + 1), by = "person_id"]
preg_cohort[, c("date_MS", "cohort_entry_date", "cohort_exit_date", "pregnancy_with_MS_detail") := NULL]

# Calculate start_candidate_period and end_candidate_period
# KEEP <= since we may have periods of only 1 day
preg_cohort[DU_pregnancy_study_exit_date + 1 <= DU_pregnancy_study_entry_date_lead - 1,
            c("start_candidate_period", "end_candidate_period") := list(DU_pregnancy_study_exit_date + 1,
                                                                        DU_pregnancy_study_entry_date_lead - 1)]




