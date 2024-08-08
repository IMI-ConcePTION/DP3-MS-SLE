# Match D3_DU_MS_pregnancy_cohort to D3_DU_MS_cohort
# input: D3_DU_MS_pregnancy_cohort, D3_DU_MS_cohort
# output: D4_DU_matched_MS-PREGNANCY-COHORT_to_MS-COHORT

# from SAP 4.3.2 Cohort of matched non-pregnant women of childbearing age with MS

smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

full_preg_cohort <- unique(D3_DU_PREGNANCY_COHORT_variables[, c("entry_spell_category", "pregnancy_start_date",
                                                                "pregnancy_end_date", "type_of_pregnancy_end", "has_MS_ever",
                                                                "pregnancy_with_MS_detail") := NULL])

preg_ms_cohort <- full_preg_cohort[pregnancy_with_MS == 1, ]

# preg_info <- preg_ms_cohort[, c("cohort_entry_date", "cohort_exit_date", "date_MS", "pregnancy_id", "pregnancy_id", "pregnancy_id", "pregnancy_id", "pregnancy_id", "pregnancy_id", "pregnancy_id")]
small_preg_ms_cohort <- copy(full_preg_cohort)[, .(person_id, pregnancy_id, DU_pregnancy_study_entry_date, DU_pregnancy_study_exit_date)]

setnames(full_preg_cohort, c("start_preg_period_pre_4", "end_preg_period_pre_4", "start_preg_period_pre_3",
                             "end_preg_period_pre_3", "start_preg_period_pre_2", "end_preg_period_pre_2",
                             "start_preg_period_pre_1", "end_preg_period_pre_1", "start_preg_period_during_1",
                             "end_preg_period_during_1", "start_preg_period_during_2", "end_preg_period_during_2",
                             "start_preg_period_during_3", "end_preg_period_during_3", "start_preg_period_after_1",
                             "end_preg_period_after_1", "start_preg_period_pre_all", "end_preg_period_pre_all"),
         c("start_preg_period_pre_4_MS_pregnancy_id", "end_preg_period_pre_4_MS_pregnancy_id",
           "start_preg_period_pre_3_MS_pregnancy_id", "end_preg_period_pre_3_MS_pregnancy_id",
           "start_preg_period_pre_2_MS_pregnancy_id", "end_preg_period_pre_2_MS_pregnancy_id",
           "start_preg_period_pre_1_MS_pregnancy_id", "end_preg_period_pre_1_MS_pregnancy_id",
           "start_preg_period_during_1_MS_pregnancy_id", "end_preg_period_during_1_MS_pregnancy_id",
           "start_preg_period_during_2_MS_pregnancy_id", "end_preg_period_during_2_MS_pregnancy_id",
           "start_preg_period_during_3_MS_pregnancy_id", "end_preg_period_during_3_MS_pregnancy_id",
           "start_preg_period_after_1_MS_pregnancy_id", "end_preg_period_after_1_MS_pregnancy_id",
           "start_preg_period_pre_all_MS_pregnancy_id", "end_preg_period_pre_all_MS_pregnancy_id"))

preg_cohort <- copy(full_preg_cohort)
preg_cohort[, ord_match := 0]
preg_cohort[, is_pregnancy := 0]
setnames(preg_cohort, "pregnancy_id", "MS_pregnancy_id")
preg_cohort <- preg_cohort[, c("MS_pregnancy_id", "person_id", "birth_date", "ord_match", "is_pregnancy",
                               "cohort_entry_date", "cohort_exit_date", "date_MS", "DU_pregnancy_study_entry_date",
                               "DU_pregnancy_study_exit_date", "start_preg_period_pre_4_MS_pregnancy_id",
                               "end_preg_period_pre_4_MS_pregnancy_id", "start_preg_period_pre_3_MS_pregnancy_id",
                               "end_preg_period_pre_3_MS_pregnancy_id", "start_preg_period_pre_2_MS_pregnancy_id",
                               "end_preg_period_pre_2_MS_pregnancy_id", "start_preg_period_pre_1_MS_pregnancy_id",
                               "end_preg_period_pre_1_MS_pregnancy_id", "start_preg_period_during_1_MS_pregnancy_id",
                               "end_preg_period_during_1_MS_pregnancy_id", "start_preg_period_during_2_MS_pregnancy_id",
                               "end_preg_period_during_2_MS_pregnancy_id", "start_preg_period_during_3_MS_pregnancy_id",
                               "end_preg_period_during_3_MS_pregnancy_id", "start_preg_period_after_1_MS_pregnancy_id",
                               "end_preg_period_after_1_MS_pregnancy_id", "start_preg_period_pre_all_MS_pregnancy_id",
                               "end_preg_period_pre_all_MS_pregnancy_id")]

smart_load("D4_candidate_matches_MS_non_pregnant", dirtemp, extension = extension)

full_cand_match <- D4_candidate_matches_MS_non_pregnant

matched_pregnancies <- small_preg_ms_cohort[full_cand_match, .(pregnancy_id, i.person_id, x.DU_pregnancy_study_entry_date,
                                                                    start_candidate_period, x.DU_pregnancy_study_exit_date,
                                                                    end_candidate_period),
                                            on = .(DU_pregnancy_study_entry_date >= start_candidate_period,
                                                   DU_pregnancy_study_exit_date <= end_candidate_period),
                                            nomatch = NULL]
setnames(matched_pregnancies, c("x.DU_pregnancy_study_entry_date", "x.DU_pregnancy_study_exit_date"),
         c("DU_pregnancy_study_entry_date", "DU_pregnancy_study_exit_date"))

matched_pregnancies <- matched_pregnancies[sample(.N)]



matched_pregnancies[, index := 1:.N]
test <- matched_pregnancies[, .(i.person_id, DU_pregnancy_study_entry_date, DU_pregnancy_study_exit_date, index)]
setkeyv(test, c("i.person_id", "DU_pregnancy_study_entry_date", "DU_pregnancy_study_exit_date"))

test_overlap <- foverlaps(test, test)
test_overlap_card <- test_overlap[, .(cardinality = .N), by = c("index")]

# test_merged <- test_overlap_card[matched_pregnancies[, .(pregnancy_id, i.person_id, DU_pregnancy_study_entry_date,
#                                                          DU_pregnancy_study_exit_date, index)],
#                                  on = "index", allow.cartesian = TRUE]
test_merged <- matched_pregnancies[, cardinality := .N, by = "pregnancy_id"]

setcolorder(test_merged, c("pregnancy_id", "DU_pregnancy_study_entry_date", "DU_pregnancy_study_exit_date", "index",
                           "i.person_id"))
setorder(test_merged, cardinality, index)


my_wrapper <- function(x){
  tmp_vect <- vector("integer")
  function(x) {
    x <- x[index %not in% tmp_vect,]
    tmp <- x[x[, .I[sample(.N, min(4,.N))]]]
    tmp_vect <<- c(tmp_vect, test_overlap[index %in% tmp[, index], i.index])
    return(tmp)
  }
}
my_fun <- my_wrapper()

matched_cohort <- test_merged[, my_fun(.SD), by = "pregnancy_id"]
matched_cohort[, c("index", "cardinality") := NULL]
setnames(matched_cohort, "i.person_id", "person_id")
matched_cohort[, ord_match := 1:.N, by = "pregnancy_id"]
matched_cohort[, is_pregnancy := 1]

full_cand_match <- unique(full_cand_match[, .(person_id, date_MS, cohort_entry_date, cohort_exit_date, birth_date)])
matched_cohort <- full_cand_match[matched_cohort, on = "person_id"]

full_preg_cohort[, c("person_id", "cohort_entry_date", "cohort_exit_date", "date_MS", "pregnancy_with_MS",
                     "number_of_pregnancies_in_the_study", "number_of_pregnancies_with_MS_in_the_study",
                     "has_previous_pregnancy", "time_since_previous_pregnancy", "categories_time_since_previous_pregnancy",
                     "trimester_when_pregnancy_ended") := NULL]

matched_cohort <- full_preg_cohort[matched_cohort, on = c("pregnancy_id", "DU_pregnancy_study_entry_date",
                                                     "DU_pregnancy_study_exit_date")]

setnames(matched_cohort, "pregnancy_id", "MS_pregnancy_id")
matched_cohort <- matched_cohort[, c("MS_pregnancy_id", "person_id", "birth_date", "ord_match", "is_pregnancy",
                                     "cohort_entry_date", "cohort_exit_date", "date_MS", "DU_pregnancy_study_entry_date",
                                     "DU_pregnancy_study_exit_date", "start_preg_period_pre_4_MS_pregnancy_id",
                                     "end_preg_period_pre_4_MS_pregnancy_id", "start_preg_period_pre_3_MS_pregnancy_id",
                                     "end_preg_period_pre_3_MS_pregnancy_id", "start_preg_period_pre_2_MS_pregnancy_id",
                                     "end_preg_period_pre_2_MS_pregnancy_id", "start_preg_period_pre_1_MS_pregnancy_id",
                                     "end_preg_period_pre_1_MS_pregnancy_id", "start_preg_period_during_1_MS_pregnancy_id",
                                     "end_preg_period_during_1_MS_pregnancy_id", "start_preg_period_during_2_MS_pregnancy_id",
                                     "end_preg_period_during_2_MS_pregnancy_id", "start_preg_period_during_3_MS_pregnancy_id",
                                     "end_preg_period_during_3_MS_pregnancy_id", "start_preg_period_after_1_MS_pregnancy_id",
                                     "end_preg_period_after_1_MS_pregnancy_id", "start_preg_period_pre_all_MS_pregnancy_id",
                                     "end_preg_period_pre_all_MS_pregnancy_id")]

full_cohort <- rbindlist(list(matched_cohort, preg_cohort))

# Save the file
smart_save(full_cohort, diroutput, override_name = "D4_DU_matched_MS_PREGNANCY_COHORT_to_MS_COHORT",
           extension = extension, save_copy = "csv")
