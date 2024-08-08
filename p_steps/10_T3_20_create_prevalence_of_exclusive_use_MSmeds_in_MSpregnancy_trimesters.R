##%######################################################%##
#                                                          #
####    CHECK USE OF MS MEDICATION DURING PREGNANCY     ####
#                                                          #
##%######################################################%##

# Load dataset of pregnancies and medicines conceptsets
smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

medications <- rbindlist(lapply(concept_sets_of_our_study_DU, function(x) {
  return(get(load(paste0(dirconceptsets, x, ".Rdata"))[[1]])[, .(person_id, date)][, concept := x])
}))
medications <- unique(medications)

D3_DU_PREGNANCY_COHORT_variables <- D3_DU_PREGNANCY_COHORT_variables[pregnancy_with_MS == 1,]

# Keep only necessary columns
pregnancy_df <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, start_preg_period_pre_all,
                                                     end_preg_period_pre_all, start_preg_period_during_1,
                                                     end_preg_period_during_1, start_preg_period_during_2,
                                                     end_preg_period_during_2, start_preg_period_during_3,
                                                     end_preg_period_during_3, trimester_when_pregnancy_ended,
                                                     start_preg_period_pre_4, end_preg_period_pre_4,
                                                     start_preg_period_pre_3, end_preg_period_pre_3,
                                                     start_preg_period_pre_2, end_preg_period_pre_2,
                                                     start_preg_period_pre_1, end_preg_period_pre_1,
                                                     start_preg_period_after_1, end_preg_period_after_1)]

# Melt period columns for period before and trimester during pregnancies
colA = c("start_preg_period_pre_all", paste0("start_preg_period_during_", 1:3),
         paste0("start_preg_period_pre_", 1:4), "start_preg_period_after_1")
colB = c("end_preg_period_pre_all", paste0("end_preg_period_during_", 1:3),
         paste0("end_preg_period_pre_", 1:4), "end_preg_period_after_1")
pregnancy_df = melt(pregnancy_df, measure = list(colA, colB), variable.name = "period_name",
                    value.name = c("start_period", "end_period"), na.rm = T, variable.factor = F)

pregnancy_df[.(period_name = as.character(1:9), to = c("number_before_pregnancy", paste0("number_tri_", 1:3),
                                                       paste0("number_before_pregnancy_", 1:4),
                                                       "number_after_pregnancy")),
             on = "period_name", period_name := i.to]

# Join medication to corresponding time period
all_cols <- c(union(colnames(pregnancy_df), colnames(medications)), "y.start_period")

med_in_preg <- medications[pregnancy_df, .(person_id, pregnancy_id, trimester_when_pregnancy_ended, period_name,
                                           start_period, end_period, date = x.date, concept),
                           on = .(person_id, date >= start_period, date <= end_period), allow.cartesian = T]

med_in_preg[, n_rows := .N, by = c("person_id", "pregnancy_id")]
med_in_preg[is.na(date), n_NA := .N, by = c("person_id", "pregnancy_id")]
med_in_preg <- med_in_preg[is.na(n_NA) | (!is.na(n_NA) & n_rows == n_NA), ]
med_in_preg[, c("n_rows", "n_NA") := NULL]

# Remove person_id since it's not useful anymore
med_in_preg[, person_id := NULL]

# Calculate number of medicines in each period
med_in_preg <- med_in_preg[, .(use_n = .N),
                           by = c("pregnancy_id", "concept", "period_name", "trimester_when_pregnancy_ended")]
med_in_preg <- med_in_preg[is.na(concept), use_n := 0]

# Retransform periods in wide format
med_in_preg <- dcast(med_in_preg, pregnancy_id + concept + trimester_when_pregnancy_ended ~ period_name,
                     fill = 0, value.var = "use_n")

# Generate binary variable of usage in period
med_in_preg <- med_in_preg[, use_before_pregnancy := fifelse(number_before_pregnancy == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_tri_1 := fifelse(number_tri_1 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_tri_2 := fifelse(number_tri_2 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_tri_3 := fifelse(number_tri_3 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_before_pregnancy_1 := fifelse(number_before_pregnancy_1 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_before_pregnancy_2 := fifelse(number_before_pregnancy_2 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_before_pregnancy_3 := fifelse(number_before_pregnancy_3 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_before_pregnancy_4 := fifelse(number_before_pregnancy_4 == 0, 0, 1)]
med_in_preg <- med_in_preg[, use_after_pregnancy := fifelse(number_after_pregnancy == 0, 0, 1)]

# Select, rename and reorder columns
med_in_preg <- med_in_preg[, .(pregnancy_id, medication_label = concept, use_before_pregnancy, number_before_pregnancy,
                               trimester_when_pregnancy_ended, use_tri_1, use_tri_2, use_tri_3, number_tri_1,
                               number_tri_2, number_tri_3, use_before_pregnancy_1, use_before_pregnancy_2,
                               use_before_pregnancy_3, use_before_pregnancy_4, number_before_pregnancy_1,
                               number_before_pregnancy_2, number_before_pregnancy_3, number_before_pregnancy_4,
                               use_after_pregnancy, number_after_pregnancy)]

# Save the file
smart_save(med_in_preg, dirtemp, override_name = "D4_DU_individual_prevalence_of_use_MSmeds_in_MSpregnancy_trimesters",
           extension = extension, save_copy = "csv")
