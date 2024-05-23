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

# TODO remove for release
# medications <- rbindlist(lapply(medications$person_id, function(x) copy(medications)[, person_id := x]))
# TODO add for release
D3_DU_PREGNANCY_COHORT_variables <- D3_DU_PREGNANCY_COHORT_variables[pregnancy_with_MS == 1,]

# Keep only necessary columns
pregnancy_df <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, start_preg_period_pre_all,
                                                     end_preg_period_pre_all, start_preg_period_during_1,
                                                     end_preg_period_during_1, start_preg_period_during_2,
                                                     end_preg_period_during_2, start_preg_period_during_3,
                                                     end_preg_period_during_3, trimester_when_pregnancy_ended)]

# Melt period columns for period before and trimester during pregnancies
colA = c("start_preg_period_pre_all", paste0("start_preg_period_during_", 1:3))
colB = c("end_preg_period_pre_all", paste0("end_preg_period_during_", 1:3))
pregnancy_df = melt(pregnancy_df, measure = list(colA, colB), variable.name = "period_name",
                    value.name = c("start_period", "end_period"), na.rm = T, variable.factor = F)

pregnancy_df[.(period_name = as.character(1:4), to = c("number_before_pregnancy", "number_tri_1",
                                                       "number_tri_2", "number_tri_3")),
             on = "period_name", period_name := i.to]

# Join medication to corresponding time period
all_cols <- c(union(colnames(pregnancy_df), colnames(medications)), "y.start_period")
med_in_preg <- medications[pregnancy_df, .(person_id, pregnancy_id, trimester_when_pregnancy_ended, period_name,
                                           start_period, end_period, date = x.date, concept),
                           on = .(person_id, date >= start_period, date <= end_period), allow.cartesian = T]

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

# Select, rename and reorder columns
med_in_preg <- med_in_preg[, .(pregnancy_id, medication_label = concept, use_before_pregnancy, number_before_pregnancy,
                               trimester_when_pregnancy_ended, use_tri_1, use_tri_2, use_tri_3, number_tri_1,
                               number_tri_2, number_tri_3)]

# Save the file
smart_save(med_in_preg, dirtemp, override_name = "D4_DU_individual_prevalence_of_use_MSmeds_in_MSpregnancy_trimesters",
           extension = extension, save_copy = "csv")
