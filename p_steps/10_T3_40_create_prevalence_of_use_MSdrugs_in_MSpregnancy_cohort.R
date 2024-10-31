##%######################################################%##
#                                                          #
####            CREATE PREVALENCE OF USE OF             ####
####             MS MEDICATION IN MS COHORT             ####
#                                                          #
##%######################################################%##

# Load dataset of pregnancies and medicines conceptsets
smart_load("D4_DU_matched_MS_PREGNANCY_COHORT_to_MS_COHORT", diroutput, extension = extension)

preg_matched_cohort <- D4_DU_matched_MS_PREGNANCY_COHORT_to_MS_COHORT
setkey(preg_matched_cohort, person_id, DU_pregnancy_study_entry_date, DU_pregnancy_study_exit_date)

cols <- c("DU_pregnancy_study_entry_date", "DU_pregnancy_study_exit_date", "end_preg_period_pre_4_MS_pregnancy_id",
          "start_preg_period_pre_3_MS_pregnancy_id", "end_preg_period_pre_3_MS_pregnancy_id",
          "start_preg_period_pre_2_MS_pregnancy_id", "end_preg_period_pre_2_MS_pregnancy_id",
          "start_preg_period_pre_1_MS_pregnancy_id", "end_preg_period_pre_1_MS_pregnancy_id",
          "start_preg_period_during_1_MS_pregnancy_id", "end_preg_period_during_1_MS_pregnancy_id",
          "start_preg_period_during_2_MS_pregnancy_id", "end_preg_period_during_2_MS_pregnancy_id",
          "start_preg_period_during_3_MS_pregnancy_id", "end_preg_period_during_3_MS_pregnancy_id",
          "start_preg_period_after_1_MS_pregnancy_id", "end_preg_period_after_1_MS_pregnancy_id")
preg_matched_cohort[ , (cols) := lapply(.SD, as.IDate), .SDcols = cols]
preg_matched_cohort[, Age := age_fast(birth_date, start_preg_period_during_1_MS_pregnancy_id)]
preg_matched_cohort[, Calendartime := year(start_preg_period_during_1_MS_pregnancy_id)]

medications <- rbindlist(lapply(concept_sets_of_our_study_DU, function(x) {
  return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]])[, .(person_id, date)][, concept := x])
}))
medications <- unique(medications)

# TODO remove for release
# medications <- rbindlist(lapply(medications$person_id, function(x) copy(medications)[, person_id := x]))

periods_of_time <- list(list("DU_pregnancy_study_entry_date", "end_preg_period_pre_4_MS_pregnancy_id"),
                        list("start_preg_period_pre_3_MS_pregnancy_id", "end_preg_period_pre_3_MS_pregnancy_id"),
                        list("start_preg_period_pre_2_MS_pregnancy_id", "end_preg_period_pre_2_MS_pregnancy_id"),
                        list("start_preg_period_pre_1_MS_pregnancy_id", "end_preg_period_pre_1_MS_pregnancy_id"),
                        list("start_preg_period_during_1_MS_pregnancy_id", "end_preg_period_during_1_MS_pregnancy_id"),
                        list("start_preg_period_during_2_MS_pregnancy_id", "end_preg_period_during_2_MS_pregnancy_id"),
                        list("start_preg_period_during_3_MS_pregnancy_id", "end_preg_period_during_3_MS_pregnancy_id"),
                        list("start_preg_period_after_1_MS_pregnancy_id", "end_preg_period_after_1_MS_pregnancy_id"))

# Calculate yearly prevalence of use of each medication
prevalence_of_use_yearly <- CountPrevalence(preg_matched_cohort, medications, UoO_id = c("person_id", "MS_pregnancy_id"),
                                            key = "person_id",
                                            Start_date = "DU_pregnancy_study_entry_date",
                                            End_date = "DU_pregnancy_study_exit_date",
                                            Name_condition = "concept",
                                            Date_condition = "date", Type_prevalence = "of use",
                                            Periods_of_time = periods_of_time,
                                            Start_study_time = max(recommended_start_date, study_start),
                                            End_study_time = study_end, Conditions = unique(medications[, concept]),
                                            Strata = c("is_pregnancy", "birth_date", "Age", "Calendartime"), Aggregate = F)

# In case of a pregnancy we should use the pregnancy_id as identifier
prevalence_of_use_yearly[, person_id := paste(person_id, MS_pregnancy_id, sep = "_")]
prevalence_of_use_yearly[, MS_pregnancy_id := NULL]

prevalence_of_use_yearly <- prevalence_of_use_yearly[in_population == 1, ]
prevalence_of_use_yearly[, c("DU_pregnancy_study_entry_date", "DU_pregnancy_study_exit_date", "birth_date", "in_population") := NULL]

trimesters_dates <- copy(preg_matched_cohort)
colA = c("start_preg_period_pre_4_MS_pregnancy_id", "start_preg_period_pre_3_MS_pregnancy_id",
         "start_preg_period_pre_2_MS_pregnancy_id", "start_preg_period_pre_1_MS_pregnancy_id", 
         "start_preg_period_during_1_MS_pregnancy_id", "start_preg_period_during_2_MS_pregnancy_id", 
         "start_preg_period_during_3_MS_pregnancy_id", "start_preg_period_after_1_MS_pregnancy_id")
colB = c("end_preg_period_pre_4_MS_pregnancy_id", "end_preg_period_pre_3_MS_pregnancy_id",
         "end_preg_period_pre_2_MS_pregnancy_id", "end_preg_period_pre_1_MS_pregnancy_id",
         "end_preg_period_during_1_MS_pregnancy_id", "end_preg_period_during_2_MS_pregnancy_id",
         "end_preg_period_during_3_MS_pregnancy_id", "end_preg_period_after_1_MS_pregnancy_id")
trimesters_dates[, start_preg_period_pre_4_MS_pregnancy_id := as.IDate(start_preg_period_pre_4_MS_pregnancy_id)]
trimesters_dates <- melt(trimesters_dates, measure = list(colA, colB),
                         value.name = c("start", "end"))

# In case of a pregnancy we should use the pregnancy_id as identifier
trimesters_dates[, person_id := paste(person_id, MS_pregnancy_id, sep = "_")]
trimesters_dates[, MS_pregnancy_id := NULL]

trimesters_dates <- trimesters_dates[, .(person_id, variable, start, end)]
trimesters_dates <- trimesters_dates[!is.na(start), ]

all_trimesters <- c("pre_4", "pre_3", "pre_2", "pre_1", "during_1", "during_2", "during_3", "after_1")

trimesters_dates[.(variable = as.factor(1:8), to = all_trimesters), on = "variable", variable := i.to]
setnames(trimesters_dates, "variable", "pregnancy_period")
trimesters_dates[, timeframe := paste(start, end, sep = "-")]
trimesters_dates[, c("start", "end") := NULL]

prevalence_of_use_yearly <- merge(prevalence_of_use_yearly, trimesters_dates, all.x = T, by = c("person_id", "timeframe"))
prevalence_of_use_yearly[, timeframe := NULL]

# Modify column names and add missing medications (as 0)
cols_to_add <- setdiff(paste0("prev_", concept_sets_of_our_study_DU), colnames(prevalence_of_use_yearly))
prevalence_of_use_yearly[, (cols_to_add) := 0L]
setnames(prevalence_of_use_yearly, paste0("prev_", concept_sets_of_our_study_DU), concept_sets_of_our_study_DU)
prevalence_of_use_yearly[ , (concept_sets_of_our_study_DU) := lapply(.SD, as.integer), .SDcols = concept_sets_of_our_study_DU]

# Create the category anydrug
prevalence_of_use_yearly[, anydrug := do.call(pmax, .SD), .SDcols = concept_sets_of_our_study_DU]

# Melt the medications
prevalence_of_use_yearly <- melt(prevalence_of_use_yearly, measure.vars = c(concept_sets_of_our_study_DU, "anydrug"),
                                 variable.name = "medication")

prevalence_of_use_yearly <- dcast(prevalence_of_use_yearly, person_id + is_pregnancy + Age + Calendartime +
                                    medication ~ pregnancy_period, value.var = "value", fun.aggregate = sum, fill = NA)

# Modify column names and add missing periods
cols_to_add <- setdiff(all_trimesters, colnames(prevalence_of_use_yearly))
prevalence_of_use_yearly[, (cols_to_add) := 0L]

prevalence_of_use_yearly[, all_pre := do.call(pmax, c(.SD, na.rm = T)), .SDcols = c("pre_4", "pre_3", "pre_2", "pre_1")]
prevalence_of_use_yearly[, all_during := do.call(pmax, c(.SD, na.rm = T)), .SDcols = c("during_1", "during_2", "during_3")]
prevalence_of_use_yearly[, all_after := do.call(pmax, c(.SD, na.rm = T)), .SDcols = c("after_1")]

# Melt the medications
prevalence_of_use_yearly <- melt(prevalence_of_use_yearly, measure.vars = c(all_trimesters, "all_pre", "all_during", "all_after"),
                                 variable.name = "pregnancy_period", na.rm = TRUE)

prevalence_of_use_yearly[, Calendartime1 := cut(Calendartime, c(2004, 2009, 2014, 2019),
                                                c("2005-2009", "2010-2014", "2015-2019"))]
prevalence_of_use_yearly[, Calendartime2 := "2005-2019"]

# Assign levels for Cube
assigned_levels <- vector(mode="list")
assigned_levels[["is_pregnancy"]] <- c("is_pregnancy")
assigned_levels[["Age"]] <- c("Age", "Ageband")
assigned_levels[["Calendartime"]] <- c("Calendartime", "Calendartime1", "Calendartime2")
assigned_levels[["pregnancy_period"]] <- c("pregnancy_period")
assigned_levels[["medication"]] <- c("medication")

assigned_statistics <- vector(mode="list")
assigned_statistics[["value"]] <- "sum"
assigned_statistics[["denom_preg_use"]] <- "sum"

assigned_rule <- vector(mode="list")
assigned_rule[["Age"]][["Ageband"]] <- list("split_in_bands", "Age", Agebands_LMP)

prevalence_of_use_yearly <- prevalence_of_use_yearly[, denom_preg_use := 1]

# TODO add anytry
# Calculate sums of usage and number of medication for each level. Calculate 
preg_med_ind <- Cube(input = copy(prevalence_of_use_yearly)[, person_id := NULL],
                     dimensions = c("is_pregnancy", "Age", "Calendartime", "pregnancy_period", "medication"),
                     levels = assigned_levels,
                     computetotal = c("Age"),
                     rule_from_numeric_to_categorical = assigned_rule,
                     measures = c("value", "denom_preg_use"),
                     statistics = assigned_statistics
)

preg_med_ind <- preg_med_ind[Age_LevelOrder != 1, ]
preg_med_ind[Age_LevelOrder == 2, Age_LevelOrder := 1]
preg_med_ind[Age_LevelOrder == "AllAge", Age_LevelOrder := "allAgeband"]
preg_med_ind[, medication_LevelOrder := fifelse(medication_LabelValue == "anydrug", 99, 1)]
preg_med_ind[, pregnancy_period_LevelOrder := fifelse(pregnancy_period_LabelValue %in% c("all_pre", "all_during", "all_after"),
                                                      99, 1)]
setnames(preg_med_ind, c("is_pregnancy_LabelValue", "medication_LabelValue", "medication_LevelOrder",
                         "Calendartime_LabelValue", "Calendartime_LevelOrder", "Age_LabelValue", "Age_LevelOrder",
                         "pregnancy_period_LabelValue", "pregnancy_period_LevelOrder", "value_sum", "denom_preg_use_sum"),
         c("is_pregnancy", "medication_label", "medication_level_order", "Calendartime_label",
           "Calendartime_level_order", "Ageband_label", "Age_level_order", "pregnancy_period_label",
           "pregnancy_period_level_order", "num_preg_use", "denom_preg_use"))
preg_med_ind[, is_pregnancy_LevelOrder := NULL]
setcolorder(preg_med_ind, c("is_pregnancy", "medication_label", "medication_level_order", "Calendartime_label",
                            "Calendartime_level_order", "Ageband_label", "Age_level_order", "pregnancy_period_label",
                            "pregnancy_period_level_order"))

# Save the file
smart_save(preg_med_ind, diroutput, override_name = "D4_DU_prevalence_of_use_MSmeds_in_MSpregnancy_cohort",
           extension = extension, save_copy = "csv")
