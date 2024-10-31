##%######################################################%##
#                                                          #
####            CREATE PREVALENCE OF USE OF             ####
####             MS MEDICATION IN MS COHORT             ####
#                                                          #
##%######################################################%##

if (thisdatasource %not in% datasources_only_preg) {
  
  # Load dataset of pregnancies and medicines conceptsets
  smart_load("D4_DU_MS_COHORT", diroutput, extension = extension)
  
  medications <- rbindlist(lapply(concept_sets_of_our_study_DU, function(x) {
    return(get(load(paste0(dirconceptsets, x, ".RData"))[[1]])[, .(person_id, date)][, concept := x])
  }))
  medications <- unique(medications)
  
  # Calculate yearly prevalence of use of each medication
  prevalence_of_use_yearly <- CountPrevalence(D4_DU_MS_COHORT, medications, c("person_id"), Start_date = "cohort_entry_date",
                                              End_date = "cohort_exit_date", Birth_date = "birth_date",
                                              Name_condition = "concept", Date_condition = "date", Type_prevalence = "of use",
                                              Increment_period = "year",
                                              Start_study_time = max(recommended_start_date, study_start),
                                              End_study_time = study_end, Conditions = unique(medications[, concept]),
                                              include_remaning_ages = F, Age_bands = ageband_definition_level_1,
                                              Aggregate = F)
  
  # Extract year from timeframe and set CalendarTime level_order
  prevalence_of_use_yearly[, timeframe := as.integer(substr(timeframe, 1, 4))]
  prevalence_of_use_yearly[, CalendarTime_level_order := 1]
  
  # Calculate 5 years and whole period prevalence of use of each medication
  level_2_periods <- list(list("20050101", "20091231"),
                          list("20100101", "20141231"),
                          list("20150101", "20191231"))
  
  level_99_periods <- list(list("20050101", "20191231"))
  
  prevalence_of_use_5y_all <- CountPrevalence(D4_DU_MS_COHORT, medications, c("person_id"), Start_date = "cohort_entry_date",
                                              End_date = "cohort_exit_date", Birth_date = "birth_date",
                                              Name_condition = "concept", Date_condition = "date", Type_prevalence = "of use",
                                              Periods_of_time = c(level_2_periods, level_99_periods),
                                              Start_study_time = max(recommended_start_date, study_start),
                                              End_study_time = study_end, Conditions = unique(medications[, concept]),
                                              include_remaning_ages = F, Age_bands = ageband_definition_level_1,
                                              Aggregate = F)
  
  # Extract year from timeframe and set CalendarTime level_order
  prevalence_of_use_5y_all[, timeframe := paste(year(as.Date(substr(timeframe, 1, 10))),
                                                year(as.Date(substr(timeframe, 12, 21))), sep = "-")]
  prevalence_of_use_5y_all[, CalendarTime_level_order := fifelse(timeframe == "2005-2019", 99, 2)]
  
  # Combine prevalence datasets
  prevalence_of_use <- rbindlist(list(prevalence_of_use_yearly, prevalence_of_use_5y_all))
  
  # Keep only periods when persons are in population and remove unnecessary columns
  prevalence_of_use <- prevalence_of_use[in_population != 0, ]
  prevalence_of_use[, c("cohort_entry_date", "cohort_exit_date") := NULL, ]
  
  # Modify column names and add missing medications (as 0)
  cols_to_add <- setdiff(paste0("prev_", concept_sets_of_our_study_DU), colnames(prevalence_of_use))
  prevalence_of_use[, (cols_to_add) := 0L]
  setnames(prevalence_of_use, paste0("prev_", concept_sets_of_our_study_DU), concept_sets_of_our_study_DU)
  prevalence_of_use[ , (concept_sets_of_our_study_DU) := lapply(.SD, as.integer), .SDcols = concept_sets_of_our_study_DU]
  
  # Create the category anydrug
  prevalence_of_use[, anydrug := do.call(pmax, .SD), .SDcols = concept_sets_of_our_study_DU]
  
  # Melt the medications
  prevalence_of_use <- melt(prevalence_of_use, measure.vars = c(concept_sets_of_our_study_DU, "anydrug"),
                            variable.name = "medication")
  
  # Calculate manually the total irrespective by agebands
  prevalence_of_use_allages <- unique(copy(prevalence_of_use)[, Ageband := NULL])
  prevalence_of_use_allages <- prevalence_of_use_allages[, .(in_population = sum(in_population), value = sum(value)),
                                                         by = c("timeframe", "medication", "CalendarTime_level_order")]
  prevalence_of_use_allages[, Ageband := "allAgeband"]
  
  # Calculate manually the total by timeframe, medication and ageband
  prevalence_of_use <- prevalence_of_use[, .(in_population = sum(in_population), value = sum(value)),
                                         by = c("timeframe", "medication", "Ageband", "CalendarTime_level_order")]
  
  # Combine previous datasets to get aggregate dataset with correct totals for ageband "allAgeband"
  # Add Ageband level_order and medication level_order
  prevalence_of_use <- rbindlist(list(prevalence_of_use[, Ageband_level_order := 1],
                                      prevalence_of_use_allages[, Ageband_level_order := 99]), use.names = T)
  prevalence_of_use[, medication_level_order := fifelse(medication == "anydrug", 99, 1)]
  
  # Cleaned name and order of columns
  prevalence_of_use <- prevalence_of_use[, .(medication_label = medication, medication_level_order,
                                             CalendarTime_label = timeframe, CalendarTime_level_order,
                                             Ageband_label = Ageband, Ageband_level_order, numerator = value,
                                             denominator = in_population)]
  
  # Save the file
  smart_save(prevalence_of_use, diroutput, override_name = "D4_DU_prevalence_of_use_MSdrugs_in_MS_cohort",
             extension = extension, save_copy = "csv")
  
}
