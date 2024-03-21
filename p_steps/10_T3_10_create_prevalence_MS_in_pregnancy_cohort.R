##%######################################################%##
#                                                          #
####       CALCULATE PREVALENCE OF MS PREGANCIES        ####
####          WITH RESPECT TO ALL PREGNANCIES           ####
#                                                          #
##%######################################################%##

# Load dataset of pregnancies and medicines conceptsets
smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

# Keep only variables of interest
preg_cohort <- D3_DU_PREGNANCY_COHORT_variables[, .(pregnancy_id, birth_date, pregnancy_start_date, pregnancy_with_MS)]

# Calculate ageband at LMP
preg_cohort[, age_at_LMP := age_fast(birth_date, pregnancy_start_date)]
preg_cohort[, ageband_at_LMP := cut(age_at_LMP, c(15, 20, 25, 30, 35, 40, 45, 50),
                                    c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"), right = F)]
preg_cohort[, c("age_at_LMP", "birth_date") := NULL]

# Extract the year of the LMP and create variables containing longer periods
preg_cohort[, pregnancy_start_date := lubridate::year(pregnancy_start_date)]
preg_cohort[, pregnancy_start_date_2 := cut(pregnancy_start_date, c(2005, 2010, 2015, 2020),
                                            c("2005-2009", "2010-2014", "2015-2019"), right = F)]
preg_cohort[, pregnancy_start_date_99 := "2005-2019"]

# Assign levels for Cube
assigned_levels <- vector(mode="list")
assigned_levels[["pregnancy_with_MS"]] <- c("pregnancy_with_MS")
assigned_levels[["Ageband"]] <- c("ageband_at_LMP")
assigned_levels[["pregnancy_start_date"]] <- c("pregnancy_start_date", "pregnancy_start_date_2",
                                               "pregnancy_start_date_99")

# Create placeholder to sum in Cube. Each pregnancy is counted as 1
preg_cohort[, placeholder := 1]

# Calculate sums of pregnancies in each dimension/level
preg_cohort <- Cube(input = preg_cohort,
                    dimensions = c("Ageband", "pregnancy_start_date", "pregnancy_with_MS"),
                    levels = assigned_levels,
                    computetotal = c("Ageband", "pregnancy_with_MS"),
                    measures = c("placeholder")
)

# Clean dataset by removing unneeded columns and change column names
preg_cohort[, pregnancy_with_MS_LevelOrder := NULL]
setnames(preg_cohort, c("pregnancy_with_MS_LabelValue", "Ageband_LabelValue", "pregnancy_start_date_LabelValue",
                        "Ageband_LevelOrder", "pregnancy_start_date_LevelOrder"),
         c("pregnancy_with_MS", "Ageband_label", "CalendarTime_label",
           "Ageband_level_order", "CalendarTime_level_order"))

# Placeholder_sum became numerator when we counting pregnancy_with_MS
preg_cohort_num <- preg_cohort[pregnancy_with_MS == 1,][, pregnancy_with_MS := NULL]
setnames(preg_cohort_num, "placeholder_sum", "numerator")

# Placeholder_sum became denominator when we counting all the pregnancies irrespective of MS
preg_cohort_den <- preg_cohort[pregnancy_with_MS == "Allpregnancy_with_MS",][, pregnancy_with_MS := NULL]
setnames(preg_cohort_den, "placeholder_sum", "denominator")

# Need to create a data.frame with all the possible combinations of strata
# We may not have all of them in the previous datasets (e.g. 2015 in "normal" datasources)
empty_df <- as.data.table(expand.grid(pregnancy_start_date = 2005:2019,
                                      ageband_at_LMP = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                                      placeholder = 1))
empty_df[, pregnancy_start_date_2 := cut(pregnancy_start_date, c(2005, 2010, 2015, 2020),
                                         c("2005-2009", "2010-2014", "2015-2019"), right = F)]
empty_df[, pregnancy_start_date_99 := "2005-2019"]

# Assign levels for Cube
assigned_levels <- vector(mode="list")
assigned_levels[["Ageband"]] <- c("ageband_at_LMP")
assigned_levels[["pregnancy_start_date"]] <- c("pregnancy_start_date", "pregnancy_start_date_2",
                                               "pregnancy_start_date_99")

# Calculate sums of usage and number of medication for each level. Calculate 
empty_df <- Cube(input = empty_df,
                 dimensions = c("Ageband", "pregnancy_start_date"),
                 levels = assigned_levels,
                 computetotal = c("Ageband"),
                 measures = c("placeholder")
)

# Clean dataset by removing unneeded columns and change column names. (Similar to above)
empty_df[, placeholder_sum := NULL]
setnames(empty_df, c("Ageband_LabelValue", "pregnancy_start_date_LabelValue", "Ageband_LevelOrder",
                     "pregnancy_start_date_LevelOrder"),
         c("Ageband_label", "CalendarTime_label", "Ageband_level_order", "CalendarTime_level_order"))

# Add to the dataframe with the full strata the numerator and denominator
full_preg_cohort_num <- preg_cohort_num[empty_df, on = .(Ageband_label, CalendarTime_label, Ageband_level_order,
                                                         CalendarTime_level_order)]
full_preg_cohort <- preg_cohort_den[full_preg_cohort_num, on = .(Ageband_label, CalendarTime_label, Ageband_level_order,
                                                                 CalendarTime_level_order)]

# Missing values generated by the right join are filled by 0s
setnafill(full_preg_cohort, fill = 0, cols = c("numerator", "denominator"))

# Clean final dataset
full_preg_cohort <- full_preg_cohort[, .(datasource = thisdatasource, CalendarTime_label, CalendarTime_level_order,
                                         Ageband_label, Ageband_level_order, numerator, denominator)]

# Save the file
smart_save(full_preg_cohort, diroutput, override_name = "D4_DU_prevalence_MS_in_pregnancy_cohort",
           extension = extension, save_copy = "csv")
