##%######################################################%##
#                                                          #
####    AGGREGATE OF MS MEDICATION DURING PREGNANCY     ####
#                                                          #
##%######################################################%##

# Load individual level dataset
preg_med_ind <- smart_load("D4_DU_individual_prevalence_of_use_MSmeds_in_MSpregnancy_trimesters", dirtemp,
                           extension = extension, return = T)

# Medications needs to be aggregate before Cube since their level categories are not mutually exclusive
preg_med_ind <- preg_med_ind[is.na(medication_label), medication_label := "missing"]
preg_med_ind_any_medication <- preg_med_ind[,
                                            .(use_before_pregnancy = max(use_before_pregnancy),
                                              number_before_pregnancy = sum(number_before_pregnancy), 
                                              use_tri_1 = max(use_tri_1), use_tri_2 = max(use_tri_2),
                                              use_tri_3 = max(use_tri_3), number_tri_1 = sum(number_tri_1),
                                              number_tri_2 = sum(number_tri_2), number_tri_3 = sum(number_tri_3),
                                              use_before_pregnancy_1 = max(use_before_pregnancy_1),
                                              use_before_pregnancy_2 = max(use_before_pregnancy_2),
                                              use_before_pregnancy_3 = max(use_before_pregnancy_3),
                                              use_before_pregnancy_4 = max(use_before_pregnancy_4),
                                              number_before_pregnancy_1 = sum(number_before_pregnancy_1),
                                              number_before_pregnancy_2 = sum(number_before_pregnancy_2),
                                              number_before_pregnancy_3 = sum(number_before_pregnancy_3),
                                              number_before_pregnancy_4 = sum(number_before_pregnancy_4),
                                              use_after_pregnancy = max(use_after_pregnancy),
                                              number_after_pregnancy = sum(number_after_pregnancy)),
                                            by = c("pregnancy_id", "trimester_when_pregnancy_ended")]
preg_med_ind_any_medication[, medication_label := "anydrug"]

preg_med_ind <- rbindlist(list(preg_med_ind[, medication_level_order := 1],
                               preg_med_ind_any_medication[, medication_level_order := 99]), use.names = T, fill = T)

# Recode names
setnames(preg_med_ind, "use_before_pregnancy", "before_pregnancy")

# Create variable to take account of different pattern of use during the pregnancy
preg_med_ind[, during_pregnancy := fcase(use_tri_1 == 1 & use_tri_2 == 0 & use_tri_3 == 0, "tri1only",
                                         use_tri_1 == 0 & use_tri_2 == 1 & use_tri_3 == 0, "tri2only",
                                         use_tri_1 == 0 & use_tri_2 == 0 & use_tri_3 == 1, "tri3only",
                                         use_tri_1 == 1 & use_tri_2 == 1 & use_tri_3 == 0, "tri1tri2only",
                                         use_tri_1 == 1 & use_tri_2 == 0 & use_tri_3 == 1, "tri1tri3only",
                                         use_tri_1 == 0 & use_tri_2 == 1 & use_tri_3 == 1, "tri2tri3only",
                                         use_tri_1 == 1 & use_tri_2 == 1 & use_tri_3 == 1, "alltri",
                                         default = "notri")]
preg_med_ind[, during_pregnancy_any := during_pregnancy]
preg_med_ind[during_pregnancy_any != "notri", during_pregnancy_any := "anytri"]

preg_med_ind <- preg_med_ind[, trimester_2 := fifelse(trimester_when_pregnancy_ended != "t3",
                                                      "t1+t2", trimester_when_pregnancy_ended)]
preg_med_ind <- preg_med_ind[, trimester_3 := "t1+t2+t3"]

# Create general variables for use (and removed unnecessary columns)
# preg_med_ind[, use_general := pmax(before_pregnancy, use_tri_1, use_tri_2, use_tri_3)]
preg_med_ind[, use_general := 1]
preg_med_ind[, number_medications := rowSums(.SD), .SDcols = c("number_before_pregnancy", "number_tri_1",
                                                               "number_tri_2", "number_tri_3")]
preg_med_ind[, c("number_before_pregnancy", "number_tri_1", "number_tri_2", "number_tri_3") := NULL]

# Assign levels for Cube
assigned_levels <- vector(mode="list")
assigned_levels[["medication"]] <- c("medication_label")
assigned_levels[["medication_level_order"]] <- c("medication_level_order")
assigned_levels[["before_pregnancy"]] <- c("before_pregnancy")
assigned_levels[["during_pregnancy"]] <- c("during_pregnancy", "during_pregnancy_any")
assigned_levels[["trimester_when_pregnancy_ended"]] <- c("trimester_when_pregnancy_ended", "trimester_2", "trimester_3")

assigned_statistics <- vector(mode="list")
assigned_statistics[["number_medications"]] <- c("sum", "median")
assigned_statistics[["use_general"]] <- "sum"

# TODO add anytry
# Calculate sums of usage and number of medication for each level. Calculate 
preg_med_ind <- Cube(input = preg_med_ind,
                     dimensions = c("medication"),
                     levels = assigned_levels,
                     computetotal = c("before_pregnancy"),
                     measures = c("number_medications", "use_general"),
                     statistics = assigned_statistics
)
preg_med_ind <- preg_med_ind[!(during_pregnancy_LevelOrder == 99 & during_pregnancy_LabelValue == "notri"), ]

# Cleaning after Cube
preg_med_ind <- unique(preg_med_ind[, .(medication_label = medication_LabelValue,
                                        medication_level_order = medication_level_order_LabelValue,
                                        before_pregnancy = before_pregnancy_LabelValue,
                                        during_pregnancy = during_pregnancy_LabelValue,
                                        trimester_when_pregnancy_ended = trimester_when_pregnancy_ended_LabelValue,
                                        numerator_preg_use = use_general_sum,
                                        numerator_number_medications = number_medications_sum,
                                        median_number_medications = number_medications_median)])

preg_med_ind[before_pregnancy == "Allbefore_pregnancy", before_pregnancy := "any"]

# Save the file
smart_save(preg_med_ind, diroutput, override_name = "D4_DU_prevalence_of_exclusive_use_MSmeds_in_MSpregnancy_trimesters",
           extension = extension, save_copy = "csv")
