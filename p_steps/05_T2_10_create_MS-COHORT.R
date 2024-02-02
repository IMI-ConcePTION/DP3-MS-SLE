##%######################################################%##
#                                                          #
####              COMBINE SAP 1 POPULATION              ####
####          AND CHOSEN MS ALGORITHMS EVENTS           ####
#                                                          #
##%######################################################%##

smart_load("D3_study_population_SAP1", dirtemp, extension = extension)
smart_load("D3_algorithms_MS", dirtemp, extension = extension)

# Keep only records related to the chosen algorithm from SAP 1
D3_algorithms_MS <- D3_algorithms_MS[algorithm == chosen_MS_algorithm[[thisdatasource]], ]

# Keep only necessary columns
D3_study_population_SAP1 <- D3_study_population_SAP1[, c("person_id", "entry_spell_category", "birth_date",
                                                         "cohort_entry_date", "cohort_exit_date")]
D3_algorithms_MS <- D3_algorithms_MS[, c("person_id", "date")]

# Full join, population with algorithms
D3_SAP1_MS_COHORT <- merge(D3_algorithms_MS, D3_study_population_SAP1, all = T, by = "person_id")

# Change name of variables
setnames(D3_SAP1_MS_COHORT, "date", "date_MS")

# Saving the dataset
smart_save(D3_SAP1_MS_COHORT, dirtemp, extension = extension, save_copy = "csv")
