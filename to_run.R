#-------------------------------
# DP3 for IMI ConcePTION Demonstration Projects

# authors: Rosa Gini, Davide Messina, Marie Beslay
# based on previous scripts

# v3.0.0 - 27 November 2024 
# Implemented Drug Utilization study

rm(list=ls(all.names=TRUE))

# set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#----------------
#LOAD PARAMTETERS
#----------------

source(paste0(thisdir,"/p_parameters/01_parameters_program.R")) #GENERAL
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R")) #CDM
source(paste0(thisdir,"/p_parameters/03_concept_sets.R")) #CONCEPTSETS
source(paste0(thisdir,"/p_parameters/04_itemsets.R")) #ITEMSETS
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R")) #SUBPOPULATIONS
source(paste0(thisdir,"/p_parameters/06_variable_lists.R")) #OUTCOMES AND COVARIATES
source(paste0(thisdir,"/p_parameters/07_algorithms.R")) #ALGORITHMS
source(paste0(thisdir,"/p_parameters/08_study_design.R")) #STUDY DESIGN
source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R")) #SAVING AND CLEANING PARAMETERS

#----------------
# RUN STEPS
#----------------

# DU
launch_step("p_steps/01_T2_10_create_persons.R")
launch_step("p_steps/01_T2_20_apply_CreateSpells.R")
launch_step("p_steps/01_T2_30_CreateConceptSetDatasets.R")
launch_step("p_steps/01_T2_31_CreateItemSetDatasets.R")
launch_step("p_steps/01_T2_32_CreatePromptSetDatasets.R")
launch_step("p_steps/01_T2_40_clean_spells.R")
launch_step("p_steps/01_T2_50_selection_criteria_from_PERSON_to_study_population.R")
launch_step("p_steps/02_T3_10_create_study_population.R")
launch_step("p_steps/03_T2_10_create_study_population_baseline_variables.R")
launch_step("p_steps/03_T2_20_create_main_components.R")
launch_step("p_steps/03_T2_21_create_components_MS.R")
launch_step("p_steps/03_T2_22_create_components_SLE.R")
launch_step("p_steps/03_T2_30_create_algorithms_MS.R")

# DU
launch_step("p_steps/05_T2_10_create_MS-COHORT.R")
launch_step("p_steps/05_T2_20_create_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort.R")
launch_step("p_steps/05_T2_30_create_selection_criteria_from_pregnancies.R")
launch_step("p_steps/05_T2_40_CreateConceptSetDatasetsDU.R")
launch_step("p_steps/06_T3_10_apply_selection_criteria_to_create_MS-COHORT.R")
launch_step("p_steps/06_T3_20_apply_selection_criteria_to_pregnancies.R")
launch_step("p_steps/07_T2_10_collect_variables_for_pregnancies.R")
launch_step("p_steps/08_T4_20_create_descriptives_pregnancies.R")
launch_step("p_steps/09_T3_10_create_candidate_matches_MS_non_pregnant.R")
launch_step("p_steps/09_T3_20_match.R")
launch_step("p_steps/10_T3_10_create_prevalence_MS_in_pregnancy_cohort.R")
launch_step("p_steps/10_T3_20_create_prevalence_of_exclusive_use_MSmeds_in_MSpregnancy_trimesters.R")
launch_step("p_steps/10_T3_21_aggregate_prevalence_of_exclusive_use_MSmeds_in_MSpregnancy_trimesters.R")
launch_step("p_steps/10_T3_30_create_prevalence_of_use_MSdrugs_in_MScohort.R")
launch_step("p_steps/10_T3_40_create_prevalence_of_use_MSdrugs_in_MSpregnancy_cohort.R")
# launch_step("p_steps/11_T4_10_create_template_on_prevalence_of_use_MSdrugs_in_MSpregnancy_cohort.R")
launch_step("p_steps/11_T4_20_create_templates_on_prevalence_MS_in_pregnancy_cohort.R")
launch_step("p_steps/11_T4_30_create_template_on_prevalence_of_use_MSdrugs_in_MS_cohort.R")
launch_step("p_steps/11_T4_40_create_template_on_prevalence_of_use_MSdrugs_in_MSpregnancy_cohort.R")
launch_step("p_steps/11_T4_50_matching diagnostics.R")
