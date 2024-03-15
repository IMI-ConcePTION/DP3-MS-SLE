#-------------------------------
# DP3 for IMI ConcePTION Demonstration Projects

# authors: Rosa Gini, Davide Messina, Marie Beslay
# based on previous scripts

# v3.0.0-beta - 15 March 2024
# Implemented template 3
# Bugfix for template 1

# v3.0.0-alpha - 04 March 2024
# Implemented until template 1 and 2
# A few parameters to be still setup in other DAPs so test only for EFEMERIS

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
launch_step("p_steps/05_T2_10_create_MS-COHORT.R")
launch_step("p_steps/05_T2_20_create_selection_criteria_from_SAP1_MS_cohort_to_DU_MS_cohort.R")
launch_step("p_steps/05_T2_30_create_selection_criteria_from_pregnancies.R")
launch_step("p_steps/05_T2_40_CreateConceptSetDatasetsDU.R")
launch_step("p_steps/06_T3_10_apply_selection_criteria_to_create_MS-COHORT.R")
launch_step("p_steps/06_T3_20_apply_selection_criteria_to_pregnancies.R")
launch_step("p_steps/07_T2_10_collect_variables_for_pregnancies.R")
launch_step("p_steps/08_T4_20_create_descriptives_pregnancies.R")
launch_step("p_steps/10_T3_20_create_prevalence_of_exclusive_use_MSmeds_in_MSpregnancy_trimesters.R")
launch_step("p_steps/10_T3_21_aggregate_prevalence_of_exclusive_use_MSmeds_in_MSpregnancy_trimesters.R")
launch_step("p_steps/11_T4_10_create_template_on_prevalence_of_use_MSdrugs_in_MSpregnancy_cohort.R")
