#-------------------------------
# DP3 for IMI ConcePTION Demonstration Projects

# authors: Rosa Gini, Davide Messina, Marie Beslay
# based on previous scripts

# v2.1.0 - 22 August 2023
# Added summary_levels_yearly for average prevalence
# Added meaning_of_visit matching
# Updated and validate meanings for UOSL, THL and EFEMERIS
# Create a copy in csv of each intermediate dataset
# Bugfixes

# v2.0.0 - 26 June 2023
# Added Cube function to aggregate data in dimensions
# Removed final datasets
# Reworked the masking of datasets
# FISABIO has subpopulation
# UOSL has recommended_start_date in 2008
# Bugfixes

# v1.0.4 - 04 May 2023
# Fixed subpopulation for THL and SAIL databank
# SAIL databank has prescriptions
# Fix create spell step for subpopulation

# v1.0.3 - 02 May 2023
# Added subpopulations for THL and SAIL databank
# Age in D5_N_women_and_ranges for study_population at cohort entry date

# v1.0.2 - 20 March 2023
# D4_prevalence_average_point now at aggregated level
# Countprevalence for average point in a cycle to reduce RAM utilization

# v1.0.1 - 08 March 2023
# CountPrevalence uses less RAM (and dataset rows) for point prevalence computation
# Minor fixes

# v1.0.0 - 07 March 2023
# Release opened to the DAPs
# Added missing ATC codes
# Created parameter datasources_only_preg for EFEMERIS, THL and FISABIO
# Creation of folder g_export_smallcountsremoved which contains datasets with count less than 5 censored
# Datasource with not empty "date_prescription" in CDM is only SNDS

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
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

# 01 RETRIEVE RECORDS FRM CDM

# CREATE EXCLUSION CRITERIA and CHECK CORRECT DATE OF BIRTH
launch_step("p_steps/01_T2_10_create_persons.R")

# COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS
launch_step("p_steps/01_T2_20_apply_CreateSpells.R")

# APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY RECORDS WITH CODES OF INTEREST
launch_step("p_steps/01_T2_30_CreateConceptSetDatasets.R")

# RETRIEVE ITEMSET DATASETS AND PROMPT DATASETS
launch_step("p_steps/01_T2_31_CreateItemSetDatasets.R")
launch_step("p_steps/01_T2_32_CreatePromptSetDatasets.R")

# CLEAN THE SPELLS
launch_step("p_steps/01_T2_40_clean_spells.R")

# CREATE EXCLUSION CRITERIA for persons/spells AND APPLY THME TO GET THE STUDY POPULATION
launch_step("p_steps/01_T2_50_selection_criteria_from_PERSON_to_study_population.R")
launch_step("p_steps/02_T3_10_create_study_population.R")

# CREATE COMPONENTS AND ALGORITHMS
launch_step("p_steps/03_T2_10_create_study_population_baseline_variables.R")
launch_step("p_steps/03_T2_20_create_main_components.R")
launch_step("p_steps/03_T2_21_create_components_MS.R")
launch_step("p_steps/03_T2_22_create_components_SLE.R")
launch_step("p_steps/03_T2_30_create_algorithms_MS.R")

# COMPUTE PREVALENCE
launch_step("p_steps/03_T3_40_compute_period_prevalence_MS.R")
launch_step("p_steps/03_T3_41_compute_persontime_prevalence_MS.R")
launch_step("p_steps/03_T3_42_compute_monthly_point_prevalence_MS.R")

# AGGREGATE PREVALENCE
# launch_step("p_steps/03_T3_50_aggregate_prevalence_MS.R")
launch_step("p_steps/03_T3_50_point_prevalence_multiple_lookback.R")

# COMPUTE BASIC STATISTICS
launch_step("p_steps/04_T4_10_create_N_women_and_ranges_MS.R")

# Remove SMALL COUNT FROM EXPORT DATASET
launch_step("p_steps/05_T5_10_censor_small_count.R")
