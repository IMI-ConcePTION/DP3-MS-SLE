#-------------------------------
# sample script for IMI ConcePTION Demonstration Projects

# authors: Rosa Gini, Claudia Bartolini, Olga Paoletti, Davide Messina, Giorgio Limoncella
# based on previous scripts 

# v 2.0 - 25 September 2022
# Improve of the scriptbased on CVM script 

# v 1.0 - 27 June 2022
# Initial release

rm(list=ls(all.names=TRUE))

#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#-------------------------------
# @DAP: please modify the parametr dirinput and set it to the directory where your CDM instance is stored

dirinput <- paste0(thisdir,"/i_simulated_data_instance/")

#-------------------------------
# @DAP: please modify the parametr dirpregnancyinput and set it to the directory where your CDM instance is stored

dirpregnancyinput <- paste0(thisdir,"/i_simulated_data_instance/pregnancy/")


#----------------
#LOAD PARAMTETERS
#----------------

# general parameters of the script
source(paste0(thisdir,"/p_parameters/01_parameters_program.R"))

# parameters of the CDM
source(paste0(thisdir,"/p_parameters/02_parameters_CDM.R"))

# concept sets
source(paste0(thisdir,"/p_parameters/03_concept_sets.R"))

# item sets
source(paste0(thisdir,"/p_parameters/04_itemsets.R"))

# subpopulations (not to be used in DPs)
source(paste0(thisdir,"/p_parameters/05_subpopulations_restricting_meanings.R"))

# parameters for algortihms
source(paste0(thisdir,"/p_parameters/06_algorithms.R"))

# parameters for study_design
source(paste0(thisdir,"/p_parameters/07_study_design.R"))

source(paste0(thisdir,"/p_parameters/99_saving_all_parameters.R"))


#----------------
# RUn STEPS
#----------------

# CREATE EXCLUSION CRITERIA and CHECK CORRECT DATE OF BIRTH
launch_step("p_steps/01_T2_10_create_persons.R")

#COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS
launch_step("p_steps/01_T2_20_apply_CreateSpells.R")

# APPLY THE FUNCTION CreateConceptSetDatasets TO CREATE ONE DATASET PER CONCEPT SET CONTAINING ONLY RECORDS WITH CODES OF INTEREST
launch_step("p_steps/01_T2_31_CreateConceptSetDatasets.R")

# RETRIEVE ITEMSET DATASETS
launch_step("p_steps/01_T2_32_CreateItemSetDatasets.R")

# RETRIEVE PROMPT DATASETS
launch_step("p_steps/01_T2_33_CreatePromptSetDatasets.R")



system.time(source(paste0(thisdir,"/p_steps/step_01_1_T2.1_create_conceptset_datasets.R")))
system.time(source(paste0(thisdir,"/p_steps/step_01_2_T2.1_create_spells.R")))
system.time(source(paste0(thisdir,"/p_steps/step_01_3_T2.1_create_dates_in_PERSONS.R")))
system.time(source(paste0(thisdir,"/p_steps/step_01_4_T2.1_create_prompt_and_itemset_datasets.R")))

#02 quality checks

system.time(source(paste0(thisdir,"/p_steps/step_02_1_T2_create_QC_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_02_2_T3_apply_QC_exclusion_criteria.R")))

#03 create exclusion criteria
system.time(source(paste0(thisdir,"/p_steps/step_03_1_T2_create_exclusion_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_03_2_T2_merge_persons_concept.R")))

#04 apply exclusion criteria
system.time(source(paste0(thisdir,"/p_steps/step_04_1_T3_apply_exclusion_criteria.R")))
system.time(source(paste0(thisdir,"/p_steps/step_04_2_T3_apply_quality_check_exclusion_criteria_doses.R")))
##use flowchart (apply also quality checks)

#05 create D3s 
