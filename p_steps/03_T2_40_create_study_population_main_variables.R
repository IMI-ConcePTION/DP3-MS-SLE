#------------------------------------------------------------------
# create events and components of OUTCOMES 

# input: concept set datasets of outcomes (narrow and possible), D4_study_population
# output: for each outcome OUTCOME, D3_components_OUTCOME.RData and D3_events_OUTCOME_type.RData, for type = narrow, possible

### STEP SETUP
# Vocabulary for value of new variable type_of_date and old variable which contains dates
vocabulary_covariate_times <- vector()
vocabulary_covariate_times[["baseline"]] <- "study_entry_date"
vocabulary_covariate_times[["vax1"]] <- "date_vax1"
# vocabulary_covariate_times[["first_covid"]] <- "b"

# Create an empty covariate dataset
empty_covariate_df <- data.table(person_id = character(),
                                 date = as.Date(as.POSIXct(character())),
                                 type_outcome = character(),
                                 meaning_renamed = character(),
                                 codvar = character(),
                                 type_of_date = character())

# Create an empty D3_covariates_ALL with the correct column types
base_matrix <- matrix(nrow = 0, ncol = length(COV_variables) + 2)
empty_covariates_ALL <- setnames(data.table(base_matrix), c("person_id", "type_of_date", COV_variables))
empty_covariates_ALL[, person_id := as.character(person_id)][, type_of_date := as.character(type_of_date)]
empty_covariates_ALL <- empty_covariates_ALL[, (COV_variables) := lapply(.SD, function(x) as.Date(as.POSIXct(x))),
                                             .SDcols = sapply(empty_covariates_ALL, is.logical)]

print('create events and create components of OUTCOMES and CONTROLS')

### STEP START
for (subpop in subpopulations_non_empty) {

  print(subpop)
  
  cov_ALL <- data.table()
  
  # Import the study population
  name_D4_study_population <- paste0("D4_study_population", suffix[[subpop]])
  load(paste0(diroutput, name_D4_study_population, ".RData")) 
  study_population <- get(name_D4_study_population)[, .(person_id, study_entry_date)]
  
  # Create another empty D3_covariates_ALL with all the combinations of covariate_time and person_id
  empty_row_covariates_ALL <- as.data.table(expand.grid(person_id = unlist(study_population[, .(person_id)]),
                                                        type_of_date = names(vocabulary_covariate_times)))
  empty_row_covariates_ALL <- rbind(empty_covariates_ALL, empty_row_covariates_ALL, fill = T)
  
  # Import doses dataset and retain only date of first vaccination
  load(paste0(dirtemp,"D3_vaccines_curated.RData"))
  D3_vaccines_curated <- D3_vaccines_curated[dose_curated == 1, ][, .(person_id, date_curated)]
  setnames(D3_vaccines_curated, "date_curated", "date_vax1")
  
  # Merge population and first vaccinations. Left join since we want all the persons in the study population
  study_population <- merge(study_population, D3_vaccines_curated, all.x = T, by = "person_id")
  
  for (covariate_time in names(vocabulary_covariate_times)) {
    print(paste("NOW CALCULATING THE COVARIATE AT TIME:", covariate_time))
    
    # Add to selectionOUTCOME the correct variable to select
    date_var_name <- vocabulary_covariate_times[[covariate_time]]
    selectionOUTCOME <- sprintf("!is.na(%s) & date >= %s - 365 & date < %s", date_var_name, date_var_name, date_var_name)
    
    # delete records that are not observed in this whole subpopulation
    if (this_datasource_has_subpopulations){
      selectionOUTCOME <- paste0(selectionOUTCOME, ' & ', select_in_subpopulationsEVENTS[[subpop]])
    }
    
    for (COVARIATE in COV_variables) {
      print(COVARIATE)
      
      # Create the list of dataset generated after createconceptsetdataset
      nameconceptset <- variable_definition[[COVARIATE]]
      nameconceptset <- nameconceptset[nameconceptset %in% sub('\\.RData$', '', list.files(dirconceptsets))]
      
      # Load, then select/create only variable of interest. If otiginal concept empty use a base df.
      conceptsets_list <- lapply(nameconceptset, function(x) {
        df <- get(load(paste0(dirconceptsets, x, ".RData"))[[1]])
        if(nrow(df) == 0) {df <- empty_covariate_df} else {df <- df[, .(person_id, date, type_outcome = COVARIATE,
                                                                        meaning_renamed, codvar, type_of_date = covariate_time)]}
        return(df)
      })
      
      # Merge the concepts, join with study population and the take the maximum date for each person
      components <- MergeFilterAndCollapse(listdatasetL= conceptsets_list,
                                           datasetS = study_population,
                                           condition = selectionOUTCOME,
                                           key = "person_id",
                                           strata = c("person_id", "type_outcome", "type_of_date"),
                                           summarystat = list(list(c("max"), "date", "last_date")))
      
      # Append to cov_ALL to create a single file with all the covariates
      cov_ALL <- rbind(cov_ALL, components)
    }
  }
  
  # Covariates as columns, dates as values
  cov_ALL <- dcast(cov_ALL, person_id + type_of_date ~ type_outcome, value.var = "last_date")
  
  # Join with the empty_covariates_ALL the previous dataset to add missing covariates(columns) 
  half_full_covariates_ALL <- rbind(empty_covariates_ALL, cov_ALL, fill = T)
  
  # Update join with dataset with all rows/columns but empty and the one calculated above with all the observations
  full_covariates_ALL <- empty_row_covariates_ALL[half_full_covariates_ALL, on = .(person_id, type_of_date),
                                                  names(half_full_covariates_ALL) := mget(
                                                    paste0("i.", names(half_full_covariates_ALL)))]
  
  # Save D3_covariates_ALL
  nameobjectCOVARIATES <- paste0("D3_covariates_ALL", suffix[[subpop]])
  assign(nameobjectCOVARIATES, full_covariates_ALL)
  save(nameobjectCOVARIATES, file = paste0(dirtemp, nameobjectCOVARIATES, ".RData"), list = nameobjectCOVARIATES)
}  
  