##%######################################################%##
#                                                          #
####           AGGREGATE PERIOD, PERSONTIME,            ####
####              AVERAGE POINT PREVALENCE              ####
#                                                          #
##%######################################################%##

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  # Load normal dataset and combine them
  name_dfs <- paste(c("D5_prevalence_period", "D5_prevalence_persontime", "D5_prevalence_average_point"), outcome, sep = "_")
  prevalence_aggregated <- rbindlist(lapply(name_dfs, smart_load, direxp, return = T, extension = extension),
                                     use.names = T, fill = T)
  
  # Save normal aggregated dataset
  export_name <- paste("D5_prevalence_aggregated_all", outcome, sep = "_")
  smart_save(prevalence_aggregated, direxp, override_name = export_name, extension = "csv")
  
  # Load masked dataset and combine them
  name_dfs <- paste(name_dfs, "masked", sep = "_")
  prevalence_aggregated_masked <- rbindlist(lapply(name_dfs, smart_load, direxpmask, return = T, extension = extension),
                                     use.names = T, fill = T)
  
  # Save masked aggregated dataset
  export_name <- paste(export_name, "masked", sep = "_")
  smart_save(prevalence_aggregated_masked, direxpmask, override_name = export_name, extension = "csv")
  
  # Load masked dataset and combine them
  name_dfs <- paste(name_dfs, "masked", sep = "_")
  prevalence_aggregated_masked <- rbindlist(lapply(name_dfs, smart_load, direxpred, return = T, extension = extension),
                                     use.names = T, fill = T)
  
  # Save masked aggregated dataset
  export_name <- paste(export_name, "masked", sep = "_")
  smart_save(prevalence_aggregated_masked, direxpred, override_name = export_name, extension = "csv")
  
  # names_vars_to_censor <- c("numerator", paste("numerator", sprintf("%02d", seq_len(60)), sep="_"))
  # value_vars_to_censor <- rep(5, length(names_vars_to_censor))
  # names(value_vars_to_censor) <- names_vars_to_censor
  # 
  # update_vector("datasets_to_censor", dirpargen, export_name)
  # update_vector("variables_to_censor", dirpargen, value_vars_to_censor)
}
