# Create D4_prevalence_period_MS
# input: D3_study_population_SAP1, conceptset
# period_prevalence: D3_clean_spells

for (outcome in OUTCOME_variables) {
  print(outcome)
  
  name_dfs <- paste(c("prevalence_period", "prevalence_persontime", "prevalence_average_point",
                      "prevalence_aggregated_multiple_lookback"), outcome, sep = "_")
  
  for (name_df in name_dfs) {
    if (name_df == paste("prevalence_average_point", outcome, sep = "_")) {
      numerator_to_censor <- paste("numerator", sprintf("%02d", seq_len(60)), sep = "_")
      denominator_to_select <- paste("denominator", sprintf("%02d", seq_len(60)), sep = "_")
      percentage_to_select <- as.vector(outer(c("percentage", "lowerCI", "upperCI"), sprintf("%02d", seq_len(60)), paste, sep = "_"))
    } else {
      numerator_to_censor <- "numerator"
      denominator_to_select <- "denominator"
      percentage_to_select <- c("percentage", "lowerCI", "upperCI")
    }
    
    # Load algorithms
    prevalence_df <- smart_load(paste("D4", name_df, sep = "_"), diroutput, return = T, extension = extension)
    
    # Keep only levels from Cube decided after discussion
    prevalence_df <- filter_by_Cube_levels(prevalence_df)
    
    # Add percentage base on wilson method (default)
    prevalence_df[, (percentage_to_select) := unlist(lapply(seq_along(numerator_to_censor), function(x) {
      as.list(Hmisc::binconf(get(numerator_to_censor[[x]]), get(denominator_to_select[[x]]), return.df = T))
      }), recursive = F)]
    # prevalence_df[numerator == 0, lowerCI := 0]
    
    # Create a filtered version of the prevalence excluding the row with at least a small count
    prevalence_df_masked <- remove_Threshold(prevalence_df, 5, numerator_to_censor)
    
    # Only for THL remove numerator and denominator
    if (thisdatasource == "THL") {
      prevalence_df[, c(numerator_to_censor, denominator_to_select) := NULL]
      prevalence_df_masked[, c(numerator_to_censor, denominator_to_select) := NULL]
    }
    
    smart_save(prevalence_df, direxp, override_name = paste("D5", name_df, sep = "_"), extension = extension, save_copy = "csv")
    smart_save(prevalence_df_masked, direxpmask, override_name = paste("D5", name_df, "masked", sep = "_"),
               extension = extension, save_copy = "csv")
    
    # update_vector("datasets_to_censor", dirpargen, paste("D4_prevalence_period", outcome, sep = "_"))
    # update_vector("variables_to_censor", dirpargen, c("numerator" = 5))
    
  }
}
