#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#study_years_datasource

study_start <- ymd(20050101)

study_end <- min(instance_creation, recommended_end_date, ymd(20191231), na.rm = T)
rm(recommended_end_date)

#----------------------------
# admissible gap between observation periods (DAP-specific)
admissible_gap_obs_periods <- vector(mode = "list")
admissible_gap_obs_periods[['ARS']] <- 365
admissible_gap_obs_periods[['TEST']] <- 365
admissible_gap_obs_periods[['BIPS']] <- 30

gap_days <- ifelse(thisdatasource %not in% names(admissible_gap_obs_periods),
               1, admissible_gap_obs_periods[[thisdatasource]])

rm(admissible_gap_obs_periods)

# define number of days a spells should not be shorter
min_spell_lenght <- 365
