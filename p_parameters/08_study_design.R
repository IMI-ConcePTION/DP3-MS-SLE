#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#study_years_datasource
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

datasources_only_preg <- c('EFEMERIS', 'THL', 'RDRU_FISABIO')
datasources_obs_per_from_pregnancies <- c("RDRU_FISABIO")

recommended_start_date_vect <- vector(mode = "list")
recommended_start_date_vect[['ARS']] <- ymd(20030101)
recommended_start_date_vect[['TEST']] <- ymd(20030101)
recommended_start_date_vect[['THL']] <- ymd(19960101)
recommended_start_date_vect[['SNDS']] <- ymd(20150101)
recommended_start_date_vect[['EFEMERIS']] <- ymd(20050101)
recommended_start_date_vect[['UOSL']] <- ymd(20080101)
recommended_start_date_vect[['RDRU_FISABIO']] <- ymd(20130101)
recommended_start_date_vect[['SAIL Databank']] <- ymd(19980101)
recommended_start_date_vect[['FERR']] <- ymd(20090101)

if (thisdatasource %not in% names(recommended_start_date_vect)) {
  stop(paste0("DATASOURCE not present inside the list: ",
             paste(names(recommended_start_date_vect), collapse = ", "),
             ".\nPlease open an issue"))
}

summary_threshold <- 5

recommended_start_date <- recommended_start_date_vect[[thisdatasource]]
rm(recommended_start_date_vect)

study_start <- max(ymd(20050101), recommended_start_date)

ageband_definition_level_1 <- c(15, 19, 24, 29, 34, 39, 44, 49)
ageband_definition_level_2 <- c(15, 24, 29, 34, 39, 49)
ageband_definition_level_3 <- c(15, 24, 34, 49)

s <- c(1, 2, 3, 5, 8, "all")
