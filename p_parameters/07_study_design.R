#-----------------------------------
# Parameters specific for each study
#-----------------------------------

#study_years_datasource

firstjan2021 <- ymd(20210101)

study_start <- ymd(20200101)
start_lookback <- ymd(20190101)

study_end <- min(instance_creation, recommended_end_date, na.rm = T)
rm(recommended_end_date)

study_years <- c("2020","2021")


firstYearComponentAnalysis <- "2019"
secondYearComponentAnalysis <- "2020"


#----------------------------
# admissible gap between observation periods (DAP-specific)
admissible_gap_obs_periods <- vector(mode = "list")
admissible_gap_obs_periods[['ARS']] <- 365
admissible_gap_obs_periods[['TEST']] <- 365
admissible_gap_obs_periods[['BIPS']] <- 30

gap_days <- ifelse(thisdatasource %not in% names(admissible_gap_obs_periods),
               1, admissible_gap_obs_periods[[thisdatasource]])


# define number of days a spells should not be shorter
min_spell_lenght <- 365
