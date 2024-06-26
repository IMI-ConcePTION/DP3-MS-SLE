##%######################################################%##
#                                                          #
####            CLEAN THE SPELLS and CREATE             ####
####        BASIS FOR FUTURE EXCLUSION CRITERIAS        ####
#                                                          #
##%######################################################%##

# Load datasets
smart_load("D3_PERSONS", dirtemp, extension = extension)
smart_load("D3_output_spells_category", dirtemp, extension = extension)
D3_output_spells_category[, entry_spell_category := data.table:::as.Date.IDate(entry_spell_category)]
D3_output_spells_category[, exit_spell_category := data.table:::as.Date.IDate(exit_spell_category)]

smart_load("OBSERVATION_PERIODS_inverted", dirtemp, extension = extension)
setnames(OBSERVATION_PERIODS_inverted, c("op_start_date", "op_end_date"), c("entry_spell_category", "exit_spell_category"))
OBSERVATION_PERIODS_inverted[, entry_spell_category := ymd(entry_spell_category)]
OBSERVATION_PERIODS_inverted[, exit_spell_category := ymd(exit_spell_category)]

if ("op_meaning" %in% colnames(D3_output_spells_category)) {
  OBSERVATION_PERIODS_inverted <- OBSERVATION_PERIODS_inverted[, .(person_id, entry_spell_category, exit_spell_category,
                                                                   op_meaning, num_spell = 0)]
} else {
  setnames(OBSERVATION_PERIODS_inverted, "op_meaning", "category")
  OBSERVATION_PERIODS_inverted <- OBSERVATION_PERIODS_inverted[, .(person_id, entry_spell_category, exit_spell_category,
                                                                   category, num_spell = 0)]
}

D3_output_spells_category <- rbindlist(list(D3_output_spells_category, OBSERVATION_PERIODS_inverted), use.names = T)
if ("op_meaning" %not in% colnames(D3_output_spells_category)) D3_output_spells_category[, op_meaning := ""]

# Combine persons and spells, then select only the column we need and create new ones
person_spell <- merge(D3_output_spells_category, D3_PERSONS, all.x = T, by = "person_id")
person_spell <- person_spell[, .(person_id, birth_date, death_date, entry_spell_category_crude = entry_spell_category,
                                 exit_spell_category_crude = exit_spell_category, op_meaning, num_spell)]

# If the spell start within 60 days from the birth the spell start becomes the birth date
person_spell[, entry_spell_category := data.table::fifelse(birth_date < entry_spell_category_crude - 60 |
                                                             birth_date > instance_creation,
                                                           entry_spell_category_crude, birth_date)]

# Censor spells which ends after the death date
person_spell[, exit_spell_category := pmin(exit_spell_category_crude, death_date, birth_date + floor(50 * 365.25) - 1, na.rm = T)]

# Find if person is too old or young when spell start/end
person_spell[, too_old_at_start_spell := data.table::fifelse(entry_spell_category > exit_spell_category, 1, 0)]
person_spell[, too_young_at_exit_spell := data.table::fifelse(
  exit_spell_category < birth_date + ceiling(15 * 365.25) | study_end < birth_date + ceiling(15 * 365.25), 1, 0)]

# find spells that do not overlap the study period (using original start/end)
person_spell[, no_overlap_study_period := data.table::fifelse(
  entry_spell_category > study_end | exit_spell_category < study_start, 1, 0)]

# Censor spells which start before the recommended start date
person_spell[, entry_spell_category := pmax(entry_spell_category, recommended_start_date, na.rm = T)]

# Censor spells which ends after the study_end
person_spell[, exit_spell_category := pmin(exit_spell_category, study_end, na.rm = T)]

# find spells that end before they start (using original start/end)
person_spell[, starts_after_ending := data.table::fifelse(exit_spell_category < entry_spell_category, 1, 0)]

# find spells that are shorter than x days (using cleaned start/end)
person_spell[, spell_less_than_12_months_fup := data.table::fifelse(
  correct_difftime(exit_spell_category, entry_spell_category) < min_spell_lenght &
    thisdatasource %not in% datasources_only_preg, 1, 0)]

# Calculate cohort entry and exit date (censor for age and study_start)
person_spell[, cohort_entry_date := pmax(entry_spell_category, study_start, birth_date + ceiling(15 * 365.25))]
person_spell[, cohort_exit_date := pmin(exit_spell_category, birth_date + floor(50 * 365.25) - 1)]

# Create variable which says if the start/end of spell has been changed
person_spell[, entry_spell_category_cleaned := data.table::fifelse(cohort_entry_date != entry_spell_category_crude, 1, 0)]
person_spell[, exit_spell_category_cleaned := data.table::fifelse(cohort_exit_date != exit_spell_category_crude, 1, 0)]
person_spell[, c("cohort_entry_date", "cohort_exit_date") := NULL]

if (thisdatasource %in% c("RDRU_FISABIO")) {
  OBSERVATION_PERIODS_preg <- as.data.table(get(load(paste0(dirpregnancy, "D3_pregnancy_final.RData"))[[1]]))
  OBSERVATION_PERIODS_preg <- OBSERVATION_PERIODS_preg[, .(person_id, pregnancy_id, pregnancy_start_date, pregnancy_end_date)]
  OBSERVATION_PERIODS_preg[, pregnancy_start_date := pregnancy_start_date %m-% months(3)]
  OBSERVATION_PERIODS_preg[, pregnancy_end_date := pregnancy_end_date %m+% months(3)]
  
  firstjan2013 <- lubridate::ymd(20130101)
  
  setkey(OBSERVATION_PERIODS_preg, person_id, pregnancy_start_date, pregnancy_end_date)
  preg_obs <- data.table::foverlaps(person_spell[starts_after_ending == 0, ], OBSERVATION_PERIODS_preg,
                                    by.x = c("person_id", "entry_spell_category_crude", "exit_spell_category_crude"))
  preg_obs <- unique(preg_obs[, .(pregnancy_start_date = min(pregnancy_start_date),
                                  pregnancy_end_date = max(pregnancy_end_date),
                                  entry_spell_category, exit_spell_category),
                              by = c("person_id", "entry_spell_category_crude", "exit_spell_category_crude")])
  preg_obs <- preg_obs[, .(person_id, entry_spell_category, exit_spell_category,
                           additional_entry_cleaned = fifelse(pregnancy_start_date < lubridate::ymd(20130101), 1, 0),
                           additional_exit_cleaned = fifelse(exit_spell_category_crude != pregnancy_end_date, 1, 0))]
  person_spell <- merge(person_spell, preg_obs, by = c("person_id", "entry_spell_category", "exit_spell_category"), all.x = T)
  person_spell[, entry_spell_category_cleaned := pmax(entry_spell_category_cleaned, additional_entry_cleaned, na.rm = T)]
  person_spell[, exit_spell_category_cleaned := pmax(exit_spell_category_cleaned, additional_exit_cleaned, na.rm = T)]
  person_spell[, c("additional_entry_cleaned", "additional_exit_cleaned") := NULL]
}

# add a criteria that identify the specific spell of interest
person_spell[, is_the_study_spell := data.table::fifelse(starts_after_ending == 0 & no_overlap_study_period == 0 & too_old_at_start_spell == 0 & too_young_at_exit_spell == 0 & spell_less_than_12_months_fup == 0, 1, 0)]

# Remove spells which are not last
person_spell[is_the_study_spell == 1, max_exit_spell_category := max(exit_spell_category), by = person_id]
person_spell[exit_spell_category != max_exit_spell_category &
               thisdatasource %not in% datasources_only_preg, is_the_study_spell := 0]
person_spell[, c("max_exit_spell_category") := NULL]

# Remove censored spells
person_spell[(entry_spell_category_cleaned == 1 | exit_spell_category_cleaned == 1) &
               thisdatasource %in% datasources_only_preg, is_the_study_spell := 0]

person_spell[is.na(is_the_study_spell), is_the_study_spell := 0]

smart_save(person_spell, dirtemp, override_name = "D3_clean_spells", extension = extension, save_copy = "csv")
