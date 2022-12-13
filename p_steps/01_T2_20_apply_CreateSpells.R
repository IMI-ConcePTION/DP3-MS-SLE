# OBSERVATION PER IODS -----------------------------------------------------
#COMPUTE SPELLS

# input: OBSERVATION_PERIODS
# output: D3_output_spells_category


print("COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS")

# import input datasets
OBSERVATION_PERIODS <- read_CDM_tables("OBSERVATION_PERIODS")

OBSERVATION_PERIODS <- OBSERVATION_PERIODS[, op_meaning:="all"]
D3_output_spells_category <- CreateSpells(
  dataset = OBSERVATION_PERIODS,
  id = "person_id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  replace_missing_end_date = study_end,
  gap_allowed = gap_days
)

D3_output_spells_category <- as.data.table(D3_output_spells_category)
setkeyv(
  D3_output_spells_category,
  c("person_id", "entry_spell_category", "exit_spell_category", "num_spell", "op_meaning")
)

save(D3_output_spells_category, file = paste0(dirtemp,"D3_output_spells_category.RData"))
