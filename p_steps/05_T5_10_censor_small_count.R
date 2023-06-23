smart_load("datasets_to_censor", dirpargen, extension = ".rds")
smart_load("variables_to_censor", dirpargen, extension = ".rds")

for (df_name in datasets_to_censor) {
  DRE_Threshold(diroutput, direxp, Varlist = variables_to_censor, FileContains = df_name, suffix = "masked")
}
