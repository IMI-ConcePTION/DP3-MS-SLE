smart_load("datasets_to_censor", dirpargen, extension = ".rds")
smart_load("variables_to_censor", dirpargen, extension = ".rds")

for (df_name in datasets_to_censor) {
  DRE_Threshold(direxp, direxpmask, Varlist = variables_to_censor, FileContains = df_name, suffix = "masked")
}

smart_load("datasets_to_censor_check", dirpargen, extension = ".rds")

for (df_name in datasets_to_censor_check) {
  DRE_Threshold(direxp, direxpcheck, Varlist = variables_to_censor, FileContains = df_name, suffix = "masked")
}