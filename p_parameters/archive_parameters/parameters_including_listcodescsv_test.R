#--------------------------
# COVID_vaccine

concept_set_codes_our_study_pre[["Covid_vaccine"]][["ATC"]] <- c("J07BX03")


#--------------------------
# procedure for mechanical ventilation

concept_set_codes_our_study_pre[["ICU_VENTILATION"]][["ICD9PROC"]] <- c("96.70","96.71","96.72")
concept_set_codes_our_study_pre[["ICU_VENTILATION"]][["ICD10ES"]] <- c("5A19")



#--------------------------
# results from covid test recorded with a code

concept_set_codes_our_study_pre[["COVID_test_coded"]][["Veneto_lab_coding_system"]] <- c("91.12.1_0")


#--------------------------
# load conceptsets from csv files

csvdiagnosis <- "20220525_ROC20_full_codelist"
csvdrugproxies <- "20220525_ROC20_drug_proxies_codelist"

# load diagnosis
codelist_diagnosis <- fread(paste0(thisdir,"/p_parameters/archive_parameters/",csvdiagnosis,".csv"))
codelist_diagnosis <- df_to_list_of_list(codelist_diagnosis, codying_system_recode = "auto", imputed_tags = F, type_col = "type")

# load drug proxies
codelist_drug_proxies <- fread(paste0(thisdir,"/p_parameters/archive_parameters/", csvdrugproxies,".csv"))
codelist_drug_proxies <- df_to_list_of_list(codelist_drug_proxies, code_col = "atc_codes", concepts_col = "drug_proxy",
                                            codying_system_col = F, codying_system_recode = "auto", type_col = "type_drug_proxy")
# save(codelist_test, file = paste(thisdir, "20220525_ROC20_drug_proxies_codelist.RData", sep = "\\"))


#--------------------------
# ARDS: manual imputation

codelist_diagnosis[["ARDS_AESI_possible"]][["Free_text"]] <-
  c("ARDSpossible")

#--------------------------
# chronic conditions: manual imputation of exemption codes in PEDIANET, they are coded either in ICD9 or with local codes, vocabulary "internal_code/ICD9"

for (condition in c("ANYMALIGNANCY_COV","CHRONICPULMONARYDISEASE_CH","AIDS_CH_possible","KDCHRONIC_COV","DM12_COV","SICKLECELL_COV")){
  codelist_diagnosis[[condition]][["internal_code/ICD9"]] <-
    codelist_diagnosis[[condition]][["ICD9"]]
}
condition <- "AIDS_CH_possible"
codelist_diagnosis[[condition]][["internal_code/ICD9"]] <- c( codelist_diagnosis[[condition]][["internal_code/ICD9"]],c("020", "020.042", "020.042.079.53"))
condition <- "CHRONICPULMONARYDISEASE_CH"
codelist_diagnosis[[condition]][["internal_code/ICD9"]] <- c( codelist_diagnosis[[condition]][["internal_code/ICD9"]],c("057", "024.518.83", "007", "007.493"))
condition <- "KDCHRONIC_COV"
codelist_diagnosis[[condition]][["internal_code/ICD9"]] <- c( codelist_diagnosis[[condition]][["internal_code/ICD9"]],c("023", "023.585", "061", "061.581.1", "061.581.2", "061.582.1", "061.582.2", "061.582.4", "061.587", "061.590.0", "062.753.13", "RJ0030", "RJG010", "RJG020" ))
condition <- "DM12_COV"
codelist_diagnosis[[condition]][["internal_code/ICD9"]] <- c( codelist_diagnosis[[condition]][["internal_code/ICD9"]],c("013","013.250" ))
condition <- "ANYMALIGNANCY_COV"
codelist_diagnosis[[condition]][["internal_code/ICD9"]] <- c( codelist_diagnosis[[condition]][["internal_code/ICD9"]],c("048", "RB0010", "RB0020", "RB0030","RB0040","RB0050","RB0060","RB0070","RB0071","RGB010","RGB020","RGB021", "RDG050", "RC0230"))
condition <- "SICKLECELL_COV"
codelist_diagnosis[[condition]][["internal_code/ICD9"]] <- c( codelist_diagnosis[[condition]][["internal_code/ICD9"]],c("RDG010"))


