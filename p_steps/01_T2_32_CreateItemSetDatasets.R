# -----------------------------------------------------------------------
# RETRIEVE ITEMSET DATASETS

# input: SURVEY_ID, SURVEY_OBSERVATIONS
# output: itemset datasets


print("RETRIEVE ITEMSETS")


# RETRIEVE FROM SURVEY_OBSERVATIONS ALL itemset datasets from source_table,source_column (all study variables: if one has no itemset, the dataset is empty)
#-----------------------------------------------------


CreateItemsetDatasets(EAVtables = ConcePTION_CDM_EAV_tables_retrieve_source,
                      datevar= ConcePTION_CDM_datevar_retrieve,
                      dateformat= "YYYYmmdd",
                      rename_col = list(person_id = person_id_retrieve, date = date_retrieve),
                      study_variable_names = study_variables_of_our_study,
                      itemset = itemset_AVpair_our_study_this_datasource,
                      dirinput = dirinput,
                      diroutput = dirtemp,
                      extension = c("csv"))

# RETRIEVE FROM SURVEY_OBSERVATIONS ALL itemset datasets from origin,meaning (only study variables having this specification in the datasource are retrieved)
#-----------------------------------------------------

CreateItemsetDatasets(EAVtables = ConcePTION_CDM_EAV_tables_retrieve_meaning,
                      datevar= ConcePTION_CDM_datevar_retrieve,
                      dateformat= "YYYYmmdd",
                      rename_col = list(person_id = person_id_retrieve, date = date_retrieve),
                      study_variable_names = study_variables_this_datasource_meaning,
                      itemset = itemset_AVpair_our_study_this_datasource_meaning,
                      dirinput = dirinput,
                      diroutput = dirtemp,
                      extension = c("csv"))
