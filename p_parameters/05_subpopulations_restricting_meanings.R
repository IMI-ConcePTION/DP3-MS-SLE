###################################################################
# DESCRIBE THE PARAMETERS OF SUBPOPULATIONS RESTRICTING
###################################################################

# datasources_with_subpopulations lists the datasources where some meanings of events should be excluded during some observation periods, associated with some op_meanings

datasources_with_single_subpopulation <- c("THL", "SAIL Databank")

# this_datasource_has_subpopulations <- ifelse(thisdatasource %in% datasources_with_subpopulations, TRUE, FALSE) 

# subpopulations associates with each datasource the label of its subpopulations
subpopulations <- list()

# op_meaning_sets labels each group of op_meaning that are relevant
op_meaning_sets <- vector(mode="list")

# op_meaning_sets separates all the op_meanings available in OBSERVATION_PERIODS in op_meaning_sets (3-levels list: the datasource, and the op_meaning_sets)
op_meanings_list_per_set <- vector(mode="list")

# op_meaning_sets associates to each subpopulation the corresponding overlap of op_meaning_sets
op_meaning_sets_in_subpopulations <- vector(mode="list")

# # exclude_meaning_renamed associates to each subpopulation the corresponding meaning of events that should not be processed (3-levels list: the datasource, and the subpopulation) 
# exclude_meaning_renamed <- vector(mode="list") 
# exclude_itemset_of_so <- vector(mode="list") 


# datasource THL
subpopulations[["THL"]] = c("GP")

op_meaning_sets[["THL"]] <- c("meanings_GP")
op_meanings_list_per_set[["THL"]][["meanings_GP"]] <- c("observed_in_medicine_data")

op_meaning_sets_in_subpopulations[["THL"]][["GP"]] <- c("meanings_GP")

# datasource SAIL Databank
subpopulations[["SAIL Databank"]] = c("GP")

op_meaning_sets[["SAIL Databank"]] <- c("meanings_GP", "meanings_residency")
op_meanings_list_per_set[["SAIL Databank"]][["meanings_GP"]] <- c("gp_lookback","gp_registration")
op_meanings_list_per_set[["SAIL Databank"]][["meanings_residency"]] <- c("residency")

op_meaning_sets_in_subpopulations[["SAIL Databank"]][["GP"]] <- c("meanings_GP", "meanings_residency")


# if (this_datasource_has_subpopulations == TRUE){ 
#   # define selection criterion for events
#   select_in_subpopulationsEVENTS <- vector(mode="list")
#   for (subpop in subpopulations[[thisdatasource]]){
#     select <- "!is.na(person_id) "
#     for (meaningevent in exclude_meaning_renamed[[thisdatasource]][[subpop]]){
#       select <- paste0(select," & meaning_renamed!= '",meaningevent,"'")
#     }
#     select_in_subpopulationsEVENTS[[subpop]] <- select
#   }
#   # define selection criterion for Survey_OBSERVATIONS
#   select_in_subpopulationsSO <- vector(mode="list")
#   for (subpop in subpopulations[[thisdatasource]]){
#     select <- "(!is.na(person_id) "
#     for (itemsetSO in exclude_itemset_of_so[[thisdatasource]][[subpop]]){
#       select <- paste0(select," & so_source_table != '",itemsetSO[1],"' & so_source_column != '",itemsetSO[2],"'")
#     }
#     select <- paste0(select,")")
#     select_in_subpopulationsSO[[subpop]] <- select
#   }
#   
#   # create multiple directories for export
#   direxpsubpop <- vector(mode="list")
#   for (subpop in subpopulations[[thisdatasource]]){
#     direxpsubpop[[subpop]] <- paste0(thisdir,"/g_export_", subpop,'/')
#     suppressWarnings(if (!file.exists(direxpsubpop[[subpop]])) dir.create(file.path(direxpsubpop[[subpop]])))
#     file.copy(paste0(dirinput,'/METADATA.csv'), direxpsubpop[[subpop]], overwrite = T)
#     file.copy(paste0(dirinput,'/CDM_SOURCE.csv'), direxpsubpop[[subpop]], overwrite = T)
#     file.copy(paste0(dirinput,'/INSTANCE.csv'), direxpsubpop[[subpop]], overwrite = T)
#     
#     file.copy(paste0(thisdir,'/to_run.R'), direxpsubpop[[subpop]], overwrite = T)
#   }
# }

# if (this_datasource_has_subpopulations==F) {
#   dummytables <- paste0(direxp, "Dummy tables/")
#   suppressWarnings(if (!file.exists(dummytables)) dir.create(file.path(dummytables)))
# }


# suffix <- vector(mode="list")
# 
# if (this_datasource_has_subpopulations == FALSE) {
#   subpopulations_non_empty <- c('ALL')
#   #subpopulations[[thisdatasource]] <- c('ALL')
#   suffix[['ALL']] <- ''
#   direxpsubpop <- vector(mode="list")
#   direxpsubpop[['ALL']] <- paste0(thisdir, "/g_export/")
#   
#   dirtablesubpop <- vector(mode="list")
#   dirtablesubpop[['ALL']] <- paste0(direxpsubpop[['ALL']], "Dummy tables/")
#   suppressWarnings(if (!file.exists(dirtablesubpop[['ALL']])) dir.create(file.path(paste0(dirtablesubpop[['ALL']]))))
#   
#   dirD4D5subpop <- vector(mode="list")
#   dirD4D5subpop[['ALL']] <- paste0(direxpsubpop[['ALL']], "D4-D5 tables/")
#   suppressWarnings(if (!file.exists(dirD4D5subpop[['ALL']])) dir.create(file.path(paste0(dirD4D5subpop[['ALL']]))))
# }else{
#   subpopulations_non_empty <- subpopulations[[thisdatasource]]
#   dirtablesubpop <- vector(mode="list")
#   dirD4D5subpop <- vector(mode="list")
#   for (subpop in subpopulations_non_empty) {
#     suffix[[subpop]] <- paste0('_', subpop)
#     dirtablesubpop[[subpop]] <- paste0(direxpsubpop[[subpop]], "Dummy tables/")
#     suppressWarnings(if(!file.exists(dirtablesubpop[[subpop]])) dir.create(file.path(paste0(dirtablesubpop[[subpop]]))))
#     
#     
#     dirD4D5subpop[[subpop]] <- paste0(direxpsubpop[[subpop]], "D4-D5 tables/")
#     suppressWarnings(if (!file.exists(dirD4D5subpop[[subpop]])) dir.create(file.path(paste0(dirD4D5subpop[[subpop]]))))
#   }
# }
# 
# for (subpop in subpopulations[[thisdatasource]]) {
#   fileConn <- file(paste0(direxpsubpop[[subpop]], "subpop.txt"))
#   writeLines(subpop, fileConn)
#   close(fileConn)
# }

rm(subpopulations)


