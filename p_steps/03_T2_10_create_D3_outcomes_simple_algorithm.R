#------------------------------------------------------------------
# create events and components of OUTCOMES 

# input: concept set datasets of outcomes (narrow and possible), D4_study_population
# output: for each outcome OUTCOME, D3_components_OUTCOME.RData and D3_events_OUTCOME_type.RData, for type = narrow, possible

print('create events and create components of OUTCOMES and CONTROLS')

# 
# load(paste0(dirpargen,"subpopulations_non_empty.RData"))

##for each var in OUTCOME and for each negative outcome create D3_var including all dates when that outcome is observed (use the corresponding conceptsets)

for (OUTCOME in c(OUTCOME_variables[OUTCOME_variables %not in% SECCOMPONENTS], CONTROL_variables)) {
  tempOUTCOME <- vector(mode="list")
  componentsOUTCOME <- vector(mode="list")
  print(OUTCOME)
  for (subpop in subpopulations_non_empty) {
    print(subpop)
    load(paste0(diroutput,"D4_study_population",suffix[[subpop]],".RData")) 
    study_population <- get(paste0("D4_study_population", suffix[[subpop]]))
    
    COHORT_TMP <- as.data.table(study_population)  

    COHORT_TMP <- COHORT_TMP[,.(person_id, study_entry_date)]
   
    namenewvar<-c()
    
    counter<-0
    counter2<-0
    summarystatOUTCOME<-vector(mode="list")
    addvarOUTCOME <- vector(mode="list")
    FirstJan<-vector(mode="list")
    for (year in ComponentAnalysisYears) {
      FirstJan[[year]] <- ymd(paste0(year,"0101"))
      
      for (level1 in c("HOSP","PC")) {
        namenewvar <- paste0(OUTCOME,level1, year, sep = "_")
        counter<-counter+1
        counter2<-counter2+1
        summarystatOUTCOME[[counter2]]<-list(c("max"),namenewvar,namenewvar)
        addvarOUTCOME[[counter]]=list(c(namenewvar),"1",paste0("(",condmeaning[[level1]], ") & date <= as.Date('",FirstJan[[year]],"') + 365 & date >= as.Date('",FirstJan[[year]],"')"))
        counter<-counter+1
        addvarOUTCOME[[counter]]=list(c(namenewvar),"0",paste0("is.na(",namenewvar,")"))
      }
    }
    
    selectionOUTCOME <- "date >= study_entry_date - 365 "
    
    # delete records that are not observed in this whole subpopulation
    if (this_datasource_has_subpopulations == TRUE){
      selectionOUTCOME <- paste0(selectionOUTCOME,' & ',select_in_subpopulationsEVENTS[[subpop]])
    }
    
    nameconceptsetdatasetOUTCOMEtype <- variable_definition[[OUTCOME]]
    conceptsets_list <- lapply(nameconceptsetdatasetOUTCOMEtype,
                               function(x) get(load(paste0(dirconceptsets, x,".RData"))[[1]]))
    components <- MergeFilterAndCollapse(
      listdatasetL= conceptsets_list,
      condition = selectionOUTCOME,
      key = c("person_id"),
      datasetS = COHORT_TMP,
      additionalvar = addvarOUTCOME,
      saveintermediatedataset = T,
      nameintermediatedataset = paste0(dirtemp,'tempfile'),
      strata = c("person_id"),
      summarystat = summarystatOUTCOME
    )
    
    load(paste0(dirtemp,'tempfile.RData') )
    
    tempOUTCOME <- tempfile
    componentsOUTCOME<- components 
    
    nameobjectOUTCOMEtype <- paste0('D3_events_', OUTCOME, '_simple', suffix[[subpop]])
    foroutput <- tempOUTCOME
    assign(nameobjectOUTCOMEtype,foroutput)
    save(nameobjectOUTCOMEtype,file=paste0(direvents,paste0(nameobjectOUTCOMEtype,".RData")),list = nameobjectOUTCOMEtype)
    rm(foroutput)
    rm(nameobjectOUTCOMEtype,list = nameobjectOUTCOMEtype)
    
    
    
    rm(nameconceptsetdatasetOUTCOMEtype)
    
    nameobjectOUTCOME <- paste0("D3_components","_",OUTCOME,suffix[[subpop]])
    componentsOUTCOMEfinal <- vector(mode = 'list')
    OUTCOME_narrow <- componentsOUTCOME
    
    temp2 <- merge(COHORT_TMP,OUTCOME_narrow, by="person_id",all.x  = T)
    for (i in names(temp2)) temp2[is.na(get(i)), (i):=0]
    componentsOUTCOMEfinal <- temp2
    
    assign(nameobjectOUTCOME, componentsOUTCOMEfinal)
    
    save(nameobjectOUTCOME,file=paste0(dircomponents,paste0(nameobjectOUTCOME,".RData")),list= nameobjectOUTCOME)
    rm(OUTCOME_narrow, temp2,componentsOUTCOMEfinal,componentsOUTCOME,tempOUTCOME)
    rm(nameobjectOUTCOME, list = nameobjectOUTCOME)
    
    rm(addvarOUTCOME,study_population,summarystatOUTCOME, COHORT_TMP,tempfile,components)
  }
}
