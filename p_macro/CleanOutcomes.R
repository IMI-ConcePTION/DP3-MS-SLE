CleanOutcomes <- function(Dataset = NULL, Person_id, Rec_period = NULL, Outcomes = NULL, Name_event = NULL, Date_event = NULL, print = F){
  
  if(length(Outcomes) != length(Rec_period)) stop("Specifiy the same number of Rec_periods as Outcomes")
  
  Dataset  <- copy(Dataset)[get(Name_event) %in% Outcomes]
  tmp <- copy(Dataset[0])
  
  for (i in 1:length(Outcomes)){
    
    
    if(print) print(paste("Remove ",Outcomes[i], "outcomes (conditions or vaccines) within a the Rec_period distance of ",Rec_period[i]," days"))
    Dataset_temp  <- copy(Dataset)[get(Name_event) == Outcomes[i],]
    
    it=1
    while(nrow(Dataset_temp) > 0){ 
      
      setorderv(Dataset_temp,c(Person_id,Name_event,Date_event))
      Dataset_temp <- Dataset_temp[,D := shift(get(Date_event)),by = c(Person_id,Name_event) ]
      Dataset_temp <- Dataset_temp[,dif := get(Date_event) - D]
      Dataset_temp <- Dataset_temp[is.na(dif), dif := 0 ][,dif := as.numeric(dif)]
      Dataset_temp <- Dataset_temp[,cumdif := cumsum(dif),by = c(Person_id,Name_event)]
      
      Dataset_temp2 <- Dataset_temp[ cumdif <= Rec_period[i],]
      setorderv(Dataset_temp2,c(Person_id,Name_event,Date_event))
      Dataset_temp2 <- Dataset_temp2[,.SD[1],by = c(Person_id,Name_event)][,Iteration := it]
      
      tmp <- rbindlist(list(tmp, Dataset_temp2),fill=T)
      rm(Dataset_temp2)
      
      Dataset_temp <- Dataset_temp[cumdif > Rec_period[i],]
      
      lapply(c("dif","cumdif","D"), function(x){Dataset_temp <-Dataset_temp[,eval(x) := NULL]})
      if(print) print(paste0("Cycle ",it))
      it=it+1
      gc()
    }
    
    
    
    rm(Dataset_temp, it)
    gc()
    
    
    
    
    
    
  }  
  lapply(c("dif","cumdif","D"), function(x){tmp <- tmp[,eval(x) := NULL]})
  setorderv(tmp,c(Person_id,Name_event,Date_event))
  return(tmp)
  
} 
