



CalculateNumeratorAggregated <- function(
                                          Dataset,
                                          Person_id,
                                          Start_date, 
                                          End_date, 
                                          Dataset_events, 
                                          Agebands.file = NULL, 
                                          Times.file = NULL, 
                                          Name_event, 
                                          Date_event, 
                                          Birth_date = NULL, 
                                          Strata = NULL){
      
      Increment <- colnames(Times.file)[1]
      
      Dataset <- copy(Dataset)
      Dataset_events <- copy(Dataset_events)
      
      setnames(Dataset_events, c(eval(Person_id), eval(Name_event), eval(Date_event)), c("ID","EVNT", "DTEVNT"))
      
      if(!is.null(Agebands.file)) setnames(Dataset, c(eval(Person_id), eval(Start_date), eval(End_date), eval(Birth_date)), c("ID", "ST", "EN", "DTBRT"))
      if(is.null(Agebands.file))  setnames(Dataset, c(eval(Person_id), eval(Start_date), eval(End_date)), c("ID", "ST", "EN"))
      
      Dataset_events <- copy(Dataset_events)[, dummy0 := DTEVNT]
      
      setkeyv(Dataset, c("ID", "ST", "EN"))
      Events <- foverlaps(Dataset_events, Dataset, by.x = c("ID","DTEVNT","dummy0"), nomatch = 0L, type = "any")[, ":=" (ST =NULL, EN = NULL)]
      
      rm(Dataset, Dataset_events)
      gc()
      
      if(!is.null(Agebands.file)){
        Events <- Events[, Age :=  floor(time_length(interval(DTBRT, DTEVNT),"year"))][, dummy := Age]
        setkeyv(Events, c("Age", "dummy"))
        Events <- foverlaps(Agebands.file, Events, by.x = c("ST", "EN"), nomatch = 0L, type = "any")[, ":=" (dummy = NULL, ST =NULL, EN = NULL, row = NULL, Age = NULL, DTBRT = NULL)]
      }
      
      if(!is.null(Times.file)){
      setkeyv(Events, c("DTEVNT","dummy0"))
      Events <- foverlaps(Times.file, Events, by.x = c(Increment,"End"), nomatch = 0L, type = "any")[, ":=" (End =NULL)]
      }
      
      Events <- Events[, dummy0 := NULL][, var := 1]
      
      by <- Strata
      
      if(!is.null(Agebands.file)) by <- c(by, "Ageband")
      if(!is.null(Times.file)) by <- unique(c(by, colnames(Times.file)[1]))
      
      if(length(by) == 0) stop("No by variables")
      Events <- data.table::dcast(Events[, EVNT := paste0(EVNT,"_b")], formula(paste0(paste(by, collapse = " + "), " ~  EVNT")) , value.var = "var", fun.aggregate = sum)
      
      
      lapply(by, function(x) Events[, eval(x) := as.character(get(x))])
      
      if(Increment=="month"){Events[,eval(Increment) := substr(get(Increment),1,7)]}
      if(Increment=="year"){Events[,eval(Increment) := substr(get(Increment),1,4)]}
      
      
      return(Events)
}
