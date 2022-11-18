
#Author: Roel Elbers MSc.
#email: r.j.h.elbers@umcutrecht.nl
#Organisation: UMC Utrecht, Utrecht, The Netherlands
#Date: 15/07/2021

CountPersonTime <- function(Dataset_events = NULL, Dataset, Person_id, Start_study_time, End_study_time, Start_date,
                            End_date, Birth_date = NULL,Rec_period = NULL, Strata = NULL,Outcomes_nrec = NULL,
                            Outcomes_rec = NULL, Name_event = NULL, Date_event = NULL, Age_bands = NULL,
                            Unit_of_age = "year" , Increment = "year", include_remaning_ages = T, Aggregate = T,
                            print = F, check_overlap = T, save_intermediate = NULL, load_intermediate = F){
  
  if(print) print("Version 13.9")
  
  # Check if demanded R packages are installed, install if not,  and activate
  ################################################################################################################################
  if(print) print("Check packages data.table and lubridate")
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  ################################################################################################################################
  
  
  #Check some basic assumptions
  ################################################################################################################################
  if(!is.null(Dataset_events) & length(c(Outcomes_nrec, Outcomes_rec)) == 0) stop("No events specified in Outcomes_rec or Outcomes_nrec")
  if(is.null(Dataset_events) & length(c(Outcomes_nrec, Outcomes_rec)) > 0) stop("No Dataset_events specified while Outcomes_rec or Outcomes_nrec are specified")
  if(any(Outcomes_rec %in% Outcomes_nrec)){stop("Overlapping event names for Outcomes_rec and Outcomes_nrec")}
  if(length(Outcomes_rec) != length(Rec_period)) stop("Outcomes_rec and Rec_period are not of the same length")
  ################################################################################################################################
  
  #Set character labels to integer id's
  ################################################################################################################################
  
  if (!load_intermediate) {
  Dataset <- SetToInteger(Data = Dataset, colls = c(Person_id, Strata))
  Dictionary <-  Dataset$Dictionary
  Dataset <- Dataset$Data
  }
  
  if(!is.null(Dataset_events)) Dataset_events <- RenameId(Data = Dataset_events, colls = "person_id", Dictionary = Dictionary, AddId = T)
  
  if (!load_intermediate) {
  tmpname_dic <- tempfile(pattern = "dic", tmpdir = tempdir(), fileext = ".rds")
  saveRDS(Dictionary,  tmpname_dic) 
  rm(Dictionary)
  gc()
  }
  
  ################################################################################################################################
  
  
  #Store datasets that are needed later in the process in temp files to reduce in memeory load
  ################################################################################################################################
  if(!is.null(Dataset_events)){
    tmpname <- tempfile(pattern = "events", tmpdir = tempdir(), fileext = ".rds")
    saveRDS(Dataset_events, tmpname) 
    rm(Dataset_events)
    gc()
  }
  ################################################################################################################################
  
  if (!is.logical(load_intermediate)) {stop("Parameter 'load_intermediate' accepts only logical constants")}
  if (!missing(save_intermediate)) {
    tryCatch(dirname(save_intermediate), error = function(e)
      stop("Parameter 'save_intermediate' accepts only valid filepaths with already existing folders"))
  }
  
  if (load_intermediate) {
    if (!missing(save_intermediate) && file.exists(save_intermediate)) {
      load(save_intermediate)
     
    } else {
      Dataset <- as.data.table(Dataset)
      
    }
  } else {
    
    #Set character input for study dates to date format
    ################################################################################################################################
    if(print) print("Assign date format to Start_study_time and End_study_time")
    Start_study_time <- as.IDate(as.character(Start_study_time),"%Y%m%d")
    End_study_time <- as.IDate(as.character(End_study_time),"%Y%m%d")
    ################################################################################################################################
    
    
    
    
    CheckAndPrepareDates(Dataset = Dataset, Person_id = Person_id, Start_study = Start_study_time, End_study = End_study_time, Start_date = Start_date, End_date = End_date, 
                          Birth_date = Birth_date, Age_bands = Age_bands, Increment = Increment, print = print, check_overlap = check_overlap
    )
    gc()
    
    #Select relevant spells
    ################################################################################################################################
    intv <- as.IDate(c(Start_study_time, End_study_time))
  
    Dataset <- Dataset[, c(Person_id, Start_date, End_date, Birth_date, Strata) , with = F][get(Start_date) %between% intv|get(End_date) %between% intv|(get(Start_date) < Start_study_time & get(End_date) > End_study_time)] 
    

    if(nrow(Dataset) == 0){
      Dataset <- NULL
      if(print) print("No subjects with any observation time within studyperiod. NULL is returned")
      return(Dataset)
    }
    
    Dataset[get(Start_date) < Start_study_time,eval(Start_date) := Start_study_time]
    Dataset[get(End_date) > End_study_time,eval(End_date) := End_study_time]
    
    
    
    
  #Determine age bands per person level spell
  ################################################################################################################################
    
    #Create file with all the needed agebands
    ###
    
    if(!is.null(Age_bands)){
      Agebands_list <-  CreateAgebandIntervals(ages = Age_bands, include = include_remaning_ages)
      
      
      
    
    
    #Create file with agebands for every spell. If multiple agebands in a spell than split the spell.
    ###
    
    Dataset <- SplitSpellsAgeBands(
      Dataset = Dataset,
      Start_date = Start_date,
      End_date = End_date,
      Birth_date = Birth_date,
      Unit_of_age = Unit_of_age,
      Agebands_list = Agebands_list[, .(Ageband, ST, EN)],
      print = print
    )
    
  }else{
    Agebands_list = NULL
  }  
    
  ################################################################################################################################  
  
      
  #Enlarge table by time increment. 
  ################################################################################################################################
    
  if(print) print(paste0("Transform input date to a dataset per ", Increment, ". This step increases the size of the file with respect to the choosen increment" ))
  
  #Create a file with the relevant time intervals like with age bands.This is used for joining with the aim to assign labels 
  Dummy <- CreateTimeIntervals(Start_study_time = Start_study_time, End_study_time = End_study_time, Increment = Increment)
  Dummy[, ID := as.integer(row.names(Dummy))]
    
  setkeyv(Dataset, c(Start_date, End_date))
  Dataset <- foverlaps(Dummy, Dataset, by.x=c(Increment, "End"), nomatch = 0L, type = "any")
  
  Dataset <- Dataset[get(Start_date) <= get(Increment) & get(End_date) >= get(Increment),eval(Start_date) := get(Increment)]
  Dataset <-Dataset[get(End_date) >= End & get(Start_date) <= End, eval(End_date) := End][, End := NULL][, eval(Increment) := NULL]
  #Dataset2 <- merge(Dataset, Dummy[, c(Increment, "ID"), with = F], by = "ID", all.x = T )[, `:=`  (month = NULL)]
  setnames(Dataset, "ID", Increment)
  #[, `:=`  (End = NULL, eval(Increment) = NULL)]
  
  gc()
  
  #Add id's for timing to dictionary
  ################################################################################################################################
  
  
  if(Increment=="month"){Dummy[,eval(Increment) := substr(get(Increment),1,7)]}
  if(Increment=="year"){Dummy[,eval(Increment) := substr(get(Increment),1,4)]}
  
  Dictionary <-  readRDS(tmpname_dic)
  Dictionary[[Increment]] <- Dummy[, c(Increment, "ID"), with = F]
  
  if(!is.null(Agebands_list)){
    Dictionary[["Ageband"]] <- as.data.table(cbind(Ageband = Agebands_list$Label, ID = Agebands_list$Ageband))[, ID := as.integer(ID)][, Ageband := as.character(Ageband)]
    
  }
  

  saveRDS(Dictionary, tmpname_dic )
  
  rm(Dictionary)
  gc()
  
  ################################################################################################################################
  
  
  #Calculate person times and if needed create the intermediate file. 
  ################################################################################################################################
  
  if(print) print("Calculate general persontime")
  Dataset[,Persontime := .(get(End_date) - get(Start_date) + 1)]
  
  
  if (!missing(save_intermediate)) {
    if(print) print("Save intermediate table")
    Dictionary <-  readRDS(tmpname_dic)
    save(Dataset, intv, Dictionary,  file = save_intermediate)
    rm(Dictionary)
    gc()
  } 
  
  }
  
  ################################################################################################################################
  
  #add column parameters for rest of script. These varaibles are used for selction, sorting or aggrgating.
  ################################################################################################################################
  
  if(is.null(Age_bands)){
    
    by_colls <- c(Strata, Increment)
    sort_order <- c(Person_id, Start_date, End_date, Strata)}else{
    
    by_colls <- c(Strata, Increment, "Ageband")  
    sort_order <- c(Person_id, Start_date, End_date, "Ageband", Strata)
  
    }
  
  if(Aggregate) sort_order <- by_colls
  
  if(length(c(Outcomes_nrec, Outcomes_rec)) > 0) coln <- c(sort_order, Increment, "Persontime", 
                                                           paste0("Persontime_",c(Outcomes_nrec, Outcomes_rec)),
                                                           paste0(c(Outcomes_nrec, Outcomes_rec),"_b")
                                                           )else{coln <- c(sort_order, Increment, "Persontime")}
  
  ################################################################################################################################
  
  
  #If there is no events dataset saved as tmpname, then do the aggregation if needed.
  ################################################################################################################################
  if(!exists("tmpname") & Aggregate){
    Dataset <- Dataset[, lapply(.SD, sum), .SDcols = "Persontime", by = by_colls]
  }
  ################################################################################################################################
  

  #If there is a events dataset saved as tmpname, then load and prepare
  ################################################################################################################################
  
  if(exists("tmpname")){
    Dataset_events <- readRDS(tmpname)[, c(Person_id, Name_event, Date_event) , with = F][
      #get(Date_event) %between% intv & 
        get(Name_event) %in% c(Outcomes_nrec, Outcomes_rec) &
        get(Person_id) %in% unique(Dataset[[Person_id]])  
      , ]
    
  }

  #Separate recurrent from not recurrent events and clean the recurrent events with lag time > 0
  
  if(!is.null(Outcomes_nrec)) Dataset_events_nrec <- Dataset_events[get(Name_event) %in% Outcomes_nrec,]
  
  if(!is.null(Outcomes_rec)){
    if((sum(Rec_period == 0) != length(Rec_period)) & exists("Dataset_events")){
      
        rec1 <- Outcomes_rec[ Outcomes_rec  %in% Outcomes_rec[!Rec_period == 0]]
        rec0 <- Outcomes_rec[!Outcomes_rec %in% rec1]
        Rec_period1 <- Rec_period[Outcomes_rec %in% rec1]
          
        Dataset_events1 <- Dataset_events[get(Name_event) %in% rec1,]
        Dataset_events0 <- Dataset_events[get(Name_event) %in% rec0,]
        
        rm(Dataset_events)
        gc()
              
        Dataset_events1 <- CleanOutcomes(Dataset = Dataset_events1, Person_id = "person_id", Rec_period = Rec_period, Outcomes = Outcomes_rec, Name_event = "name_event", Date_event = "date_event")[, Iteration := NULL]
        
        Dataset_events <- rbindlist(list(Dataset_events0, Dataset_events1), use.names = T, fill = T)
        
        rm(Dataset_events0, Dataset_events1)
        gc()
  
  
    }
  }else{
    rm(Dataset_events)
    gc()
    }
  
  ################################################################################################################################  
  
  
  #Not recurrent events
  ################################################################################################################################
  
  if(exists("Dataset_events_nrec") & !is.null(Outcomes_nrec)){
    
    set1 <- Dataset[person_id %in% unique(Dataset_events_nrec$person_id),]
    set2 <- Dataset[!person_id %in% unique(Dataset_events_nrec$person_id),]
    
    rm(Dataset)
    gc()
    
    SUB <- CalculateNumeratorNotRecurrent(
                
                Dataset_events = Dataset_events_nrec, 
                Dataset = set1, 
                Person_id = Person_id,
                Start_date = Start_date,
                End_date = End_date, 
                Strata = Strata, 
                Outcomes_nrec = Outcomes_nrec,
                Name_event = Name_event, 
                Date_event = Date_event,  
                Aggregate = F,
                print = print 
                
                )
    
    rm(set1)  
    gc()  
    
    colls <- paste0("Persontime_",Outcomes_nrec)
    colls <- colls[colls %in% colnames(SUB)]
    lapply(colls, function(x) set2 <- set2[, eval(x) := Persontime])
    rm(colls)
      
    Dataset <- rbindlist(list(SUB, set2), fill = T, use.names = T)
    Dataset[is.na(Dataset), ] <- 0
    
    if(Aggregate){
      
      
      colls <- c("Persontime",paste0(Outcomes_nrec,"_b"),paste0("Persontime_",Outcomes_nrec))
      colls <- colls[colls %in% colnames(Dataset)]
      
      Dataset <- Dataset[, lapply(.SD, sum), .SDcols = colls, by = by_colls]
      rm(colls)
      
    }
    
    rm(set2)
    gc()
  
  }
  ################################################################################################################################  
    
    
  
  #Recurrent events
  ################################################################################################################################
  
  
  if(exists("Dataset_events") & !is.null(Outcomes_rec)){
    
    
    if(print) print("Calculate recurrent events not aggregated")
  
    set1 <- Dataset[person_id %in% unique(Dataset_events$person_id),]
    set2 <- Dataset[!person_id %in% unique(Dataset_events$person_id),]
    rm(Dataset)
    
    mergeback <- colnames(set1)[!colnames(set1) %in% c("Persontime")]
    SUB <- CalculateSubstractionDenominator(
      Dataset = set1[, ..mergeback],
      Start_date = Start_date,
      End_date = End_date,
      Dataset_events = Dataset_events,
      Person_id = Person_id,
      Name_event = Name_event,
      Date_event = Date_event,
      Outcomes_rec = Outcomes_rec,
      Rec_period = Rec_period,
      Aggregate = Aggregate,
      Strata = by_colls,
      Include_count = T,
      print = print
      
    )
    
    rm(Dataset_events)
    gc()
    
    if(Aggregate){
    set1 <- set1[, .(Persontime = sum(Persontime)) , by = by_colls]
    set2 <- set2[, .(Persontime = sum(Persontime)) , by = by_colls]
    mergeback <- mergeback[!mergeback %in% c(Start_date, End_date, Person_id, Birth_date)]
    Dataset <- rbindlist(list(set1, set2), fill = T, use.names = T)
    Dataset <- Dataset[, .(Persontime = sum(Persontime)) , by = by_colls]
    Dataset <-  merge(Dataset, SUB, by = mergeback, all.x = T)
    }
    
    if(!Aggregate){
    Dataset <-  merge(set1, SUB, by = mergeback, all.x = T)
    Dataset <- rbindlist(list(Dataset, set2), fill = T, use.names = T)
    }
    
    
    rm(mergeback, set1, set2, SUB)
    gc()
    
    #temp <- copy(Dataset)
    #Dataset <- copy(temp)
    
    colls <- Outcomes_rec[Rec_period != 0]
    colls <- colls[paste0("SUBTRCUM_", colls) %in% colnames(Dataset)]
    
    if(!Aggregate){
    lapply(colls, function(x)
      
      
      Dataset[, eval(paste0("Persontime_", x)) := 
                
                fifelse(!is.na(get(paste0("SUBTRCUM_",x))), 
                        get(End_date) - get(Start_date) + 1 - get(paste0("SUBTRCUM_",x)), 
                        Persontime)
                ][, eval(paste0("SUBTRCUM_",x)) := NULL]
      )
    }else{
      
      #Dataset <- merge(x= readRDS(tmpname3), y = Dataset, by = by_colls, all.x = T)
      
      lapply(colls, function(x)
        
    
        Dataset[, eval(paste0("Persontime_", x)) := 
                  
                  fifelse(!is.na(get(paste0("SUBTRCUM_",x))), 
                          Persontime - get(paste0("SUBTRCUM_",x)), 
                          Persontime)
        ][, eval(paste0("SUBTRCUM_",x)) := NULL]
      )
      
      
      }
    
    rm(colls)
    
    #colls <- Outcomes_rec[Rec_period == 0]
    colls <- Outcomes_rec[!paste0("Persontime_", Outcomes_rec) %in% colnames(Dataset)]
    lapply(colls, function(x) Dataset[, eval(paste0("Persontime_", x)) := Persontime])
    
    rm(colls)
    gc()
  }
  
  ################################################################################################################################
  
  Dataset[is.na(Dataset), ] <- 0

  Outcomes <- c(Outcomes_nrec, Outcomes_rec)
  
  if(length(Outcomes) > 0 & exists("tmpname")){
    
    
    #colls <- colnames(Dataset)[grepl(pattern = paste0(paste0(Outcomes_rec,"_b"), collapse = "|"), colnames(Dataset))]
    
    
    B_MISSING <- Outcomes[!paste0(Outcomes,"_b") %in% unique(colnames(Dataset))]
    if(length(B_MISSING) > 0) lapply(paste0(B_MISSING,"_b"), function(x){Dataset <- Dataset[,eval(x) := 0]})
    
    P_MISSING <- Outcomes[!paste0("Persontime_",Outcomes) %in% unique(colnames(Dataset))]
    if(length(P_MISSING) > 0) lapply(paste0("Persontime_",P_MISSING), function(x){Dataset <- Dataset[,eval(x) := 0]})
    
    rm(P_MISSING, B_MISSING)
    
    
  }
  
  rm(Outcomes)
  
  
  Dictionary <-  readRDS(tmpname_dic)
  if(Aggregate){colls <- names(Dictionary)[!names(Dictionary) %in% Person_id]}else{colls <- names(Dictionary)}
  
  Dataset <- RenameId(Data = Dataset, colls = colls , Dictionary = Dictionary, AddId = F)
  rm(Dictionary, colls)
  gc()
  
  Dataset <- Dataset[, coln, with=FALSE]
  setorderv(Dataset, sort_order)
  rm(sort_order)

  if(exists("tmpname")) if(file.exists(tmpname))  unlink(tmpname)
  if(exists("tmpname_dic")) if(file.exists(tmpname_dic))  unlink(tmpname_dic)
  #if(exists("tmpname3")) if(file.exists(tmpname3))  unlink(tmpname3)
  
  
  return(Dataset)
  
  
  
  
  
  
  
  
  
  }
  

  
  