CountPrevalence <- function(Dataset_cohort, Dataset_events, UoO_id,key=NULL,Start_date, End_date,  Birth_date = NULL,Name_condition,Date_condition, Date_end_condition=NULL, Type_prevalence, Points_in_time=NULL, Increment = NULL, Periods_of_time=NULL, Increment_period=NULL, Conditions , Start_study_time, End_study_time, Age_bands = NULL, Unit_of_age = "year" ,include_remaning_ages = T, Strata =NULL, Aggregate = T, drop_not_in_population = F){
  
  print("Version 1.0")
  # Check if demanded R packages are installed, install if not,  and activate
  ################################################################################################################################
  print("Check packages data.table and lubridate")
  if (!require("data.table")) install.packages("data.table")
  library(data.table)
  
  if (!require("lubridate")) install.packages("lubridate")
  library(lubridate)
  ################################################################################################################################
  
  #Set character input for study dates to date format
  ################################################################################################################################
  print("Assign date format to Start_study_time and End_study_time")
  Start_study_time<-as.IDate(Start_study_time, "%Y%m%d")
  End_study_time<-as.IDate(End_study_time, "%Y%m%d")
  
  ################################################################################################################################
  #create the object used as choosen key (between key and unit of observation)
  if(!is.null(key)) {
    choosen_key<-key
  }else{
    choosen_key<-UoO_id
  }
  
  #create the object containing the columns of ids to keep
  if (all(UoO_id %in% choosen_key)) {
    id_columns_tokeep <- choosen_key
  } else {
    id_columns_tokeep <- unique(c(choosen_key, UoO_id))
  }
  
  
  #keep only the conditions/drugs of interest in the Dataset_events
  Dataset_events<-Dataset_events[get(Name_condition) %in% Conditions,]
  
  
  #keep only the columns of interest in the Dataset_events
  cols_to_keep_events=c(choosen_key,Name_condition,Date_condition)
  Dataset_events<-unique(Dataset_events[,cols_to_keep_events,with=F])
  
  #keep only the columns of interest in the Dataset_cohort
  cols_to_keep_cohort=c(id_columns_tokeep,Start_date,End_date)
  if (!is.null(Strata)) cols_to_keep_cohort<-c(cols_to_keep_cohort,Strata)
  if (!is.null(Birth_date)) cols_to_keep_cohort<-c(cols_to_keep_cohort,Birth_date)
  
  
  if (Type_prevalence == "point") { 
    if (!is.null(Points_in_time)) {
      if(Points_in_time[[1]] %in% names(Dataset_cohort)) {
        cols_to_keep_cohort<-c(cols_to_keep_cohort,Points_in_time)
      }
    }
  }
  
  
  if (Type_prevalence == "period" | Type_prevalence == "of use") { 
    if (!is.null(Periods_of_time)) {
      if(Periods_of_time[[1]][[1]] %in% names(Dataset_cohort)) {
        cols_to_keep_cohort<-c(cols_to_keep_cohort,unlist(lapply(Periods_of_time, `[[`, 1)),unlist(lapply(Periods_of_time, `[[`, 2)))
        cols_to_keep_cohort<-unique(c(cols_to_keep_cohort,Points_in_time))
      }
    }
  }
  
  Dataset_cohort<-unique(Dataset_cohort[,cols_to_keep_cohort,with=F])
  
  # #check if study start and stop dates are valid
  # ################################################################################################################################
  # 
  # print("Check if Start_study_time and End_study_time are valid")
  # if(!sum(Start_study_time==seq.Date(as.Date("19000101","%Y%m%d"),Sys.Date(),by = Increment))==1){
  #   
  #   if(Increment == "year"){stop("Change the start date to the first of january. Wrong study start date can produce invalid results.")}
  #   if(Increment == "month"){stop("Change the start date to the first of month. Wrong study start date can produce invalid results.")}
  #   if(Increment == "week"){stop("Change the start date to a monday. Wrong study start date can produce invalid results.")}
  #   
  # }
  # 
  # if(!sum(End_study_time==seq.Date(as.Date("19000101","%Y%m%d"),end_date_new ,by = Increment)-1)==1){
  #   
  #   if(Increment == "year"){stop("Change the end date to the 31th of december. Wrong study start date can produce invalid results.")}
  #   if(Increment == "month"){stop("Change the end date to the last day of the month. Wrong study start date can produce invalid results.")}
  #   if(Increment == "week"){stop("Change the end date to a sunday. Wrong study start date can produce invalid results.")}
  #   
  # }
  
  gc()
  
  ################################################################################################################################
  #select the dates columns and tranform them to date format
  if(!is.null(Birth_date)) {
    date_cols<-c(Start_date,End_date,Birth_date)
  }else{
    date_cols<-c(Start_date,End_date)
    
    if (!is.null(Periods_of_time)) {
      if (Periods_of_time[[1]][[1]] %in% colnames(Dataset_cohort)) {
        periods<-unlist(Periods_of_time)
        date_cols<-c(date_cols,periods[!(periods %in% date_cols)])
      }
    }
    
    
    if (!is.null(Points_in_time)) {
      if (Points_in_time[[1]] %in% colnames(Dataset_cohort)) {
        periods<-unlist(Points_in_time)
        date_cols<-c(date_cols,periods[!(periods %in% date_cols)])
      }
    } 
    
    
  }
  
  
  # Reduce memory size using integers
  Dataset_cohort[, c(date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
  #Dataset_cohort[, c(Strata) := lapply(.SD, as.integer), .SDcols = Strata]
  gc()
  
  
  ###################################à
  #In case of prevalence of use, remove the dispensations/prescriptions outside the Start_study_time-End_study_time interval
  if (Type_prevalence=="of use")  Dataset_events<-Dataset_events[get(Date_condition) >= Start_study_time & get(Date_condition)<=End_study_time,]
  
  
  #Check if start, end and birth date are all filled. If end date is not filled it will be replaced by the study end date
  ################################################################################################################################
  
  if(!is.null(Birth_date)) {
    print("Check if date columns in input data are valid and in correct order")
    if(sum(is.na(Dataset_cohort[,.(get(Start_date))]))>0){stop("Empty start dates")}
    if(!is.null(Age_bands)){if(sum(is.na(Dataset_cohort[,.(get(Birth_date))]))>0){stop("Empty birth dates")}}
    if(sum(is.na(Dataset_cohort[,.(get(End_date))]))>0){print(paste0(sum(is.na(Dataset_cohort[,.(get(End_date))]))," empty end dates will be filled with the end study date. This may cause overlapping intervals"))}
    Dataset_cohort[is.na(get(End_date)),eval(End_date) := End_study_time]
    
    gc()
    
    #Check the order of dates
    ################################################################################################################################
    wrong_End_date<-nrow(Dataset_cohort[get(Start_date)>get(End_date),])
    if (wrong_End_date>0){warning(paste0(wrong_End_date," end date(s) prior to start date"))}
    wrong_Start_date<-nrow(Dataset_cohort[get(Start_date)>Sys.Date(),])
    if (wrong_Start_date>0){warning(paste0(wrong_Start_date," start date(s) in future"))}
    
    if(!is.null(Age_bands)){
      wrong_Birth_date<-nrow(Dataset_cohort[get(Start_date)<get(Birth_date),])
      if (wrong_Birth_date>0){warning(paste0(wrong_Start_date," start date(s) before birth date"))}}
  }
  
  ################################################################################################################################
  #Check if the subjects have overlap in the time intervals (within strata???), deend_period_datesd by end-start date.
  ################################################################################################################################
  print("Check if observation periods do not have overlap")
  test_overlap<-Dataset_cohort[!is.na(get(End_date))&!is.na(get(End_date))&get(End_date)>get(Start_date),][,.(get(choosen_key), as.integer(get(Start_date)), as.integer(get(End_date)))]
  setkey(test_overlap,V1,V2,V3)
  test_overlap2<-as.data.table(foverlaps(test_overlap, test_overlap, type="any", which=TRUE))
  test_overlap2<-test_overlap2[xid!=yid,]
  test_overlap[,id:=as.integer(rownames(test_overlap))]
  overlap_subjects<-unlist(unique(test_overlap2[test_overlap, on = .(xid = id), nomatch=NULL][,.(V1)]))
  
  #REMOVEEEEEEEEEEEEE AFTER TESTING
  #Dataset_cohort<-Dataset_cohort[!(choosen_key %in% overlap_subjects),]
  
  #REMOVE AFTER TESTING
  if(length(overlap_subjects) > 0){
    stop("Subjects have overlapping person time: ")
    stop(paste0(overlap_subjects," "))
  }
  
  rm(test_overlap,test_overlap2,overlap_subjects)
  gc()
  
  #Determine the ages at the beginning and end of all observation periods. Output is a starting point for calculation and splitting of
  # age bands
  ################################################################################################################################
  if(!is.null(Age_bands)){
    
    print(paste0("Calculate ages at the start and end of every observation period by ", Increment_period))
    
    
    if (nrow(Dataset_cohort) > 0){
      
      Dataset_cohort[, age_start := floor(time_length(interval(get(Birth_date), get(Start_date)), Unit_of_age)) ]
      Dataset_cohort[, age_end := floor(time_length(interval(get(Birth_date), get(End_date)), Unit_of_age)) ]
      
    } else{
      Dataset_cohort[,age_start := NA]
      Dataset_cohort[,age_end := NA]
    }   
    
  }
  
  #Calculate agebands in 2 steps ((1)split/recalculate start/end ages and (assign row to ageband) )
  ################################################################################################################################
  if(!is.null(Age_bands)){
    print("Create agebands")
    if(nrow(Dataset_cohort) > 0){
      
      #Produce a dataset with Agebands and the start and end age of that ageband. This can be used to merge top all cases in the Dataset that overlap with the start and end age.
      Agebands_list <- list()
      
      for (k in 1:length(Age_bands)){
        
        if( k == 1) Agebands_list[[k]] <- paste0(Age_bands[k],"-",Age_bands[k+1])
        if( k > 1 &k!= length(Age_bands)) Agebands_list[[k]] <- paste0(Age_bands[k]+1,"-",Age_bands[k+1])
        if( k== length(Age_bands) & include_remaning_ages == T) Agebands_list[[k]] <- paste0(Age_bands[k]+1,"+")
        
      }
      Agebands_list <- as.data.table(do.call(rbind, Agebands_list))
      colnames(Agebands_list)<- "Ageband"
      
      Agebands_list[,row := row.names(Agebands_list) ]
      Agebands_list[,ST := as.numeric(gsub("[^[:digit:].]", "\\1",strsplit(as.character(Ageband),"-")[[1]][1])),by = row ]
      Agebands_list[,EN := as.numeric(gsub("[^[:digit:].]", "\\1",strsplit(as.character(Ageband),"-")[[1]][2])),by = row ]
      Agebands_list[is.na(EN),EN := 4000 ]
      
      #Merge the overlapping
      setkeyv(Dataset_cohort, c("age_start","age_end"))
      Dataset_cohort <- foverlaps(Agebands_list, Dataset_cohort, by.x = c("ST","EN"), nomatch = 0L, type = "any")
      
      # select the rows that doubled by the merge. In these, multiple agebands occur witing the obeservation period. So start and end dated need to be adapted
      Dataset_cohort <- Dataset_cohort[, row := row.names(Dataset_cohort)]
      Dataset_cohort <- Dataset_cohort[age_start < ST  ,(Start_date) := as.IDate(add_with_rollback(get(Birth_date), period(ST,units = Unit_of_age),roll_to_first = T, preserve_hms = T)), by = row]
      Dataset_cohort <- Dataset_cohort[age_end > EN  ,(End_date) := as.IDate(add_with_rollback(get(Birth_date), period(EN + 1,units = Unit_of_age), roll_to_first = T, preserve_hms = T)) - 1, by = row]
      Dataset_cohort <- Dataset_cohort[,':=' (age_start = NULL, age_end = NULL,ST = NULL, EN = NULL, row = NULL)]
    }
    Dataset_cohort[,birth_date:=NULL]
  }
  
  
  ################################################################################################################################
  
  #Remove records of Dataset_cohort that don’t overlap the window between Start_study_time and End_study_time
  Dataset_cohort<-Dataset_cohort[get(Start_date)>End_study_time | get(End_date)< Start_study_time, no_overlap:=1]
  rm_rows<-nrow( Dataset_cohort[no_overlap==1,])
  if(rm_rows>0) message(paste0(rm_rows, " records removed because do not overlap the window between start and end of the study"))
  Dataset_cohort<-Dataset_cohort[no_overlap!=1 | is.na(no_overlap),]
  Dataset_cohort<-Dataset_cohort[,no_overlap:=NULL]
  ####################################################
  
  
  #check all strings in the parameter conditions are in the unique(columns names) #######
  check<-Conditions %in% unique(Dataset_events[,get(Name_condition)])
  for (s in Conditions[!check]) warning(paste0("The string ",s, " indicated in the Condition parameter is not in the dataset" ))
  
  #######################
  ##########################################
  
  #POINT PREVALENCE
  if (Type_prevalence == "point") {
    print("Point prevalence computation")
    
    #Create the start and end of each period of interest
    #if Periods_of_time is provided by the users, extract the column names
    if (!is.null(Points_in_time)) { 
      if(Points_in_time[[1]] %in% names(Dataset_cohort)) {
        #we assume it is only one column
        setnames(Dataset_cohort,Points_in_time,"value")
        Dataset_cohort<-Dataset_cohort[!is.na(value),]
      }else{
        Points_in_time<-as.IDate(Points_in_time,"%Y%m%d")
      }
      
    }else{
      if (Increment!= "month") {
        Points_in_time<-seq.Date(Start_study_time,End_study_time ,by = Increment)
        if (Increment=="week") min_days_distance<-7
        if (Increment=="year") min_days_distance<-365
        diff<-as.numeric(difftime(End_study_time,tail(Points_in_time, n=1),units = "days"))
        if (diff<7 & diff!=0) Points_in_time<-head(Points_in_time, - 1) 
      }else{
        n_month<-floor(interval(Start_study_time, End_study_time) / months(1))
        Points_in_time<-add_with_rollback(Start_study_time, months(1:n_month), roll_to_first = F)
        Points_in_time<-c(Start_study_time,Points_in_time)
        new_vector<-Points_in_time < End_study_time
        Points_in_time<-Points_in_time[new_vector]
      }
    }
    
    Dataset_cohort<-rbindlist(lapply(Points_in_time, function(x) data.frame(Dataset_cohort, value=x)))
    
    Dataset_cohort[,in_population:=as.factor(fifelse(get(Start_date)<=value & value<=get(End_date),1,0))]
    
    if (drop_not_in_population) {
      Dataset_cohort <- Dataset_cohort[in_population == 1, ]
    }
    
    dataset<-merge(Dataset_cohort,Dataset_events, by=choosen_key,all.x=T,allow.cartesian=T )
    rm(Dataset_cohort, Dataset_events)
    
    dataset <- split(dataset, by = "in_population")
    dataset[["1"]]<-dataset[["1"]][get(Date_condition)<=value & in_population==1 & !is.na(get( Date_condition)),constant:=1][is.na(constant),constant:=0]
    
    dcast_vars=c(id_columns_tokeep,Start_date,End_date,"value","in_population",Date_condition)
    if (!is.null(Age_bands)) dcast_vars<-c(dcast_vars,"Ageband")
    if (!is.null(Strata)) dcast_vars<-c(dcast_vars,Strata)
    
    f = as.formula(sprintf('%s ~ %s', paste(dcast_vars, collapse = "+ "), Name_condition))
    
    dataset[["1"]]<-dcast(dataset[["1"]],f, value.var = "constant" ,fill=0)
    dataset[["1"]]<-dataset[["1"]][,"NA":=NULL]
    
    cols_to_remove <- setdiff(colnames(dataset[["0"]]), colnames(dataset[["1"]]))
    cols_to_add <- setdiff(colnames(dataset[["1"]]), colnames(dataset[["0"]]))
    
    dataset[["0"]][, (cols_to_remove) := NULL]
    dataset[["0"]][, (cols_to_add) := 0]
    
    dataset <- rbindlist(dataset, use.names = T, fill = T)
    dataset[, in_population := as.numeric(levels(in_population))[in_population]]
    
    cols_to_rename <- names(dataset)[names(dataset) %in% Conditions]
    setnames(dataset, cols_to_rename, paste0("prev_",cols_to_rename))
    
    
    setnames(dataset,"value","timeframe")
    cols<-paste0("prev_", Conditions)
    myvector<-c(choosen_key,Start_date,End_date,"timeframe","in_population",cols)
    if (choosen_key != UoO_id) myvector<-c(myvector, UoO_id)
    if (!is.null(Age_bands)) myvector<-c(myvector, "Ageband")
    if (!is.null(Strata)) myvector<-c(myvector, Strata)
    dataset<-unique(dataset[,..myvector])
    
    dataset<-dataset[,(cols) := lapply(.SD, function(x)max(x)), .SDcols = cols,by=c("timeframe",choosen_key)]
    
    dataset<-unique(dataset[,..myvector])
  }
  
  
  # PREVALENCE OF USE
  if (Type_prevalence=="of use") {
    print("Period prevalence computation")
    #Create the start and end of each period of interest
    #if Periods_of_time is provided by the users, extract the column names
    if (!is.null(Periods_of_time)) { 
      start_period_dates<-unlist(lapply(Periods_of_time, `[[`, 1))
      end_period_dates<-unlist(lapply(Periods_of_time, `[[`, 2))
      
      if(start_period_dates[[1]] %in% names(Dataset_cohort)) {
        if (length(start_period_dates)==1){
          setnames(Dataset_cohort,start_period_dates,"value1")
          setnames(Dataset_cohort,end_period_dates,"value2")
        }else{
          id_variables<-c(id_columns_tokeep,Start_date,End_date )
          if (!is.null(Strata)) id_variables<-c(id_variables,Strata)
          Dataset_cohort<-melt(setDT(Dataset_cohort), id.vars =id_variables , measure.vars = list(start_period_dates,end_period_dates))
        }
        Dataset_cohort<-Dataset_cohort[!is.na(value1),]
      }else{
        start_period_dates<-as.IDate(start_period_dates,"%Y%m%d")
        end_period_dates<-as.IDate(end_period_dates,"%Y%m%d")
      }
      
    }else{
      #if Increment_period is provided, create couples of start and end dates basen on increment
      if (Increment_period!= "month") {
        start_period_dates<-seq.Date(Start_study_time,End_study_time ,by = Increment_period)
        if (Increment_period=="week") {
          end_period_dates=start_period_dates+6
          if (tail(start_period_dates, n=1)>=End_study_time) {
            end_period_dates<-end_period_dates[-length(end_period_dates)]
            start_period_dates<-start_period_dates[-length(start_period_dates)]
          }
        }
        if (Increment_period=="day") {
          end_period_dates=start_period_dates+1
          end_period_dates<-end_period_dates[-length(end_period_dates)]
          start_period_dates<-start_period_dates[-length(start_period_dates)]
        }
        if (Increment_period=="year") {
          end_period_dates<-start_period_dates-days(1)
          end_period_dates<-c(end_period_dates,ymd(paste0(year(End_study_time),"1231")))
          end_period_dates<-end_period_dates[-1]
        }
      }else{
        start_period_dates<-seq.Date(Start_study_time,End_study_time+month(1) ,by = Increment_period)
        end_period_dates<-start_period_dates-days(1)
        end_period_dates<-end_period_dates[-1]
        start_period_dates<-start_period_dates[-length(start_period_dates)]
      }
    }
    
    
    if(!(start_period_dates[[1]] %in% names(Dataset_cohort))) {
      CJ.dt = function(X,Y) {
        stopifnot(is.data.table(X),is.data.table(Y))
        k = NULL
        X = X[, c(k=1, .SD)]
        setkey(X, k)
        Y = Y[, c(k=1, .SD)]
        setkey(Y, NULL)
        X[Y, allow.cartesian=TRUE][, k := NULL][]
      }
      
      #tranform the 2 vectors start_period_dates and end_period_dates into a datatable
      DT2<- data.table(value1=start_period_dates,value2=end_period_dates)
      
      #compute all the overlaps between Dataset_cohort and the dataset containig the computed periods
      setkeyv(Dataset_cohort,c(Start_date,End_date))
      setkeyv(DT2,colnames(DT2))
      
      Dataset_cohort<-foverlaps(Dataset_cohort,DT2)
      
      #expand the dataset containig the computed periods (DT2) as many times as the number of unique id in Dataset_cohort
      tmp<-CJ.dt(unique(Dataset_cohort[,..choosen_key]),DT2)
      
      #merge Dataset_cohort and the expanded datatable to get all the possible combinations of periods
      Dataset_cohort<-merge(tmp,Dataset_cohort,all.x=T,by=c(choosen_key,"value1","value2"),allow.cartesian = T)
      
      rm(tmp)
    }
    #create the binary in_population checking if the Start_date exist in that row
    Dataset_cohort[,in_population:=fifelse(is.na(get(Start_date)),0,1) ]
    
    
    #create the timeframe column
    # if (!is.null(Periods_of_time)) {
    #   print("No")
    # }else{
    Dataset_cohort<-Dataset_cohort[,timeframe:=paste0(value1,"-",value2)]
    
    #}
    
    #remove only dates after end of our study (dates before are kept because you are prevalent since the day of the diagnosis on )
    #filter only the diagnosis of interest (in the Conditions parameter)
    Dataset_events<-Dataset_events[ get(Name_condition) %in% Conditions & get(Date_condition)<=End_study_time,]
    
    # #in future evaluate to add a filter to reduce the number of rows in events
    # Dataset_events<-merge(Dataset_events,Dataset_cohort[,.(choosen_key,Start_date)],all.x=T,by=choosen_key)
    
    #add the information on diagnosis (Dataset_events)
    Dataset_events[,cond_date2:=get(Date_condition)]
    setkeyv(Dataset_events,c(choosen_key,Date_condition,"cond_date2"))
    
    #Dataset_cohort<-Dataset_cohort[,Start_date_past:=NULL]
    # Dataset_cohort<-Dataset_cohort[!is.na(get(Start_date)),value1:=c( as.IDate("19900101","%Y%m%d"),value1[-1]),by=choosen_key]
    
    setkeyv(Dataset_cohort,c(choosen_key,"value1","value2"))
    
    dataset<-foverlaps(Dataset_cohort,Dataset_events)
    
    dataset[,cond_date2:=NULL]
    
    #if the subject is not in population remove his Date_condition and Name_condition
    dataset<-unique(dataset[in_population==0,c(Name_condition,Date_condition):=as.list(rep(NA,2))])
    
    #prepare the dataset for the dcast:
    #converte Date_condition in integer 
    # add "prev_" in all the column Name_condition
    dataset[,(Date_condition):=as.integer(get(Date_condition))]
    dataset<-unique(dataset[!is.na(get(Date_condition)),(Date_condition):=1])
    dataset<-dataset[,(Name_condition):=paste0("prev_",gsub(" ", "",get(Name_condition)))]
    
    #define the formula for the dcast
    
    dcast_vars=c(id_columns_tokeep,Start_date,End_date,"value1","in_population","timeframe")
    if (!is.null(Age_bands)) dcast_vars<-c(dcast_vars,"Ageband")
    if (!is.null(Strata)) dcast_vars<-c(dcast_vars,Strata)
    
    f = as.formula(sprintf('%s ~ %s', paste(dcast_vars, collapse = "+ "), Name_condition))
    
    dataset<-dcast(dataset,f, value.var = Date_condition ,fill=0)
    
    
    #reorder and remove not necessary columns 
    setorder(dataset,"value1")
    dataset<-dataset[,"value1":=NULL]
    if ("prev_NA" %in% colnames(dataset)) dataset<-dataset[,prev_NA:=NULL]
    
    #extract all the column names containing "prev_"
    cols<-colnames( dataset )[ grepl("^prev_", colnames( dataset )) ]
    
    dataset<-dataset[ in_population==1,(cols) := lapply(.SD, function(x)max(x)), .SDcols = cols,by=c(choosen_key,"timeframe")] 
    
    
  }
  
  #PERIOD PREVALENCE
  if (Type_prevalence=="period") {
    print("Period prevalence computation")
    #Create the start and end of each period of interest
    #if Periods_of_time is provided by the users, extract the column names
    if (!is.null(Periods_of_time)) { 
      start_period_dates<-unlist(lapply(Periods_of_time, `[[`, 1))
      end_period_dates<-unlist(lapply(Periods_of_time, `[[`, 2))
      
      if(start_period_dates[[1]] %in% names(Dataset_cohort)) {
        if (length(start_period_dates)==1){
          Dataset_cohort[,value1:=get(start_period_dates)]
          Dataset_cohort[,value2:=get(end_period_dates)]
          
        }else{
          id_variables<-c(id_columns_tokeep,Start_date,End_date )
          if (!is.null(Strata)) id_variables<-c(id_variables,Strata)
          Dataset_cohort<-melt(setDT(Dataset_cohort), id.vars =id_variables , measure.vars = list(start_period_dates,end_period_dates))
        }
        Dataset_cohort<-Dataset_cohort[!is.na(value1),]
      }else{
        start_period_dates<- as.IDate(start_period_dates,"%Y%m%d")
        end_period_dates<-as.IDate(end_period_dates,"%Y%m%d")
      }
      
    }else{
      #if Increment_period is provided, create couples of start and end dates basen on increment
      if (Increment_period!= "month") {
        start_period_dates<-seq.Date(Start_study_time,End_study_time ,by = Increment_period)
        if (Increment_period=="week") {
          end_period_dates=start_period_dates+6
          if (tail(start_period_dates, n=1)>=End_study_time) {
            end_period_dates<-end_period_dates[-length(end_period_dates)]
            start_period_dates<-start_period_dates[-length(start_period_dates)]
          }
        }
        if (Increment_period=="day") {
          end_period_dates=start_period_dates+1
          end_period_dates<-end_period_dates[-length(end_period_dates)]
          start_period_dates<-start_period_dates[-length(start_period_dates)]
        }
        if (Increment_period=="year") {
          end_period_dates<-start_period_dates-days(1)
          end_period_dates<-c(end_period_dates,ymd(paste0(year(End_study_time),"1231")))
          end_period_dates<-end_period_dates[-1]
        }
      }else{
        start_period_dates<-seq.Date(Start_study_time,End_study_time %m+% months(1), by = Increment_period)
        end_period_dates<-start_period_dates-days(1)
        end_period_dates<-end_period_dates[-1]
        start_period_dates<-start_period_dates[-length(start_period_dates)]
      }
    }
    
    
    if(!(start_period_dates[[1]] %in% names(Dataset_cohort))) {
      CJ.dt = function(X,Y) {
        stopifnot(is.data.table(X),is.data.table(Y))
        k = NULL
        X = X[, c(k=1, .SD)]
        setkey(X, k)
        Y = Y[, c(k=1, .SD)]
        setkey(Y, NULL)
        X[Y, allow.cartesian=TRUE][, k := NULL][]
      }
      
      #tranform the 2 vectors start_period_dates and end_period_dates into a datatable
      DT2<- data.table(value1=start_period_dates,value2=end_period_dates)
      
      #compute all the overlaps between Dataset_cohort and the dataset containig the computed periods
      
      setkeyv(Dataset_cohort,c(Start_date,End_date))
      setkeyv(DT2,colnames(DT2))
      Dataset_cohort<-foverlaps(Dataset_cohort,DT2)
      
      #expand the dataset containig the computed periods (DT2) as many times as the number of unique id in Dataset_cohort
      tmp<-CJ.dt(unique(Dataset_cohort[,..choosen_key]),DT2)
      
      #merge Dataset_cohort and the expanded datatable to get all the possible combinations of periods
      Dataset_cohort<-merge(tmp,Dataset_cohort,all.x=T,by=c(choosen_key,"value1","value2"),allow.cartesian = T)
      
      rm(tmp)
    }
    #create the binary in_population checking if the Start_date exist in that row
    Dataset_cohort[,in_population:=fifelse(is.na(get(Start_date)),0,1) ]
    
    
    #create the timeframe column
    # if (!is.null(Periods_of_time)) {
    #   print("No")
    # }else{
    Dataset_cohort<-Dataset_cohort[,timeframe:=paste0(value1,"-",value2)]
    
    #}
    
    #remove only dates after end of our study (dates before are kept because you are prevalent since the day of the diagnosis on )
    #filter only the diagnosis of interest (in the Conditions parameter)
    Dataset_events<-Dataset_events[ get(Name_condition) %in% Conditions & get(Date_condition)<=End_study_time,]
    
    # #in future evaluate to add a filter to reduce the number of rows in events
    # Dataset_events<-merge(Dataset_events,Dataset_cohort[,.(choosen_key,Start_date)],all.x=T,by=choosen_key)
    
    #add the information on diagnosis (Dataset_events)
    
    Dataset_events[,cond_date2:=as.IDate(End_study_time)]
    Dataset_events[,(Date_condition):=as.IDate(get(Date_condition))]
    setkeyv(Dataset_events,c(choosen_key,Date_condition,"cond_date2"))
    
    #Dataset_cohort<-Dataset_cohort[,Start_date_past:=NULL]
    Dataset_cohort<-Dataset_cohort[!is.na(get(Start_date)),value1:=c( as.IDate("19900101","%Y%m%d"),value1[-1]),by=choosen_key]
    
    setkeyv(Dataset_cohort,c(choosen_key,"value1","value2"))
    dataset<-foverlaps(Dataset_cohort,Dataset_events)
    
    dataset[,cond_date2:=NULL]
    
    #if the subject is not in population remove his Date_condition and Name_condition
    dataset<-unique(dataset[in_population==0,c(Name_condition,Date_condition):=as.list(rep(NA,2))])
    
    #prepare the dataset for the dcast:
    #converte Date_condition in integer 
    # add "prev_" in all the column Name_condition
    dataset<-unique(dataset[, (Date_condition):= fifelse(!is.na(get(Date_condition)) & get(Date_condition) <= get(End_date), 1, 0)])
    dataset<-dataset[,(Name_condition):=paste0("prev_",gsub(" ", "",get(Name_condition)))]
    
    #define the formula for the dcast
    
    dcast_vars=c(id_columns_tokeep,Start_date,End_date,"value1","in_population","timeframe")
    if (!is.null(Age_bands)) dcast_vars<-c(dcast_vars,"Ageband")
    if (!is.null(Strata)) dcast_vars<-c(dcast_vars,Strata)
    
    f = as.formula(sprintf('%s ~ %s', paste(dcast_vars, collapse = "+ "), Name_condition))
    
    dataset<-dcast(dataset,f, value.var = Date_condition ,fill=0)
    
    
    #reorder and remove not necessary columns 
    setorder(dataset,"value1")
    dataset<-dataset[,"value1":=NULL]
    if ("prev_NA" %in% colnames(dataset)) dataset<-dataset[,prev_NA:=NULL]
    
    #extract all the column names containing "prev_"
    cols<-colnames( dataset )[ grepl("^prev_", colnames( dataset )) ]
    #dataset<-dataset[ in_population==1,(cols) := lapply(.SD, function(x)cummax(x)), .SDcols = cols,by=choosen_key] #non farlo per use
    
  }
  
  #all columns with all 0 if diagnosis is not in the dataset of events
  
  for (cond in Conditions) {
    if (!(paste0("prev_",gsub(" ", "",cond)) %in% colnames(dataset))) {
      dataset[,(cond):=0]
    }
  }
  
  if (Aggregate == T) {
    Aggr_variables<-c("timeframe")
    if (!is.null(Age_bands)) Aggr_variables<-c(Aggr_variables,"Ageband")
    if (!is.null(Strata)) Aggr_variables<-c(Aggr_variables,Strata)
    cols <- c(cols, "in_population")
    dataset <- dataset[, lapply(.SD, sum), .SDcols=cols, by  = Aggr_variables]
  }
  
  return(dataset)
}
