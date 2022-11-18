



CalculateSubstractionDenominator <- function(
  
  Dataset,
  Start_date,
  End_date,
  Dataset_events,
  Person_id,
  Name_event,
  Date_event,
  Outcomes_rec,
  Rec_period, 
  Aggregate = F,
  Strata = NULL,
  Include_count = T,
  print = F
  
  
  
  
){

if(length(Outcomes_rec) != length(Rec_period)) stop("Outcomes_rec and Rec_period are not of the same length")
  
Dataset <- copy(Dataset)
Dataset_events <- copy(Dataset_events)


#Standardize names because data.table gives problems in some occasions when working with varaibles as inputs
setnames(Dataset_events, c(eval(Person_id), eval(Name_event), eval(Date_event)), c("ID","EVNT", "DTEVNT"))
setnames(Dataset, c(eval(Person_id), eval(Start_date), eval(End_date)), c("ID", "ST", "EN"))

Outcomes_rec1 <-  Outcomes_rec[ Outcomes_rec  %in% Outcomes_rec[!Rec_period == 0]]  
Outcomes_rec0 <-  Outcomes_rec[! Outcomes_rec %in% Outcomes_rec1] 

#Dataset_events_rec0  <- copy(Dataset_events)[EVNT %in% Outcomes_rec0]
if(!Include_count) Dataset_events_rec1  <- copy(Dataset_events)[EVNT %in% Outcomes_rec1]
if(Include_count) Dataset_events_rec1  <- copy(Dataset_events)[EVNT %in% c(Outcomes_rec1, Outcomes_rec0)]

Rec_period1 <- Rec_period[Outcomes_rec %in% Outcomes_rec1]

rm(Dataset_events)
gc()

#Calculate for every observation period the amount of persontime that needs to subtracted for every event
################################################################################################################################


if(nrow(Dataset_events_rec1) > 0){
  
  if(print) print("Calculate start and end dates for the censoring periods")
  
  if(length(Rec_period1) > 0){
  for(i in 1:length(Rec_period1)){
    
    if(print) print(paste("Set censoring start and end dates for outcome ",Outcomes_rec1[i], " with a duration of ",Rec_period1[i]," days"))
    #Dataset_events_rec1 <- Dataset_events_rec1[dif != 0 & get(Name_event) == Outcomes_rec1[i] & dif < Rec_period1[i], Delete := T ][is.na(Delete),]
    Dataset_events_rec1 <- Dataset_events_rec1[EVNT == Outcomes_rec1[i], ":=" (RecSTDT = DTEVNT, RecENDT = DTEVNT + Rec_period1[i])]  
    gc()
    
    }
  }
  
  if(Include_count) Dataset_events_rec1 <- Dataset_events_rec1[EVNT %in% Outcomes_rec0, ":=" (RecSTDT = DTEVNT, RecENDT = DTEVNT), ]
  
  if(print) print("Calculate days to subtract from persontime for recurrent events")
  
  setkeyv(Dataset, c("ID", "ST", "EN"))
  Dataset_events_rec1 <- foverlaps(Dataset_events_rec1, Dataset, by.x = c("ID","RecSTDT","RecENDT"), nomatch = 0L, type = "any")
  
  #NEW Code
  ####
  
  if(nrow(Dataset_events_rec1) > 0 & Include_count){
  
  
    if(!Aggregate){
      Dataset_events_count <- data.table::dcast(Dataset_events_rec1[data.table::between(DTEVNT, ST, EN, incbounds = T),][,":=" (var2 = 1, EVNT = paste0(EVNT,"_b"))], ID + ST + EN ~  EVNT, value.var = "var2", fill = 0, fun.aggregate = sum)
      Dataset <- merge(x = Dataset, y = Dataset_events_count, by = c("ID", "ST", "EN"), allow.cartesian = F, all.x = T, all.y = F)
      rm(Dataset_events_count)
      gc()
      }
    
    if(Aggregate) {
      Dataset1 <- data.table::dcast(Dataset_events_rec1[data.table::between(DTEVNT, ST, EN, incbounds = T),][,":=" (var2 = 1, EVNT = paste0(EVNT,"_b"))], formula(paste0(paste(Strata, collapse = " + "), " ~  EVNT")) , value.var = "var2", fun.aggregate = sum)
    }
    
  Dataset_events_rec1  <- Dataset_events_rec1[EVNT %in% Outcomes_rec1]
      
  }
  
  
  ####
  
  if(nrow(Dataset_events_rec1) > 0){
    
    
    Dataset_events_rec1 <- Dataset_events_rec1[,':=' (start_date2 = pmax(ST, RecSTDT, na.rm = T), end_date2 = pmin(EN + 1, RecENDT, na.rm = T))]     
    Dataset_events_rec1 <- Dataset_events_rec1[, SUBTR := (as.numeric(end_date2) - as.numeric(start_date2))]
    
    Dataset_events_rec1 <- Dataset_events_rec1[, var := paste0("SUBTRCUM_", EVNT)]
    
    if(Aggregate) Dataset2 <- data.table::dcast(Dataset_events_rec1, formula(paste0(paste(Strata, collapse = " + "), " ~  var")) , value.var = "SUBTR", fun.aggregate = sum)
    
    if(!Aggregate){
      Dataset_events_rec1 <- data.table::dcast(Dataset_events_rec1, ID + ST + EN ~  var, value.var = "SUBTR", fun.aggregate = sum)
      Dataset <- merge(x = Dataset, y = Dataset_events_rec1 , by = c("ID", "ST", "EN"), all.x = T , all.y = F, allow.cartesian = F)
    }
    
  }else{
    
    print("All recurrent events fall witout the period of interest")
  }
  
  rm(Dataset_events_rec1)  
  gc()
  
  if(Aggregate){
    

    if(Include_count){
      if(exists("Dataset2")){ 
        Dataset <- merge(Dataset1, Dataset2, all = T, by = Strata)
        rm(Dataset1, Dataset2)
        gc()
      }else{
        Dataset <- Dataset1
        rm(Dataset1)
        gc()
        }
    
    }else{
      Dataset <- Dataset2
      rm(Dataset2)
      gc()
    }
  }
  
if(!Aggregate)setnames(Dataset, c("ID", "ST", "EN"), c(eval(Person_id), eval(Start_date), eval(End_date)))  
  
}else{Dataset <- NULL}


return(Dataset)


}
