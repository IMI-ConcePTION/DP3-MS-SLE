



CheckAndPrepareDates <- function(
  Dataset, 
  Person_id, 
  Start_study, 
  End_study, 
  Start_date,
  End_date, 
  Birth_date, 
  Age_bands,
  Increment, 
  print , 
  check_overlap
  
  
  ){
  

          #
          ################################################################################################################################
          end_date_new <- as.Date(ifelse(End_study <= Sys.Date(), Sys.Date(), End_study + 1),origin = "1970-01-01")
          ################################################################################################################################
          
          #check if study start and stop dates are valid
          ################################################################################################################################
          
          if(print) print("Check if Start_study and End_study are valid")
          
          if(!sum(Start_study == seq.Date(as.Date("19000101","%Y%m%d"),Sys.Date(),by = Increment))==1){
            
            if(Increment == "year"){stop("Change the start date to the first of january. Wrong study start date can produce invalid results.")}
            if(Increment == "month"){stop("Change the start date to the first of month. Wrong study start date can produce invalid results.")}
            if(Increment == "week"){stop("Change the start date to a monday. Wrong study start date can produce invalid results.")}
            
          }
          
          if(!sum(End_study==seq.Date(as.Date("19000101","%Y%m%d"),end_date_new ,by = Increment)-1)==1){
            
            if(Increment == "year"){stop("Change the end date to the 31th of december. Wrong study start date can produce invalid results.")}
            if(Increment == "month"){stop("Change the end date to the last day of the month. Wrong study start date can produce invalid results.")}
            if(Increment == "week"){stop("Change the end date to a sunday. Wrong study start date can produce invalid results.")}
            
          }
          
          gc()
          
          ################################################################################################################################
          
          date_cols<-c(Start_date,End_date,Birth_date)
          
          # Reduce memory size using integers
          Dataset[, c(date_cols) := lapply(.SD, as.IDate), .SDcols = date_cols]
          #Dataset[, c(Strata) := lapply(.SD, as.integer), .SDcols = Strata]
          gc()
          
          
          #Check if start, end and birth date are all filled. If end date is not filled it will be replaced by the and study date
          ################################################################################################################################
          
          
          if(print) print("Check if date columns in input data are valid and in correct order")
          if(sum(is.na(Dataset[,.(get(Start_date))]))>0){stop("Empty start dates")}
          if(!is.null(Age_bands)){if(sum(is.na(Dataset[,.(get(Birth_date))]))>0){stop("Empty birth dates")}}
          if(sum(is.na(Dataset[,.(get(End_date))]))>0){print(paste0(sum(is.na(Dataset[,.(get(End_date))]))," empty end dates will be filled with the end study date. This may cause overlapping intervals"))}
          Dataset[is.na(get(End_date)),eval(End_date) := End_study]
          
          gc()
          
          #Check the order of dates
          ################################################################################################################################
          wrong_End_date<-nrow(Dataset[get(Start_date)>get(End_date),])
          if (wrong_End_date>0){warning(paste0(wrong_End_date," end date(s) prior to start date"))}
          wrong_Start_date<-nrow(Dataset[get(Start_date)>Sys.Date(),])
          if (wrong_Start_date>0){warning(paste0(wrong_Start_date," start date(s) in future"))}
          
          if(!is.null(Age_bands)){
            wrong_Birth_date<-nrow(Dataset[get(Start_date)<get(Birth_date),])
            if (wrong_Birth_date>0){warning(paste0(wrong_Start_date," start date(s) before birth date"))}}
          ################################################################################################################################
          
          
          if(check_overlap){
            
            if(print) print("Check if observation periods do not have overlap")
            test_overlap<-Dataset[!is.na(get(End_date))&!is.na(get(End_date))&get(End_date)>get(Start_date),][,.(get(Person_id), as.integer(get(Start_date)), as.integer(get(End_date)))]
            setkey(test_overlap,V1,V2,V3)
            test_overlap2<-as.data.table(foverlaps(test_overlap, test_overlap, type="any", which=TRUE))
            test_overlap2<-test_overlap2[xid!=yid,]
            test_overlap[,id:=as.integer(rownames(test_overlap))]
            overlap_subjects<-unlist(unique(test_overlap2[test_overlap, on = .(xid = id), nomatch=NULL][,.(V1)]))
            
            if(length(overlap_subjects) > 0){
              warning("Subjects have overlapping person time: ")
              warning(paste0(overlap_subjects," "))
              stop("Invalid results could be generated so the script is stopped")
            }
            
            rm(test_overlap,test_overlap2,overlap_subjects)
            gc()
            
          }
################################################################################################################################

return(Dataset)

}
