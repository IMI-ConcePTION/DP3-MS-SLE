

CreateTimeIntervals <- function(Start_study_time, End_study_time, Increment){

        start <- Start_study_time
        end <- End_study_time
        Time_Increment <- as.IDate(seq.Date(start, end, Increment))
        
        if(length(Time_Increment) > 1){
          Time_Increment_end <- as.IDate(seq.Date(Time_Increment[2], by = Increment, length = length(Time_Increment)))-1
        }else{
          Time_Increment_end <- end
        }
        
        Dummy <- as.data.table(cbind(Increment = Time_Increment,End = Time_Increment_end))
        Dummy <- Dummy[, Increment := as.IDate(Increment)]
        Dummy <- Dummy[, End := as.IDate(End)]
        colnames(Dummy) <- c(Increment,"End")
        
        return(Dummy)


}