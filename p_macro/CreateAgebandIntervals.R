


CreateAgebandIntervals <- function(ages, include){

        Agebands_list <- list()
        
        for (k in 1:length(ages)){
          
          if( k == 1) Agebands_list[[k]] <- paste0(ages[k],"-",ages[k+1])
          if( k > 1 &k!= length(ages)) Agebands_list[[k]] <- paste0(ages[k]+1,"-",ages[k+1])
          if( k== length(ages) & include == T) Agebands_list[[k]] <- paste0(ages[k]+1,"+")
          
        }
        
        Agebands_list <- as.data.table(do.call(rbind, Agebands_list))
        colnames(Agebands_list)<- "Label"
        
        Agebands_list[,Ageband := as.integer(row.names(Agebands_list)) ]
        Agebands_list[,ST := as.numeric(gsub("[^[:digit:].]", "\\1",strsplit(as.character(Label),"-")[[1]][1])),by = Ageband ]
        Agebands_list[,EN := as.numeric(gsub("[^[:digit:].]", "\\1",strsplit(as.character(Label),"-")[[1]][2])),by = Ageband ]
        Agebands_list[is.na(EN),EN := 4000 ]
        
}
