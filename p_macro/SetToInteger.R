

SetToInteger <- function(Data, colls){

Data <- copy(Data)

list <- list()

for(i in 1:length(colls)){
            x <- colls[i]
            
            MAP <- as.data.table(cbind(unique(Data[[x]]), c(1:length(unique(Data[[x]])))))
            colnames(MAP) <- c(x, "ID")
            MAP <- MAP[, ID := as.integer(ID)]
            Data <- merge(Data, MAP, by = x, all.x = T)             
            
            Data[, eval(x) := NULL]
            
            setnames(Data, "ID", x)
            
            list[[x]] <- MAP
            
            rm(MAP, x)
            gc()
            
            }
            
    list2 <- list() 
    list2$Data <- Data
    list2$Dictionary <- list
    
    
    return(list2)

}


RenameId <- function(Data, Dictionary, colls, AddId = F){
  
  Data <- copy(Data)
  
  for(i in 1:length(colls)){
    x <- colls[i]
    
    MAP <- Dictionary[[x]]
    
    if(!AddId){  
    setnames(Data, x, "ID")
    Data <- merge(Data, MAP, by = "ID", all.x = T)             
    Data[, ID := NULL]
    }
    
    if(AddId){  
      #setnames(Data, x, "ID")
      Data <- merge(Data, MAP, by = x, all.x = T)
      Data[, eval(x) := NULL]
      #Data[, ID := as.integer(ID)]
      setnames(Data, "ID", x)
      
    }
    
    
    rm(MAP, x)
    gc()
    
  }
  
  
  
  return(Data)
  
}

  
  


