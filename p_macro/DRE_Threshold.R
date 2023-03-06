DRE_Threshold <- function(Inputfolder, Outputfolder, Varlist = NULL, Delimiter = ",", NAlist = NULL, FileContains = NULL){
  
  if(is.null(FileContains)) FileContains <- "."
  Varlist <- sapply(unique(names(Varlist)), function(x) Varlist[[x]])
  
  Inputfolder <- gsub("/$", "", Inputfolder)
  Outputfolder <- gsub("/$", "", Outputfolder)
  
  files_names <- list.files(path = Inputfolder, recursive = T, pattern = "\\.csv$")
  files_names <- files_names[grep(FileContains, files_names)]
  
  for (file_name in files_names) {
    File <- fread(file.path(Inputfolder, file_name), sep = Delimiter, stringsAsFactors = F)
    
    if(any(colnames(File) %in% names(Varlist))) {
      
      Varlist_in_file <- Varlist[names(Varlist) %in% colnames(File)]
      # File[, (names(Varlist_in_file)) := lapply(.SD, as.character), .SDcols = names(Varlist_in_file)]
      
      for(x in 1:length(Varlist_in_file)) {
        Var_in_file <- names(Varlist_in_file[x])
        File[, (Var_in_file) := as.character(get(Var_in_file))]
        
        if(any(colnames(File) %in% NAlist)) File[as.integer(get(Var_in_file)) < Varlist_in_file[[x]],
                                                 which(colnames(File) %in% NAlist)] <- NA
        File[as.integer(get(Var_in_file)) < Varlist_in_file[[x]] & as.integer(get(Var_in_file)) > 0 ,
             (Var_in_file) := paste0("< ", Varlist_in_file[x])] 
      }
    }
    
    final_file_path <- file.path(Outputfolder, file_name)
    temp_dir <- dirname(final_file_path)
    
    if(!dir.exists(temp_dir)) dir.create(temp_dir, showWarnings = T, recursive = T)
    fwrite(File, file = final_file_path, sep = Delimiter, col.names = T, row.names = F, na = "", append = F)
  }
}
