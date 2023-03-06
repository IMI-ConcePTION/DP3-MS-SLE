smart_load <- function(df, folder, subpop = F, extension = "qs", return = F) {
  
  subpop_str <- if (isFALSE(subpop)) "" else suffix[[subpop]]
  extension <- if (!grepl("\\.", extension)) paste0(".", extension) else extension
  folder <- paste0(gsub("/$", "", folder), "/")
  
  file_name <- paste0(folder, df, subpop_str, extension)
  if (extension == ".qs") {
    tmp <- qs::qread(file_name, nthreads = parallel::detectCores()/2)
  } else if (extension == ".fst") {
    tmp <- fst::read.fst(file_name, as.data.table = T)
  } else if  (extension == ".rds") {
    tmp <- readRDS(file_name)
  } else if (extension == ".csv") {
    tmp <- data.table::fread(file_name)
  } else {
    load(file_name, envir = .GlobalEnv, verbose = FALSE)
  }
  if (return) {
    return(tmp)
  } else {
    assign(df, tmp, envir = .GlobalEnv)
  }
}
