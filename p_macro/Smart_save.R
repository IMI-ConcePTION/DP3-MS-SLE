smart_save <- function(df, folder, subpop = F, extension = "qs", override_name = F, save_copy = NULL) {
  
  subpop_str <- if (isFALSE(subpop)) "" else suffix[[subpop]]
  df_name <- if (isFALSE(override_name)) deparse(substitute(df)) else override_name
  extension <- if (!grepl("\\.", extension)) paste0(".", extension) else extension
  folder <- paste0(gsub("/$", "", folder), "/")
  
  file_name <- paste0(folder, df_name, subpop_str, extension)
  
  if (extension == ".qs") {
    qs::qsave(df, file_name, preset = "high", nthreads = parallel::detectCores()/2)
  } else if (extension == ".fst") {
    fst::write.fst(df, file_name, compress = 100)
  } else if (extension == ".csv") {
    data.table::fwrite(df, file_name)
  } else {
    saveRDS(df, file_name)
  }
  
  if (!is.null(save_copy)) {
    save_copy <- if (!grepl("\\.", save_copy)) paste0(".", save_copy) else save_copy
    file_name <- paste0(folder, df_name, subpop_str, save_copy)
    
    if (is.list(df)) {
      if (save_copy == ".qs") {
        qs::qsave(df, file_name, preset = "high", nthreads = parallel::detectCores()/2)
      } else if (save_copy == ".fst") {
        fst::write.fst(df, file_name, compress = 100)
      } else if (save_copy == ".csv") {
        data.table::fwrite(df, file_name)
      } else {
        saveRDS(df, file_name)
      }
    }
  }
}
