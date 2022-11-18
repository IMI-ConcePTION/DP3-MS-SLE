launch_step <- function(x) {
  
  if ("dirpargen" %in% ls(envir = .GlobalEnv)) {
    dirpargen <- get("dirpargen", envir = .GlobalEnv)
  } else {
    stop("Folder of the generated parameters is not called 'dirpargen'")
  }
  
  print(system.time(source(paste0(thisdir,"/", x))))

  rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
  load(paste0(dirpargen, "parameters.RData"), envir = .GlobalEnv)
  rm(list=ls(), inherits = T)
  
}