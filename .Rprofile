# #set the directory where the file is saved as the working directory
# if (!require("rstudioapi")) install.packages("rstudioapi")
# thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
list.of.packages <- c("blogdown", "here", "xml2", "tibble", "magrittr", "readr", "readxl", "tidyr",
                      "stringr", "readxl", "data.table", "rcorpora", "xslt", "git2r", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))
rm(new.packages, list.of.packages)
