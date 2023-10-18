#set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
list.of.packages <- c("xml2", "tibble", "dplyr", "magrittr", "readr", "readxl", "tidyr", "stringr", "readxl",
                      "data.table", "rcorpora", "xslt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

#load the function
source(paste0(thisdir,"/../main functions.R"))
source(paste0(thisdir,"/../auxiliary functions.R"))
source(paste0(thisdir,"/../styles_and_parameters.R"))
source(paste0(thisdir,"/../clean and sanitize.R"))

#set the styles
thissteps_style <- c(cell_style = "circle")
thisdatamodels_style <- c(cell_style = "yellow")
thisarrows_style <- c(arrow_style = "curved arrow")

# run the function
test_xml <- create_diagram(
  path = paste0(thisdir,"/index.xlsx"), 
  pages = 1, 
  arrows_style = thisarrows_style, 
  steps_style = thissteps_style, 
  datamodels_style = thisdatamodels_style, 
  direction = "TB"
)

#export the output
writeLines(test_xml, "test.html")