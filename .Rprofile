# #set the directory where the file is saved as the working directory
# if (!require("rstudioapi")) install.packages("rstudioapi")
# thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
list.of.packages <- c("blogdown", "here", "xml2", "tibble", "magrittr", "readxl", "tidyr",
                      "stringr", "data.table", "rcorpora", "xslt", "git2r", "dplyr", "gert")
new.packages <- list.of.packages[!(list.of.packages %in% utils::installed.packages()[,"Package"])]
if(length(new.packages)) utils::install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))
rm(new.packages, list.of.packages)

tryCatch(invisible(blogdown::find_hugo(version = "0.95.0")),
         error = function(e) blogdown::install_hugo(version = "0.95.0"))

# Now set options to customize the behavior of blogdown for this project. Below
# are a few sample options; for more options, see
# https://bookdown.org/yihui/blogdown/global-options.html
options(
  # to automatically serve the site on RStudio startup, set this option to TRUE
  blogdown.serve_site.startup = FALSE,
  # to disable knitting Rmd files on save, set this option to FALSE
  blogdown.knit.on_save = TRUE,
  # build .Rmd to .md; to build to .html (via Pandoc), set this option to 'html'
  blogdown.method = 'markdown'
)

# fix Hugo version
options(blogdown.hugo.version = "0.95.0")

options(blogdown.knit.serve_site = FALSE)

source(here::here("p_macro", "generate_DAG.R"))
source(here::here("p_macro", "generate_Rmd.R"))
source(here::here("p_macro", "stage_website.R"))
source(here::here("p_macro", "generate_website.R"))

setwd(paste0(getwd(), "/g_website"))

