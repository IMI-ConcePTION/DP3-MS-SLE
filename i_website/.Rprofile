# REMEMBER to restart R after you modify and save this file!

# First, execute the global .Rprofile if it exists. You may configure blogdown
# options there, too, so they apply to any blogdown projects. Feel free to
# ignore this part if it sounds too complicated to you.
if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# load packages
list.of.packages <- c("xml2", "tibble", "dplyr", "magrittr", "readr", "readxl", "tidyr", "stringr", "readxl",
                      "data.table", "rcorpora", "xslt", "git2r", "here")
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


# Location and name index
index_loc <- here::here("i_codebooks", "00_index.xlsx")

# Location codebooks
codebooks_loc <- here::here("i_codebooks")

source(here::here("i_website", "R", "load_functions.R"))
source(here::here("i_website", "R", "load_data.R"))
