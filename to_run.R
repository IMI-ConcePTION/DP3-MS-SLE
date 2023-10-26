source(paste0(getwd(), "/p_macro/to_run_create_diagram.R"))

setwd(paste0(getwd(), "/i_website"))
source(here::here("i_website", "R", "load_functions.R"))
source(here::here("i_website", "R", "load_data.R"))

blogdown::build_site(build_rmd = T)
