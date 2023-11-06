# Create new datamodels
source(paste0(getwd(), "/p_macro/to_run_create_diagram.R"))

# Transform to html
setwd(paste0(getwd(), "/i_website"))
blogdown::build_site(build_rmd = T)

# Move to docs folder(publishable)
blogdown::serve_site()
