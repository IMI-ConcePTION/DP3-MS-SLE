generate_DAG <- function(publish = F) {
  
  # Set index location
  index_path <- here::here("i_codebooks", "00_index.xlsx")
  
  #load the function
  source(here::here("p_macro", "main functions.R"))
  source(here::here("p_macro","auxiliary functions.R"))
  source(here::here("p_macro", "styles_and_parameters.R"))
  source(here::here("p_macro", "clean and sanitize.R"))
  
  #set the styles
  thissteps_style <- c(cell_style = "circle")
  thisdatamodels_style <- c(cell_style = "yellow")
  thisarrows_style <- c(arrow_style = "curved arrow")
  
  if (publish) {
    link = here::here("diagram_draft.html")
    target = here::here("g_website", "static", "diagram_draft.html")
    remote = sub('\\.git$', '', git2r::remote_url())
  } else {
    link = here::here("diagram_local.html")
    target = here::here("g_website", "static", "diagram_local.html")
    remote = "http://localhost:4321"
  }
  
  # run the function
  test_xml <- create_diagram(
    path = index_path, 
    pages = 1, 
    arrows_style = thisarrows_style, 
    steps_style = thissteps_style, 
    datamodels_style = thisdatamodels_style, 
    direction = "TB",
    remote = remote,
    branch = "main"
  )
  
  #export the output
  writeLines(test_xml, target)
  
  log <- suppressWarnings(R.utils::createLink(link = link, target = target, overwrite = T))
  rm(log)
  
  suppressMessages(blogdown::stop_server())
  
  suppressMessages(blogdown::build_site())
  
  suppressMessages(blogdown::serve_site())
}
