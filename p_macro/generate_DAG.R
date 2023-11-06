generate_DAG <- function() {
  
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
  
  # run the function
  test_xml <- create_diagram(
    path = index_path, 
    pages = 1, 
    arrows_style = thisarrows_style, 
    steps_style = thissteps_style, 
    datamodels_style = thisdatamodels_style, 
    direction = "TB"
  )
  
  #export the output
  writeLines(test_xml, here::here("i_website", "static", "diagram_draft.html"))
  
  log <- suppressWarnings(R.utils::createLink(link = "diagram_draft.html",
                                              target = here::here("i_website", "static", "diagram_draft.html"),
                                              overwrite = T))
  rm(log)
  
}
