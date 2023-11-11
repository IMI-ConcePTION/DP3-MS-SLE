stage_website <- function() {
  
  generate_DAG(publish = T)
  
  gert::git_add("docs")
  gert::git_add("i_website")
  return(invisible(NULL))
}
