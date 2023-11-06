stage_website <- function() {
  gert::git_add("docs")
  gert::git_add("i_website")
  return(invisible(NULL))
}
