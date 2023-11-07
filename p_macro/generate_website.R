generate_website <- function() {
  
  all_changes <- gert::git_status(pathspec = "i_codebooks")
  change_codebook <- all_changes %>%
    dplyr::filter(basename(file) != "00_index.xlsx")
  
  files_in_content <- list.files(here::here("i_website", "content"), recursive = T)
  changes <- files_in_content[sub('\\..*$', '', basename(files_in_content)) %in% sub('\\..*$', '', basename(change_codebook$file))]
  
  changes <- file.path("content", changes)
  
  blogdown::build_site(build_rmd = changes)
}
