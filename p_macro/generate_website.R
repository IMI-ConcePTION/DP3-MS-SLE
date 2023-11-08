generate_website <- function() {
  
  all_changes <- gert::git_status(pathspec = "i_codebooks")
  change_codebook <- all_changes %>%
    dplyr::filter(basename(file) != "00_index.xlsx")
  
  files_in_content <- list.files(here::here("i_website", "content"), recursive = T)
  changes <- files_in_content[sub('\\..*$', '', basename(files_in_content)) %in% sub('\\..*$', '', basename(change_codebook$file))]
  
  changes <- file.path("content", changes)
  
  blogdown::build_site(build_rmd = changes)
}

filter_newfile()
filter_timestamp()
blogdown::filter_md5sum(list.files(here::here("docs"), recursive = T), db = "g_parameters/md5sum_test.txt")
blogdown:::require_rebuild()

test_fun <- function() {
  blogdown::filter_md5sum(list.files(here::here("i_codebooks"), full.names = T, recursive = T), db = here::here("g_parameters", "md5sum_test.txt"))
}

test <- test_fun()

# Set index location
index_path <- here::here("i_codebooks", "00_index.xlsx")

#load the index
index_file <- read_excel(index_path) %>%
  dplyr::rename(FILE = "dataset_name") %>%
  dplyr::select(FILE)

test_1 <- test[sub('\\..*$', '', basename(test)) %in% index_file$FILE]

