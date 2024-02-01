sanitize_index <- function(x) {
  lookup <- c(SLUG = "slug", PROGRAM = "step_producing_this_dataset", FOLDER_VAR = "folder_where_the_datset_is_stored",
              FILE = "dataset_name", INPUT_DATA = "input_datasets_for_the_step")

  return(x %>%
           rename(all_of(lookup)) %>%
           mutate(INPUT_DATA = gsub("\\r\\n", "", INPUT_DATA)) %>%
           mutate(LINK = tolower(PROGRAM),
                  PROGRAM = stringr::str_extract(PROGRAM, "^([^_]*_){2}[^_]*")) %>%
           select(PROGRAM, FOLDER_VAR, FILE, INPUT_DATA, SLUG, LINK))
}

sanitize_output <- function(x) {
  return(x %>%
           mutate(TYPE = "OUTPUT") %>%
           select(-INPUT_DATA) %>%
           unique())
}

sanitize_input <- function(x) {
  return(x %>%
           drop_na(FILE) %>%
           mutate(FILE = INPUT_DATA) %>%
           select(-INPUT_DATA) %>%
           mutate(TYPE = "INPUT") %>%
           unique() %>%
           separate_rows(FILE, sep = " ", convert = FALSE))
}

extract_SLUG <- function(x) {
  return(x %>% select(FILE, SLUG))
}