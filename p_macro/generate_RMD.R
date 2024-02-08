generate_Rmd <- function() {
  
  blogdown::stop_server()
  
  if (file.exists(here::here("g_parameters", "md5sum_codebook_backup.txt"))) {
    file.copy(here::here("g_parameters", "md5sum_codebook_backup.txt"), here::here("g_parameters", "md5sum_codebook.txt"),
              overwrite = T)
  }
  
  changed_codebooks <- blogdown::filter_md5sum(list.files(here::here("i_codebooks"), full.names = T, recursive = T),
                                              db = here::here("g_parameters", "md5sum_codebook.txt"))
  
  # Set index location
  index_path <- here::here("i_codebooks", "00_index.xlsx")
  
  source(here::here("p_macro", "clean and sanitize.R"))
  # remote = sub('\\.git$', '', git2r::remote_url())
  
  #load the index
  index_file <- read_excel(index_path) %>%
    sanitize_index() %>%
    dplyr::select(PROGRAM, FILE, SLUG) %>%
    dplyr::mutate(PROGRAM = as.integer(stringr::str_extract(PROGRAM, "\\d+")),
                  SLUG = dplyr::if_else(!is.na(SLUG), SLUG, FILE)) %>%
    dplyr::group_by(PROGRAM) %>%
    dplyr::mutate(WEIGHT = row_number())
  
  changed_codebooks <- changed_codebooks[sub('\\..*$', '', basename(changed_codebooks)) %in% index_file$FILE]
  
  generate_codebook_page <- function(single_row, changes) {
    
    level <- trimws(single_row[1])
    folder_path <- paste0(getwd(), "/content/step_", level)
    
    if (!(single_row[2] %in% sub('\\..*$', '', basename(changes))) & file.exists(
      here::here("g_website", "content", paste0("step_", level), paste0(single_row[2], ".Rmd")))) {
      return()
    }
    
    if (!file.exists(folder_path)){ 
      dir.create(folder_path)
      
      path_file <- blogdown::new_post("_index.en", kind = "chapter_codebook", open = F,
                                      file = paste0("step_", level, "/_index.en.md"),
                                      subdir = paste0("step_", level, "/"), ext = ".md")
      
      x = xfun::read_utf8(paste(getwd(), path_file, sep = "/"))
      xfun::write_utf8(c(x, '', '{{%children containerstyle="div" style="h2" description="true" %}}'),
                       paste(getwd(), path_file, sep = "/"))
      
      do.call(blogdown:::modify_yaml, list(path_file, weight = as.integer(single_row[4]),
                                           menuTitle = paste0("Step_", level),
                                           datetime = Sys.time()))
    }
    
    if (file.exists(here::here("g_website", "content", paste0("step_", level), paste0(single_row[2], ".md")))) {
      file.remove(here::here("g_website", "content", paste0("step_", level), paste0(single_row[2], ".md")))
    }
    
    slug <- single_row[[3]]
    attr(slug, which = "quoted") <- TRUE
    path_file <- blogdown::new_post(single_row[[2]], kind = "codebook",
                                    open = F, file = paste0("step_", level, "/", single_row[2], ".Rmd"))
    
    name_excel <- paste0(single_row[2], ".xlsx")
    attr(name_excel, which = "quoted") <- TRUE
    description <- "`r get_description(rmarkdown::metadata$name_excel)`"
    attr(description, which = "quoted") <- TRUE
    do.call(blogdown:::modify_yaml, list(path_file, weight = as.integer(single_row[4]), name_excel = name_excel,
                                         description = description, slug = slug,
                                         datetime = Sys.time()))
    
    return()
  }
  
  res <- apply(index_file, 1, generate_codebook_page, changed_codebooks)
  
  file.copy(here::here("g_parameters", "md5sum_codebook.txt"), here::here("g_parameters", "md5sum_codebook_backup.txt"),
            overwrite = T)
  
  invisible(NULL)
  
}
