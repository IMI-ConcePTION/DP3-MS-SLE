
thisdir <- getwd()

# Set index location
index_path <- here::here("i_codebooks", "00_index.xlsx")

#load the function
source(here::here("p_macro", "main functions.R"))
source(here::here("p_macro","auxiliary functions.R"))
source(here::here("p_macro", "styles_and_parameters.R"))
source(here::here("p_macro", "clean and sanitize.R"))

source(here::here("p_macro", "load_functions.R"))

remote = sub('\\.git$', '', git2r::remote_url())

#load the index
index_file <- read_excel(index_path) %>%
  sanitize_index() %>%
  dplyr::select(PROGRAM, FILE, SLUG) %>%
  dplyr::mutate(PROGRAM = as.integer(stringr::str_extract(PROGRAM, "\\d+")),
                SLUG = dplyr::if_else(!is.na(SLUG), SLUG, FILE)) %>%
  dplyr::group_by(PROGRAM) %>%
  dplyr::mutate(WEIGHT = row_number())

generate_codebook_page <- function(single_row) {
  
  # if (file.exists(paste0(thisdir, "/i_codebooks/", single_row[2], ".xlsx"))){ 
  #   return()
  # }
  
  level <- single_row[1]
  folder_path <- paste0(getwd(), "/content/step_", level)
  
  if (!file.exists(folder_path)){ 
    dir.create(folder_path)
    
    path_file <- blogdown::new_post("_index.en", kind = "chapter_codebook", open = F,
                                    file = paste0("step_", level, "/_index.en.md"),
                                    subdir = paste0("step_", level, "/"), ext = ".md")
    
    x = xfun::read_utf8(paste(getwd(), path_file, sep = "/"))
    xfun::write_utf8(c(x, '', '{{%children containerstyle="div" style="h2" description="true" %}}'),
                     paste(getwd(), path_file, sep = "/"))
    
    do.call(blogdown:::modify_yaml, list(path_file, weight = as.integer(single_row[4]),
                                         menuTitle = paste0("Step_", level)))
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
                                       description = description, slug = slug))
  
  return()
}
setwd(paste0(getwd(), "/i_website"))
res <- apply(index_file, 1, generate_codebook_page)

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
