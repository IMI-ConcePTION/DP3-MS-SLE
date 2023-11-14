generate_website <- function() {
  
  if (file.exists(here::here("g_parameters", "md5sum_Rmd_backup.txt"))) {
    file.copy(here::here("g_parameters", "md5sum_Rmd_backup.txt"), here::here("g_parameters", "md5sum_Rmd.txt"),
              overwrite = T)
  }
  
  blogdown::stop_server()
  
  blogdown::build_site(build_rmd = blogdown::filter_md5sum(blogdown:::list_rmds(check = TRUE),
                                                           db = here::here("g_parameters", "md5sum_Rmd.txt")))
  blogdown::serve_site()
  
  file.copy(here::here("g_parameters", "md5sum_Rmd.txt"), here::here("g_parameters", "md5sum_Rmd_backup.txt"),
            overwrite = T)
  
  invisible(NULL)
  
}
