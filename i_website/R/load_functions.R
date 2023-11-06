# Create the HTML version of acodebook
html_codebook <- function(name_codebook) {
  path_excel <- here::here(codebooks_loc, name_codebook)
  
  sheets <- list()
  
  if (!file.exists(path_excel)) {
    return(htmltools::HTML('<div class="tabcontent"></div>'))
  }
  
  for (sheet_name in readxl::excel_sheets(path_excel)) {
    sheets[[sheet_name]] <- readxl::read_excel(path_excel,
                                               sheet = sheet_name)
  }
  
  tab_part_ids <- c("defaultOpen", lapply(seq_len(length(names(sheets)) - 1), function(x) NULL))
  names(tab_part_ids) <- names(sheets)
  
  tab_part <- htmltools::div(class="tab", lapply(names(sheets), function(x) {
    htmltools::tag('button', varArgs= list(class="tablinks", onclick=paste("openCity(event, ", x,")", sep = "'"), 
                                           id = tab_part_ids[[x]], x))
  }))
  
  content_part <- c()
  for (sheet_name in names(sheets)) {
    sheet <- sheets[[sheet_name]]
    
    if (ncol(sheet) == 0) {
      content_part <- htmltools::HTML('<div class="tabcontent"></div>')
    } else {
      
      content_part <- htmltools::tagList(content_part,
                                         htmltools::div(id = sheet_name, class = "tabcontent",
                                                        reactable::reactable(sheet[1:20,], sortable = FALSE, searchable = TRUE,
                                                                             pagination = FALSE, highlight = TRUE,
                                                                             bordered = TRUE, striped = TRUE, height = 600,
                                                                             style = list(maxWidth = 1800))))
      
    }
  }
  
  htmltools::tagList(tab_part, content_part)
}

# Extract the description of a codebook
get_description <- function(name_codebook) {
  path_excel <- here::here(codebooks_loc, name_codebook)
  
  if (!file.exists(path_excel)) {
    return("")
  }
  
  metadata_sheet <- readxl::read_excel(path_excel, sheet = "Metadata")
  
  description <- metadata_sheet$metadata_content[which(metadata_sheet$medatata_name %in% c("description", "Content of the dataset", "content", "content of the D3"))]
  
  return(gsub("\"", "'", description))
}
