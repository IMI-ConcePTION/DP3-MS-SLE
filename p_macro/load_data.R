# Load index and find if each entry have a respective codebook
data <- readxl::read_excel(index_loc)

missing_codebooks <- setdiff(data$dataset_name, tools::file_path_sans_ext(list.files(codebooks_loc)))
if (length(missing_codebooks) > 0) {
  message(paste("Missing codebooks for:", paste(missing_codebooks, collapse = ", ")))
}
rm(data, missing_codebooks)
