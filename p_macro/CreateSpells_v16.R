#' 'CreateSpells'
#'
#'
#'CreateSpells takes as input a dataset with multiple time windows per unit of observation. Multiple categories of time windows may be recorded per unit, and time windows of the same unit may overlap, even within the same category. The purpose of the function is creating a dataset where the time windows of the each person and category are disjoint (a time window which is disjoint form the others is called spell). Additionally, a category  '_overall' is added, where time windows are processed regardless of their category. As an option, overlap of pairs of categories are also processed: each pair will be associated to spells where both values are recorded.
#'
#' @param dataset name of dataset
#' @param id variable containing the identifier of the unit of observation
#' @param start_date variable containing the start date (the date must me ordered as Year Month Day)
#' @param end_date variable containing the end date (the date must me ordered as Year Month Day)
#' @param category (optional) categorical variable
#' @param replace_missing_end_date (optional). When specified, it contains a date to replace end_date when it is missing.
#' @param overlap (optional) default FALSE. If TRUE, overlaps of pairs of categories are processed as well.
#' @param dataset_overlap (optional) if overlap TRUE, the name of the file containing the overlap dataset
#' @param only_overlaps (optional) if only_overlaps TRUE, skip the calculation the spells
#' @param gap_allowed (optional) Allowed gap in days between two observation periods after which they are counted as a different spell
#' @param birth_date (optional) variable containing the date of birth (the date must me ordered as Year Month Day)
#' @param gap_allowed_birth (optional) Allowed gap in days between start date of spell and birth date otherwise start date will be converted to birth date
#' @importFrom data.table :=
#' @export CreateSpells

# NOTE: Developed under R  4.3.0


CreateSpells <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                         overlap = F, dataset_overlap = "df_overlap", only_overlaps = F, gap_allowed = 1,
                         birth_date = NULL, gap_allowed_birth = 1){
  # pass_all_arguments("sanitize_inputs")
  dataset <- data_preparation(dataset, start_date, end_date, replace_missing_end_date)
  
  if (!only_overlaps) {
    
    dataset <- data_preparation_2(dataset, category)
    if (!missing(birth_date)) {
      dataset <- data_preparation_3(dataset, birth_date, gap_allowed_birth)
    }
    
    dataset <- CreateSpells.internal(dataset, id, start_date, end_date, category, gap_allowed)
    assign("output_spells_category", dataset)
  }
  
  #OPTIONAL SECTION REGARDING OVERLAPS
  
  if(overlap || only_overlaps){
    # dataset <- sanitize_inputs_overlap(dataset, id, start_date, end_date, category, gap_allowed)
    
    export_df <- overlap.internal(dataset, id, start_date, end_date, category, gap_allowed)
    assign(dataset_overlap, export_df, envir = parent.frame())
  }
  
  if(!only_overlaps) return(output_spells_category)
}

# Pass all argument of a previous the external function to an inner one
pass_all_arguments <- function(x) {
  mycall <- match.call(sys.function(sys.parent()), sys.call(sys.parent()))
  mycall[[1]] <- as.symbol(x) # use inner 1
  eval(mycall)
}

# General preparation of the data
data_preparation <- function(dataset, start_date, end_date, replace_missing_end_date) {
  
  ..start_date <- ..end_date <- "Shut up!"
  
  if (!inherits(dataset$start_date, 'Date')) dataset[, (start_date) := lubridate::ymd(get(..start_date))]
  if (!inherits(dataset$end_date, 'Date')) dataset[, (end_date) := lubridate::ymd(get(..end_date))]
  
  if (any(is.na(dataset[[end_date]]))) {
    
    message("Some end dates are missing")
    
    if (is.null(replace_missing_end_date)){
      
      warning("Since parameter 'replace_missing_end_date' has not been specified,
              those periods have been removed from computation of spells (Warning 01)")
      prev_env <- environment(NULL)
      dataset <- dataset[!is.na(get(prev_env$end_date)), ]
      
    } else {
      
      replace_missing_end_date <- lubridate::ymd(replace_missing_end_date)
      message(paste("Replacing missing end date/s with", replace_missing_end_date, "and removing all periods with start > end"))
      
      prev_env <- environment(NULL)
      dataset[is.na(get(prev_env$end_date)), (end_date) := replace_missing_end_date]
      dataset <- dataset[get(prev_env$start_date) <= get(prev_env$end_date)]
      
    }
  }
  
  return(dataset)
}

# Preparation for the categories
data_preparation_2 <- function(dataset, category) {
  
  #add level overall if category is given as input and has at least 2 categories
  if (!is.null(category) && length(unique(dataset[[category]])) >= 2){
    dataset <- data.table::rbindlist(list(dataset, data.table::copy(dataset)[, (category) := "_overall"]))
    message("The level 'overall' is added as the is more than one category")
  }
  
  return(dataset)
}

#Preparation for overlaps
data_preparation_3 <- function(dataset, start_date, birth_date, gap_allowed_birth) {
  
  ..birth_date <- prev_env <- "Shut up!"
  
  if (!inherits(dataset$birth_date, 'Date')) dataset[, (birth_date) := lubridate::ymd(get(..birth_date))]
  dataset[get(prev_env$start_date) - gap_allowed_birth <= get(prev_env$birth_date), (start_date) := birth_date]
  
  return(dataset)
}

# Internal function for calculating overlaps
overlap.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {
  
  # ..to_keep_start <- ..to_keep_end <- ..select_col <- num_spell <- ..id <- "Shut up!"
  num_spell <- ..id <- "Shut up!"
  
  data.table::setnames(dataset, c(start_date, end_date, category),
                       c("entry_spell_category", "exit_spell_category", "category"))
  
  start_date <- "entry_spell_category"
  end_date <- "exit_spell_category"
  category <- "category"
  
  # Create list of unique not missing categories
  unique_cat <- unique(dataset[!is.na(category), category])
  
  # Create the combinations of pairs of categories
  permut <- utils::combn(unique_cat, 2, simplify = F)
  
  export_df <- list()
  
  # Cycle for each pair
  for (i in permut) {
    
    p_1 <- i[1]
    p_2 <- i[2]
    
    #	For each pair of values A and B, create two temporary datasets
    to_keep_start <- c(id, start_date, end_date, category)
    to_keep_end <- c(id, start_date, end_date)
    dataset <- dataset[, ..to_keep_start]
    
    dataset_filtered_1 <- data.table::copy(dataset)[get("category") == p_1][, ..to_keep_end]
    dataset_filtered_2 <- dataset[get("category") == p_2][, ..to_keep_end]
    
    data.table::setkeyv(dataset_filtered_2, to_keep_end)
    dataset_filtered_1 <- data.table::foverlaps(dataset_filtered_1, dataset_filtered_2, nomatch = NULL)
    rm(dataset_filtered_2)
    
    foverlaps_start <- paste0("i.", start_date)
    foverlaps_end <- paste0("i.", end_date)
    
    dataset_filtered_1[, (start_date) := pmax(get(start_date), get(foverlaps_start))]
    dataset_filtered_1[, (end_date) := pmin(get(end_date), get(foverlaps_end))]
    
    dataset_filtered <- dataset_filtered_1[, ..to_keep_end]
    rm(dataset_filtered_1)
    
    dataset_filtered <- dataset_filtered[, (category) := paste(p_1, p_2, sep = "_")]
    
    select_col <- c(id, start_date, end_date, category)
    dataset_filtered <- dataset_filtered[, ..select_col]
    
    dataset_filtered <- dataset_filtered[, num_spell := data.table::rowid(get(..id))]
    
    export_df <- append(export_df, list(dataset_filtered))
  }
  
  export_df <- data.table::rbindlist(export_df)
  
  return(export_df)
  
}

# Internal function for calculating the spells
CreateSpells.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {
  
  row_id <- .N <- lag_end_date <- ..end_date <- num_spell <- ..start_date <- . <- "Shut up!"
  
  if(is.null(category)){
    order_vec <- c(id, start_date, end_date)
    grouping_vars <- id
  } else {
    order_vec <- c(id, category, start_date, end_date)
    grouping_vars <- c(id, category)
  }
  #group by and arrange the dataset
  data.table::setorderv(dataset, order_vec)
  
  #row id by group
  dataset[, row_id := seq_len(.N), by = grouping_vars]
  
  #lagged end_date
  dataset[, lag_end_date := data.table::fifelse(row_id > 1, data.table::shift(get(..end_date)), get(..end_date))]
  
  # cumulative max for dates
  dataset[, lag_end_date := as.integer(lag_end_date)]
  dataset[, lag_end_date := cummax(lag_end_date), by = grouping_vars]
  dataset[, lag_end_date := as.Date(lag_end_date, "1970-01-01")]
  
  #compute the number of spell
  dataset[, num_spell := data.table::fifelse(row_id > 1 & get(..start_date) <= lag_end_date + gap_allowed, 0, 1)]
  dataset[, num_spell := cumsum(num_spell), by = grouping_vars]
  dataset[, num_spell := as.integer(num_spell)]
  
  #group by num spell and compute min and max date for each one
  keep_col <- c(grouping_vars, "num_spell", "entry_spell_category", "exit_spell_category")
  grouping_vars <- c(grouping_vars, "num_spell")
  
  dataset <- dataset[, .(entry_spell_category = min(get(..start_date)),
                         exit_spell_category = max(get(..end_date))), by = grouping_vars]
  dataset <- dataset[, keep_col, with = FALSE]
  
}

sanitize_inputs <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                            overlap = F, dataset_overlap = NA_character_, only_overlaps = F, gap_allowed = 1,
                            birth_date = NULL, gap_allowed_birth = 1) {
  
  . <- "Shut up!"
  
  # Function to check if x is a date or is an input that ymd() can accept
  is.ymd_or_date <- function(x) {
    if (!inherits(x, 'Date')) {
      y <- tryCatch(lubridate::ymd(x), error=function(e) F, warning=function(w) F)
    } else {
      return(T)
    }
    if (!isFALSE(y)) y <- T
    return(y)
  }
  
  # Check if there are any missing dates
  token_missing_dates <- vetr::vet_token(!is.na(.),
                                         "%s is missing, please specify a valid date (Error 01)")
  
  # Check if x is/can be a date
  token_is_ymd_or_date <- vetr::vet_token(is.ymd_or_date(.),
                                          "%s should be a date or string/integer interpretable by lubridate::ymd (Error 02)")
  
  # Check if x is a column of dataset
  token_col <- vetr::vet_token(. %in% colnames(dataset),
                               paste("%s is not a column in", deparse(substitute(dataset)), "(Error 03)"))
  
  # Want to calculate overlap but no category specified
  token_exist_categories <- vetr::vet_token(!(isTRUE(.) && is.null(category)),
                                            "%s is set to TRUE, however the overlaps can not be computed as
                                            the category argument has not been specified (Error 04)")
  
  # Want to calculate overlap but insufficient categories
  token_n_categories <- vetr::vet_token(!(isTRUE(.) && length(unique(dataset[[category]])) < 2),
                                        "%s is set to TRUE, however the overlaps can not be computed as
                                        has less than two categories (Error 05)")
  
  # Check if any overlap is T or if dataset_overlap has default value
  token_overlap <- vetr::vet_token(I(is.na(.) || (isTRUE(overlap) || isTRUE(only_overlaps))),
                                   "it is set to %s, however neither overlap or only_overlaps
                                   argument are set to TRUE (Error 06)")
  
  # Check if dataset is a data.frame then transform it to data.table
  vetr::vetr(dataset = data.frame())
  dataset = data.table::as.data.table(dataset)
  
  vetr::vetr(
    id = character(1L) && token_col,
    start_date = character(1L) && token_col,
    end_date = character(1L) && token_col,
    category = NULL || character(1L) && token_col,
    replace_missing_end_date = NULL || (token_missing_dates && token_is_ymd_or_date),
    overlap = logical(1L) && token_exist_categories && token_n_categories,
    only_overlaps = logical(1L) && token_exist_categories && token_n_categories,
    dataset_overlap = character(1L) && token_overlap,
    gap_allowed = integer(1L),
    birth_date = NULL || character(1L) && token_col,
    gap_allowed_birth = integer(1L),
  )
  
  # Check if there are any missing dates
  token_missing_start_dates <- vetr::vet_token(!is.na(.[[start_date]]),
                                               "Some start dates of %s are missing, please update those values or
                                               deleted the records (Error 07)")
  
  # Check if x is/can be a date
  token_is_ymd_or_start_date <- vetr::vet_token(is.ymd_or_date(.[[start_date]]),
                                                "All start dates in %s should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 08)")
  
  # Check if x is/can be a date
  token_is_ymd_or_end_date <- vetr::vet_token(is.ymd_or_date(.[[end_date]]),
                                              "All end dates in %s  should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 09)")
  
  # Check for periods with end date before start date
  vetr::vet(token_missing_start_dates && token_is_ymd_or_start_date && token_is_ymd_or_end_date, dataset, stop = T)
  
  # Check for periods with end date before start date
  token_impossible_period <- vetr::vet_token(all(.[[start_date]] <= .[[end_date]], na.rm = T),
                                             paste("Inside %s, there are observation period/s with",
                                                   deparse(substitute(start_date)),
                                                   "less than", deparse(substitute(end_date)), "(Error 10)"))
  vetr::vet(token_impossible_period, dataset, stop = T)
  
  # Check if there are any missing dates
  if (!is.null(birth_date)) {
    token_missing_start_dates <- vetr::vet_token(!is.na(.[[birth_date]]),
                                                 "Some birth dates of %s are missing, please update those values or
                                               deleted the records (Error 11)")
    vetr::vet(token_missing_start_dates, dataset, stop = T)
    
    # Check if x is/can be a date
    token_is_ymd_or_end_date <- vetr::vet_token(is.ymd_or_date(.[[birth_date]]),
                                                "All birth dates in %s should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 12)")
    vetr::vet(token_is_ymd_or_end_date, dataset, stop = T)
    
    # Check for periods with end date before start date
    token_period_before_birth <- vetr::vet_token(all(.[[birth_date]] <= .[[start_date]]),
                                                 paste("Inside %s, there are observation period/s with",
                                                       deparse(substitute(birth_date)),
                                                       "before", deparse(substitute(start_date)), "(Error 13)"))
    vetr::vet(token_period_before_birth, dataset, stop = T)
  }
  
  
  return()
}


sanitize_inputs_overlap <- function(dataset, id, start_date, end_date, category, gap_allowed) {
  
  ..end_date <- ..start_date <- row_id <- .N <- lag_end_date <- num_spell <- ..id <- . <- "Shut up!"
  
  # Function to check if x is a date or is an input that ymd() can accept
  is.ymd_or_date <- function(x) {
    x <- tryCatch(lubridate::ymd(x[!is.na(x)]), error=function(e) F, warning=function(w) F)
    if (all(lubridate::is.Date(x))) x <- T
    return(x)
  }
  
  # Function to check if dataset has overlaps within categories (unwanted)
  has.overlaps_within_categories <- function(dataset, id, start_date, end_date, category, gap_allowed) {
    
    data.table::setorderv(dataset, c(id, start_date))
    dataset[, (end_date) := data.table::shift(get(..end_date)), by = c(id, category)]
    dataset[, (end_date) := get(..end_date) + gap_allowed]
    
    
    prev_env <- environment(NULL)
    return(nrow(dataset[!is.na(get(prev_env$end_date)) & get(prev_env$start_date) <= get(prev_env$end_date)]) == 0)
  }
  
  # Check if x is a column of dataset
  token_col <- vetr::vet_token(. %in% colnames(dataset),
                               paste("%s is not a column in", deparse(substitute(dataset)), "(Error 01)"))
  
  # Check if dataset is a data.frame then transform it to data.table
  vetr::vetr(dataset = data.frame())
  dataset = data.table::as.data.table(dataset)
  
  vetr::vetr(
    id = character(1L) && token_col,
    start_date = character(1L) && token_col,
    end_date = character(1L) && token_col,
    category = character(1L) && token_col,
    gap_allowed = integer(1L)
  )
  
  # Remove category "_overall"
  prev_env <- environment(NULL)
  dataset <- dataset[get(prev_env$category) != "_overall",]
  
  # Want to calculate overlap but insufficient categories
  token_n_categories <- vetr::vet_token(length(unique(.[[category]])) > 1,
                                        "%s does not have enough categories to calculate overlaps,
                                        two are need '_overall' excluded (Error 02)")
  # Want to calculate overlap but insufficient categories
  vetr::vet(token_n_categories, dataset, stop = T)
  
  # Check if there are any missing dates
  token_missing_start_dates <- vetr::vet_token(!is.na(.[[start_date]]),
                                               "Some start dates of %s are missing, please update those values or
                                               deleted the records (Error 03)")
  
  # Check if there are any missing dates
  token_missing_end_dates <- vetr::vet_token(!is.na(.[[end_date]]),
                                             "Some end dates of %s are missing, please update those values or
                                               deleted the records (Error 04)")
  
  # Check if x is/can be a date
  token_is_ymd_or_start_date <- vetr::vet_token(is.ymd_or_date(.[[start_date]]),
                                                "All start dates in %s should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 05)")
  
  # Check if x is/can be a date
  token_is_ymd_or_end_date <- vetr::vet_token(is.ymd_or_date(.[[end_date]]),
                                              "All end dates in %s  should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 06)")
  
  # Check for periods with end date before start date
  vetr::vet(token_missing_start_dates && token_missing_end_dates && token_is_ymd_or_start_date && token_is_ymd_or_end_date,
            dataset, stop = T)
  
  dataset[, (start_date) := lubridate::ymd(get(..start_date))]
  dataset[, (end_date) := lubridate::ymd(get(..end_date))]
  
  # Check for periods with end date before start date
  token_impossible_period <- vetr::vet_token(all(.[[start_date]] <= .[[end_date]], na.rm = T),
                                             paste("Inside %s, there are observation period/s with",
                                                   deparse(substitute(start_date)),
                                                   "less than", deparse(substitute(end_date)), " (Error 07)"))
  vetr::vet(token_impossible_period, dataset, stop = T)
  
  # Check for overlapping periods with the same categories
  token_overlapping_period <- vetr::vet_token(has.overlaps_within_categories(., id, start_date, end_date,
                                                                             category, gap_allowed),
                                              "Inside %s, there are overlapping observation periods within categories (Error 08),
                                              This error may be also caused by setting the parameter gap_allowed with different values in CreateSpells and CreateOverlap")
  vetr::vet(token_overlapping_period, data.table::as.data.table(dataset), stop = T)
  
  return(dataset)
}
