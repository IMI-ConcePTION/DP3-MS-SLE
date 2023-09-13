#-----------------------------------
# general parameters of the program
#-----------------------------------
# understand which datasource the script is querying

#setwd("..")
#setwd("..")
# dirbase <- getwd()
# dirinput <- paste0(dirbase,"/CDMInstances/CVM2205_EFFICACY_CHILDREN/")

dirinput <- paste0(thisdir,"/i_simulated_data_instance/")
dirpregnancy <- ""

extension = "rds"

# Sanitize dirpregnancy
if (dirpregnancy == "" | is.null(dirpregnancy) | is.na(dirpregnancy)) {
  skip_pregnancy = T
} else {
  dirpregnancy <- paste0(gsub("/$", "", dirpregnancy), "/")
  if (file.exists(paste0(dirpregnancy, "D3_pregnancy_final.RData"))) {
    skip_pregnancy = F
  } else {
    stop("there is no D3_pregnancy_final inside the folder specified in dirpregnancy")
  }
}

set_and_create_dir <- function(x) {
  x <- paste0(thisdir, x)
  dir.create(file.path(x), showWarnings = F)
  return(x)
}

###################################################################
# SET AND CREATE FOLDERS IF NECESSARY
###################################################################

diroutput <- set_and_create_dir("/g_output/")
dirtemp <- set_and_create_dir("/g_intermediate/")
dirconceptsets <- set_and_create_dir("/g_intermediate/concept_sets/")
direxp <- set_and_create_dir("/g_export/")
direxpmask <- set_and_create_dir("/g_export_masked/")
direxpcheck <- set_and_create_dir("/g_export_to_check/")
dirmacro <- set_and_create_dir("/p_macro/")
dirpargen <- set_and_create_dir("/g_parameters/")
direvents <- set_and_create_dir("/g_intermediate/events/")
dircomponents <- set_and_create_dir("/g_intermediate/components/")
  
rm(set_and_create_dir)

# load packages
read_library <- function(...) {
  x <- c(...)
  invisible(lapply(x, library, character.only = TRUE))
}

list.of.packages <- c("lubridate", "stringr", "readr", "data.table", "readxl", "qs", "dplyr", "purrr", "RcppAlgos")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
invisible(lapply(list.of.packages, require, character.only = T))

rm(read_library, new.packages, list.of.packages)
#----------------

#--------------
# load macros

source(paste0(dirmacro,"CreateConceptSetDatasets_v21.R"))
source(paste0(dirmacro,"CreateItemsetDatasets.R"))
source(paste0(dirmacro,"MergeFilterAndCollapse_v5.R"))
source(paste0(dirmacro,"CreateSpells_v15.R"))
source(paste0(dirmacro,"CreateFlowChart.R"))
source(paste0(dirmacro,"CountPrevalence.R"))
source(paste0(dirmacro,"CountPersonTimeV13.6.R"))
source(paste0(dirmacro,"Smart_load.R"))
source(paste0(dirmacro,"Smart_save.R"))
source(paste0(dirmacro,"DRE_Threshold.R"))
source(paste0(dirmacro,"Cube.R"))
source(paste0(dirmacro,"launch_step.R"))

###################################################################
# RETRIEVE INFORMATION FROM CDM_SOURCE
###################################################################

CDM_SOURCE<- fread(paste0(dirinput,"CDM_SOURCE.csv"))
thisdatasource <- as.character(CDM_SOURCE[1,3])
instance_creation <- ymd(CDM_SOURCE[1,"date_creation"])
recommended_end_date <- ymd(CDM_SOURCE[1,"recommended_end_date"])
rm(CDM_SOURCE)
# thisdatasource = "EFEMERIS"

###################################################################
# CREATE EMPTY FILES
###################################################################

files <- sub('\\.csv$', '', list.files(dirinput))

if (!any(str_detect(files,"^SURVEY_ID"))) {
  print("Creating empty SURVEY_ID since none were found")
  fwrite(data.table(person_id = character(0), survey_id = character(0), survey_date = character(0),
                    survey_meaning = character(0)),
         paste0(dirinput, "SURVEY_ID_empty", ".csv"))
}

if (!any(str_detect(files,"^SURVEY_OBSERVATIONS"))) {
  print("Creating empty SURVEY_OBSERVATIONS since none were found")
  fwrite(data.table(person_id = character(0), so_date = character(0), so_source_table = character(0),
                    so_source_column = character(0), so_source_value = character(0), so_unit = character(0),
                    survey_id = character(0)),
         paste0(dirinput, "SURVEY_OBSERVATIONS_empty", ".csv"))
}

if (!any(str_detect(files,"^MEDICINES"))) {
  print("Creating empty MEDICINES since none were found")
  fwrite(data.table(person_id = character(0), medicinal_product_id = integer(0),
                    medicinal_product_atc_code = character(0), date_dispensing = integer(0),
                    date_prescription = logical(0), disp_number_medicinal_product = numeric(0),
                    presc_quantity_per_day = logical(0), presc_quantity_unit = logical(0),
                    presc_duration_days = logical(0), product_lot_number = logical(0),
                    indication_code = logical(0), indication_code_vocabulary = logical(0),
                    meaning_of_drug_record = character(0), origin_of_drug_record = character(0),
                    prescriber_speciality = logical(0), prescriber_speciality_vocabulary = logical(0),
                    visit_occurrence_id = character(0)),
         paste0(dirinput, "MEDICINES_empty", ".csv"))
}

rm(files)

#############################################
#SAVE METADATA TO direxp
#############################################

file.copy(paste0(dirinput,'/METADATA.csv'), direxp, overwrite = T)
file.copy(paste0(dirinput,'/CDM_SOURCE.csv'), direxp, overwrite = T)
file.copy(paste0(dirinput,'/INSTANCE.csv'), direxp, overwrite = T)

#############################################
#SAVE to_run.R TO direxp
#############################################

file.copy(paste0(thisdir,'/to_run.R'), direxp, overwrite = T)

#############################################
#Add lists for censoring final dataset
#############################################
smart_save(c(), dirpargen, extension = ".rds", override_name = "datasets_to_censor")
smart_save(c(), dirpargen, extension = ".rds", override_name = "datasets_to_censor_check")
smart_save(c(), dirpargen, extension = ".rds", override_name = "variables_to_censor")

#############################################
#FUNCTION TO COMPUTE AGE
#############################################

Agebands_countpersontime = c(0, 4, 11, 17, 24, 29, 39, 49, 59, 69, 79)
Agebands_labels = c("0-4","5-11","12-17","18-24","25-29", "30-39", "40-49","50-59","60-69", "70-79","80+")

age_fast = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

`%not in%` = Negate(`%in%`)

correct_difftime <- function(t1, t2, t_period = "days") {
  return(difftime(t1, t2, units = t_period) + 1)
}

calc_precise_week <- function(time_diff) {
  # correction in case a person exit the same date it enter
  time_diff <- fifelse(time_diff == 1, time_diff + 1, time_diff)
  weeks_frac <- time_length(time_diff - 1, "week")
  fifelse(weeks_frac%%1==0, weeks_frac, floor(weeks_frac) + 1)
}

join_and_replace <- function(df1, df2, join_cond, old_name) {
  temp <- merge(df1, df2, by.x = join_cond[1], by.y = join_cond[2])
  temp[, join_cond[1] := NULL]
  setnames(temp, old_name, join_cond[1])
}

read_CDM_tables <- function(x, keepcols = NULL) {
  final_table <- data.table()
  for (file in files_ConcePTION_CDM_tables[[x]]) {
    temp <- fread(paste0(dirinput, file, ".csv"), colClasses = list(character = "person_id"), select = keepcols)
    final_table <- rbindlist(list(final_table, temp), fill = T)
    rm(temp)
  }
  return(final_table)
}

better_foverlaps <- function(x, y, by.x = if (!is.null(key(x))) key(x) else key(y), 
                             by.y = key(y), maxgap = 0L, minoverlap = 1L,
                             type = c("any", "within", "start", "end", "equal"),
                             mult = c("all", "first", "last"), nomatch = getOption("datatable.nomatch", NA),
                             which = FALSE, verbose = getOption("datatable.verbose")) {
  
  duplicated_x <- by.x[duplicated(by.x)]
  if (length(duplicated_x) > 0) {
    new_col <- paste0(duplicated_x, "_copy")
    x[, (new_col) := get(duplicated_x)]
    by.x = c(unique(by.x), new_col)
    return(foverlaps(x = x, y = y, by.x = by.x, by.y = by.y, maxgap = maxgap, minoverlap = minoverlap, type = type,
                     mult = mult, nomatch = nomatch, which = which, verbose = verbose)[, (new_col) := NULL])
  }
  
  duplicated_y <- by.y[duplicated(by.y)]
  if (length(duplicated_y) > 0) {
    new_col <- paste0(duplicated_y, "_copy")
    y[, (new_col) := get(duplicated_y)]
    by.y = c(unique(by.y), new_col)
    setkeyv(y, by.y)
    return(foverlaps(x = x, y = y, by.x = by.x, by.y = by.y, maxgap = maxgap, minoverlap = minoverlap, type = type,
                     mult = mult, nomatch = nomatch, which = which, verbose = verbose)[, (new_col) := NULL])
  }
  
  return(foverlaps(x = x, y = y, by.x = by.x, by.y = by.y, maxgap = maxgap, minoverlap = minoverlap, type = type,
                   mult = mult, nomatch = nomatch, which = which, verbose = verbose))
}

set_names_components <- function(x) {
  cols_to_change <- names(x)[grepl("component", names(x))]
  new_cols_names <- lapply(strsplit(cols_to_change, "_"), function (y) {
    fifelse(is.na(y[5]), paste(y[1], y[3], y[4], y[2], sep = "_"), paste(y[1], y[3], y[5], y[4], y[2], sep = "_"))
  })
  setnames(x, cols_to_change, unlist(new_cols_names))
  return(x)
}

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

update_vector <- function(df, folder, value) {
  tmp <- smart_load(df, folder, extension = ".rds", return = T)
  tmp <- c(tmp, value)
  smart_save(tmp, folder, extension = ".rds", override_name = df)
}
