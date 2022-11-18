



SplitSpellsAgeBands <- function(Dataset, Start_date, End_date, Birth_date, Unit_of_age, Agebands_list, print){


if(print) print("Calculate ages at start and end spells")

if (nrow(Dataset) > 0){
    Dataset[, age_start := floor(time_length(interval(get(Birth_date), get(Start_date)), Unit_of_age)) ]
    Dataset[, age_end := floor(time_length(interval(get(Birth_date), get(End_date)), Unit_of_age)) ]
    } else{
      Dataset[,age_start := NA]
      Dataset[,age_end := NA]
} 

if(print) print("Create agebands")
 
#Merge the overlapping
setkeyv(Dataset, c("age_start","age_end"))
Dataset <- foverlaps(Agebands_list, Dataset, by.x = c("ST","EN"), nomatch = 0L, type = "any")
    
#Select the rows that doubled by the merge. In these, multiple agebands occur witing the obeservation period. So start and end dated need to be adapted
Dataset <- Dataset[, row := row.names(Dataset)]
Dataset <- Dataset[age_start < ST  ,eval(Start_date) := as.IDate(add_with_rollback(get(Birth_date), period(ST,units = Unit_of_age), roll_to_first = T, preserve_hms = T)), by = row]
Dataset <- Dataset[age_end > EN  ,eval(End_date) := as.IDate(add_with_rollback(get(Birth_date), period(EN + 1,units = Unit_of_age), roll_to_first = T, preserve_hms = T)) - 1, by = row]
Dataset <- Dataset[,':=' (age_start = NULL, age_end = NULL,ST = NULL, EN = NULL, row = NULL)]

return(Dataset)
    
}





