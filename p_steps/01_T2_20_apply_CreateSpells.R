##%######################################################%##
#                                                          #
####      COMPUTE SPELLS FROM OBSERVATION_PERIODS       ####
#                                                          #
##%######################################################%##

print("COMPUTE SPELLS OF TIME FROM OBSERVATION_PERIODS")

# import input datasets
OBSERVATION_PERIODS <- read_CDM_tables("OBSERVATION_PERIODS")

if (thisdatasource %in% datasources_obs_per_from_pregnancies) {
  
  if (thisdatasource == "UOSL") {
    op_meanings_list_per_set[["UOSL"]][["meanings_MEDICINES"]] <- unique(OBSERVATION_PERIODS[, op_meaning])
  }
  
  OBSERVATION_PERIODS_preg <- as.data.table(get(load(paste0(dirpregnancy, "D3_pregnancy_final.RData"))[[1]]))
  
  if (thisdatasource %in% c("UOSL", "TEST")) {
    op_meanings_list_per_set[[thisdatasource]][["meanings_MEDICINES"]] <- unique(OBSERVATION_PERIODS[, op_meaning])
  }
  
  if (thisdatasource %in% c("UOSL")) {
    OBSERVATION_PERIODS_preg <- OBSERVATION_PERIODS_preg[(type_of_pregnancy_end %in% c("LB", "SB") & PROMPT == "yes") | (type_of_pregnancy_end %in% c("SA", "T")), ]
  }
  
  setnames(OBSERVATION_PERIODS_preg, c("pregnancy_start_date", "pregnancy_end_date"), c("op_start_date", "op_end_date"))
  OBSERVATION_PERIODS_preg <- OBSERVATION_PERIODS_preg[, .(person_id, op_start_date, op_end_date)]
  
  OBSERVATION_PERIODS_preg[, op_meaning := "pregnancy"]
  OBSERVATION_PERIODS_preg[, op_origin := "output_pregnancy_algorithm"]
  OBSERVATION_PERIODS_preg[, op_start_date := op_start_date %m-% months(3)]
  OBSERVATION_PERIODS_preg[, op_end_date := op_end_date %m+% months(3)]
  OBSERVATION_PERIODS_preg[, op_start_date := as.character(format(op_start_date, "%Y%m%d"))]
  OBSERVATION_PERIODS_preg[, op_end_date := as.character(format(op_end_date, "%Y%m%d"))]
  
  OBSERVATION_PERIODS <- rbindlist(list(OBSERVATION_PERIODS, OBSERVATION_PERIODS_preg), use.names=TRUE)
  
}

# Create spells will automatically correct inverted spells so we need to remove those spells and add them later on
# is.na(op_end_date) & ymd(op_start_date) > study_end   keep record with missing end iff start after study_end 
#                                                       -> inverted spells after recoding of createspells
# ymd(op_start_date) > ymd(op_end_date)   vanilla inverted spells
OBSERVATION_PERIODS_inverted <- copy(OBSERVATION_PERIODS)[(is.na(op_end_date) & ymd(op_start_date) > study_end) | ymd(op_start_date) > ymd(op_end_date), ]
# if end is missing imputes it to study end
OBSERVATION_PERIODS_inverted <- OBSERVATION_PERIODS_inverted[is.na(op_end_date), op_end_date := study_end]
smart_save(OBSERVATION_PERIODS_inverted, dirtemp, extension = extension, save_copy = "csv")

# is.na(op_end_date) & ymd(op_start_date) <= study_end   keep record with missing end iff start before study end -> correct spells
OBSERVATION_PERIODS <- OBSERVATION_PERIODS[(is.na(op_end_date) & ymd(op_start_date) <= study_end) | ymd(op_start_date) <= ymd(op_end_date), ]

if (thisdatasource %not in% this_datasource_has_subpopulations) {
  
  OBSERVATION_PERIODS <- OBSERVATION_PERIODS[, op_meaning:="all"]
  D3_output_spells_category <- CreateSpells(
    dataset = OBSERVATION_PERIODS,
    id = "person_id",
    start_date = "op_start_date",
    end_date = "op_end_date",
    category ="op_meaning",
    replace_missing_end_date = study_end,
    gap_allowed = gap_days
  )
  
  D3_output_spells_category <- as.data.table(D3_output_spells_category)
  setkeyv(D3_output_spells_category,
          c("person_id", "entry_spell_category", "exit_spell_category", "num_spell", "op_meaning"))
  
} else {
  
  # for each op_meaning_set, create the dataset of the corresponding spells
  output_spells_category_meaning_set <- vector(mode="list")
  for (op_meaning_set in op_meaning_sets[[thisdatasource]]){
    periods_op_meaning_set <- OBSERVATION_PERIODS
    cond_op_meaning_set <- ""
    for (op_meaning in op_meanings_list_per_set[[thisdatasource]][[op_meaning_set]]){
      if (cond_op_meaning_set=="") {cond_op_meaning_set = paste0("op_meaning=='",op_meaning,"'")
      }else{
        cond_op_meaning_set=paste0(cond_op_meaning_set, " | op_meaning=='",op_meaning,"'")
      }
    }
    periods_op_meaning_set <- periods_op_meaning_set[eval(parse(text = cond_op_meaning_set)),]
    periods_op_meaning_set <- periods_op_meaning_set[,op_meaning:= op_meaning_set]
    print(paste0("COMPUTE SPELLS OF TIME FOR ",op_meaning_set,": ",cond_op_meaning_set))
    output_spells_op_meaning_set <- CreateSpells(
      dataset=periods_op_meaning_set,
      id="person_id" ,
      start_date = "op_start_date",
      end_date = "op_end_date",
      category ="op_meaning",
      replace_missing_end_date = study_end,
      gap_allowed = gap_days
    )
    if (nrow(output_spells_op_meaning_set)>0){
      output_spells_op_meaning_set<-as.data.table(output_spells_op_meaning_set)
      setkeyv(
        output_spells_op_meaning_set,
        c("person_id", "entry_spell_category", "exit_spell_category", "num_spell", "op_meaning")
      )
    }else{
      output_spells_op_meaning_set <- empty_spells
    }
    output_spells_category_meaning_set[[op_meaning_set]] <- output_spells_op_meaning_set
  }
  save(output_spells_category_meaning_set,file=paste0(dirtemp,"output_spells_category_meaning_set.RData"))
  
  # creates spells of overlapping op_meaning sets
  load(paste0(dirtemp,"output_spells_category_meaning_set.RData"))
  for (subpop in subpopulations[[thisdatasource]]){
    op_meaning_sets_in_subpop <- op_meaning_sets_in_subpopulations[[thisdatasource]][[subpop]]
    if (length(op_meaning_sets_in_subpop)>1){
      runninglen = 1
      while (runninglen < length(op_meaning_sets_in_subpop)) {
        op_meaning_set_first = op_meaning_sets_in_subpop[1]
        if (runninglen > 1){
          for (j in 2:runninglen) {
            print(j)
            op_meaning_set_first = paste0(op_meaning_set_first,'_',op_meaning_sets_in_subpop[j])
          }
        }
        op_meaning_set_second = op_meaning_sets_in_subpop[runninglen+1]
        overlap_op_meaning_sets <- paste0(op_meaning_set_first,'_',op_meaning_set_second)
        # compute spells overlap corresponding to overlap_op_meaning_sets, unless it has been already computed
        if (!(overlap_op_meaning_sets %in% names(output_spells_category_meaning_set))){
          print(paste0("COMPUTE SPELLS OF TIME FOR ",overlap_op_meaning_sets))
          inputfirst <- output_spells_category_meaning_set[[op_meaning_set_first]]
          inputsecond <- output_spells_category_meaning_set[[op_meaning_set_second]]
          # check whether one of the two composing input files is empty (if so, the overlap is also empty), otherwise use CreateSpells again
          if(nrow(inputfirst)==0 | nrow(inputsecond)==0){
            output_spells_category_meaning_set[[overlap_op_meaning_sets]] <- c()
          }else{
            input_observation_periods_overlap <- as.data.table(rbind(inputfirst,inputsecond,fill = T))
            temp <- CreateSpells(
              dataset=input_observation_periods_overlap,
              id="person_id" ,
              start_date = "entry_spell_category",
              end_date = "exit_spell_category",
              category = "op_meaning",
              gap_allowed = gap_days,
              overlap = T,
              dataset_overlap = "overlap",
              replace_missing_end_date = study_end,
              only_overlaps = T
            )
            output_spells_category_meaning_set[[overlap_op_meaning_sets]] <- get("overlap")
          }
        }
        runninglen = runninglen + 1
      }
      
    }
  }
  
  D3_output_spells_category <- vector(mode="list")
  for (subpop in subpopulations[[thisdatasource]]){
    print(subpop)
    op_meaning_sets_in_subpop <- op_meaning_sets_in_subpopulations[[thisdatasource]][[subpop]]
    print(op_meaning_sets_in_subpop)
    print(length(op_meaning_sets_in_subpop))
    if (length(op_meaning_sets_in_subpop)==1){
      D3_output_spells_category <- output_spells_category_meaning_set[[op_meaning_sets_in_subpop]]
    }
    if (length(op_meaning_sets_in_subpop)>1){
      concat_op_meaning_sets_in_subpop = op_meaning_sets_in_subpop[1]
      for (j in 2:length(op_meaning_sets_in_subpop)){
        concat_op_meaning_sets_in_subpop = paste0(concat_op_meaning_sets_in_subpop,'_',op_meaning_sets_in_subpop[j])
      }
      D3_output_spells_category <- output_spells_category_meaning_set[[concat_op_meaning_sets_in_subpop]]
    }
  }
}


smart_save(D3_output_spells_category, dirtemp, extension = extension, save_copy = "csv")
