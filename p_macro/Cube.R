Cube <- function(input, dimensions, levels, measures, statistics = NULL, computetotal = NULL,
                 rule_from_numeric_to_categorical = NULL, order = NULL, label = NULL, summary_threshold = NULL,
                 proportion = NULL, savhierarchy = F) {
  
  input <- copy(input)
  accepted_functions <- c("sum", "mean", "max", "min", "median", "mode", "sd")
  
  # Calculate all possible combination of the dimensions
  dimensions_combinations <- c()
  for (i in seq_along(levels)) {
    dimensions_combinations <- c(dimensions_combinations, combn(names(levels), i, simplify = F))
  }
  
  # Calculate all possible combination of levels
  result.list <- c()
  for (dm_comb in dimensions_combinations) {
    test <- c()
    for (dm in dm_comb) {
      test <- append(test, list(lapply(levels[[dm]],
                                       function(x) levels[[dm]][which(levels[[dm]] == x):length(levels[[dm]])])))
    }
    result.df <- expand.grid(test)
    result.list <- unique(c(result.list, lapply(apply(result.df, 1, identity), unlist, use.names = F)))
  }
  result.list <- c(result.list, list(character()))
  
  if (!is.null(order)) {
    for (dm in names(order)){
      if (length(order[[dm]]) == 1) {
        tmp <- unique(input[, unlist(c(names(order[[dm]]), order[[dm]]), use.names = F), with = F])
        setorderv(tmp, unlist(order[[dm]]))
        order[[dm]][[names(order[[dm]])]] <- unlist(tmp[, names(order[[dm]]), with = F], use.names = F)
        
      }
    }
  }
  
  # Add new variables based on existing levels
  for (dm in names(levels)) {
    for (new_var in names(rule_from_numeric_to_categorical[[dm]])) {
      detail_new_var <- rule_from_numeric_to_categorical[[dm]][[new_var]]
      if (detail_new_var[[1]] == "split_in_bands") {
        
        cut_labels <- c()
        for (i in 1:(length(detail_new_var[[3]]) - 1)) {
          cut_labels <- c(cut_labels, paste(detail_new_var[[3]][[i]], detail_new_var[[3]][[i + 1]] - 1, sep = "-"))
        }
        
        input[, (new_var) := as.character(cut(get(..detail_new_var[[2]]), detail_new_var[[3]], cut_labels, right = F))]
      }
    }
  }
  
  # Check if the dimension is valid (higher dimension is multiple of lower one)
  for (dm in names(levels)) {
    if (length(levels[[dm]]) > 1) {
      for (i in 2:length(levels[[dm]])) {
        
        test_df <- unique(input[, c(levels[[dm]][[i - 1]], levels[[dm]][[i]]), with = F])
        
        test_df_single_col <- test_df[, levels[[dm]][[i - 1]], with = F]
        
        if (any(table(test_df_single_col) > 1)) {
          stop(paste("The assigned dimension", dm, "is not a dimension, because values",
                     paste(names(table(test_df_single_col)[which(table(test_df_single_col) > 1)]), collapse = "/"),
                     "of the level", levels[[dm]][[i - 1]], "are not uniquely assigned to one value in the next level",
                     levels[[dm]][[i]]))
        } 
        
      }
    }
  }
  
  
  
  statistic_list <- list()
  measure_list <- c()
  measure_name_list <- c()
  
  if (!is.null(proportion)) {
    proportion_measures <- unique(sapply(proportion, names))
    
    for (measure in measures) {
      
      if (measure %in% proportion_measures && is.null(statistics[[measure]])) {
        statistics[[measure]] <- c(statistics[[measure]], "sum")
      }
    }
  }
  
  if (is.null(statistics)) {
    statistics <- list()
    for (measure in measures) {
      statistics[[measure]] <- "sum"
    }
  }
  
  measures <- intersect(measures, names(statistics))
  
  levels_vocabulary <- list()
  for (dm in dimensions) {
    lvl_voc <- unique(input[, levels[[dm]], with = F])
    if (dm %in% computetotal) {
      total_lvl <- paste0("All", dm)
      lvl_voc[, (total_lvl) := total_lvl]
    } 
    levels_vocabulary[[dm]] <- lvl_voc
  }
  
  if (savhierarchy) {
    assign("ouput_Hierarchy", levels_vocabulary, envir = parent.frame())
  }
  
  for (measure in measures) {
    
    for (statistic in statistics[[measure]]) {
      
      if (!(statistic %in% accepted_functions)) {
        stop(paste(statistic, "is not an accepted function"))
      }
      
      # else {
      #   statistic <- paste0("do.call(", statistic, ", .SD)")
      # }
      
      measure_name <- paste(measure, statistic, sep = "_")
      measure_list <- c(measure_list, measure_name)
      measure_name_list <- measure
      statistic <- parse(text = paste0("lapply(.SD,", statistic, ")"))
      
      tmp <- data.table::groupingsets(input, jj = c(statistic),
                                      by = unlist(levels, use.names = F),
                                      sets = result.list,
                                      .SDcols = measure_name_list)
      
      if(measure_name_list %in% colnames(tmp)) {
        to_change_name <- measure_name_list
      } else {
        to_change_name <- "V1"
      }
      
      setnames(tmp, to_change_name, measure_name)
      # setnames(tmp, paste0("V", seq_along(measures)), measure_list)
      
      tmp_2 <- if (!exists("tmp_2")) copy(tmp) else cbind(tmp_2, tmp[, ncol(tmp), with = F])
      rm(tmp)
    }
  }
  
  if (exists("tmp_2")) {
    input <- tmp_2
    rm(tmp_2)
  }
  
  # Calculate the statistics
  # tmp <- data.table::groupingsets(input, j = c(eval(statistic_list)),
  #                                   by = unlist(levels, use.names = F),
  #                                   sets = result.list,
  #                                   .SDcols = measures)
  # setnames(input, paste0("V", seq_along(measures)), measure_name_list)
  
  # Filter to remove total for not necessary columns
  not_computetotal <- setdiff(names(levels), computetotal)
  max_level <- lapply(levels, function(x) x[[length(x)]])
  if (!is.null(unlist(max_level[not_computetotal]))) {
    input <- na.omit(input, unlist(max_level[not_computetotal]))
  }
  
  # For each dimension create an order column with max as 99. In case need to compute the total create a new category 
  order_cols <- c()
  for (dm in names(levels)) {
    
    new_col <- paste(dm, "LevelOrder", sep = "_")
    
    last_lvl <- levels[[dm]][[length(levels[[dm]])]]
    
    input[, (new_col) := rowSums(is.na(.SD)) + 1, .SDcols = levels[[dm]]]
    
    if (dm %in% computetotal) {
      input[, (last_lvl) := as.character(get(last_lvl))]
      input[is.na(get(last_lvl)), (last_lvl) := paste0("All", dm)]
    } 
    
    input[get(new_col) == max(get(new_col)), (new_col) := 99]
    order_cols <- c(order_cols, new_col)
  }
  
  # Find the first non NA value for multiple levels dimensions
  multiple_levels <- levels[sapply(levels, function(x) length(x) > 1)]
  if (length(multiple_levels) != 0) {
    input <- input[, (names(multiple_levels)) := lapply(multiple_levels, function(x) {
      fcoalesce(input[, lapply(.SD, as.character), .SDcols = x])
    })]
    
    to_remove_cols <- setdiff(unlist(multiple_levels), names(multiple_levels))
    
    # Remove unnecessary columns and reorder the remaining ones
    input[, (to_remove_cols) := NULL]
  }
  
  single_levels <- levels[sapply(levels, function(x) length(x) == 1)]
  if (length(single_levels) != 0) {
    for (name_lvl in names(single_levels)) {
      setnames(input, single_levels[[name_lvl]], name_lvl)
    }
  }
  
  # Create propotion
  if (!is.null(proportion)) {
    for (dm in names(proportion)) {
      for (proportion_measure in names(proportion[[dm]])) {
        for (denominator in proportion[[dm]][[proportion_measure]]) {
          dm_order_name <- paste(dm, "LevelOrder", sep = "_")
          
          temp <- copy(input)[get(dm_order_name) == denominator, ]
          
          # df_levels_vocabulary <- unique(copy(input)[, c(dm, dm_order_name), with = F])
          # dcast(df_levels_vocabulary, Geography ~ Geography_order, value.var = "Geography")
          
          measure_name <- paste(proportion_measure, "denominator", sep = "_")
          setnames(temp, paste(proportion_measure, "sum", sep = "_"), measure_name)
          
          cols_keep <- c(measure_name, dimensions, paste(setdiff(dimensions, dm), "LevelOrder", sep = "_"))
          temp <- temp[, cols_keep, with = F]
          # temp <- temp[get(dm_order_name) != 99, (dm_order_name) := get(dm_order_name) - 1]
          # temp <- temp[get(dm_order_name) == 99, (dm_order_name) := -1]
          # temp <- temp[get(dm_order_name) == -1, (dm_order_name) := .I[which.max(get(dm_order_name))] + 1]
          
          if (denominator == 99) {
            denominator_new <- length(names(levels_vocabulary[[dm]]))
          } 
          level_to_recode <- names(levels_vocabulary[[dm]])[[denominator_new]]
          
          levels_vocabulary_df <- melt(levels_vocabulary[[dm]], id.vars = c(level_to_recode),
                                       measure.vars = names(levels_vocabulary[[dm]]),
                                       value.name = "V1")
          levels_vocabulary_df[, variable := NULL]
          levels_vocabulary_df <- unique(levels_vocabulary_df)
          temp2 <- temp[levels_vocabulary_df, on = c(paste(dm, "==", level_to_recode)), allow.cartesian = T]
          temp2[, c(dm) := NULL]
          setnames(temp2, "V1", dm)
          
          cols_group_by <- setdiff(cols_keep, measure_name)
          
          input <- merge(input, temp2, by = cols_group_by, all.x = T)
          proportion_name <- paste("prop", dm, proportion_measure, denominator, sep = "_")
          
          input[, (proportion_name) := get(paste(proportion_measure, "sum", sep = "_")) / get(measure_name)]
          
          input[, c(measure_name) := NULL]
          
        }
      }
    }
  }
  
  setcolorder(input, c(measure_list, names(levels), order_cols))
  
  if (!is.null(order)) {
    for (dm in names(order)){
      ordered_list <- unlist(order[[dm]], use.names = F)
      ordered_list <- c(ordered_list, sort(setdiff(unique(input[[dm]]), ordered_list)))
      
      input <- merge(input,
                     data.table::data.table(ordered_list, seq_along(ordered_list)),
                     by.x = dm,
                     by.y = "ordered_list",
                     all.x = T)
      setorderv(input, "V2")
      
      setnames(input, "V2", paste(dm, "ValueOrder", sep = "_"))
    }
  }
  
  level_label_names <- paste(names(levels), "LabelValue", sep = "_")
  setnames(input, names(levels), level_label_names)
  
  if (is.numeric(summary_threshold)) {
    
    summary_threshold <- as.integer(summary_threshold)
    tmp <- copy(input)
    
    for(measure in measure_list) {
      tmp[, (measure) := fifelse(get(measure) < summary_threshold & get(measure) > 0, F, T)] 
    }
    
    tmp <- tmp[, lapply(.SD, all), by = order_cols, .SDcols = measure_list]
    
    assign("summary_threshold", tmp, envir = parent.frame())
  }
  
  return(input)
}