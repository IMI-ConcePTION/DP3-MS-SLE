##%######################################################%##
#                                                          #
####             GENERATE TEMPLATE 1 AND 2              ####
#                                                          #
##%######################################################%##

smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

preg_cohort <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study,
                                                    pregnancy_with_MS, pregnancy_with_MS_detail,
                                                    categories_time_since_previous_pregnancy)]

tmp1 <- preg_cohort[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study, categories_time_since_previous_pregnancy,
                        strata = 1)]
tmp2 <- preg_cohort[pregnancy_with_MS == 1, .(person_id, number_of_pregnancies_in_the_study, pregnancy_id,
                                              categories_time_since_previous_pregnancy, strata = 2)]

preg_cohort_strat <- rbindlist(list(tmp1, tmp2), use.names = T)
preg_cohort_strat[, strata := as.factor(strata)]

# Initial part of first shell table
D5_1 <- preg_cohort_strat[, .(n1 = .N), by = "strata"]
D5_1_temp <- preg_cohort_strat[number_of_pregnancies_in_the_study == 1, .(n2 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p2 := n2 / n1 * 100]
D5_1_temp <- preg_cohort_strat[number_of_pregnancies_in_the_study > 1, .(n3 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p3 := n3 / n1 * 100]

header_string <- list(label = '',
                      stat_1 = '**Number of pregnancies in the Pregnancy cohort N (%)**',
                      stat_2 = '**Number of pregnancies in the MS-Pregnancy cohort N (%)**')

tab1a <- preg_cohort_strat[, total := 1] %>%
  gtsummary::tbl_summary(label = list(total ~ "N"),
                         type = list(total ~ 'continuous'), 
                         statistic = list(
                           total ~ "{sum}"
                         ),
                         digits = total ~ 0,
                         by = strata, include = total) |>
  gtsummary::modify_header(label = '',
                           stat_1 = '**Number of pregnancies in the Pregnancy cohort N (%)**',
                           stat_2 = '**Number of pregnancies in the MS-Pregnancy cohort N (%)**') |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)

preg_cohort_strat_mult <- preg_cohort_strat[number_of_pregnancies_in_the_study > 1, ]
setorder(preg_cohort_strat_mult, person_id, strata)
preg_cohort_strat_mult <- preg_cohort_strat_mult[!is.na(categories_time_since_previous_pregnancy), ]

# Second part of first shell table
D5_1[, strata := as.factor(strata)]
D5_1_temp <- preg_cohort_strat_mult[categories_time_since_previous_pregnancy == "More than 15 months",
                                    .(n4 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p4 := n4 / n3 * 100]
D5_1_temp <- preg_cohort_strat_mult[categories_time_since_previous_pregnancy == "Between 12 and 15 months",
                                    .(n5 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p5 := n5 / n3 * 100]
D5_1_temp <- preg_cohort_strat_mult[categories_time_since_previous_pregnancy == "Between 6 and 12 months",
                                    .(n6 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p6 := n6 / n3 * 100]
D5_1_temp <- preg_cohort_strat_mult[categories_time_since_previous_pregnancy == "Between 3 and 6 months",
                                    .(n7 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p7 := n7 / n3 * 100]
D5_1_temp <- preg_cohort_strat_mult[categories_time_since_previous_pregnancy == "Less than 3 months",
                                    .(n8 = .N), by = "strata"]
D5_1 <- merge(D5_1, D5_1_temp, all.x = T)
D5_1[, p8 := n8 / n3 * 100]

D5_1[, strata := as.numeric(strata)]
setnafill(D5_1, fill = 0)
setnames(D5_1, "strata", "column_identifier")

D5_1_mask <- D5_1[, lapply(.SD, as.character)]
columns_to_mask_simple <- c("n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8")
columns_to_mask_compl <- c("p2", "p3", "p4", "p5", "p6", "p7", "p8")
for (col_mask in columns_to_mask_simple) {
  
  if (col_mask != "n1") {
    col_to_mask <- columns_to_mask_compl[which(columns_to_mask_simple == col_mask) - 1]
    D5_1_mask[between(as.numeric(get(col_mask)), 1, 4), (col_to_mask) := NA]
  }
  
  D5_1_mask[between(as.numeric(get(col_mask)), 1, 4), (col_mask) := "<5"]
}

smart_save(D5_1, direxp, override_name = "D5_DU_for_Template_1", extension = extension, save_copy = "csv")
smart_save(D5_1_mask, direxpmask, override_name = "D5_DU_for_Template_1_masked",
           extension = extension, save_copy = "csv")
smart_save(D5_1_mask, direxpred, override_name = "D5_DU_for_Template_1_masked_simplified",
           extension = extension, save_copy = "csv")

tab1b <- preg_cohort_strat_mult |>
  gtsummary::tbl_summary(include = categories_time_since_previous_pregnancy,
                         by = strata,
                         label = list(categories_time_since_previous_pregnancy ~ "Study population"),
                         statistic = categories_time_since_previous_pregnancy ~ "{n}") |>
  # remove header row
  gtsummary::modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "categories_time_since_previous_pregnancy" & row_type %in% "label"))
  ) |>
  gtsummary::modify_header(label = '',
                           stat_1 = '**Number of pregnancies in the Pregnancy cohort N (%)**',
                           stat_2 = '**Number of pregnancies in the MS-Pregnancy cohort N (%)**') |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)

preg_cohort_strat[, total := 1]
preg_cohort_strat[number_of_pregnancies_in_the_study > 1 , number_of_pregnancies_in_the_study := 2]
preg_cohort_strat[, number_of_pregnancies_in_the_study := factor(number_of_pregnancies_in_the_study, levels = c(1, 2),
                                                                 labels = c("From women having 1 pregnancy",
                                                                            "From women having more than 1 pregnancy"))]

tab1c <- preg_cohort_strat %>%
  gtsummary::tbl_custom_summary(include = number_of_pregnancies_in_the_study,
                                by = strata,
                                label = list(number_of_pregnancies_in_the_study ~ "Study population"),
                                stat_fns = number_of_pregnancies_in_the_study ~ function(data, ...) {
                                  dplyr::tibble(data) |>
                                    summarise(total = sum(total))
                                },
                                digits = number_of_pregnancies_in_the_study ~ 0, 
                                statistic = number_of_pregnancies_in_the_study ~ "{total}") |>
  # remove header row
  gtsummary::modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "number_of_pregnancies_in_the_study" & row_type %in% "label"))
  ) |>
  gtsummary::modify_header(label = '',
                           stat_1 = '**Number of pregnancies in the Pregnancy cohort N (%)**',
                           stat_2 = '**Number of pregnancies in the MS-Pregnancy cohort N (%)**') |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)

final_table_1 <- gtsummary::tbl_stack(list(tab1a, tab1c, tab1b), group_header = c("", "", "Number of pregnancies according to the time period since the previous pregnancy (in women having more than 1 pregnancy)")) |>
  gtsummary::as_gt() |>
  gt::tab_footnote(
    footnote = "Time period between the delivery date of the previous pregnancy and the LMP date of the actual pregnancy",
    locations = gt::cells_row_groups(groups = "Number of pregnancies according to the time period since the previous pregnancy (in women having more than 1 pregnancy)")
  ) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups(groups = everything())
  ) |>
  # Solution to issue https://github.com/rstudio/gt/issues/140
  gt::tab_style(
    style = gt::cell_borders(sides = c("bottom"), color = "white", weight = gt::px(2)),
    location = gt::cells_row_groups(groups = "")
  )

export_name <- "D5_DU_for_Template_1"
save_tbl_summary(final_table_1, dirDUfinaltables, export_name)


preg_cohort_filtered <- copy(preg_cohort)[pregnancy_with_MS_detail %in% c("long before pregnancy", "recently before pregnancy",
                                                                          "right before pregnancy", "during pregnancy"), ]

preg_cohort_filtered[, pregnancy_with_MS_detail := factor(pregnancy_with_MS_detail,
                                                          levels = c("long before pregnancy", "recently before pregnancy",
                                                                     "right before pregnancy", "during pregnancy"),
                                                          labels = c("More than 12 months prior to pregnancy",
                                                                     "3-12 months prior to pregnancy",
                                                                     "0-3 months prior to pregnancy", "during pregnancy"))]

# Second shell table
D5_2 <- preg_cohort_filtered[, .(column_identifier = 1, n1 = .N)]
D5_2_temp <- preg_cohort_filtered[pregnancy_with_MS_detail == "More than 12 months prior to pregnancy", .(n2 = .N)]
D5_2 <- cbind(D5_2, D5_2_temp)
D5_2_temp <- preg_cohort_filtered[pregnancy_with_MS_detail == "3-12 months prior to pregnancy", .(n3 = .N)]
D5_2 <- cbind(D5_2, D5_2_temp)
D5_2_temp <- preg_cohort_filtered[pregnancy_with_MS_detail == "0-3 months prior to pregnancy", .(n4 = .N)]
D5_2 <- cbind(D5_2, D5_2_temp)
D5_2_temp <- preg_cohort_filtered[pregnancy_with_MS_detail == "during pregnancy", .(n5 = .N)]
D5_2 <- cbind(D5_2, D5_2_temp)

setnafill(D5_2, fill = 0)

D5_2_mask <- D5_2[, lapply(.SD, as.character)]
columns_to_mask_simple <- c("n1", "n2", "n3", "n4", "n5")
for (col_mask in columns_to_mask_simple) {
  D5_2_mask[between(as.numeric(get(col_mask)), 1, 4), (col_mask) := "<5"]
}

smart_save(D5_2, direxp, override_name = "D5_DU_for_Template_2", extension = extension, save_copy = "csv")
smart_save(D5_2_mask, direxpmask, override_name = "D5_DU_for_Template_2_masked",
           extension = extension, save_copy = "csv")
smart_save(D5_2_mask, direxpred, override_name = "D5_DU_for_Template_2_masked_simplified",
           extension = extension, save_copy = "csv")

header_string <- list(label = '**Date of MS diagnosis**',
                      stat_0 = '**Number of pregnancies N (%)**')

tab2b <- preg_cohort_filtered %>%
  gtsummary::tbl_summary(label = list(pregnancy_with_MS_detail ~ "N"),
                         include = pregnancy_with_MS_detail) |>
  # remove header row
  gtsummary::modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "pregnancy_with_MS_detail" & row_type %in% "label"))
  ) |>
  gtsummary::modify_header(label = '**Date of MS diagnosis**',
                           stat_0 = '**Number of pregnancies N (%)**') |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(T) ~ NA)

tab2a <- preg_cohort_filtered[, total := 1] %>%
  gtsummary::tbl_summary(label = list(total ~ "N"),
                         statistic = list(total ~ "{n}"),
                         include = total) |>
  # remove header row
  gtsummary::modify_table_body(
    ~ .x |> 
      dplyr::mutate(label = "")
  ) |>
  gtsummary::modify_header(label = '**Date of MS diagnosis**',
                           stat_0 = '**Number of pregnancies N (%)**') |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(T) ~ NA)

final_table_2 <- gtsummary::tbl_stack(list(tab2a, tab2b)) |>
  gtsummary::as_gt() 

export_name <- "D5_DU_for_Template_2"
save_tbl_summary(final_table_2, dirDUfinaltables, export_name)
