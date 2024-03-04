smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

preg_cohort <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study,
                                                    pregnancy_with_MS, pregnancy_with_MS_detail,
                                                    pregnancy_start_date, pregnancy_end_date)]

preg_cohort_filtered <- copy(preg_cohort)[pregnancy_with_MS_detail %in% c("long before pregnancy", "recently before pregnancy",
                                                           "right before pregnancy", "during pregnancy"), ]

tmp1 <- preg_cohort[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study, pregnancy_start_date,
                        pregnancy_end_date, strata = 1)]
tmp2 <- preg_cohort[pregnancy_with_MS == 1, .(person_id, number_of_pregnancies_in_the_study, pregnancy_id,
                                              pregnancy_start_date, pregnancy_end_date, strata = 2)]

preg_cohort_strat <- rbindlist(list(tmp1, tmp2), use.names = T)

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
  gtsummary::modify_header(header_string) |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)


preg_cohort_strat[, strata := as.factor(strata)]
preg_cohort_strat_mult <- preg_cohort_strat[number_of_pregnancies_in_the_study > 2, ]
setorder(preg_cohort_strat_mult, person_id, strata, pregnancy_start_date)
# TODO ask if ceiling or floor is correct
preg_cohort_strat_mult[, dist := ceiling(as.period(interval(shift(pregnancy_end_date), pregnancy_start_date)) / months(1)),
                       by = c("person_id", "strata")]
preg_cohort_strat_mult <- preg_cohort_strat_mult[!is.na(dist), ]
# TODO Change this in regards to floor/ceiling above
preg_cohort_strat_mult[, dist := cut(dist, c(0, 3, 6, 12, 15, Inf),
                                     labels = c("Less than 3 months", "Between 3 and 6 months",
                                                "Between 6 and 12 months", "Between 12 and 15 months",
                                                "More than 15 months"))]

tab1b <- preg_cohort_strat_mult |>
  gtsummary::tbl_summary(include = dist,
                         by = strata,
                         label = list(dist ~ "Study population"),
                         statistic = dist ~ "{n}") |>
  # remove header row
  gtsummary::modify_table_body(
    ~ .x |> 
      dplyr::filter(!(variable %in% "dist" & row_type %in% "label"))
  ) |>
  gtsummary::modify_header(header_string) |>
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
  gtsummary::modify_header(header_string) |>
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
  gtsummary::modify_header(header_string) |>
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
  gtsummary::modify_header(header_string) |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(T) ~ NA)

final_table_2 <- gtsummary::tbl_stack(list(tab2a, tab2b)) |>
  gtsummary::as_gt() 

export_name <- "D5_DU_for_Template_2"
save_tbl_summary(final_table_2, dirDUfinaltables, export_name)
