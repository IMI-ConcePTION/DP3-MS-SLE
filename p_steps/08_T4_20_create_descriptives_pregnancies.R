smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

preg_cohort <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study,
                                                    pregnancy_with_MS, pregnancy_start_date, pregnancy_end_date)]

tmp1 <- preg_cohort[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study, pregnancy_start_date,
                        pregnancy_end_date, strata = 1)]
tmp2 <- preg_cohort[pregnancy_with_MS == 1, .(person_id, number_of_pregnancies_in_the_study, pregnancy_id,
                                              pregnancy_start_date, pregnancy_end_date, strata = 2)]

preg_cohort_strat <- rbindlist(list(tmp1, tmp2), use.names = T)

header_string <- list(label = '',
                      stat_1 = '**Number of pregnancies in the Pregnancy cohort N (%)**',
                      stat_2 = '**Number of pregnancies in the MS-Pregnancy cohort N (%)**')

preg_cohort_strat %>%
  gtsummary::tbl_summary(label = list(number_of_pregnancies_in_the_study ~ "N"),
                         type = list(number_of_pregnancies_in_the_study ~ 'continuous'), 
                         statistic = list(
                           number_of_pregnancies_in_the_study ~ "{sum}"
                         ),
                         by = strata, include = number_of_pregnancies_in_the_study, percent = "row") |>
  gtsummary::modify_header(header_string) |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)


preg_cohort_strat[, strata := as.factor(strata)]
preg_cohort_strat_mult <- preg_cohort_strat[number_of_pregnancies_in_the_study > 2, ]
setorder(preg_cohort_strat_mult, person_id, strata, pregnancy_start_date)
# TODO ask if ceiling or floor is correct
preg_cohort_strat_mult[, dist := ceiling(as.period(interval(shift(pregnancy_end_date), pregnancy_start_date)) / months(1)),
                       by = c("person_id", "strata")]
preg_cohort_strat_mult <- preg_cohort_strat_mult[!is.na(dist), ]






preg_cohort_strat[, total := number_of_pregnancies_in_the_study]
preg_cohort_strat[number_of_pregnancies_in_the_study > 1 , number_of_pregnancies_in_the_study := 2]
preg_cohort_strat[, number_of_pregnancies_in_the_study := factor(number_of_pregnancies_in_the_study, levels = c(1, 2),
                                                                 labels = c("From women having 1 pregnancy",
                                                                            "From women having more than 1 pregnancy"))]

preg_cohort_strat %>%
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


# stat_fns = everything() ~ function(data, ...) dplyr::tibble(value = data$value),
# statistic = everything() ~ "{value}",

