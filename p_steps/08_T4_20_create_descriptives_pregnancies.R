smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

tmp <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study,
                                            pregnancy_with_MS)]

tmp1 <- tmp[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study, strata = 1)]
tmp2 <- tmp[pregnancy_with_MS == 1, .(person_id, number_of_pregnancies_in_the_study, pregnancy_id, strata = 2)]

tmp4 <- rbindlist(list(tmp1, tmp2), use.names = T)

header_string <- list(label = '',
                      stat_1 = '**Number of pregnancies in the Pregnancy cohort N (%)**',
                      stat_2 = '**Number of pregnancies in the MS-Pregnancy cohort N (%)**')

tmp4 %>%
  gtsummary::tbl_summary(label = list(number_of_pregnancies_in_the_study ~ "N"),
                         type = list(number_of_pregnancies_in_the_study ~ 'continuous'), 
                         statistic = list(
                           number_of_pregnancies_in_the_study ~ "{sum}"
                         ),
                         by = strata, include = number_of_pregnancies_in_the_study, percent = "row") |>
  gtsummary::modify_header(header_string) |>
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)

tmp4[, total := number_of_pregnancies_in_the_study]
tmp4[number_of_pregnancies_in_the_study > 1 , number_of_pregnancies_in_the_study := 2]
tmp4[, strata := as.factor(strata)]

tmp4 %>%
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

