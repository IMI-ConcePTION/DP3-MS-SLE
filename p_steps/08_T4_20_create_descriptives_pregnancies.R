smart_load("D3_DU_PREGNANCY_COHORT_variables", dirtemp, extension = extension)

tmp <- D3_DU_PREGNANCY_COHORT_variables[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study,
                                            pregnancy_with_MS, MS_developed_during_pregnancy)]

tmp1 <- tmp[, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study, strata = 1)]
tmp2 <- tmp[pregnancy_with_MS == 1, .(person_id, number_of_pregnancies_in_the_study, pregnancy_id, strata = 2)]
tmp3 <- tmp[MS_developed_during_pregnancy == 1, .(person_id, pregnancy_id, number_of_pregnancies_in_the_study, strata = 3)]

tmp4 <- rbindlist(list(tmp1, tmp2, tmp3), use.names = T)
tmp4[number_of_pregnancies_in_the_study > 1 , number_of_pregnancies_in_the_study := 2]
tmp4[, strata := factor(strata, levels = c(1, 2, 3),
                        labels = c('**Number of pregnancies in the Pregnancy cohort N (%)**',
                                   '**Number of pregnancies in the MS-Pregnancy cohort N (%)**',
                                   '**Number of pregnancies since MS diagnosis from women with MS N (%)**'))]

tmp4[, total := 1] %>%
  gtsummary::tbl_summary(label = list(number_of_pregnancies_in_the_study ~ "Study population"),
                         by = strata, include = number_of_pregnancies_in_the_study, percent = "row") %>%
  gtsummary::modify_footnote(gtsummary::all_stat_cols(FALSE) ~ NA)





