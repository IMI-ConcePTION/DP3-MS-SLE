concept_set_codes_our_study_pre <- list()

#--------------------------
# Multiple sclerosis (MS)

concept_set_codes_our_study_pre[["MS"]][["ICD9CM"]] <- c("340")
concept_set_codes_our_study_pre[["MS"]][["ICD10"]] <- c("G35")
concept_set_codes_our_study_pre[["MS"]][["ICPC2"]] <- c("N86")
concept_set_codes_our_study_pre[["MS"]][["READ"]] <- c("F20..00", "F203.00", "F20..11", "F20z.00", "666A.00", "9kG..00",
                                                       "9mD..", "9mD0.", "9mD1.", "666B.00", "8CS1.00", "8IAb.",
                                                       "F202.00", "F207.00", "F200.00", "F208.00", "F201.00", "F204.00",
                                                       "F206.00", "8Cc2.00", "8Cc1.00", "8Cc4.00", "8Cc0.00", "ZRVE.00",
                                                       "8Cc3.00")


#--------------------------
# MS-specific DMT (DMT-MS_SPEC)

concept_set_codes_our_study_pre[["DMT-MS_SPEC"]][["ATC"]] <- c("L04AA34", "L04AC01", "N07XX09", "L04AX07", "L04AA27",
                                                               "L03AX13", "L03AB07", "L03AB08", "L04AA36", "L03AB13",
                                                               "L04AA31", "L01XC10", "L04AA42")

#--------------------------
# MS-non-specific DMT (DMT-MS_UNSPEC)

concept_set_codes_our_study_pre[["DMT-MS_UNSPEC"]][["ATC"]] <- c("L04AA23", "L04AX01", "L04AA40", "L01DB07", "L01XC02",
                                                                 "L04AX03", "L01AA01", "L04AA13", "L04AA06", "L04AD02")



#--------------------------
# (SLE)

concept_set_codes_our_study_pre[["SLE"]][["ICD9CM"]] <- c("710.0")
concept_set_codes_our_study_pre[["SLE"]][["ICD10"]] <- c("M32.1", "M32.8", "M32.9")
concept_set_codes_our_study_pre[["SLE"]][["READ"]] <- c("N000.", "N000z", "Nyu43", "H57y4", "X704X", "XaBE1", "XaC1J")


#--------------------------
# SLE DMT (DMT-MS_SPEC)

concept_set_codes_our_study_pre[["DMT-SLE"]][["ATC"]] <- c("P01BA02", "M09AX", "P01BA01", "P01AX05", "H02AB06",
                                                           "H02AB07", "H02AB04", "H02BX01", "S01CA08", "L04AX01",
                                                           "L04AX03", "LO1BA01", "L04AA06", "L01AA01", "L04AD01",
                                                           "L04AD02", "D11AH01", "L04AA26", "L01XC02", "LO4AB02")
