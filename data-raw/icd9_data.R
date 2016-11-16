## First load the data
sepsis_icd9_organfailure = unlist(read.csv("data-raw/ICD9_codes_major_organ_failure.csv", stringsAsFactors = FALSE))
sepsis_icd9_infection = unlist(read.csv("data-raw/ICD9_codes_systemic_infection.csv", stringsAsFactors = FALSE))

## then, save as .rdata files
save(sepsis_icd9_organfailure, file = "data/sepsis_icd9_organfailure.rdata")
save(sepsis_icd9_infection, file = "data/sepsis_icd9_infection.rdata")
