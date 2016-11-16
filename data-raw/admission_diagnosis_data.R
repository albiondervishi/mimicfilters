## First load the data
sepsis_admdx_inclusion = unlist(read.csv("data-raw/admission_diagnosis_inclusion.csv", stringsAsFactors = FALSE))
sepsis_admdx_exclusion = unlist(read.csv("data-raw/admission_diagnosis_exclusion.csv", stringsAsFactors = FALSE))

## then, save as .rdata files
save(sepsis_admdx_inclusion, file = "data/sepsis_admdx_inclusion.rdata")
save(sepsis_admdx_exclusion, file = "data/sepsis_admdx_exclusion.rdata")
