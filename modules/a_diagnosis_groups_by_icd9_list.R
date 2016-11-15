# Vincent Major
# April 10 2016
# Script to preprocess the DIAGNOSIS_DATA_TABLE.csv by one vector of ICD-9 codes

# Usage:
#   a_diagnosis_groups_by_icd9_list = function(icd9.codes.vector, path.raw){
# Where,
#   icd.codes.vector is a data.frame or vector of the ICD-9 codes in either format e.g 123.45 or 12345. Leading zeros will be replaced if nchar < 3 e.g. 1 --> 001 and the function works in a hierarchical manner so that a code of 123 will include any code within the range 123.01-123.99.
#   path.raw is the subdirectory that contains the raw DIAGNOSIS_DATA_TABLE.csv file, e.g. "raw"

# Output is the DIAGNOSIS table subsetted to the rows that include ICD-9 codes within the given list. The SUBJECT_ID or HADM_ID can then easily be extracted from the output data.frame.

a_diagnosis_groups_by_icd9_list = function(icd9.codes.vector, path.raw){
  # a function that takes one list of icd9 codes and the path to the raw data directory!
  icd9.codes.vector = as.character(unlist(icd9.codes.vector))
  
  writeLines('\nLoading diagnoses from raw csv file')
  # Check if the processed raw file exists and is where you think it is
  if(file.exists(file.path(path.raw,"DIAGNOSES_ICD_DATA_TABLE.csv")) == FALSE){
    writeLines(paste0("The raw DIAGNOSES_ICD_DATA_TABLE.csv is not located at ", file.path(path.raw,'DIAGNOSES_ICD_DATA_TABLE.csv')))
    stop()
  }
  # Actually load the diagnoses file
  diagnoses.raw = read.csv(file.path(path.raw,"DIAGNOSES_ICD_DATA_TABLE.csv"), header = T, stringsAsFactors = F)
  diagnoses.raw$ICD9_CODE = as.character(diagnoses.raw$ICD9_CODE)
  # check the icd9.codes.vector for periods and remove them.
  if(any(grepl("\\.",icd9.codes.vector))){
    icd9.codes.vector = sapply(icd9.codes.vector, function(x) gsub(pattern = "\\.", replacement = "", x))
  }
  # Pad the shorter than 3 characters with leading zeros.
  icd9.codes.vector = sapply(icd9.codes.vector, function(x) if(nchar(x) == 1){paste0("00",x)}else if(nchar(x) == 2){paste0("0",x)} else {x})
  
  # Subset the raw data
  matches <- grepl(paste0("^",icd9.codes.vector,collapse="|"), diagnoses.raw$ICD9_CODE)
  output = diagnoses.raw[matches,]
  writeLines('a_diagnosis_group_by_icd_9_list.R successfully completed')
  
  return(output)

}