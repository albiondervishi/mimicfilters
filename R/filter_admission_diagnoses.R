# Vincent Major
# April 10 2016
# Function that uses two predefined lists of strings to include and exclude patients by their unstructured string-based admission diagnosis.

# Usage,
#   c_string_based_admission = function (ADMISS, in_list, ex_list)
# Where,
#   ADMISS is a data.frame subset of the raw ADMISSIONS table that includes the DIAGNOSIS field.
#   in_list is a character vector of inclusion terms to match on
#   ex_list is a character vector of exclusion terms to match on

# Output is a list of HADM_ID vectors for included, missed and excluded in that order i.e. list(included_HADM_ID,missed_HADM_ID,excluded_HADM_ID)

## Load real data
# data = read_csv("../raw/ADMISSIONS_DATA_TABLE.csv")
# df = data[1:100,c('HADM_ID', 'DIAGNOSIS')]
# temp_str = str_to_lower(data$DIAGNOSIS[14])

#### Helper functions
inc_or_not_exc = function(temp, inclusion, exclusion){
  ## take in a single substring value and the inclusion and exclusion lists
  ## run through this tree
  ## if the substring matches the exclusion then output is false and no need to test inclusion
  ## otherwise, test inclusion and TRUE if matches
  ## otherwise NA for missing
  if(!is.null(exclusion)){
    if(any(stringr::str_detect(temp, exclusion))){
      output = FALSE
      return(output)
    }
  }
  if(any(stringr::str_detect(temp, inclusion))){
    output = TRUE
  } else {
    output = NA
  }
  return(output)
}

#### Meat of the function

#' Filter a patient based on their admission diagnosis.
#'
#' The input must be a single string. The string will be pushed to lowercase
#' and split on any kind of punctuation to yield multiple substrings.
#' Each substring will be compared against a list of inclusion and exclusion
#' critera.
#' The returned values are logical with TRUE and FALSE representing membership
#' in the inclusion and exclusion critera respectively and NA, neither.
#' All substrings are returned for transparency but care should be taken when
#' filtering so that individuals satisfying both inclusion and exclusion are
#' not mistakingly included.
#'
#' @param temp_str A string containing the individual's admission diagnosis
#' @param temp_df A data.frame containing admission diagnoses
#' @param diagnosis_column A character string of the name of the diagnosis field
#' @param inclusion A character vector containing the desired inclusion criteria
#' @param exclusion A character vector containing the desired exclusion criteria
#' @return A vector of logical values describing membership to inclusion/exclusion
#' @examples
#' # some fundamental types
#' inclusion = c('PNEUMONIA','PNA','SEPSIS','FEVER','ALTERED MENTAL STATUS')
#' exclusion = c('CHF','CONGESTIVE HEART FAILURE','INTRACRANIAL HEMORRHAGE','SUBARACHNOID HEMORRHAGE')
#' temp_str = c('CHF - FEVER / test')
#' temp_df = data.frame(HADM_ID = c('1', '2', '3', '4'),
#'    diagnosis = c('CHF - FEVER / test', 'pneumonia',
#'        'hemorrhage', 'intracranial hemorrhage'))
#' # call the functions
#' filter_admissiondiagnosis_string(temp_str, inclusion, exclusion)
#' # mapping
#' sapply(temp_df$diagnosis, function(x) filter_admissiondiagnosis_string(x, inclusion, exclusion))
#' # direct
#' # filter_admissiondiagnosis_df(temp_df, 'diagnosis', inclusion, exclusion)
#'
#' @export
filter_admissiondiagnosis_string = function(temp_str, inclusion, exclusion = NULL){

  ## First, check the type of both inclusion and exclusion
  if(typeof(inclusion) != 'character'){
    inclusion = as.character(inclusion)
  }
  ## then, make sure each is lower case
  inclusion = sapply(inclusion, stringr::str_to_lower)

  ## split the input string by any punctuation
  temp_list = stringr::str_to_lower(
    stringr::str_trim(unlist(stringr::str_split(temp_str, "[:punct:]"), 'both')))
  ## Old separator list
  #separator = c(';', ',', '?', 'S/P', 'R/O', 'W/', '\\', '/', '-')

  ## case for the null exclusion default
  if(!is.null(exclusion)){
    ## Same procedure as above
    if(typeof(exclusion) != 'character'){
      exclusion = as.character(exclusion)
    }
    exclusion = sapply(exclusion, stringr::str_to_lower)
  }

  ## test against both inclusion and exclusion lists
  mask = data.frame('diagnosis_substring' = temp_list, 'mask' = sapply(temp_list, function(x) inc_or_not_exc(x, inclusion, exclusion)) )
  rownames(mask) = NULL

  return(mask)
}

#' @describeIn filter_admissiondiagnosis_string Apply to data.frame of admission diagnoses
filter_admissiondiagnosis_df = function(temp_df, diagnosis_column, inclusion, exclusion = NULL){
  ## starting with the temp_df and the diagnosis_column

  ## check that the column exists
  if(!(diagnosis_column %in% colnames(temp_df))){
    stop("The supplied diagnosis_column argument does not match a column name in the supplied data frame.")
  }

  ## map the existing function across each row
  s = lapply(temp_df[,diagnosis_column], function(x) filter_admissiondiagnosis_string(x, inclusion, exclusion))
  ## replicate rows where the diagnosis is split into > 1 substring and then
  ## add the substrings and logical calls to the right.
  temp = cbind(temp_df[rep(1:nrow(temp_df), sapply(s, nrow)),], do.call("rbind", s))

  return(temp)
}

#' @describeIn filter_admissiondiagnosis_string Apply to data.frame of admission diagnoses using predefined criteria
filter_admissiondiagnosis_string_for_sepsis = function(temp_str){

  ## simply a special case wrapper to call filter_admissiondiagnosis_df for
  ## the given lists of inclusion and exclusion criteria for severe sepsis

  ## the lists that we used for the AMIA 2016 work and others:
  inclusion = c("pneumonia", "pna", "sepsis", "fever", "altered mental status",
                "hypotension", "acute renal failure", "respiratory failure",
                "pancreatitis", "liver failure", "shortness of breath", "cellulitis",
                "dyspnea", "respiratory distress", "urosepsis", "urinary tract infection",
                "pyelonephritis", "cardiac arrest", "hypoxia", "endocarditis",
                "weakness", "renal failure", "diabetic ketoacidosis", "cholangitis",
                "dehydration", "cholecystitis", "failure to thrive", "septic shock",
                "unresponsive", "hepatic encephalopathy", "bacteremia", "colitis",
                "epidural abscess", "wound infection", "mental status changes",
                "arrest", "shock", "febrile neutropenia", "uti", "cardiogenic shock",
                "hepatitis", "adult respiratory distress syndrome", "ards", "diverticulitis",
                "meningitis", "aspiration pneumonia", "encephalopathy", "hepatic failure",
                "neutropenia", "r/o sepsis", "thrombocytopenia", "diarrhea",
                "tachycardia", "hyperglycemia", "methicillin resistant staph aureus",
                "change in mental status", "mental status change", "pancytopenia",
                "resp failure", "respiratory arrest", "spontaneous bacterial peritonitis",
                "arf", "infection", "influenza", "septic knee", "sob", "septic shock",
                "elevated creatinine", "elevated  lfts", "empyema", "hypotensive",
                "infected graft", "osteomyelitis", "pea arrest", "post arrest",
                "post op infection", "fib arrest", "infection", "hypoglycemia",
                "hypothermia", "nausea", "vomiting")
  exclusion = c("chf", "congestive heart failure", "intracranial hemorrhage",
                "subarachnoid hemorrhage", "gastrointestinal bleed", "upper gi bleed",
                "lower gi bleed", "gi bleed", "pulmonary embolis", "pulmonary embolus",
                "upper gastrointestinal bleed", "acute subdural hematoma", "subdural hematoma",
                "bowel obstruction", "syncope", "hip fracture", "stemi", "s/p motor vehicle accident",
                "acute leukemia", "stroke", "s/p fall", "fall", "transient ischemic attack",
                "tia", "overdose", "heart failure", "head bleed", "blunt trauma",
                "stroke/tia", "variceal bleed", "cerebrovascular accident", "chf exacerbation",
                "trauma", "tylenol overdose", "subarachnoid hematoma", "femur fracture",
                "intraparenchymal hemorrhage", "subdural hemorrhage", "transient ischemic attack (tia)",
                "bright red blood per rectum", "ruptured aaa", "stroke-transient ischemic attack",
                "tylenol od")

  output = filter_admissiondiagnosis_string(temp_str, inclusion, exclusion)
  return(output)

}

#' @describeIn filter_admissiondiagnosis_string Apply to data.frame of admission diagnoses using predefined criteria
filter_admissiondiagnosis_df_for_sepsis = function(temp_df, diagnosis_column){

  ## simply a special case wrapper to call filter_admissiondiagnosis_df for
  ## the given lists of inclusion and exclusion criteria for severe sepsis

  ## the lists that we used for the AMIA 2016 work and others:
  inclusion = c("pneumonia", "pna", "sepsis", "fever", "altered mental status",
    "hypotension", "acute renal failure", "respiratory failure",
    "pancreatitis", "liver failure", "shortness of breath", "cellulitis",
    "dyspnea", "respiratory distress", "urosepsis", "urinary tract infection",
    "pyelonephritis", "cardiac arrest", "hypoxia", "endocarditis",
    "weakness", "renal failure", "diabetic ketoacidosis", "cholangitis",
    "dehydration", "cholecystitis", "failure to thrive", "septic shock",
    "unresponsive", "hepatic encephalopathy", "bacteremia", "colitis",
    "epidural abscess", "wound infection", "mental status changes",
    "arrest", "shock", "febrile neutropenia", "uti", "cardiogenic shock",
    "hepatitis", "adult respiratory distress syndrome", "ards", "diverticulitis",
    "meningitis", "aspiration pneumonia", "encephalopathy", "hepatic failure",
    "neutropenia", "r/o sepsis", "thrombocytopenia", "diarrhea",
    "tachycardia", "hyperglycemia", "methicillin resistant staph aureus",
    "change in mental status", "mental status change", "pancytopenia",
    "resp failure", "respiratory arrest", "spontaneous bacterial peritonitis",
    "arf", "infection", "influenza", "septic knee", "sob", "septic shock",
    "elevated creatinine", "elevated  lfts", "empyema", "hypotensive",
    "infected graft", "osteomyelitis", "pea arrest", "post arrest",
    "post op infection", "fib arrest", "infection", "hypoglycemia",
    "hypothermia", "nausea", "vomiting")
  exclusion = c("chf", "congestive heart failure", "intracranial hemorrhage",
    "subarachnoid hemorrhage", "gastrointestinal bleed", "upper gi bleed",
    "lower gi bleed", "gi bleed", "pulmonary embolis", "pulmonary embolus",
    "upper gastrointestinal bleed", "acute subdural hematoma", "subdural hematoma",
    "bowel obstruction", "syncope", "hip fracture", "stemi", "s/p motor vehicle accident",
    "acute leukemia", "stroke", "s/p fall", "fall", "transient ischemic attack",
    "tia", "overdose", "heart failure", "head bleed", "blunt trauma",
    "stroke/tia", "variceal bleed", "cerebrovascular accident", "chf exacerbation",
    "trauma", "tylenol overdose", "subarachnoid hematoma", "femur fracture",
    "intraparenchymal hemorrhage", "subdural hemorrhage", "transient ischemic attack (tia)",
    "bright red blood per rectum", "ruptured aaa", "stroke-transient ischemic attack",
    "tylenol od")

  output = filter_admissiondiagnosis_df(temp_df, diagnosis_column, inclusion, exclusion)
  return(output)

}
