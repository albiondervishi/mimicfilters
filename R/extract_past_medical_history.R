# Vincent Major
# April 10 2016
# A function to take in the raw discharge note in a data.frame subset of the raw NOTE table with CATEGORY == "Discharge". At least the input data.frame must have HADM_ID and TEXT fields. A regex process is used to extract the Past Medical History Section from the raw Discharge note. Unfortunately, instances of 'Prior Medical History' as well as 'HISTORY OF PRESENT ILLNESS' also occur and are included.

# Usage,
#   d_past_medical_history_regex_raw_note = function(NOTE_HADM_from_file)
# Where,
#   NOTE_HADM_from_file is a data.frame subset of the raw NOTE table with every row $CATEGORY == "Discharge".

# Output is a data.frame of HADM_ID and Past Medical History snippets that can subsequently be regexed for strings of interest.

## load real data
#library(readr)
#data = read_csv("../preprocess/NOTE_HADM_from_file.csv")
#data = data[1:10,]
#temp_str = data$TEXT[1]
# exclusion = c('CHF', '\bHF\b', 'heart.{1,3}failure', 'HFREF', 'HFPEF',
#               'cardiomyopathy', 'depressed ejection fraction','depressed EF','depressed LVEF',
#               'global hypokinesis','ventricular hypokinesis', 'ventricular dysfunction')


#### Helper functions
extract_pastmedicalhistory_string = function(temp_str){

  # Including Past and Prior Medical History for a few cases of the latter.
  x = gregexpr('p.{3,4} medical history:', temp_str, ignore.case = TRUE)
  a = x[[1]][1]
  if(a == -1)
  {
    x = gregexpr('history of present illness:', temp_str, ignore.case = TRUE)
    a = x[[1]][1]
  }

  if(a == -1) {
    return(NA)
  }

  y = gregexpr("\\b[a-z]+(:)\\W", temp_str)
  b = y[[1]][which(y[[1]] > a)[2]]
  # b = y[[1]][match(x[[1]][1],y[[1]])+1]+20
  if(is.na(b) == TRUE)
  {
    b = nchar(temp_str)
  }
  output = substr(temp_str, a, b)
  output = trimws(gsub("\n", " ", output), 'both')

  return(output)
}

#### Meat of the function

#' Filter a patient based on their admission diagnosis.
#'
#' The input must be a single string. The string will be pushed to lowercase.
#' The string will be searched for past medical history, or history of present
#' illness. Each substring will be compared against a list of exclusion
#' critera which if present will be marked as TRUE.
#'
#' @param temp_str A string containing the individual's discharge summary note
#' @param temp_df A data.frame containing discharge summaries
#' @param text_column A character string of the name of the text field
#' @param exclusion A character vector containing the desired exclusion criteria
#' @return A vector of logical values describing membership to exclusion
#' @examples
#' temp_str = "blah past medical history: smoking related emphysema. blah: test"
#' extract_pastmedicalhistory_string(temp_str)
#' extract_pmhflag_string(temp_str, 'smok') # TRUE
#' extract_pmhflag_string(temp_str, 'etoh') # FALSE
#'
#' temp_df = data.frame(id = c(1,2,3),
#'   text = c("blah\nPast medical history: smoking related emphysema. blah: test",
#'   "blah PRIOR MEDICAL HISTORY: excessive etoh. blah: test",
#'   "blah history of present illness: obesity, hypertension, HF etc blah: test"))
#'
#' extract_pmhflag_df(temp_df, 'text', c('smok', 'etoh'))
#' extract_pmhflag_df_for_hf(temp_df, 'text')
#'
#'
#' @export
extract_pmhflag_string = function(temp_str, exclusion){

  ## two part function
  ## 1. find the part of the note (temp_str) relevant to past medical history
  ## 2. search the substring for references to the items in temp_list

  temp_str = stringr::str_to_lower(temp_str)
  exclusion = stringr::str_to_lower(exclusion)

  temp_pmh = extract_pastmedicalhistory_string(temp_str)

  # exclusion based on presence of exclusion terms within the PMH string
  output = stringr::str_detect(temp_pmh, exclusion)
  ## detections will be TRUE, any is enough to exclude

  # EXCLUDE PATIENTS WITH EF LESS THAN 55% in the past
  # rather than write another function, utilise EF_regex which will extract 'EF.{0,12}%'
  # if the value is finite, check the first (EFmin) number and remove if < 55.
#   numeric = extract_ejectionfraction_from_string(temp_pmh)
#   if(!is.null(numeric)){
#     output = c(output, TRUE)
#   }

  output = any(output)
  return(output)
}

#' @describeIn extract_pmhflag_string Apply to data.frame of discharge summaries
extract_pmhflag_df = function(temp_df, text_column, exclusion){
  ## starting with the temp_df and the diagnosis_column

  ## check that the column exists
  if(!(text_column %in% colnames(temp_df))){
    stop("The supplied diagnosis_column argument does not match a column name in the supplied data frame.")
  }

  ## map the existing function across each row
  s = lapply(unlist(temp_df[, text_column]), function(x) extract_pmhflag_string(x, exclusion))
  ## replicate rows where the diagnosis is split into > 1 substring and then
  ## add the substrings and logical calls to the right.
  temp_df$pmhflag = s

  return(temp_df)
}

#' @describeIn extract_pmhflag_string Apply with provided list of heart failure terms
extract_pmhflag_string_for_hf = function(temp_str){
  exclusion = c('CHF', '\\bHF\\b', 'heart.{1,3}failure', 'HFREF', 'HFPEF',
                'cardiomyopathy', 'depressed ejection fraction','depressed EF','depressed LVEF',
                'global hypokinesis','ventricular hypokinesis', 'ventricular dysfunction')

  extract_pmhflag_string(temp_str, exclusion)
}

#' @describeIn extract_pmhflag_string Apply to data.frame with provided list of heart failure terms
extract_pmhflag_df_for_hf = function(temp_df, text_column){
  exclusion = c('CHF', '\\bHF\\b', 'heart.{1,3}failure', 'HFREF', 'HFPEF',
                'cardiomyopathy', 'depressed ejection fraction','depressed EF','depressed LVEF',
                'global hypokinesis','ventricular hypokinesis', 'ventricular dysfunction')

  extract_pmhflag_df(temp_df, text_column, exclusion)
}
