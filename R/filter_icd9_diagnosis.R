# Vincent Major
# April 10 2016
# Script designed to be mapped through the DIAGNOSIS_DATA_TABLE.csv comparing
# against one supplied vector of ICD-9 codes.

# Usage:
#   a_diagnosis_groups_by_icd9_list = function(icd9.codes.vector, ){
# Where,
#   icd.codes.vector is a data.frame or vector of the ICD-9 codes in either format e.g 123.45 or 12345. Leading zeros will be replaced if nchar < 3 e.g. 1 --> 001 and the function works in a hierarchical manner so that a code of 123 will include any code within the range 123.01-123.99.
#   path.raw is the subdirectory that contains the raw DIAGNOSIS_DATA_TABLE.csv file, e.g. "raw"

# Output is the DIAGNOSIS table subsetted to the rows that include ICD-9 codes within the given list. The SUBJECT_ID or HADM_ID can then easily be extracted from the output data.frame.

#### Helper functions
remove_icd9_periods = function(icd9.vector){
  # check the icd9.codes.vector for periods and remove them.
  if(any(grepl("\\.",icd9.vector))){
    icd9.vector = as.character(sapply(icd9.vector, function(x) gsub(pattern = "\\.", replacement = "", x)))
  }

  return(icd9.vector)
}

pad_icd9 = function(icd9.vector, expected.length = 3){
  # Pad the first strsplit on "." shorter than 3 characters with leading zeros.

  # extract the first component before the "."
  temp.vector = as.character(sapply(icd9.vector, function(x) strsplit(x, ".", fixed = T)[[1]][1]))
  temp.indices = nchar(temp.vector) # lengths of each substring

  if(any(temp.indices < expected.length)){
    # those that require leading 0 padding
    icd9.vector = c(sapply(which(temp.indices < expected.length), function(x) paste0(paste0(rep("0",expected.length - temp.indices[x]), collapse = ""), icd9.vector[x])), icd9.vector[temp.indices == 3])
  }
  # else, leave it.

  # this is better but still does help in non "." cases
  # temp.list = sapply(test.vector, function(x) strsplit(x, ".", fixed = T))
  # lapply(temp.list, function(x) paste(sprintf("%03s", x[1]), x[2], sep = ".") )
  return(icd9.vector)
}

check_icd9.test = function(test_codes){
  # Ensure that the icd9.test object is a character with length 1

  if(typeof(test_codes) != 'character'){
    test_codes = as.character(test_codes)
  }

#   if(length(icd9.test) > 1){
#     warning(paste("Supplied icd9.test had length greater than 1.",
#                   "Elements with index above 1 were ignored.",
#                   "Consider wrapping in apply type function."))
#     icd9.test = icd9.test[1]
#   }

  # Pad the shorter than 3 characters with leading zeros.
  test_codes = pad_icd9(test_codes, 3)

  # Remove periods for consistency
  test_codes = remove_icd9_periods(test_codes)

  return(test_codes)
}

check_icd9.reference = function(reference_codes){
  # Ensure that the icd9.reference object is a character vector
  if(typeof(reference_codes) != 'character'){
    reference_codes = as.character(reference_codes)
  }

  # Pad the shorter than 3 characters with leading zeros.
  reference_codes = pad_icd9(reference_codes, 3)

  # Remove periods for consistency
  reference_codes = remove_icd9_periods(reference_codes)

  return(reference_codes)
}

#### Meat of the function

#' Filter a set of ICD-9 codes against a reference set.
#'
#' Inputs can be either character or numeric lists or vectors but are coerced
#' to character vectors. The \code{test_codes} argument is assumed to be
#' completely specified - that is, two values after the '.' period. The
#' \code{icd9.reference} set does not have to be completely specified so that,
#' '123' includes '123.34' and '123.45'.
#'
#' @param test_codes A vector or list ofICD-9 value to test.
#' @param reference_codes A vector or list of ICD-9 codes to act as reference.
#' @return A vector of logical (TRUE/FALSE) values representing inclusion/exclusion.
#' @examples
#' # With dplyr
#' data = data.frame(id = c(1, 2, 3, 4, 5, 6),
#'   ICD9 = c('123.45', '234.56', '345.67', '456.78','567.89', '678.90'))
#' dplyr::filter(data, filter_icd9_by_codes(ICD9, c('123', '345', '567')))
#'
#' # Without dplyr
#' mask = filter_icd9_by_codes(data$ICD9, c('123', '345', '567'))
#' data.filtered = data[mask,]
#'
#' @export
filter_icd9_by_codes = function(test_codes, reference_codes){
  # a function that takes one ICD-9 value to test against a vector/list of other codes.

  # Ensure that the icd9.test object is a character with length 1, remove '.'s and pad zeros
  test_codes = check_icd9.test(test_codes)

  # Ensure that the icd9.reference object is a character vector, remove '.'s and pad zeros
  reference_codes = check_icd9.reference(reference_codes)

  # Subset the raw data
  matches <- grepl(paste0("^",reference_codes, collapse="|"), test_codes)

  return(matches)

}

#' @describeIn filter_icd9_by_codes Apply filtering with two set lists for severe sepsis
filter_icd9_by_severesepsis = function(test_codes){
  # a wrapper function that takes one ICD-9 value to test against the known
  # vectors for severe sepsis

  ## read in the systemic infection codes from the data directory and ensure no coersion
  codes.infection = read.csv("data/ICD9_codes_systemic_infection.csv", colClasses = c('character', 'character'))
  ## filter by systemic infection code list
  matches.infection = filter_icd9_by_codes(test_codes, codes.infection$code)

  ## repeat for major organ failure list
  codes.organfailure = read.csv("data/ICD9_codes_major_organ_failure.csv", colClasses = c('character', 'character'))
  matches.organfailure = filter_icd9_by_codes(test_codes, codes.organfailure$code)

  ## return the intersect (A and B)
  matches = matches.infection & matches.organfailure
  return(matches)
}
