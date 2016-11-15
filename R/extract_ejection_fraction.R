# Vincent Major
# April 10 2016
# A script that given the raw, free-text echocardiogram report will search first for numeric instances of LVEF = X% and then at failure to do so revert to searching for cases of severe, moderate or mild reduced LV systolic function/hypokinesis or normal LVEF function.

# Usage:
  # b_ef_regex_from_raw_note = function (temp_str)
# Where,
#   temp_str is the large string containing the raw echocardiogram note with all grammar, newlines etc included.

# Output is a vector of numeric EF values recovered (which I suggest is subsequently min and maxed for the extremes) OR in the case of text based severe/moderate/mild searches, the values represent the extremes of the clinically defined ranges.

## Load real data
#echo_data = read_csv("../ECHO_for_inspection.csv")

#### Helper functions

ef_detect_TEE = function(temp_str){
  ## The structure of these MIMIC echocardiogram reports normally contain a
  ## header of the form
  ## <key>: <value>
  ## Match the test key if the value is TEE
  return(grepl('test:.{0,12}tee',temp_str, ignore.case = TRUE))
}
## echo_data$TEXT[4]=TRUE, 1:3=FALSE

ef_detect_extract_numeric = function(temp_str){
  # start of the regular expression algorithm finding EF ---- % and narrowing it down to one or two digits
  regex.local = "ef.{0,15}%"
  regex.digits = ".*?([0-9]{1,2}).*?"

  ## if not ef...% is found, return NA
  if(!stringr::str_detect(temp_str, regex.local)){
    return(NA)
  }

  x = stringr::str_extract_all(temp_str, regex.local)
  xx = stringr::str_match_all(x, regex.digits)

  ## if failed to return any digits, return NA
  if(nrow(xx[[1]]) == 0){
    return(NA)
  }

  ## archive of other patterns
  #str_match_all(x, '([0-9]{1,2}).+?([0-9]{1,2})')
  #str_match_all(x, '([0-9]{1,2})[ -]{0,3}([0-9]{1,2}).{0,2}')

  # will only be executed for length 1.
  # return the matches not the context ([,1])
  return(as.integer(xx[[1]][,2]))
}

ef_string_to_numeric = function(word_list){
  ## input is a list of words, now to bin them into
  output = NULL # initialize with Null to append to later
  if(any(sapply(word_list, function(x) agrepl("severe", x))))
  {
    output = c(output, 5, 29)
  }
  if(any(sapply(word_list, function(x) agrepl("moderate", x))))
  {
    output = c(output, 31, 39)
  }
  if(any(sapply(word_list, function(x) agrepl("mild", x))))
  {
    output = c(output, 41, 49)
  }
  if(any(sapply(word_list, function(x) agrepl("normal", x) | agrepl("preserved", x))))
  {
    output = c(output, 51, 99)
  }
  return(output)
}

ef_detect_coerce_string = function(temp_str){
  ## split by double newlines and then periods
  ## remove punctuation
  ## split into words by splitting on whitespace
  ## look for a set containing left venticular/LV
  ## then a set with function/dysfunction/hypokinesis
  ## which of those have severe, moderate, mild

  a = stringr::str_split(temp_str, stringr::fixed("\n\n"))
  aa = stringr::str_split(a, stringr::fixed('.'))

  aa = lapply(aa, function(x) stringr::str_replace_all(x, stringr::fixed("\n"), " "))
  aa = lapply(aa, function(x) stringr::str_replace_all(x, stringr::fixed("\\n"), " "))
  aa = lapply(aa, function(x) stringr::str_replace_all(x, "[:punct:]", " "))

  aaa = lapply(aa, function(x) stringr::str_split(x, "[:space:]") )[[1]]


  lv.mask = which(sapply(aaa, function(x) any(x %in% c("left", "LV")) & any(x %in% c("ventricle", "ventricular", "LV"))))
  # sapply(lv.mask, function(x) print(aaa[[x]])) # Looks good!
  aaa = aaa[lv.mask]
  if(length(aaa) == 0){return(NULL)}

  function.mask = which(sapply(aaa, function(x) any(x %in% c("function", "dysfunction", "depressed", "hypokinesis"))))
  aaa = aaa[function.mask]
  if(length(aaa) == 0){return(NULL)}

  severity.mask = which(sapply(aaa, function(x) any(x %in% c("severe", "severely", "moderate", "moderately", "mild", "mildly", "normal", "preserved"))))
  aaa = aaa[severity.mask]
  if(length(aaa) == 0){return(NULL)}

  output = unlist(lapply(aaa, ef_string_to_numeric))
  return(output)
}

#### Meat of the function

#' Extract a vector of ejection fraction values from a echocardiogram report.
#'
#' The input must be a single string. Newlines will be removed and the text will
#' be coereced to lowercase.
#' For string-based severity descriptors (severe, moderate, mild, and normal),
#' the text is split by double newline ('\code{\\n\\n}') and then period '\code{.}'
#' to separate sentences. The sentences are filtered to those that refer to
#' left ventricular systolic function or dysfunction. The remaining sentences
#' are classified by those that contain each of the four descriptors and these
#' are translated into numeric definitions as
#' \code{severe = c(5,29)}
#' \code{moderate = c(31,39)}
#' \code{mild = c(41,49)}
#' \code{normal = c(51,99)}
#'
#' @param temp_str A string containing a raw echocardiogram report
#' @return A vector of extracted ejection fraction values
#' @examples
#' # some fundamental types
#' temp_str = "severely depressed left ventricular systolic function"
#' extract_ejectionfraction_from_string(temp_str)
#' echo_list = list( "LVEF > 55%", "LVEF = 20 - 30%")
#' lapply(echo_list, extract_ejectionfraction_from_string)
#'
#' @export
extract_ejectionfraction_from_string = function(temp_str){
  #Initialize
  numeric.output = NULL

  ## First check for TEE and if TRUE return NA
  tee.flag = ef_detect_TEE(temp_str)
  if(tee.flag){
    return(NA)
  }

  ## Detect for strings
  string.output = ef_detect_coerce_string(temp_str)

  ## replace newlines with single space
  temp_str = gsub("\\n", ' ', temp_str)

  ## before searching, coerce to lowercase
  temp_str = stringr::str_to_lower(temp_str)

  ## search for existence of '%', if true, search for LVEF
  if(grepl("%", temp_str)){
    numeric.output = ef_detect_extract_numeric(temp_str)

    ## remove % signs - not needed if only performing a single search
#     if(is.na(numeric.results)){
#       # If failed to extract any numeric EF, % sign must be irrelevant
#       temp_str = gsub("%", "", temp_str) # remove any %
#     }
  }

#   print(string.output)
#   print(numeric.output)
  return(c(numeric.output, string.output))
}

