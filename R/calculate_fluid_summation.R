# Vincent Major
# April 10 2016
# A script that sums the fluids in, out, and urine out to a patient over a specified timerange with options for the minimum volume to include and whether or not to normalize if the time interval that fluids in are recorded is less that the specified timerange.

# Usage,
#   f_fluids_administered_summation = function(temp_INS, temp_OUTS, temp_urine, temp_ICUSTAY_INTIME, timerange, min_volume_to_include, normalize)

# Where, temp_INS - table of INS for one HADM_ID to include all of
#   temp_OUTS - table of OUTS for one HADM_ID to include all of
#   temp_urine - table of urine OUTS for one HADM_ID to include all of
#   temp_ICUSTAY_INTIME - ICUSTAY$INTIME of the individual, the start time that timerange extends
#   timerange - the time interval to summ fluids over.
#   min_volume_to_include - If a minimum fluid volume is desired, in mL. For example large intraveneous fluids will all be > 250 mL
#   normalize - TRUE/FALSE value to determine whether or not to normalize the fluids over time for instances where fluids are given for times shorter than timerange.

# Output is the summed fluids in, out and urine out over the timerange respectively as a list i.e. list(Vol_in, Vol_out, urine_out)

#' Calculate the fluids received and removed from the patient.
#'
#' Input required four data.frames and there are three optional parameters.
#'
#' @param temp_INS A data.frame containing fluids received by the patient
#' @param temp_OUTS A data.frame containing fluids out recorded from the patient
#' @param temp_urine A data.frame containing urine out recorded from the patient
#' @param temp_ICUSTAY_INTIME A subset of ICUSTAY, a data.frame containing
#' patient identifiers and the time they arrive in the ICU.
#' @param timerange A numeric of the number of hours post ICU admission to sum over
#' @param min_volume_to_include A numeric of the minimum volume to include in summation
#' @param normalize Logical value whether or not to normalize in case length
#' of stay < timerange
#' @return A list of summation values in, out and urine out.
#'
#' @export
calculate_fluid_summation = function(temp_INS, temp_OUTS, temp_urine, temp_ICUSTAY_INTIME, timerange = 24, min_volume_to_include = 0, normalize = FALSE){
  # temp_INS - table of INS for one HADM_ID to include all of
  # temp_OUTS - table of OUTS for one HADM_ID to include all of
  # temp_urine - table of urine OUTS for one HADM_ID to include all of
  # temp_ICUSTAY_INTIME - ICUSTAY$INTIME of the individual, the start time that timerange extends
  # timerange - the time interval to summ fluids over.
  # min_volume_to_include - If a minimum fluid volume is desired, in mL. For example large intraveneous fluids will all be > 250 mL
  # normalize - TRUE/FALSE value to determine whether or not to normalize the fluids over time for instances where fluids are given for times shorter than timerange.

  # Example inputs from larger INS, OUTS and urine table.
  # temp_INS =  subset(INS_by_HADM_from_file, HADM_ID == temp_HID)
  # temp_OUTS =  subset(OUTS_by_HADM_from_file, HADM_ID == temp_HID)
  # temp_urine = subset(urine_by_HADM_from_file, HADM_ID == temp_HID)

  ##
  # Ensure timerange is in days
  if(timerange >= 24){
    writeLines("timerange >= 24, assumed wrongly specified as hours - converting to days.")
    timerange = timerange/24
  }
  # Ensure INTIME is of type strptime.
  temp_ICUSTAY_INTIME = strptime(temp_ICUSTAY_INTIME, format = "%Y-%m-%d %H:%M:%S")
  # How long does the patient get fluids for?
  temp_time_IOend = max(strptime(temp_INS$CHARTTIME,format = "%Y-%m-%d %H:%M:%S"), na.rm = TRUE)
  temp_time_diff = as.numeric(difftime(temp_time_IOend, temp_ICUSTAY_INTIME, units = 'days'))
  # Limit input tables by min_volume_to_include
  temp_INS = subset(temp_INS, AMOUNT >= min_volume_to_include)
  temp_OUTS = subset(temp_OUTS, VALUE >= min_volume_to_include)
  temp_urine = subset(temp_urine, VALUE >= min_volume_to_include)

  temp_INS = subset(temp_INS, CHARTTIME <= temp_ICUSTAY_INTIME + timerange*60*60*24)
  temp_OUTS = subset(temp_OUTS, CHARTTIME <= temp_ICUSTAY_INTIME + timerange*60*60*24)
  temp_urine = subset(temp_urine, CHARTTIME <= temp_ICUSTAY_INTIME + timerange*60*60*24)

  Vol_input = sum(temp_INS$AMOUNT, na.rm = TRUE)
  Vol_output = sum(temp_OUTS$VALUE, na.rm = TRUE)
  urine_out = sum(temp_urine$VALUE,na.rm = TRUE)

  if(normalize == TRUE & temp_time_diff < timerange){
    Vol_input = Vol_input/temp_time_diff
    Vol_output = Vol_output/temp_time_diff
    urine_out = urine_out/temp_time_diff
  }
  return(list(Vol_input, Vol_output, urine_out))
}
