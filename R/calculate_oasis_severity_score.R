# Vincent Major
# April 10 2016
# A function to take in each separate OASIS parameter from a data.frame subset of CHARTEVENTS and extract the extreme and median values, over the first 24 hours, for latter use in e_chart_parameters_to_oasis_scores. Takes as input, the subsetted CHARTEVENTS table, the variable to inspect, and the ICUIN table for the time of ICU admission.

# Usage,
#   e_CHARTEVENTS_OASIS_extraction = function(CHARTEVENTS, variable, ICUIN)
# Where,
#   CHARTEVENTS is a data.frame subset of raw CHARTEVENTS of the patients of interest for only the parameter of interest (for the sake of memory).
#   variable is a string of the OASIS variable to inspect. Must be one of c('FiO2', 'HR', 'GCS', 'MAP', 'RR', 'Temp').

# Output is a data.frame of OASIS parameter values, min, max and median that can then be redirected into e_chart_parameters_to_oasis_scores

#' Extract the OASIS components from chartevents table.
#'
#' Requires a subset of the chartevents table as a data.frame as well as a
#' string indicating which variable to look for and the icuin table as a
#' data.frame to identify the time of ICU admission.
#'
#' @param CHARTEVENTS A data.frame containing chartevents for the patient
#' @param variable A string containing the OASIS parameter to search for.
#' @param ICUIN A data.frame subset of the icuin table containing patient
#' IDs and ICU admission times.
#' @return A vector of the patient ID, minimum, median and maximum variable value
#'
#' @export
extract_oasis_from_chartevents = function(CHARTEVENTS, variable, ICUIN){
  output = {}
  if(variable == 'GCS')
  {
    # Code for GCS
    a = {}
    b = {}
    c = {}
    writeLines('Processing the Glasgow coma score (GCS) data from CHARTEVENTS')
    temp_HIDs = unique(CHARTEVENTS$HADM_ID)
    for(i in 1:length(temp_HIDs))
    {
      temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
      temp_CHART = subset(temp_CHART, CHARTTIME <= temp_CHART$CHARTTIME[1] + 24*60*60) # Take only the rows with the data and time within 24 hours of the ICU admission
      temp_CHART = sort_df(temp_CHART, vars = 'VALUENUM') #Sorting in increasing VALUENUM so that the worst value is extracted next.
      if(any(is.finite(match(198,temp_CHART$ITEMID))) == TRUE)
      {
        # A output total from CareVue exists. Check how many exist and take the earliest
        output[i] = as.integer(temp_CHART$VALUENUM[match(198, temp_CHART$ITEMID)]) # extract the first case of 198
      } else
      {
        # Must be a Metavision patients - look for 220739 (Eye opening), 223900 (verbal response), and 223901 (motor response)
        temp_temp = subset(temp_CHART, ITEMID == 220739)
        a[i] = min(as.integer(temp_temp$VALUENUM)) # extract the first case since it is ordered by date from above
        a[i] = ifelse(is.finite(a[i]),a[i],NA)

        temp_temp = subset(temp_CHART, ITEMID == 223900)
        b[i] = min(as.integer(temp_temp$VALUENUM))
        b[i] = ifelse(is.finite(b[i]),b[i],NA)

        temp_temp = subset(temp_CHART, ITEMID == 223901)
        c[i] = min(as.integer(temp_temp$VALUENUM))
        c[i] = ifelse(is.finite(c[i]),c[i],NA)
        output[i] = sum(c(a[i],b[i],c[i]),na.rm = FALSE)
      }
    }
    output = cbind(HADM_ID = temp_HIDs, GCSeye = a, GCSverbal = b, GCSmotor = c,GCS = output)
  } else if(variable == 'Temp')
  {
    # Code for Temp
    writeLines('Processing the temperature (Temp) data from CHARTEVENTS')
    Tmin = {}
    Tmax = {}
    Tmedian = {}
    temp_HIDs = unique(CHARTEVENTS$HADM_ID)
    for(i in 1:length(temp_HIDs))
    {
      temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
      temp_CHART = subset(temp_CHART, CHARTTIME <= ICUIN$INTIME[match(temp_HIDs[i],ICUIN$HADM_ID)] + 24*60*60) # Take only the rows with the data and time within 24 hours of the ICU admission
      if(nrow(temp_CHART) == 0)
      {
        temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
        temp_CHART = subset(temp_CHART, CHARTTIME <= temp_CHART$CHARTTIME[1] + 24*60*60) # Take only the rows with the data and time within 24 hours of the first recording
      }
      if(nrow(temp_CHART != 0))
      {
        a = max(temp_CHART$VALUENUM[temp_CHART$VALUENUM > 50], na.rm = TRUE)
        b = min(temp_CHART$VALUENUM[temp_CHART$VALUENUM > 50], na.rm = TRUE)
        c = median(temp_CHART$VALUENUM[temp_CHART$VALUENUM > 50], na.rm = TRUE)
        # Try Fahrenheit first
        if(is.finite(a) == FALSE || is.finite(b) == FALSE)
        {
          # Give up and try Celsius and convert to F
          a = max(temp_CHART$VALUENUM, na.rm = TRUE)*1.8+32
          b = min(temp_CHART$VALUENUM, na.rm = TRUE)*1.8+32
          c = median(temp_CHART$VALUENUM, na.rm = TRUE)*1.8+32
        }
        Tmax[i] = a
        Tmin[i] = b
        Tmedian[i] = c
      } else
      {

        Tmax[i] = NA
        Tmin[i] = NA
        Tmedian[i] = NA
      }
    }
    output = cbind(HADM_ID = temp_HIDs, Tmin, Tmedian, Tmax)
  } else if(variable == 'MAP')
  {
    # Code for MAP
    writeLines('Processing the mean arterial pressure (MAP) data from CHARTEVENTS')
    sysBP = CHARTEVENTS[[1]]
    diaBP = CHARTEVENTS[[2]]
    MAPmin = {}
    MAPmax = {}
    MAPmedian = {}
    temp_HIDs = unique(sysBP$HADM_ID)
    for(i in 1:length(temp_HIDs))
    {
      temp_sys = subset(sysBP, HADM_ID == temp_HIDs[i])
      temp_dia = subset(diaBP, HADM_ID == temp_HIDs[i])
      temp_sys = subset(temp_sys, CHARTTIME <= ICUIN$INTIME[match(temp_HIDs[i],ICUIN$HADM_ID)] + 24*60*60) # Take only the rows with the data and time within 24 hours of the ICU admission
      temp_dia = subset(temp_dia, CHARTTIME <= ICUIN$INTIME[match(temp_HIDs[i],ICUIN$HADM_ID)] + 24*60*60)
      temp_sys = subset(temp_sys, VALUENUM <= 300)
      temp_dia = subset(temp_dia, VALUENUM <= 300)
      temp_sys = subset(temp_sys, VALUENUM >= 10)
      temp_dia = subset(temp_dia, VALUENUM >= 10)
      if(nrow(temp_sys) == 0 || nrow(temp_dia) == 0)
      {
        temp_sys = subset(sysBP, HADM_ID == temp_HIDs[i])
        temp_sys = subset(temp_sys, CHARTTIME <= temp_sys$CHARTTIME[1] + 24*60*60) # Take only the rows with the data and time within 24 hours of the first recording
        temp_sys = subset(temp_sys, VALUENUM <= 300)
        temp_sys = subset(temp_sys, VALUENUM >= 10)

        temp_dia = subset(sysBP, HADM_ID == temp_HIDs[i])
        temp_dia = subset(temp_dia, CHARTTIME <= temp_dia$CHARTTIME[1] + 24*60*60) # Take only the rows with the data and time within 24 hours of the first recording
        temp_dia = subset(temp_dia, VALUENUM <= 300)
        temp_dia = subset(temp_dia, VALUENUM >= 10)
      }
      if(nrow(temp_sys) != 0 && nrow(temp_dia) != 0)
      {
        temp_times = intersect(unique(temp_sys$CHARTTIME), unique(temp_dia$CHARTTIME))
        MAPmax[i] = 0
        MAPmin[i] = 1000
        for(j in 1:length(temp_times))
        {
          temp_s = subset(temp_sys, CHARTTIME == temp_times[j])
          temp_d = subset(temp_dia, CHARTTIME == temp_times[j])

          MAPmax[i] = max(MAPmax[i],2/3*max(temp_d$VALUENUM) + 1/3*max(temp_s$VALUENUM))
          MAPmin[i] = min(MAPmin[i],2/3*min(temp_d$VALUENUM) + 1/3*min(temp_s$VALUENUM))
        }
        MAPmedian[i] = 2/3*median(temp_dia$VALUENUM) + 1/3*median(temp_sys$VALUENUM)
      } else
      {
        MAPmax[i] = NA
        MAPmin[i] = NA
        MAPmedian[i] = NA
      }
    }
    output = cbind(HADM_ID = temp_HIDs, MAPmin, MAPmedian, MAPmax)
  } else if(variable == 'HR')
  {
    # Code for HR
    writeLines('Processing the heart-rate (HR) data from CHARTEVENTS')
    HRmin = {}
    HRmax = {}
    HRmedian = {}
    temp_HIDs = unique(CHARTEVENTS$HADM_ID)
    for(i in 1:length(temp_HIDs))
    {
      temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
      temp_CHART = subset(temp_CHART, CHARTTIME <= ICUIN$INTIME[match(temp_HIDs[i],ICUIN$HADM_ID)] + 24*60*60) # Take only the rows with the data and time within 24 hours of the ICU admission
      if(nrow(temp_CHART) == 0)
      {
        temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
        temp_CHART = subset(temp_CHART, CHARTTIME <= temp_CHART$CHARTTIME[1] + 24*60*60) # Take only the rows with the data and time within 24 hours of the first recording
      }
      if(nrow(temp_CHART) != 0)
      {
        HRmax[i] = max(temp_CHART$VALUENUM)
        HRmin[i] = min(temp_CHART$VALUENUM)
        HRmedian[i] = median(temp_CHART$VALUENUM, na.rm = TRUE)
      } else
      {
        HRmax[i] = NA
        HRmin[i] = NA
        HRmedian[i] = NA
      }
    }
    output = cbind(HADM_ID = temp_HIDs, HRmin, HRmedian, HRmax)
  } else if(variable == 'RR')
  {
    # Code for RR
    writeLines('Processing the respiratory-rate (RR) data from CHARTEVENTS')
    RRmin = {}
    RRmax = {}
    RRmedian = {}
    temp_HIDs = unique(CHARTEVENTS$HADM_ID)
    for(i in 1:length(temp_HIDs))
    {
      temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
      temp_CHART = subset(temp_CHART, CHARTTIME <= ICUIN$INTIME[match(temp_HIDs[i],ICUIN$HADM_ID)] + 24*60*60) # Take only the rows with the data and time within 24 hours of the ICU admission
      if(nrow(temp_CHART) == 0)
      {
        temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
        temp_CHART = subset(temp_CHART, CHARTTIME <= temp_CHART$CHARTTIME[1] + 24*60*60) # Take only the rows with the data and time within 24 hours of the first recording
      }
      if(nrow(temp_CHART) != 0)
      {
        RRmax[i] = max(temp_CHART$VALUENUM)
        RRmin[i] = min(temp_CHART$VALUENUM)
        RRmedian[i] = median(temp_CHART$VALUENUM, na.rm = TRUE)
      } else
      {
        RRmax[i] = NA
        RRmin[i] = NA
        RRmedian[i] = NA
      }
    }
    output = cbind(HADM_ID = temp_HIDs, RRmin, RRmedian, RRmax)
  } else if(variable == 'FiO2')
  {
    # Code for FiO2
    writeLines('Processing the fraction of inspired oxygen (FiO2) data from CHARTEVENTS')
    FiO2min = {}
    FiO2max = {}
    FiO2median = {}
    temp_HIDs = unique(CHARTEVENTS$HADM_ID)
    for(i in 1:length(temp_HIDs))
    {
      temp_CHART = subset(CHARTEVENTS, HADM_ID == temp_HIDs[i])
      temp_CHART = subset(temp_CHART, CHARTTIME <= ICUIN$INTIME[match(temp_HIDs[i],ICUIN$HADM_ID)] + 24*60*60) # Take only the rows with the data and time within 24 hours of the ICU admission
      # No provision for checking 24 hours from first recording for FiO2 because the absence of a recording is important!
      if(nrow(temp_CHART) != 0)
      {
        FiO2max[i] = max(temp_CHART$VALUENUM)
        FiO2min[i] = min(temp_CHART$VALUENUM)
        FiO2median[i] = median(temp_CHART$VALUENUM, na.rm = TRUE)
      } else
      {
        FiO2max[i] = NA
        FiO2min[i] = NA
        FiO2median[i] = NA
      }
    }
    output = cbind(HADM_ID = temp_HIDs, FiO2min, FiO2median, FiO2max)
  }
  return(output)
}

# Vincent Major
# April 10 2016
# A function to take in raw OASIS parameter values, as a table, and translate into scores based on the paper, Johnson et al. 2013, A New Severity of Illness Scale Using a Subset of Acute Physiology and Chronic Health Evaluation Data Elements Shows Comparable Predictive Accuracy.

# Usage,
#   e_chart_parameters_to_oasis_scores = function(OASIS_table)
# Where,
#   OASIS_table is a table of the OASIS score parameters, at least min and max. Fields that are required are age, GCS, HRmin, HRmax, MAPmin, MAPmax, RRmin, RRmax, Tmin, Tmax, urine, FiO2min, and elective_flag.

# Output is a table of scores based on the thresholds from the Johnson 2013 paper.

#' Calculate the OASIS score from the chartevents components.
#'
#' Requires a data.frame of all the OASIS component's extreme chartevents.
#'
#' @param OASIS_table A data.frame containing OASIS extreme chartevents for the patient
#' @return A data.frame of the component scores and the final, sum score
#'
#' @export
calculate_oasis_severity_score = function(OASIS_table){
  OASIS_scores = data.frame(HADM_ID = OASIS_table$HADM_ID)
  # Tin - time before ICU - remembering to convert to hours!
  OASIS_scores$Tin = bin_by_thresholds(OASIS_table$Tin*24, c(0.17,4.95,24.01,311.81), c(5,3,0,2,1))
  # values of 5, 3 and 0. no values of 2 or 1 because we have filterd so this value is <= 24 hours

  # Age
  OASIS_scores$age = bin_by_thresholds(OASIS_table$age, c(24,54,78,90), c(0,3,6,9,7))
  # All values represented: 0, 3, 6, 9, and 7

  # GCS
  OASIS_scores$GCS = bin_by_thresholds(OASIS_table$GCS, c(8,14,15), c(10,4,3,0))
  # All values represented: 10, 4, 3, and 0

  # HR
  temp = cbind(bin_by_thresholds(OASIS_table$HRmin, c(33,89,107,126), c(4,0,1,3,6)), bin_by_thresholds(OASIS_table$HRmax, c(33,89,107,126), c(4,0,1,3,6)))
  OASIS_scores$HR = apply(temp, 1, max)
  # All values represented: 4(seldom), 0, 1, 3, and 6

  # MAP
  temp = cbind(bin_by_thresholds(OASIS_table$MAPmin, c(20.65, 51, 61.33, 143.45), c(4,3,2,0,3)), bin_by_thresholds(OASIS_table$MAPmax, c(20.65, 51, 61.33, 143.45), c(4,3,2,0,3)))
  OASIS_scores$MAP = apply(temp, 1, max)
  # All values represented: 4(overly due to MAP <= 0), 3, 2, 0, and 3

  # RR
  temp = cbind(bin_by_thresholds(OASIS_table$RRmin, c(6,13,23,31,45), c(10,1,0,1,6,9)), bin_by_thresholds(OASIS_table$RRmax, c(6,13,23,31,45), c(10,1,0,1,6,9)))
  OASIS_scores$RR = apply(temp, 1, max)
  # All values represented: 10, 1, 0, 1, 6, and 9

  # Temp
  temp = cbind(bin_by_thresholds(OASIS_table$Tmin, ((c(33.22,35.94,36.40,36.89,39.89)*1.8)+32), c(3,4,2,0,2,6)), bin_by_thresholds(OASIS_table$Tmax, ((c(33.22,35.94,36.40,36.89,39.89)*1.8)+32), c(3,4,2,0,2,6)))
  OASIS_scores$Temp = apply(temp, 1, max)
  # All values represented: 3, 4, 2, 0, 2, and 6

  # urine output
  OASIS_scores$urine = bin_by_thresholds(OASIS_table$urine, c(671,1427,2544,6897), c(10,5,1,0,8))
  # All values represented: 10(overly), 5, 1, 0, and 8(seldom)

  # FiO2
  OASIS_scores$ventilated = ifelse(is.na(OASIS_table$FiO2min),0,9)
  # NEED TO FIX THIS BY CHECKING THE DATE OF FIO2 - IT NEEDS TO BE THE FIRST DAY IN ICU!

  # elective surgery
  OASIS_scores$elective = ifelse(OASIS_table$elective_flag == 1, 0, 6) # If elective 0, if not 6!

  OASIS_scores$sum = apply(OASIS_scores, 1, function(x) sum(x[2:length(x)]))
  # OASIS_scores$mort_predict = apply(OASIS_scores, 1, function(x) x[length(x)]*0.1275-6.1746)
  return(OASIS_scores)
}
