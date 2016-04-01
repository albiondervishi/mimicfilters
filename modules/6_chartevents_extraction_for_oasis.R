# Vincent Major
# function to take in each separate variables from CHARTEVENTS and extract the extreme values of the first
# 24 hours

c_CHARTEVENTS_OASIS_extraction = function(CHARTEVENTS, variable, ICUIN)
{
  
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