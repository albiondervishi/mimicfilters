# Vincent Major
# November 13th 2015
# Script to check if data frames of preprocessed csv files exist, if TRUE - do nothing
# if FALSE --> load in the data and complete any other preprocessing steps. 

library("stringr")
# library("plyr")
library("reshape")
library('icd9')
# First check if the last step is complete
# does inc_HADM_ID exist in cache?
if(file.exists('cache/noCHF_combined_table.RData'))
{
  # load everything if it exists
  load('cache/ICD9_subgroup.RData')
  load('cache/ECHO.RData')
  load('cache/ADMISS.RData')
  load('cache/inc_admiss_HADM_ID.RData')
  load('cache/mis_admiss_HADM_ID.RData')
  load('cache/exc_admiss_HADM_ID.RData')
  load('cache/ICUSTAY.RData')
  load('cache/PATIENTS.RData')
  load('cache/In_ITEMS.RData')
  # load('cache/INS_by_HADM_from_file.RData')
  # load('cache/OUTS_by_HADM_from_file.RData')
  # load('cache/urine_OUTS_by_HADM_from_file.RData')
  load('cache/LABEVENTS.RData')
  # load('cache/CHARTEVENTS.RData')
  load('cache/GCS.RData')
  load('cache/Temp.RData')
  load('cache/MAP.RData')
  load('cache/RR.RData')
  load('cache/HR.RData')
  load('cache/FiO2.RData')
  load('cache/NOTE_HADM_from_file.RData')
  load('cache/ICD9_HADM_from_file.RData')
  load('cache/ICD9_comorbidity_scores.RData')
  load('cache/DRG_HADM_from_file.RData')
  load('cache/str_table.RData')
  load('cache/in_str_table.RData')
  load('cache/CHF_HIDs.RData')
  # load('cache/combined_table.RData')
  load('cache/noCHF_combined_table.RData')
  load('cache/CHF_combined_table.RData')
  load('cache/exc_on_ward.RData')
  load('cache/no_INs.RData')
  # load('cache/volume_table.RData')
  load('cache/OASIS_table.RData')
  load('cache/OASIS_scores.RData')
  if(file.exists('cache/D_ICD9.RData'))
  {load('cache/D_ICD9.RData')}
} else
{
  
  if(file.exists('cache/ICD9_subgroup.RData') == TRUE)
  {
    load('cache/ICD9_subgroup.RData')
    writeLines('ICD9_subgroup.RData exists.')
  } else
  {
    writeLines('\ninc_HADM_ID does not exist in cache - loading from preprocessed group 1 and 2 csv')
    if(file.exists('preprocess/DIAGNOSES_group1.csv') == FALSE || file.exists('preprocess/DIAGNOSES_group2.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('Diagnoses groups do not exist in /preprocess. Calling bash script...')
      script_name = 'preprocess_ICD9_sepsis_diagnoses.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    group1 <- read.csv("preprocess/DIAGNOSES_group1.csv", comment.char="#") # 50880 patients
    group2 <- read.csv("preprocess/DIAGNOSES_group2.csv", comment.char="#") # 45617 patients
    HIDs = intersect(group1$HADM_ID,group2$HADM_ID) # 14721
    # finds the unique set of SUBJECT_IDs and HADM_IDs
    group = rbind.data.frame(group1,group2)
    matches = match(HIDs, group$HADM_ID, nomatch = 0)
    ICD9_subgroup = group[matches,]
    cache('ICD9_subgroup')
    writeLines('ICD9_subgroup.RData cached successfully')
    remove(group1, group2, HIDs, group, matches)
    #cleaning up variables
  }  
  
  HIDs = ICD9_subgroup$HADM_ID
  
  if(file.exists('cache/ECHO.RData') == TRUE)
  {
    load('cache/ECHO.RData')
    writeLines('ECHO.RData exists.')
  } else
  {
    ## ECHO
    # Taking in NOTEEVENTS.ECHO from preprocessed csv file, taking the first five columns
    # and searching through the text column for EF values using regular expressions
    # in EF_regex.R and caching the resulting table ECHO
    writeLines('\nECHO does not exist in cache - loading from preprocessed ECHO .csv')
    if(file.exists('preprocess/NOTEEVENTS_ECHO.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('Echo reports do not exist in /preprocess. Calling bash script...')
      
      script_name = 'preprocess_echo_reports.sh'
      system("pip install csvkit") # Ensures that csvkit is installed
      
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
      
      ## Different approach
      ## Not using bash script. just calling it from here
      #csvgrep_path = system("which csvgrep", intern = TRUE) # Finds the installed csvgrep path and calls it
      #system(paste0(csvgrep_path, " -c 'CATEGORY' -m 'Echo' raw/NOTEEVENTS_DATA_TABLE.csv > preprocess/NOTEEVENTS_ECHO.csv"))
    }
    NOTEEVENTS.ECHO = read.csv("preprocess/NOTEEVENTS_ECHO.csv", comment.char="#") #45,800 rows
    NOTEEVENTS.ECHO = sort_df(NOTEEVENTS.ECHO, vars = 'CHARTDATE')
    matches = match(NOTEEVENTS.ECHO$HADM_ID, HIDs) # Will have length(NOTEEVENTS.ECHO) and be filled with NAs
    ECHO = NOTEEVENTS.ECHO[which(!is.na(matches)),c('SUBJECT_ID','HADM_ID','CHARTDATE','TEXT')] # Other fields are blank
    ECHO = ECHO[-which(grepl('Test:.{0,12}TEE',ECHO$TEXT, ignore.case = TRUE)),]
    # Search and remove TEE echos. 
    rownames(ECHO) <- NULL # removing the rownames
    ECHO$EFmin = 0
    ECHO$EFmax = 0
    # taking all of the rows and the first five columns out of NOTEEVENTS.ECHO
    HIDs = unique(ECHO$HADM_ID)
    for(i in 1:nrow(ECHO))
    {
      temp_str = as.character(ECHO$TEXT[i])
      cases = EF_regex(temp_str, FALSE)
      # print(cases)
      ECHO$EFmin[i] = min(cases[[1]])
      ECHO$EFmax[i] = max(cases[[1]])
      remove(temp_str,cases)
    }
    
    # Eliminating all rows with non-finite 6th or 7th columns 
    ECHO = ECHO[is.finite(ECHO$EFmin),]
    ECHO = ECHO[is.finite(ECHO$EFmax),]
    ECHO = ECHO[(ECHO$EFmin!=0),]
    # nrow = 6256
    cache('ECHO')
    writeLines('ECHO.RData cached successfully')
    remove(NOTEEVENTS.ECHO, matches)
  }
  
  HIDs = unique(ECHO$HADM_ID) # updating current list of HADM_ID
  
  if(file.exists('cache/ADMISS.RData') == TRUE)
  {
    load('cache/ADMISS.RData')
    writeLines('ADMISS.RData exists')
  } else
  {
    # Does not exist from cache so read the csv and cache
    writeLines('\nADMISS does not exist in cache - loading from raw csv')
    ADMISSIONS <- read.csv("raw/ADMISSIONS_DATA_TABLE.csv", comment.char="#")
    
    matches = match(HIDs, ADMISSIONS$HADM_ID, nomatch = 0)
    ADMISS = ADMISSIONS[matches,]
    cache('ADMISS')
    writeLines('ADMISS.RData cached successfully')
    remove(ADMISSIONS,HIDs,matches)
    
    # sepsis = ADMISS[grep('sepsis',ADMISS$DIAGNOSIS,ignore.case =TRUE),]
    # sepsis = sepsis[-grep('rule out sepsis',sepsis$DIAGNOSIS,ignore.case = TRUE),]
    # sepsis = sepsis[-grep('R/O sepsis',sepsis$DIAGNOSIS,ignore.case = TRUE),]
    # septicemia = ADMISS[grep('septicemia',ADMISS$DIAGNOSIS,ignore.case =TRUE),]
    # septic_shock = ADMISS[grep('septic shock',ADMISS$DIAGNOSIS,ignore.case =TRUE),]
  }
  
  # inc_admiss_HADM_ID
  if(file.exists('cache/inc_admiss_HADM_ID.RData') == TRUE)
  {
    load('cache/inc_admiss_HADM_ID.RData')
    load('cache/mis_admiss_HADM_ID.RData')
    load('cache/exc_admiss_HADM_ID.RData')
    writeLines('inc_admiss_HADM_ID.RData exists')
  } else
  {
    # Does not exist from cache so read the csv and cache
    writeLines('\ninc_HADM_ID does not exist in cache - code is running')
    q = a_string_based_admission(ADMISS)
    inc_admiss_HADM_ID = unlist(q[1])
    mis_admiss_HADM_ID = unlist(q[2])
    exc_admiss_HADM_ID = unlist(q[3])
    cache('inc_admiss_HADM_ID')
    cache('mis_admiss_HADM_ID')
    cache('exc_admiss_HADM_ID')
    remove(q)
    writeLines('inc_admiss_HADM_ID.RData cached successfully')
  }
  
  HIDs = inc_admiss_HADM_ID
  
  # ICUSTAY
  if(file.exists('cache/ICUSTAY.RData') == TRUE)
  {
    load('cache/ICUSTAY.RData')
    writeLines('ICUSTAY.RData exists')
  } else
  {
    # Does not exist from cache so read the csv and cache
    writeLines('\nICUSTAY does not exist in cache - loading from raw csv')
    ICUSTAYEVENTS <- read.csv("raw/ICUSTAYS_DATA_TABLE.csv", comment.char="#")
    matches = match(HIDs, ICUSTAYEVENTS$HADM_ID, nomatch = 0)
    ICUSTAY = ICUSTAYEVENTS[matches,]
    # five cases where the HADM_ID is not present within ICUSTAYEVENTS
    # q = setdiff(HIDs, ICUSTAY$HADM_ID)
    cache('ICUSTAY')
    writeLines('ICUSTAY.RData cached successfully')
    remove(ICUSTAYEVENTS, matches)
  }
  
  # Lost five patients by absence from ICUSTAYEVENTS
  HIDs = ICUSTAY$HADM_ID
  SIDs = ICUSTAY$SUBJECT_ID
  
  # PATIENTS
  # Table of deaths in hospital and out of hospital etc. 
  if(file.exists('cache/PATIENTS.RData') == TRUE)
  {
    load('cache/PATIENTS.RData')
    writeLines('PATIENTS.RData exists')
  } else
  {
    # Does not exist from cache so read the csv and cache
    writeLines('\nPATIENTS does not exist in cache - loading csv files...')
    PATIENTS_DATA <- read.csv("raw/PATIENTS_DATA_TABLE.csv", comment.char="#")
    matches = match(SIDs, PATIENTS_DATA$SUBJECT_ID, nomatch = 0)
    PATIENTS = unique(PATIENTS_DATA[matches,])
    # Removing duplicates of SUBJECT_ID - where the patient has more than one HADM_ID linked to the same SUBJECT_ID
    # 270 repeated SUBJECT_IDs without the unique()
    cache('PATIENTS')
    writeLines('PATIENTS.RData cached successfully')
    remove(PATIENTS_DATA, matches)
    
    # To inspect the duplicated rows
#     q = sort(SIDs[which(duplicated(SIDs))])
#     temp = ICUSTAY[match(q, ICUSTAY$SUBJECT_ID),]
  }
  
  # In_ITEMS
  # loading the defining table for In_ITEMS
  if(file.exists('cache/In_ITEMS.RData') == TRUE)
  {
    load('cache/In_ITEMS.RData')
    writeLines('In_ITEMS.RData exists')
  } else
  {
    writeLines('\nIn_ITEMS does not exist in cache - loading csv files... ')
    # Does not exist from cache so read the csv and cache
    if(file.exists('preprocess/In_ITEMS.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('In_ITEMS does not exist in /preprocess. Calling bash script...')
      script_name = 'preprocess_D_ITEMS.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    In_ITEMS <- read.csv("preprocess/In_ITEMS.csv", comment.char="#")
    cache('In_ITEMS')
    writeLines('In_ITEMS.RData cached successfully')
  }
  
  # INS and OUTS 
  if(file.exists('cache/INS_by_HADM_from_file.RData') == TRUE && file.exists('cache/urine_OUTS_by_HADM_from_file.RData') == TRUE)
  {
    load('cache/INS_by_HADM_from_file.RData')
    load('cache/OUTS_by_HADM_from_file.RData')
    load('cache/urine_OUTS_by_HADM_from_file.RData')
    writeLines('INS_by_HADM_from_file.RData exists')
  } else
  {
    if(file.exists('preprocess/INS_by_HADM_from_file.csv') == FALSE || file.exists('preprocess/urine_OUTS_by_HADM_from_file.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('\nINS_by_HADM_from_file.csv does not exist in /preprocess. Calling bash script...')
      
      # create a .txt file of the HADM_IDs separated by | to load into the bash script
      awk_str = paste(HIDs, collapse = '\n')
      fileConn<-file("preprocess/generated_INOUT_awk_str.txt")
      writeLines(awk_str, fileConn)
      close(fileConn)
      
      script_name = 'preprocess_INS_HADM_from_file.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    
    
    INS_by_HADM_from_file <- read.csv("preprocess/INS_by_HADM_from_file.csv", comment.char="#")
    # Starts at 3,101,307 rows
    
    # filter by records containing UOM == 'ml' ignoring case. 'cc' was expected but does not occur.
    INS_by_HADM_from_file = INS_by_HADM_from_file[grep('ml', INS_by_HADM_from_file$AMOUNTUOM, ignore.case = TRUE),]
    # Removing outs by absense of ORDERID field
    INS_by_HADM_from_file = INS_by_HADM_from_file[!is.na(INS_by_HADM_from_file$ORDERID),]
    # Removing duplicates across the four fields
    INS_by_HADM_from_file = INS_by_HADM_from_file[!duplicated(INS_by_HADM_from_file[,c('HADM_ID','CHARTTIME','ITEMID','AMOUNT','ORDERID')]),]
    # Removing all negative volumes
    INS_by_HADM_from_file = INS_by_HADM_from_file[INS_by_HADM_from_file$AMOUNT > 0,] 
    # End with 1,266,135 rows
    INS_by_HADM_from_file$CHARTTIME = strptime(INS_by_HADM_from_file$CHARTTIME,format = "%Y-%m-%d %H:%M:%S")
    
    OUTS_by_HADM_from_file = read.csv("preprocess/OUTS_by_HADM_from_file.csv", comment.char = "#")
    OUTS_by_HADM_from_file = OUTS_by_HADM_from_file[grep('ml', OUTS_by_HADM_from_file$VALUEUOM, ignore.case = TRUE),]
    OUTS_by_HADM_from_file = OUTS_by_HADM_from_file[!is.na(OUTS_by_HADM_from_file$VALUE),2:9]
    OUTS_by_HADM_from_file = OUTS_by_HADM_from_file[!duplicated(OUTS_by_HADM_from_file[,c('HADM_ID','CHARTTIME','ITEMID','VALUE')]),]
    OUTS_by_HADM_from_file = OUTS_by_HADM_from_file[OUTS_by_HADM_from_file$VALUE > 0,]    
    OUTS_by_HADM_from_file$CHARTTIME = strptime(OUTS_by_HADM_from_file$CHARTTIME,format = "%Y-%m-%d %H:%M:%S")
    
    urine_OUTS_by_HADM_from_file = read.csv("preprocess/urine_OUTS_by_HADM_from_file.csv", comment.char = "#")
    urine_OUTS_by_HADM_from_file = urine_OUTS_by_HADM_from_file[!is.na(urine_OUTS_by_HADM_from_file$VALUE),2:9]
    urine_OUTS_by_HADM_from_file = urine_OUTS_by_HADM_from_file[!duplicated(urine_OUTS_by_HADM_from_file[,c('HADM_ID','CHARTTIME','ITEMID','VALUE')]),]
    urine_OUTS_by_HADM_from_file = urine_OUTS_by_HADM_from_file[urine_OUTS_by_HADM_from_file$VALUE > 0,]
    urine_OUTS_by_HADM_from_file$CHARTTIME = strptime(urine_OUTS_by_HADM_from_file$CHARTTIME,format = "%Y-%m-%d %H:%M:%S")
    
    cache('INS_by_HADM_from_file')
    cache('OUTS_by_HADM_from_file')
    cache('urine_OUTS_by_HADM_from_file')
    writeLines('INS_by_HADM_from_file.RData cached successfully')
  }
  
  # Labevents
  if(file.exists('cache/LABEVENTS.RData') == TRUE)
  {
    load('cache/LABEVENTS.RData')
    writeLines('LABEVENTS.RData exists')
  }
  else
  {
    if(file.exists('preprocess/LABEVENTS_HADM_from_file.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('\nLABEVENTS_HADM_from_file does not exist in /preprocess. Calling bash script...')
      
      # uses generated_INOUT_awk_str.txt from above
      # uses preprocess_APACHE_component_awk_str.txt
      
      script_name = 'preprocess_LABEVENTS_HADM_from_file.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    LABEVENTS_pre <- read.csv("preprocess/LABEVENTS_HADM_from_file.csv", comment.char="#")
    LABEVENTS_pre = sort_df(LABEVENTS_pre,vars = 'CHARTTIME')
    LABEVENTS = LABEVENTS_pre[!duplicated(LABEVENTS_pre[3:4]),]
    cache('LABEVENTS')
    writeLines('LABEVENTS.RData cached successfully')
    # There are labevents listed for all 3125 HADM_IDs but we don't know how complete they are
  }
  
  # CHARTEVENTS
  if(file.exists('cache/GCS.RData') == TRUE)
  {
    load('cache/GCS.RData')
    load('cache/Temp.RData')
    load('cache/MAP.RData')
    load('cache/RR.RData')
    load('cache/HR.RData')
    load('cache/FiO2.RData')
    writeLines('GCS.RData exists')
  }
  else
  {
    if(file.exists('preprocess/OASIS_CHARTEVENTS_HADM_from_file.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('\nOASIS_CHARTEVENTS_HADM_from_file does not exist in /preprocess. Calling bash script (~35 minutes)...')
      
      # uses generated_INOUT_awk_str.txt from above
      # uses preprocess_GCS_awk_str.txt
      
      script_name = 'preprocess_CHARTEVENTS_HADM_from_file.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    ICUIN = ICUSTAY[,c('HADM_ID','INTIME')]
    ICUIN$INTIME = strptime(ICUIN$INTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_GCS_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    # the same resulting measurement on a different day will be removed.
    # Start at 352,000 rows --> ~105,000 --> 24,756 much, much smaller!
    # AND unique(CHARTEVENTS$HADM_ID) gives 3503 HADM_IDs -- good numbers!
    # Call the exraction function for GCS
    GCS = c_CHARTEVENTS_OASIS_extraction(CHARTEVENTS, 'GCS', ICUIN)
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    cache('GCS')
    writeLines('GCS.RData cached successfully')
    
    # Temperature
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_T_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM > 10,] # Removing nonsense cases  from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM < 120,]
    # the same resulting measurement on a different day will be removed.
    # Start at 545,000 rows --> ~301,000 --> 146,600 much, much smaller!
    # AND unique(CHARTEVENTS$HADM_ID) gives 3503 HADM_IDs -- good numbers!
    # Calling the extraction function for Temperature
    Temp = c_CHARTEVENTS_OASIS_extraction(CHARTEVENTS, 'Temp', ICUIN)
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    cache('Temp')
    writeLines('Temp.RData cached successfully')
    
    
#     # MAP
#     CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_MAP_HADM_from_file.csv", comment.char="#")
#     CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
#     CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
#     cols = ncol(CHARTEVENTS_pre)
#     CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
#     CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
#     CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
#     # Very, very few legitimate values of MAP - fixed by adding results of arterial BP mean 52 and 220052
#     # We do have negative and zero values but they seem to merge into the data smoothly so we will ignore for now
#     # Extracting the MAP results
#     MAP = c_CHARTEVENTS_APACHE_extraction(CHARTEVENTS, 'MAP')
#     remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
#     cache('MAP')
#     writeLines('MAP.RData cached successfully')    
    
    # systolic BP
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_sysBP_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM > 0,] # Removing nonsense cases  from the VALUENUM column
    sysBP_temp = CHARTEVENTS
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    # diastolic BP
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_diaBP_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM > 0,] # Removing nonsense cases  from the VALUENUM column
    diaBP_temp = CHARTEVENTS
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    
    # Extracting the MAP results
    MAP = c_CHARTEVENTS_OASIS_extraction(list(sysBP_temp, diaBP_temp), 'MAP', ICUIN)
    cache('MAP')
    remove(sysBP_temp, diaBP_temp)
    writeLines('MAP.RData cached successfully')
    
    
    # HR
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_HR_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    CHARTEVENTS$VALUENUM = abs(CHARTEVENTS$VALUENUM) # Removing nonsense cases  from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM > 0,] # Removing nonsense cases  from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM < 600,] # Removing nonsense cases  from the VALUENUM column
    # Extracting the HR results
    HR = c_CHARTEVENTS_OASIS_extraction(CHARTEVENTS, 'HR', ICUIN)
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    cache('HR')
    writeLines('HR.RData cached successfully')
    
    
    # RR
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_RR_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM > 0,] # Removing nonsense cases  from the VALUENUM column
    # Extracting the HR results
    RR = c_CHARTEVENTS_OASIS_extraction(CHARTEVENTS, 'RR', ICUIN)
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    cache('RR')
    writeLines('RR.RData cached successfully')
    
    
    # FiO2 to inspect if patients are ventilated
    CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_FiO2_HADM_from_file.csv", comment.char="#")
    CHARTEVENTS_pre$CHARTTIME = strptime(CHARTEVENTS_pre$CHARTTIME, format = "%Y-%m-%d %H")
    CHARTEVENTS_pre = sort_df(CHARTEVENTS_pre,vars = 'CHARTTIME')
    cols = ncol(CHARTEVENTS_pre)
    CHARTEVENTS = CHARTEVENTS_pre[!duplicated(CHARTEVENTS_pre[2:cols]),] #Removing duplicates including date
    CHARTEVENTS = CHARTEVENTS[!duplicated(CHARTEVENTS[c(3:5,7:cols)]),] #Removing duplicates not including date
    CHARTEVENTS = CHARTEVENTS[!is.na(CHARTEVENTS$VALUENUM),] # Removing NAs from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM > 0,] # Removing nonsense cases  from the VALUENUM column
    CHARTEVENTS = CHARTEVENTS[CHARTEVENTS$VALUENUM <= 100,] # Removing nonsense cases  from the VALUENUM column
    for(i in 1:nrow(CHARTEVENTS))
    {
      if(CHARTEVENTS$VALUENUM[i] <= 1.0)
      {
        CHARTEVENTS$VALUENUM[i] = CHARTEVENTS$VALUENUM[i]*100
      }
    }
    # CHARTEVENTS$VALUENUM = apply(CHARTEVENTS,1,function(x) if(x[c('VALUENUM')] <= 1){x[c('VALUENUM')] = x[c('VALUENUM')]*100})
    # Extracting the HR results
    FiO2 = c_CHARTEVENTS_OASIS_extraction(CHARTEVENTS, 'FiO2', ICUIN)
    remove(cols, CHARTEVENTS_pre, CHARTEVENTS)
    cache('FiO2')
    writeLines('FiO2.RData cached successfully')
    
    
    # PaO2
    # CHARTEVENTS_pre <- read.csv("preprocess/CHARTEVENTS_PaO2_HADM_from_file.csv", comment.char="#")
  }
  
  # Discharge Notes
  if(file.exists('cache/NOTE_HADM_from_file.RData') == TRUE)
  {
    load('cache/NOTE_HADM_from_file.RData')
    writeLines('NOTE_HADM_from_file.RData exists')
  } else
  {
    if(file.exists('preprocess/NOTE_HADM_from_file.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('\nNOTE_HADM_from_file does not exist in /preprocess. Calling bash script...\n')
      
      # create a .txt file of the HADM_IDs separated by | to load into the bash script
      grep_str = paste(HIDs, collapse = '|')
      fileConn<-file("preprocess/generated_NOTE_csvgrep_str.txt")
      writeLines(paste('(',grep_str,')', sep = ''), fileConn)
      close(fileConn)
      
      script_name = 'preprocess_discharge_summaries.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
  
  NOTE_HADM_from_file = read.csv("preprocess/NOTE_HADM_from_file.csv", comment.char="#")
  q = sapply('ADDENDUM:', grepl, NOTE_HADM_from_file$TEXT)
  NOTE_HADM_from_file = NOTE_HADM_from_file[!q,]
  cache('NOTE_HADM_from_file')
  writeLines('NOTE_HADM_from_file.RData cached successfully')
  }
  
  
  # ICD9 lists for each HADM_ID
  if(file.exists('cache/ICD9_comorbidity_scores.RData') == TRUE)
  {
    load('cache/ICD9_HADM_from_file.RData')
    load('cache/ICD9_comorbidity_scores.RData')
    writeLines('ICD9_comorbidity_scores.RData exists')
  } else
  {
    if(file.exists('preprocess/ICD9_HADM_from_file.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('\nICD9_HADM_from_file does not exist in /preprocess. Calling bash script...\n')
      
      # uses generated_INOUT_awk_str.txt from above
      # uses preprocess_APACHE_component_awk_str.txt
      
      script_name = 'preprocess_ICD9_HADM_from_file.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    
    ICD9_HADM_from_file = read.csv("preprocess/ICD9_HADM_from_file.csv", comment.char="#")
    cache('ICD9_HADM_from_file')
    ICD9_comorbidity_scores = icd9VanWalraven(ICD9_HADM_from_file[,c('HADM_ID','ICD9_CODE')], return.df = TRUE)
    ICD9_comorbidity_scores$Charlson = icd9Charlson(ICD9_HADM_from_file[,c('HADM_ID','ICD9_CODE')], return.df = TRUE)[,2]
    cache('ICD9_comorbidity_scores')
    writeLines('ICD9_comorbidity_scores.RData cached successfully')
    
#     procedures_HADM_from_file = read.csv("preprocess/procedures_HADM_from_file.csv", comment.char="#")
#     cache('procedures_HADM_from_file')
#     writeLines('procedures.RData cached successfully')
  }
  
  
  # DRG_codes for each HADM_ID
  if(file.exists('cache/DRG_HADM_from_file.RData') == TRUE)
  {
    load('cache/DRG_HADM_from_file.RData')
    writeLines('DRG_HADM_from_file.RData exists')
  } else
  {
    if(file.exists('preprocess/DRG_HADM_from_file.csv') == FALSE)
    {
      # If either file does not exist --> proceed.
      writeLines('\nDRG_HADM_from_file does not exist in /preprocess. Calling bash script...\n')
      
      # uses generated_INOUT_awk_str.txt from above
      # uses preprocess_APACHE_component_awk_str.txt
      
      script_name = 'preprocess_DRG_HADM_from_file.sh'
      system(paste("cd ",getwd(), "/preprocess", " && chmod +x ", script_name, sep=""))
      # Ensure we have admin rights to run bash script
      system(paste("cd ",getwd(), "/preprocess", " && ./", script_name, sep=""))
    }
    
    DRG_HADM_from_file = read.csv("preprocess/DRG_HADM_from_file.csv", comment.char="#")
    cache('DRG_HADM_from_file')
    writeLines('DRG_HADM_from_file.RData cached successfully')
  }
  
  
  if(file.exists('cache/str_table.RData') & file.exists('cache/in_str_table.RData'))
  {
    load('cache/str_table.RData')
    load('cache/in_str_table.RData')
    load('cache/CHF_HIDs.RData')
    writeLines('(in_)str_table.RData exists')
  } else
  {
    writeLines('\n(in_)str_table does not exist in cache - processing files... \n')
    str_table = b_PMH_regex(NOTE_HADM_from_file)
    
    # exclusion based on presence of exclusion terms within the PMH string
    ex_list = c('CHF', ' HF ', '\nHF\n', ' HF\n', '\nHF ', 'heart.{1,3}failure', 'HFREF', 'HFPEF', 
                'cardiomyopathy', 'depressed ejection fraction','depressed EF','depressed LVEF',
                'global hypokinesis','ventricular hypokinesis', 'ventricular dysfunction')
    in_matrix = sapply(ex_list, grepl, str_table[,2], ignore.case=TRUE)
    ex_flag = apply(in_matrix, 1, function(x) any(x))
    in_str_table = data.frame(str_table[!ex_flag,1:3], row.names = NULL)
    
    # EXCLUDE PATIENTS WITH EF LESS THAN 55% in the past
    # rather than write another function, utilise EF_regex which will extract 'EF.{0,12}%'
    # if the value is finite, check the first (EFmin) number and remove if < 55.
    temp = apply(in_str_table,1, function(x) EF_regex(x[2],FALSE))
    q = 0
    flag = {}
    for(i in 1:(length(temp)))
    {
      if(all(is.finite(unlist(temp[[i]]))) == TRUE)
      {
        if(unlist(temp[[i]])[1] < 55)
        {
          q = q + 1
          flag[q] = i
        }
      }
    }

    cache('str_table')
    cache('in_str_table')
    CHF_HIDs = which(is.na(match(str_table$temp_HID,in_str_table$temp_HID)))
    cache('CHF_HIDs')
    writeLines('(in_)str_table.RData cached successfully')
    
  }
  
  in_HIDs = as.integer(as.vector((in_str_table$temp_HID))) # Just the non-CHF patients
  HIDs = as.integer(as.vector(str_table$temp_HID)) # Include CHF patients for comparision
  ex_HIDs = setdiff(HIDs, in_HIDs)
  SIDs = ICUSTAY$SUBJECT_ID[match(HIDs,ICUSTAY$HADM_ID)]
  
  # ICUSTAY forms the basis of the combined table since it was the last filtered step and 
  # includes SUBJJECT, HADM and ICUSTAY_IDs

  ## Combined table
  if(file.exists('cache/noCHF_combined_table.RData') == TRUE)
  {
    load('cache/noCHF_combined_table.RData')
    load('cache/CHF_combined_table.RData')
    load('cache/exc_on_ward.RData')
    load('cache/no_INs.RData')
    load('cache/volume_table.RData')
    writeLines('combined_table.RData exists')
  } else
  {
    writeLines('\n noCHFcombined_table does not exist in cache - processing files... \n')
    # Munging data to ensure completeness and caching resulting single table
    # HIDs and SIDs from above
    matches = match(HIDs, ICUSTAY$HADM_ID)
    combined_table = ICUSTAY[matches,c('SUBJECT_ID','HADM_ID','ICUSTAY_ID','LOS')]
    # matching HADMs over to gender column in PATIENTS
    matches = match(SIDs, PATIENTS$SUBJECT_ID)
    combined_table$gender = PATIENTS$GENDER[matches]
    # matching HADM or SUBJECT_IDs to DOB and date of admission
    matches = match(HIDs, ADMISS$HADM_ID)
    temp = ADMISS$ADMITTIME[matches]
    matches = match(SIDs, PATIENTS$SUBJECT_ID)
    combined_table$age = difftime(temp,PATIENTS$DOB[matches], units = 'days')/365.25
    # matching HADMs over to ECHO for EFmin and max
    matches = match(HIDs,ECHO$HADM_ID) # ECHO$HADM_ID is unique - no need to check for multiples
    combined_table$ECHODATE = ECHO$CHARTDATE[matches]
    combined_table$EFmin = ECHO$EFmin[matches]
    combined_table$EFmax = ECHO$EFmax[matches]
    remove(matches)
    # Binning the EFmin and EFmax using the clinically defined thresholds
    combined_table$EFbin = apply(combined_table, 1, function(x) EF_bin_3210(x[8], x[9], FALSE))
    
    combined_table$CHF_flag = sapply(HIDs,function(x) !is.na(match(x,ex_HIDs)))
    
    # Initialising
    yes = FALSE
    exc_on_ward = {}
    remove_flag = {}
    q = 0
    mort_hosp = {}
    mort_30 = {}
    mort_60 = {}
    volume_3day = {}
    out_3day = {}
    urine_1day = {}
    LoHS = {}
    DOD_1 = {}
    DOD_2 = {}
    DOD_mismatch = 0
    no_DOD = 0
    no_INs = 0
    hosp_b4_ICU = 0
    out_of_hosp = 0
    live_1 = {}
    live_2 = {}
    
    Tadmit = {}
    Tdisch = {}
    Tin = {}
    Tout = {}
    Tinstart = {}
    Tinend = {}
    Tinmax = {}
    TDOD = {}
    Techo = {}
    
    volume_table = data.frame(t(c(0,0)))
    names(volume_table) = c('ITEMID', 'VOLUME')
    
    for(i in 1:length(HIDs))
    # for(i in 1:50)
    {
      temp_log = {}
      temp_HID = HIDs[i]
      # temp_SID = ICUSTAY$SUBJECT_ID[match(temp_HID,ICUSTAY)]
      temp_SID = SIDs[i]
      temp_INS =  INS_by_HADM_from_file[which(is.finite(match(INS_by_HADM_from_file$HADM_ID, temp_HID))),]
      temp_OUTS =  OUTS_by_HADM_from_file[which(is.finite(match(OUTS_by_HADM_from_file$HADM_ID, temp_HID))),]
      temp_urine = urine_OUTS_by_HADM_from_file[which(is.finite(match(urine_OUTS_by_HADM_from_file$HADM_ID, temp_HID))),]
      temp_urine$CHARTTIME = strptime(temp_urine$CHARTTIME, format = "%Y-%m-%d %H:%M:%S")
      
      temp_time_IN = strptime(ICUSTAY$INTIME[match(temp_HID,ICUSTAY$HADM_ID)],format = "%Y-%m-%d %H:%M:%S")
      temp_time_OUT = strptime(ICUSTAY$OUTTIME[match(temp_HID,ICUSTAY$HADM_ID)],format = "%Y-%m-%d %H:%M:%S")
      temp_time_ADMIT = strptime(ADMISS$ADMITTIME[match(temp_HID,ADMISS$HADM_ID)],format = "%Y-%m-%d %H:%M:%S")
      temp_time_DISCH = strptime(ADMISS$DISCHTIME[match(temp_HID,ADMISS$HADM_ID)],format = "%Y-%m-%d %H:%M:%S")
      # require at least one row of INS and ensuring that the patient has not been in the hospital
      # for greater than 1 day before ICU admission. If yes --> flag and remove.
      if(nrow(temp_INS) !=0 & as.numeric(difftime(temp_time_IN, temp_time_ADMIT, units = 'days')) < 1)
      {
        if(yes){cat("The HADM_ID was found - perfect!")}
        remove_flag[i] = 0
        q = q+1
        ##
        temp_time_IOstart = min(strptime(temp_INS$CHARTTIME,format = "%Y-%m-%d %H:%M:%S"), na.rm = TRUE)
        temp_time_IOend = max(strptime(temp_INS$CHARTTIME,format = "%Y-%m-%d %H:%M:%S"), na.rm = TRUE)
        temp_time_diff = as.numeric(difftime(temp_time_IOend, temp_time_IOstart, units = 'days'))
        if(temp_time_diff > 3)
        {
          temp_time_IOmax = strptime(temp_time_IOstart + 3*60*60*24,format = "%Y-%m-%d %H:%M:%S")
#           for(j in 1:nrow(temp_INS))
#           {
#             temp_log[j] = strptime(temp_INS$CHARTTIME[j],format = "%Y-%m-%d %H:%M:%S") < temp_time_IOmax
#             
#           }
          
          # temp_INS = temp_INS[which(temp_log),]
          temp_INS = temp_INS[which(temp_INS$CHARTTIME <= temp_time_IOmax),]
          temp_OUTS = temp_OUTS[which(temp_OUTS$CHARTTIME <= temp_time_IOmax),]
          remove(temp_log)
        } else if(temp_time_diff < 1){temp_time_diff = 1}
        temp_Vol = round(sum(temp_INS$AMOUNT, na.rm = TRUE))
        temp_Vol_output = round(sum(temp_OUTS$VALUE, na.rm = TRUE))
        volume_3day[q] = temp_Vol
        out_3day[q] = temp_Vol_output
        temp_urine = temp_urine[temp_urine$CHARTTIME < temp_time_IN + 60*60*24,]
        urine_1day[q] = sum(temp_urine$VALUE,na.rm = TRUE)
        #########################################
        ## To inspect each separate IN ITEMID
#         temp_items = unique(temp_INS$ITEMID)
#         for(n in 1:length(temp_items))
#         {
#           temp_sum = sum(temp_INS$AMOUNT[match(temp_INS$ITEMID,temp_items[n],nomatch = 0)])
#           # Check if the ITEMIS already exists in the table
#           if(is.na(match(temp_items[n],volume_table$ITEMID)) == TRUE)
#           {
#             # Add the ITEMID to the table
#             volume_table = rbind(volume_table,c(temp_items[n],0))
#           }
#           volume_table$VOLUME[match(temp_items[n],volume_table$ITEMID)] = volume_table$VOLUME[match(temp_items[n],volume_table$ITEMID)] + temp_sum
#         }
#         remove(temp_items, temp_sum, n)
        #########################################
        
        LoHS[q] = as.numeric(difftime(temp_time_DISCH, temp_time_ADMIT, units = 'days'))
        temp_time_DOD = strptime(ADMISS$DEATHTIME[match(temp_HID,ADMISS$HADM_ID)], format = "%Y-%m-%d %H:%M:%S")
        temp_date_DOD = PATIENTS$DOD[match(temp_SID,PATIENTS$SUBJECT_ID)]
        temp_date_DOD_HOSP = PATIENTS$DOD_HOSP[match(temp_SID,PATIENTS$SUBJECT_ID)]
        mort_hosp[q] = 0
        mort_30[q] = 0
        mort_60[q] = 0
        # Calculate the total volume of ins over three days IF normalizing per day is desired.
#         if(!is.na(temp_time_diff) & temp_time_diff > 1)
#         {
#           volume_3day[q] = temp_Vol/as.numeric(temp_time_diff)
#         }
#         else
#         {
#           volume_3day[q] = temp_Vol
#         }
        
        # check of temp_time_DOD and temp_date DOD existing and matching from different tables
        temp_bool = FALSE
        if(nchar(as.character(temp_time_DOD)) > 5 | nchar(as.character(temp_date_DOD)) > 5)
        # just need one DOD
        {
          if(nchar(as.character(temp_time_DOD)) > 5 & nchar(as.character(temp_date_DOD)) > 5)
          {
            
            # If they both exist, they have to match!
            if (as.Date(temp_time_DOD) <= as.Date(temp_date_DOD)+1 & as.Date(temp_time_DOD) >= as.Date(temp_date_DOD)-1)
            {
              temp_DOD = as.Date(temp_date_DOD)
            }
            else
            {
              q = q-1
              if(yes){cat("The time and date of death do not match in ADMISS and PATIENT tables. HADM_ID = ", temp_HID, '\n')}
              DOD_mismatch = DOD_mismatch + 1
              DOD_1[DOD_mismatch] = as.Date(temp_time_DOD)
              DOD_2[DOD_mismatch] = as.Date(temp_date_DOD)
              remove_flag[i] = 1
              next
            }
          } else if(nchar(as.character(temp_date_DOD)) > 5)
          {
            temp_DOD = as.Date(temp_date_DOD)
          }
          else if(nchar(as.character(temp_time_DOD)) > 5)
          {
            temp_DOD = as.Date(temp_time_DOD)
          }
          else
          {
            q = q-1
            no_DOD = no_DOD + 1
            remove_flag[i] = 1
            next
          }
        }
        Tadmit[q] = as.character(temp_time_ADMIT)
        Tdisch[q] = difftime(temp_time_DISCH,temp_time_ADMIT, units = 'days')
        Tin[q] = difftime(temp_time_IN,temp_time_ADMIT, units = 'days')
        Tout[q] = difftime(temp_time_OUT,temp_time_ADMIT, units = 'days')
        Tinstart[q] = difftime(temp_time_IOstart,temp_time_ADMIT, units = 'days')
        Tinend[q] = difftime(temp_time_IOend,temp_time_ADMIT, units = 'days')
        if(exists('temp_time_IOmax')){
          Tinmax[q] = difftime(temp_time_IOmax,temp_time_ADMIT, units = 'days')}
        else {Tinmax[q] = Tinend[q]}
        if(exists('temp_DOD')){
          TDOD[q] = difftime(temp_DOD,temp_time_ADMIT, units = 'days')
          if((TDOD[q] <= Tdisch[q]+1 & temp_DOD >= Tdisch[q]-1) | TDOD[q] <= Tdisch[q]) # within 1 day of discharge
          {
            # the DOD of death has to exist for any mortality to be == 1 otherwise 
            # the initialised values of 0,0,0 is correct
            mort_hosp[q] = 1
          }
          if(mort_hosp[q] == 0)
          {
            if(TDOD[q]-Tdisch[q] < 30)
            {
              mort_30[q] = 1
            }
            if(TDOD[q]-Tdisch[q] < 60){
              mort_60[q] = 1
            }
          }
        }
        else {
          TDOD[q] = NA
          # Did not die, revert back to initialised 0,0,0
          }
        # Techo done below
      }
      else
      {
        
        if(nrow(temp_INS) == 0)
        {
          if(yes){cat("The HADM_ID was not found in the INs table\n")}
          no_INs = no_INs + 1
        }
        if(as.numeric(difftime(temp_time_IN, temp_time_ADMIT, units = 'days')) > 1)
        {
          if(yes){cat("The patient was in a ward for > 1 day before ICU admission\n")}
          hosp_b4_ICU = hosp_b4_ICU + 1
          exc_on_ward[hosp_b4_ICU] = as.numeric(difftime(temp_time_IN, temp_time_ADMIT, units = 'days'))
        }
        remove_flag[i] = 1
        next
      }

      remove(temp_time_DISCH, temp_time_ADMIT, temp_time_IN, temp_time_diff, temp_time_IOend, temp_DOD,
             temp_time_IOstart, temp_time_IOmax, temp_time_DOD, temp_date_DOD, temp_date_DOD_HOSP,
             temp_Vol, temp_INS, temp_bool, temp_HID, temp_SID, temp_urine)
    }
    if(DOD_mismatch != 0)
    {
      DOD_compare = cbind(DOD_1,DOD_2)
      cache('DOD_compare')
    }
    remove(DOD_1, DOD_2)
    if(out_of_hosp != 0)
    {
      live_compare = cbind(live_1,live_2)
      cache('live_compare')
    }
    remove(live_1, live_2)
    
    
    # Creating combined_table
    # Now I want to add EF as severe (3), moderate (2), mild (1) or normal (0)
    if(q != nrow(combined_table))
    {
      combined_table = combined_table[remove_flag == 0,]
    }
    
    combined_table$mort_hosp = mort_hosp
    combined_table$mort_30 = mort_30
    combined_table$mort_60 = mort_60
    combined_table$mort_bin = apply(combined_table,1, d_mort_bin_3210)
    combined_table$Vol_in_3day = volume_3day
    combined_table$Vol_out_3day = out_3day
    combined_table$urine_1day = urine_1day
    combined_table$LoHS = LoHS
    combined_table$Tadmit = Tadmit
    combined_table$Tdisch = Tdisch
    combined_table$Tin = Tin
    combined_table$Tout = Tout
    combined_table$Tinstart = Tinstart
    combined_table$Tinmax = Tinmax
    combined_table$Tinend = Tinend
    combined_table$TDOD = TDOD
    
    combined_table$Techo = apply(combined_table, 1, function(x) difftime(as.POSIXlt(x[c('ECHODATE')]), x[c('Tadmit')], units = 'days'))
    combined_table$Techo[combined_table$Techo < 0.5] = 0.5
    
    combined_table$VanWalraven = apply(combined_table, 1, function(x) ICD9_comorbidity_scores$vanWalraven[match(x[c('HADM_ID')],ICD9_comorbidity_scores$HADM_ID)])
    combined_table$Charlson = apply(combined_table, 1, function(x) ICD9_comorbidity_scores$Charlson[match(x[c('HADM_ID')],ICD9_comorbidity_scores$HADM_ID)])
    
    # elective surgery table
    elective_table = d_OASIS_surgery(NOTE_HADM_from_file)
    cache('elective_table')
    
    # OASIS table combination
    OASIS_table = data.frame(HADM_ID = combined_table$HADM_ID)
    OASIS_table$Tin = combined_table$Tin
    OASIS_table$age = combined_table$age
    OASIS_table = cbind(OASIS_table,GCS[match(OASIS_table$HADM_ID,GCS[,1]),2:5])
    OASIS_table = cbind(OASIS_table,HR[match(OASIS_table$HADM_ID,HR[,1]),2:4])
    OASIS_table = cbind(OASIS_table,MAP[match(OASIS_table$HADM_ID,MAP[,1]),2:4])
    OASIS_table = cbind(OASIS_table,RR[match(OASIS_table$HADM_ID,RR[,1]),2:4])
    OASIS_table = cbind(OASIS_table,Temp[match(OASIS_table$HADM_ID,Temp[,1]),2:4])
    OASIS_table$urine = combined_table$urine_1day
    OASIS_table = cbind(OASIS_table,FiO2[match(OASIS_table$HADM_ID,FiO2[,1]),2:4])
    OASIS_table$elective_flag = ifelse(!is.na(match(OASIS_table$HADM_ID,elective_table[,2])),1,0)
    # colnames(OASIS_table)[4] = 'GCS'
    # missmap(OASIS_table)
    OASIS_table$urine = ifelse(OASIS_table$urine < 50, NA, OASIS_table$urine)
    # missmap(OASIS_table)
    temp = kNN(OASIS_table[,2:ncol(OASIS_table)], variable = c('GCS','urine','Tmin','Tmedian','Tmax'), useImputedDist = FALSE)
    OASIS_table = cbind(HADM_ID = OASIS_table[,1],temp[,1:(ncol(temp)-5)])
    cache('OASIS_table')
    writeLines('OASIS_table.RData cached successfully')
    # missmap(OASIS_table)
    remove(temp)
    
    # OASIS scores combination
    OASIS_scores = d_OASIS_values_to_scores(OASIS_table)
    cache('OASIS_scores')
    writeLines('OASIS_scores.RData cached successfully')
    # missmap(OASIS_scores)
    combined_table$OASIS = OASIS_scores$sum
    
    noCHF_combined_table = subset(combined_table, CHF_flag == FALSE)
    CHF_combined_table = subset(combined_table, CHF_flag == TRUE)
    # cache('combined_table')
    cache('CHF_combined_table')
    cache('noCHF_combined_table')
    cache('exc_on_ward')
    cache('no_INs')
    writeLines('combined_table.RData cached successfully')
    remove(INS_by_HADM_from_file, i, j, q, remove_flag, mort_60, mort_30, mort_hosp, LoHS, urine_OUTS_by_HADM_from_file, OUTS_by_HADM_from_file)
    
    
    ## volume table stuff - no longer needed.
#     item_str = {}
#     item_category = {}
#     for(i in 1:nrow(volume_table))
#     {
#       temp_ID = as.character(volume_table$ITEMID[i])
#       if(nchar(as.character(temp_ID)) == 1)
#       {temp_ID = paste('3000',temp_ID,sep = '')}
#       else if(nchar(as.character(temp_ID)) == 2)
#       {temp_ID = paste('300',temp_ID,sep = '')}
#       else if(nchar(as.character(temp_ID)) == 3)
#       {temp_ID = paste('30',temp_ID,sep = '')}
#       if(yes){print(temp_ID)}
#       item_str[i] = as.character(In_ITEMS$LABEL[match(temp_ID,In_ITEMS$ITEMID)])
#       item_category[i] = as.character(In_ITEMS$CATEGORY[match(temp_ID, In_ITEMS$ITEMID)]) # just match the first
#       # item_abbrev = as.character(temp_INS$ORIGINALROUTE[match(temp_ID, temp_INS$ITEMID)])
#     }
#     volume_table$LABEL = item_str
#     volume_table$CATEGORY = item_category
#     volume_table = volume_table[volume_table$ITEMID != 0,]
#     volume_table$VOLUME = 0-volume_table$VOLUME
#     volume_table = sort_df(data = volume_table, vars = 'VOLUME')
#     volume_table$VOLUME = 0-round(volume_table$VOLUME)/1000
#     cache('volume_table')
#     remove(temp_ID, item_str, item_category)
  }
  

  writeLines("\nSuccessfully completed caching of all necessary files. \n")
  # Function END 
}

tiff("fluids_in_histogram_1000mL_breaks.tiff", width = 480, height = 400)
par(mar = c(4,4,0,4))
hist(noCHF_combined_table$Vol_in_3day, breaks = seq(0,(max(noCHF_combined_table$Vol_in_3day)+1000),1000), main = "", xlab = "Fluids IN over 72 hours (mL)")
dev.off()
