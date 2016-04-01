# Vincent Major
# November 13th 2015
# Script to perform system commands to preprocess the DIAGNOSIS_DATA_TABLE.csv by 
# one ICD-9 codes


## 
# Need to rewrite this as a function that takes one list, the path to the raw data, the path to the processed directory!
##

library("stringr")
# library("plyr")
library("reshape")

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
  
  ## If using library(ProjectTemplate) you can cache the table for quick loading
  # cache('ICD9_subgroup')
  writeLines('ICD9_subgroup.RData cached successfully')

  #cleaning up variables
  remove(group1, group2, HIDs, group, matches)
 }