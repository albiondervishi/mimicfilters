# Vincent Major
# April 10 2016
# A function to take in the raw discharge note in a data.frame subset of the raw NOTE table with CATEGORY == "Discharge". At least the input data.frame must have HADM_ID and TEXT fields. A regex process is used to extract the Past Medical History Section from the raw Discharge note. Unfortunately, instances of 'Prior Medical History' as well as 'HISTORY OF PRESENT ILLNESS' also occur and are included.

# Usage,
#   d_past_medical_history_regex_raw_note = function(NOTE_HADM_from_file)
# Where,
#   NOTE_HADM_from_file is a data.frame subset of the raw NOTE table with every row $CATEGORY == "Discharge".

# Output is a data.frame of HADM_ID and Past Medical History snippets that can subsequently be regexed for strings of interest.


d_past_medical_history_regex_raw_note = function(NOTE_HADM_from_file)
{
  temp_HID = {}
  temp_str = {}
  temp_str_med = {}
  for(i in 1:nrow(NOTE_HADM_from_file))
  {
    temp_HID[i] = NOTE_HADM_from_file$HADM_ID[i]
    temp_text = as.character(NOTE_HADM_from_file$TEXT[i])
    if(length(grep('history', temp_text, ignore.case = TRUE)) != 0)
    {
      # Including Past and Prior Medical History for a few cases of the latter.
      x = gregexpr('\n\nP.{3,4} Medical History:', temp_text, ignore.case = TRUE)
      a = x[[1]][1]
      if(a == -1)
      {
        x = gregexpr('\n\nHISTORY OF PRESENT ILLNESS:', temp_text, ignore.case = TRUE)
        a = x[[1]][1]
      }
      if(a == -1) {next}
      y = gregexpr('(\n\n)(.{1,40}?)(:)', temp_text, ignore.case = TRUE)
      b = y[[1]][match(x[[1]][1],y[[1]])+1]+20
      if(is.na(b) == TRUE)
      {
        b = y[[1]][match(x[[1]][1]-1,y[[1]])+1]+20 # Checking for \n\n\n
        if(is.na(b) == TRUE)
        {
          b = y[[1]][match(x[[1]][1]-2,y[[1]])+1]+20 #Checking for \n\n\n\n
        }
      }
      temp_str[i] = substr(temp_text, a, b)
      remove(x, y, a, b)
    } else
    {temp_str[i] = 0}
    if(length(grep('medication', temp_text, ignore.case = TRUE)) != 0)
    {
      x = gregexpr('\n\nMedications on Admission:', temp_text, ignore.case = TRUE)
      a = x[[1]][1]
      if(a == -1)
      {
        x = gregexpr('\n\nCurrent Medications:', temp_text, ignore.case = TRUE)
        a = x[[1]][1]
      }
      if(a == -1) {next}
      y = gregexpr('(\n\n)(.{1,40})(:)', temp_text, ignore.case = TRUE)
      b = y[[1]][match(x[[1]][1],y[[1]])+1]+20
      if(is.na(b) == TRUE)
      {
        b = y[[1]][match(x[[1]][1]-1,y[[1]])+1]+20 # checking for \n\n\n
        if(is.na(b) == TRUE)
        {
          b = y[[1]][match(x[[1]][1]-2,y[[1]])+1]+20 # checking for \n\n\n\n
        }
      }
      temp_str_med[i] = substr(temp_text, a, b)
      remove(temp_text, x, y, a, b)
    } else
    {temp_str_med[i] = 0}
  }
  
  # removing duplicated HADM_ID with the longer record to preferentially select the more detailed and 
  # leave out the ADDENDUM kind of records. 
  str_table = cbind(temp_HID,temp_str,temp_str_med)
  str_table = str_table[nchar(str_table[,2]) > 5,]
  while(length(which(duplicated(str_table[,1]))) >= 1)
  {
     for(i in which(duplicated(str_table[,1])))
    {
      temp_1 = str_table[match(str_table[i,1],str_table[,1]),]
      temp_2 = str_table[i,]
      if(nchar(temp_1[2]) > nchar(temp_2[2]))
      {t1 = temp_1[2]} else
      {t1 = temp_2[2]}
      if(nchar(temp_1[3]) > nchar(temp_2[3]))
      {t2 = temp_1[3]} else
      {t2 = temp_2[3]}
      temp_3 = cbind(temp_2[1],t1,t2)
      str_table[match(str_table[i,1],str_table[,1]),2] = NA
      str_table[i,2] = NA
      str_table = rbind(str_table,temp_3)
    }
    str_table = str_table[nchar(str_table[,2]) > 5,]
  }
  
  str_table = data.frame(str_table[,1:3], row.names = NULL)
  
  return(str_table)
}
