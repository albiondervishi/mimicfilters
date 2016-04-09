# Vincent Major
# October 21 2015
# Using two lists of strings to to include and exclude patients by their
# string-based admission diagnosis.

c_string_based_admission = function (ADMISS)
{
  # takes a data.frame ADMISS rather than just the component string.
  # Define list of strings to match on for both inclusion and exclusion
  
  in_list = c('PNEUMONIA','PNA','SEPSIS','FEVER','ALTERED MENTAL STATUS','HYPOTENSION','ACUTE RENAL FAILURE',
              'RESPIRATORY FAILURE','PANCREATITIS','LIVER FAILURE','SHORTNESS OF BREATH','CELLULITIS',
              'DYSPNEA','RESPIRATORY DISTRESS','UROSEPSIS','URINARY TRACT INFECTION','PYELONEPHRITIS',
              'CARDIAC ARREST','HYPOXIA','ENDOCARDITIS','WEAKNESS','RENAL FAILURE','DIABETIC KETOACIDOSIS',
              'CHOLANGITIS','DEHYDRATION','CHOLECYSTITIS','FAILURE TO THRIVE','SEPTIC SHOCK',
              'UNRESPONSIVE','HEPATIC ENCEPHALOPATHY','BACTEREMIA','COLITIS',
              'EPIDURAL ABSCESS','WOUND INFECTION','MENTAL STATUS CHANGES','ARREST','SHOCK',
              'FEBRILE NEUTROPENIA','UTI','CARDIOGENIC SHOCK','HEPATITIS','ADULT RESPIRATORY DISTRESS SYNDROME',
              'ARDS','DIVERTICULITIS','MENINGITIS','ASPIRATION PNEUMONIA','ENCEPHALOPATHY',
              'HEPATIC FAILURE','NEUTROPENIA','R/O SEPSIS','THROMBOCYTOPENIA','DIARRHEA','TACHYCARDIA',
              'HYPERGLYCEMIA','METHICILLIN RESISTANT STAPH AUREUS','CHANGE IN MENTAL STATUS',
              'MENTAL STATUS CHANGE','PANCYTOPENIA','RESP FAILURE','RESPIRATORY ARREST',
              'SPONTANEOUS BACTERIAL PERITONITIS','ARF','INFECTION','INFLUENZA','SEPTIC KNEE','SOB',
              'SEPTIC SHOCK','ELEVATED CREATININE','ELEVATED  LFTS','EMPYEMA','HYPOTENSIVE','INFECTED GRAFT',
              'OSTEOMYELITIS','PEA ARREST','POST ARREST','POST OP INFECTION','FIB ARREST',
              'INFECTION','HYPOGLYCEMIA','HYPOTHERMIA','NAUSEA','VOMITING'
  )
  
  ex_list = c('CHF','CONGESTIVE HEART FAILURE','INTRACRANIAL HEMORRHAGE','SUBARACHNOID HEMORRHAGE',
              'GASTROINTESTINAL BLEED','UPPER GI BLEED','LOWER GI BLEED','GI BLEED','PULMONARY EMBOLIS','PULMONARY EMBOLUS',
              'UPPER GASTROINTESTINAL BLEED','ACUTE SUBDURAL HEMATOMA','SUBDURAL HEMATOMA','BOWEL OBSTRUCTION',
              'SYNCOPE','HIP FRACTURE','STEMI','S/P MOTOR VEHICLE ACCIDENT','ACUTE LEUKEMIA','STROKE','S/P FALL',
              'FALL','TRANSIENT ISCHEMIC ATTACK','TIA','OVERDOSE','HEART FAILURE','HEAD BLEED','BLUNT TRAUMA',
              'STROKE/TIA','VARICEAL BLEED','CEREBROVASCULAR ACCIDENT','CHF EXACERBATION','TRAUMA','TYLENOL OVERDOSE',
              'SUBARACHNOID HEMATOMA','FEMUR FRACTURE','INTRAPARENCHYMAL HEMORRHAGE','SUBDURAL HEMORRHAGE',
              'TRANSIENT ISCHEMIC ATTACK (TIA)','BRIGHT RED BLOOD PER RECTUM',
              'RUPTURED AAA','STROKE-TRANSIENT ISCHEMIC ATTACK','TYLENOL OD'
  )
  
  # Patients that fail to have a diagnosis in either inc_list of ex_list will be MISSED!
  
  HADM = ADMISS$HADM_ID
  diagn = ADMISS$DIAGNOSIS
  
  # Define a bunch of characters to separate on - chosen by manual inspection.
  # R/O etc included to narrow down the string from 'R/O sepsis' to just 'sepsis' which can be captured.
  sep_char = c(';',',','?','S/P','R/O','W/','\\','/','-')
  
  # Splitting the string into its base components by splitting on each of sep_char repetitively.
  for(k in 1:length(sep_char))
    {
      yes = FALSE
      new_HADM = {}
      new_diagn = {}
      for(i in 1:length(HADM))
      {
        temp_list = unlist(strsplit(as.character(diagn[i]), sep_char[k], fixed = TRUE))
        q = length(temp_list)
        if(q == 1)
        {
        new_HADM = rbind(new_HADM, HADM[i])
        new_diagn = rbind(new_diagn, temp_list[1])
        }
        else
        {
          j = 1
          while(j <= q)
          {
            new_HADM = rbind(new_HADM, HADM[i])
            new_diagn = rbind(new_diagn, temp_list[j])
            j = j+1
          }
          
        }
      }
      
      HADM = new_HADM
      diagn = new_diagn
    }
  
  
  admiss_HADM_diag = data.frame(HADM_ID = new_HADM, DIAGNOSIS = new_diagn)
  
  # perfect! We had 7146 patients --> 8837 splitting by ';' --> 9057 splitting by ','. 
  # Still have 27 cases of '/' but in mixed context so will ignore.
  
  admiss_HADM_diag = admiss_HADM_diag[admiss_HADM_diag$DIAGNOSIS != '',]
  admiss_HADM_diag = admiss_HADM_diag[admiss_HADM_diag$DIAGNOSIS != ' ',]
  new_HADM = admiss_HADM_diag$HADM_ID
  new_diagn = admiss_HADM_diag$DIAGNOSIS
  
  # Now I need to test against the inclusion and exclusion lists.
  
  in_matrix = sapply(in_list, grepl, new_diagn, ignore.case=TRUE) #could use fixed = TRUE here but doesn't matter in this instance
  ex_matrix = sapply(ex_list, grepl, new_diagn, ignore.case=TRUE)
  
  a = apply(in_matrix, 1, function(x) any(x))
  b = apply(ex_matrix, 1, function(x) !any(x))
  remove(in_matrix, ex_matrix)
  ab = rbind(a,b) # Included but not excluded --- INCLUDED
  c = apply(ab, 2, function(x) all(x))
  
  
  anotb = rbind(a,!b) # Included but also excluded --- EXCLUDED
  e = apply(anotb, 2, function(x) all(x))
  
  include = data.frame(HADM_ID = new_HADM[c], DIAGNOSIS = new_diagn[c])
  freq_include = count(include$DIAGNOSIS)
  x = unique(include$HADM_ID)
  ADMISS_include = ADMISS[which(is.finite(match(ADMISS$HADM_ID, x))),c('HADM_ID','DIAGNOSIS')]
  
  excluded = data.frame(HADM_ID = new_HADM[e], DIAGNOSIS = new_diagn[e]) #!b to include not in but excluded
  freq_exclude = count(excluded$DIAGNOSIS)
  z = unique(excluded$HADM_ID)
  ADMISS_exclude = ADMISS[which(is.finite(match(ADMISS$HADM_ID, z))),c('HADM_ID','DIAGNOSIS')]
  
  included_HADM_ID = unique(include$HADM_ID)
  excluded_HADM_ID = unique(excluded$HADM_ID)
  print(length(included_HADM_ID))
  if(length(intersect(included_HADM_ID, excluded_HADM_ID)) != 0)
  {
    included_HADM_ID = included_HADM_ID[-match(intersect(included_HADM_ID, excluded_HADM_ID),included_HADM_ID)]
  }
  print(length(included_HADM_ID))
  
  missed_HADM_ID = setdiff(ADMISS$HADM_ID, included_HADM_ID)
  missed_HADM_ID = setdiff(missed_HADM_ID, excluded_HADM_ID)
  
  return(list(included_HADM_ID,missed_HADM_ID,excluded_HADM_ID))
}

