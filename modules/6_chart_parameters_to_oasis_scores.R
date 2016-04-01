# Vincent Major
# December 3 2015
# function to take in raw OASIS values and translate into scores


d_OASIS_values_to_scores = function(OASIS_table)
{
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