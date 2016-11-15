# Vincent Major
# April 10 2016
# A function to take in numeric values of LVEF min and max and bin them into the clinically defined ranges
#   severe (3) EF < 30
#   moderate (2) 30 <= EF << 40
#   mild (1) 40 <= EF <= 50
#   normal (0) 50 < EF

# Usage:
#   b_ef_binning_3210 = function(EFmin, EFmax)

# Where,
#   EFmin is the minimum EF obtained from the b_ef_regex_from_raw_note function
#   EFmax is the maximum EF obtained from the b_ef_regex_from_raw_note function

# Output is the integer 0, 1, 2, or 3 representing normal EF, mild, moderate or severely depressed LVEF/hypokinesis.


b_ef_binning_3210 = function(EFmin, EFmax)
{
  # Defining the threshold values
  t = c(30, 40, 50)
  
  if( EFmin < t[1] & EFmax <= t[1]) # with one < and one <=, cannot be 30,30 but can be 20,30
  {
    bin = 3 # Severe cardiomyopathy
  }
  else if(EFmin >= t[3] & EFmax > t[3]) # with one >= and one >, cannot be 50,50 but can be 50,60
  {
    bin = 0 # Normal EF
  }
  else if(EFmin <= t[3] & EFmax <= t[3]) # with one < and one <=, can be 50,50 
  {
    # Either moderate or mild!
    if(EFmin >= t[2] & EFmax >= t[2]) # with both >=, can be 40,40
    {
      bin = 1 # Mild cardiomyopathy
    }
    else if(EFmin < t[2] & EFmax <= t[2]) #with one < and one <=, can be 30,40 but cannot be 40,40
    {
      if(EFmin >= t[1] & EFmax >= t[1])
      {
        bin = 2 # Moderate cardiomyopathy
      }
      else
      {
        print(paste0("Unable to find a bin for EFmin = ", EFmin, " and EFmax = ", EFmax, "!"))
        bin = NA
      }
    }
    else
    {
      print(paste0("Unable to find a bin for EFmin = ", EFmin, " and EFmax = ", EFmax, "!"))
      bin = NA
    }
  }
  else
  {
    print(paste0("Unable to find a bin for EFmin = ", EFmin, " and EFmax = ", EFmax, "!"))
    bin = NA
  }
  return(bin)
}