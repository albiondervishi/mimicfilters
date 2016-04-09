# Vincent Major
# October 14 2015
# Function to take in EF min and max and bin them into
# severe    (3)   EF < 30
# moderate  (2)   30 <= EF << 40
# mild      (1)   40 <= EF <= 50
# normal    (0)   50 < EF

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