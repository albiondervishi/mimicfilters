# Vincent Major
# October 14 2015
# Function to take in EF min and max and bin them into
# severe    (3)   EF < 30
# moderate  (2)   30 <= EF << 44
# mild      (1)   45 <= EF < 54
# normal    (0)   55 <= EF

EF_bin_3210 = function(EFmin, EFmax, yes)
{
  # Defining the threshold values
  t = c(30, 45, 55)
  if( EFmin < t[1] & EFmax <= t[1]) # cannot be 30,30 but can be 20,30
  {
    bin = 3 # Severe cardiomyopathy
  }
  else if(EFmin >= t[3] & EFmax >= t[3])
  {
    bin = 0 # Normal EF
  }
  else if(EFmin < t[3] & EFmax <= t[3]) # cannot be 55,55 but can be 45,55 
  {
    if(EFmin >= t[2] & EFmax >= t[2]) #greater than or equal to 45
    {
      bin = 1 # Mild cardiomyopathy
    }
    else if(EFmin < t[2] & EFmax <= t[2]) #cannot be 45,45 but can be 30,45
    {
      if(EFmin >= t[1] & EFmax >= t[1])
      {
        bin = 2 # Moderate cardiomyopathy
      }
      else
      {
        if(yes){cat('Failed',EFmin, EFmax,'\n')}
        bin = NA
      }
    }
    else
    {
      if(yes){cat('Failed',EFmin, EFmax,'\n')}
      bin = NA
    }
  }
  else
  {
    if(yes){cat('Failed',EFmin, EFmax,'\n')}
    bin = NA
  }
  return(bin)
}