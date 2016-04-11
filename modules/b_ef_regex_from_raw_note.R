# Testing EF extraction regular expression code
# Vincent Major
# 24 September 2015
#

b_ef_regex_from_raw_note = function (temp_str)
{
  x=list()
  xx=list()
  xxx=list()
  for(i in 1:length(temp_str)) # Only >1 when used for unit testing a list of strings
  {
    q=1
    while(q==1)
    {
     q=0 # only one trip around this while loop unless failure to get result pursueing '%' 
    
     if(grepl('Test:.{0,12}TEE',temp_str[i], ignore.case = TRUE))
     {
       xxx[[i]] = Inf
       next
     }
     
     while(grepl('functino',temp_str[i]))
     {
       temp_str[i] = gsub("functino",'function',temp_str[i])
     }
     
     while(grepl('\n',temp_str[i]))
     {
       temp_str[i] = gsub("\\n",'',temp_str[i])
     }
     
     if(length(grep('%',temp_str[i]))!=0) # contains '%' at least once 
     {
       # start of the regular expression algorithm finding EF ---- % and narrowing it down to one or two digits
       x[i] = str_extract_all(temp_str[i], 'EF.{0,11}%')
       xx[i] = str_extract_all(x[i],'\\d.*?\\d')
       if(length(grep('%',xx[i]))!=0)
       {
         xx[i] = str_extract_all(x[i], '\\d.{0,6}\\d')
       }
       if(lengths(xx[i])==0)
       {
         x[i] = str_extract_all(temp_str[i], 'EF.{0,5}%')
         xx[i] = str_extract_all(x[i],'\\d\\d')
         if(lengths(xx[i])==0)
         {
           xx[i] = str_extract_all(x[i], '\\d')
         }
       }
       xxx[[i]] = (strtoi(xx[[i]])) # into a vector of integers
       if(is.na(xxx[[i]][1]) == TRUE | length(xxx[[i]]) == 0 | xxx[[i]][1] == 0) # If the first value about to be returned is 0 then try something else
       {
         temp_str[i] = gsub("%",'',temp_str[i]) # get rid of the '%' sign
         q=1
         next
       }
     }
    
     else 
     {
       
       severe_list = c('severely.{0,2}depressed','severe.{0,15}LV.{0,2}systolic.{0,2}dysfunction', 'severe.{0,20}left.{0,2}ventric.{0,10}systolic.{0,2}dysfunction', 'LEFT VENTRICLE:.{0,200}severe.{0,2}systolic.{0,2}dysfunction', 'left.{0,2}ventric.{0,40}severe.{0,20}systolic.{0,2}dysfunction')
       moderate_list = c('moderately.{0,2}depressed','moderate.{0,15}LV.{0,2}systolic.{0,2}dysfunction', 'moderate.{0,20}left.{0,2}ventric.{0,10}systolic.{0,2}dysfunction', 'LEFT VENTRICLE:.{0,200}moderate.{0,2}systolic.{0,2}dysfunction', 'left.{0,2}ventric.{0,40}moderate.{0,20}systolic.{0,2}dysfunction')
       mild_list = c('mildly.{0,2}depressed','mild.{0,15}LV.{0,2}systolic.{0,2}dysfunction', 'mild.{0,20}left.{0,2}ventric.{0,10}systolic.{0,2}dysfunction', 'LEFT VENTRICLE:.{0,200}mild.{0,2}systolic.{0,2}dysfunction', 'left.{0,2}ventric.{0,40}mild.{0,20}systolic.{0,2}dysfunction')
       normal_list = c('low.{0,2}normal.{0,2}EF','normal.{0,30}systolic function', 'left.{0,2}ventric.{0,10}systolic.{0,2}function.{0,10}grossly.{0,2}preserved', 'LV.{0,10}systolic.{0,2}function.{0,10}grossly.{0,2}preserved', 'preserved.{0,2}global.{0,10}ventricular.{0,2}systolic.{0,2}function', 'grossly.{0,2}preserved.{0,10}ventric.{0,10}systolic.{0,2}function', 'LV.{0,40}preserved.{0,2}systolic.{0,2}function', 'left.{0,2}ventric.{0,50}systolic.{0,2}function.{0,40}normal', 'normal.{0,40}biventricular.{0,2}systolic.{0,2}function', 'LV.{0,2}function.{0,30}normal', 'left.{0,2}ventric.{0,50}systolic.{0,2}function.{0,40}preserved')
       if(any(sapply(severe_list, grepl, temp_str[i], ignore.case = T)))
       {
         xxx[[i]] = c(5, 29)
         next
       }
       else if(any(sapply(moderate_list, grepl, temp_str[i], ignore.case = T)))
       {
         xxx[[i]] = c(31, 39)
         next
       }
       else if(any(sapply(mild_list, grepl, temp_str[i], ignore.case = T)))
       {
         xxx[[i]] = c(41, 49)
         next
       }
       else if(any(sapply(normal_list, grepl, temp_str[i], ignore.case = T)))
       {
         xxx[[i]] = c(51, 99)
         next
       }
       else
         xxx[[i]] = Inf # nothing - give up
     }
     # Should really do a complicated search for left ventricular systolic funcion is good/normal
    }
  }  
  return (xxx)
}

