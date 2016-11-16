# MIMIC-ICU-filter-modules
A repository of reusable code modules for implementation in ICU databases such as MIMIC-III. 

[![Build Status](https://travis-ci.org/vincentmajor/MIMIC-ICU-filter-modules.svg?branch=master)](https://travis-ci.org/vincentmajor/MIMIC-ICU-filter-modules)

## Installation
Packaged as a R `devtools` packages so that installation is as easy as:
```
install.packages("devtools")
devtools::install_github("vincentmajor/MIMIC-ICU-filter-modules")
library(mimicfilters)
```

## Example
```
# With dplyr
data = data.frame(id = c(1, 2, 3, 4, 5, 6),
  ICD9 = c('123.45', '234.56', '345.67', '456.78','567.89', '678.90'))
dplyr::filter(data, filter_icd9_by_codes(ICD9, c('123', '345', '567')))

# Without dplyr
mask = filter_icd9_by_codes(data$ICD9, c('123', '345', '567'))
data.filtered = data[mask,]
```

## Citations
This work was presented at AMIA 2016 in Chicago, IL on November 16th in Session 92: Utilizing Data Science for the ICU in a paper presentation entitled "Reusable filtering functions for application in the ICU: a case study".

## License
CC0
