# y2clerk 0.4.2.1
## New functions
* `pivot_freqs` - for pivoting wide a grouped freqs. Makes it easier for quick comparisons

## Bugs
* `cross_freqs` - more succinct way of running function
* all examples now running


# y2clerk 0.4.1.0
## New functions
* `cross_freqs` - for running simplified cross tabs in R

## New files
* Created a NEWS file for those interested in keeping tabs. You're welcome people.
* Created README file for quick overview of the package. 
* Created comments file for CRAN reviewers.

## Bugs
* freqs() and verbatims_y2() should no longer have notes about rlang .data reference problems. 
* fixed bug preventing creation of pdf manual


# y2clerk 0.4.0

## Overview
* The verbatims_y2 function now accepts tidyselect filtering. As in you can do responses %>% verbatims_y2() and it will take all the variables in that data frame just like freqs already did.
* One breaking change is that the first argument of verbatims_y2 is now "dataset" instead of "df". This change was made to match the argument name of freqs as well as avoid using "df", which is a function from stats and could sometimes cause problems.
* One last breaking change is that we removed the freqs_mean function. Probably no one was using this function since it was mostly an old, incomplete, development function anyway. But in case you were using it, just use freqs or freq because they do everything better anyway.
* All inner-workings updated to be in compliance with new version of dplyr v1.0.0
