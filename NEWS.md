# y2clerk 0.6.10
## Bug fixes
* `freqs` - When show_missing_levels = TRUE, now has freqs table ordered by value, slotting in the missing levels where they *should* have gone if not missing, instead of at the end of the table.


# y2clerk 0.6.9
## New features
* `verbatims_y2` now has an additional column of output: base_ns.


# y2clerk 0.6.7
## New features
* `cross_freqs` - New argument *include_overall* Boolean, whether to include the overall frequency levels for variables (default = FALSE)
## Maintenance 
* Fixed underlying suppressed warnings in freqs() function


# y2clerk 0.6.6
## New features
* `multi_freqs` - New argument *show_missing_levels* Boolean, whether to keep response levels with no data (default: TRUE). This argument functions the same way it does with freqs()
## Bug fixes
* `freqs` did not always capture all missing levels with the argument *show_missing_levels* when the data was grouped. This has been fixed, and the hotfix also applies to the new argument in *multi_freqs*


# y2clerk 0.6.4
## New functions
* `stat_test_y2` - Akin to crosstabs, run significance testing on a grouped frequencies
## Maintenance 
* Updating depreciation of .data$ in tidyselect functions


# y2clerk 0.6.3
## Bug fixes
* `multi_freqs` - Resolved bug for multi-select questions with 10+ options.


# y2clerk 0.6.2
## Bug fixes
* `freqs` - Bug fix for grouped frequencies with prompt = TRUE

# y2clerk 0.6.1
## Bug fixes
* `freqs` - 'prompt = TRUE' now properly working with the new 'show_missing_levels' argument
* `verbatims_y2` - internal update to work more in-line with the y2municipal topline() function


# y2clerk 0.6.0
## Breaking changes
* `freqs` - When freqs was run on a factor variable in the past, the "value" column would pull the character labels even when the factor had underlying numbers. This version of y2clerk updates freqs to be more consistent with other types of haven labelled variables and pulls the underlying numbers of factor variables for use in the value column. 
## New arguments
* `freqs` - show_missing_levels: whether to keep response levels with no data (default: TRUE). For example, set to TRUE if you know no one answered "very dissatisfied" on a scale, but you still want that empty level of the scale present in the output of freqs.
## New function
* `to_haven_y2` - Convert a factor variable or a character variable to a haven labelled variable


# y2clerk 0.5.4
## New functions
* `multi_freqs` Finally the new freqs function everyone has been waiting for. Run a frequencies table on a multiple answer question. Under the hood, the function looks for any respondents who answered *none* of the multiple selects for that series of questions with the same stem and filters them out before running freqs. Essentially, `multi_freqs` automates all that by-hand filtering that had to be done manually on any multiple answer questions. Conveniently allows you to input only one question in a multiple answer block. For example, the user only has to give it m_race_1, instead of m_race_1, m_race_2, m_race_3, etc..
* `append_ns` - Uses the output from the y2clerk freqs() function and adds (N = ...) to the end of each label, based off the column "n". Useful for charts that require ns for all levels


# y2clerk 0.5.2
## Bug fixes
* `pivot_freqs` The last bug fix was a temporary solution for the list columns, but this one should be more legit (using values_fill argument from pivot_wider)


# y2clerk 0.5.1
## Bug fixes
* `pivot_freqs` bug fixed where pivot_freqs was making columns into lists instead of numerics


# y2clerk 0.5.0
## Breaking changes
* `freqs` & `cross_freqs`: argument *pr* changed to *percentile*. Change was done to better align with tidyverse practice that no arguments are the beginnings of another argument (existing argument "prompt" matched to "pr", which we now want to avoid by changing "pr" to "percentile")
## Bug fixes
* `freqs` bug fixed so now grouped freqs with stat = 'mean' runs properly 


# y2clerk 0.4.4
## New function argument
* `pivot_freqs` - *columns_var*: DEFAULT = label; Used to pivot so only "label" could be the new columns in the wide df. Now you can set group_var to be the new columns, instead of rows. 


# y2clerk 0.4.3
## New function capabilities
* `freqs()` has a new argument: `unweighted_ns`. Use this argument in combination with the `wt` argument to have weighted results but unweighted ns. This is particularly useful in toplines.


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
