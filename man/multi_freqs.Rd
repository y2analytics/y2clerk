% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_freqs.R
\name{multi_freqs}
\alias{multi_freqs}
\title{Run frequencies for multiple select variables}
\usage{
multi_freqs(
  dataset,
  ...,
  remove_nas = TRUE,
  wt = NULL,
  prompt = FALSE,
  digits = 2,
  nas_group = TRUE,
  factor_group = FALSE,
  unweighted_ns = FALSE,
  show_missing_levels = TRUE
)
}
\arguments{
\item{dataset}{A dataframe.}

\item{...}{The unquoted names of a set of variables in the dataset referring to variable "stems". If nothing
is specified, the function runs a frequency on every column in given dataset.}

\item{remove_nas}{Boolean, after freqs is run (which always includes NAs), whether or not to filter out counts of NA value (default: TRUE).}

\item{wt}{The unquoted name of a weighting variable in the dataset (default: NULL).}

\item{prompt}{Boolean, whether or not to include the prompt in the dataset (default: FALSE).}

\item{digits}{Integer, number of significant digits for rounding (default: 2).}

\item{nas_group}{Boolean, whether or not to include NA values for the grouping variable in the tabulation (default: TRUE).}

\item{factor_group}{Boolean, whether or not to convert the grouping variable to a factor and use its labels instead of its underlying numeric values (default: FALSE)}

\item{unweighted_ns}{Boolean, whether the 'n' column in the freqs table should be UNweighted while results ARE weighted. This argument can only be used if a wt variable is used. If no weight variable is used, the 'n' column will always be unweighted (default: FALSE).}

\item{show_missing_levels}{Boolean, whether to keep response levels with no data (default: TRUE)}
}
\value{
A dataframe with the variable names, prompts, values, labels, counts,
stats, and resulting calculations.
}
\description{
Filters out rows that are completely NULL values (if respondent did not answer question) then runs freqs
}
\examples{

df <- data.frame(
  a = c(1, 2, 3, 1, 2, 3, 1),
  Q1_1 = c(1, NA, 1, 1, NA, 1, NA),
  Q1_2 = c(1, 1, NA, 1, NA, 1, NA),
  Q1_3 = c(NA, 1, 1, NA, 4, 1, NA),
  weights = c(0.9, 0.9, 1.1, 1.1, 1, 1, 1)
) \%>\%
  tidyr::as_tibble()


# All 3 methods below give the same output
multi_freqs(df, Q1_1)
df \%>\% multi_freqs(Q1_1)
df \%>\%
  dplyr::select(dplyr::starts_with("Q1")) \%>\%
  multi_freqs()


# Grouped examples with weights (both have same outputs)
df \%>\%
  dplyr::group_by(a) \%>\%
  multi_freqs(Q1_1, wt = weights)
df \%>\%
  dplyr::group_by(a) \%>\%
  dplyr::select(starts_with("Q1"), weights) \%>\%
  multi_freqs(wt = weights)

}
