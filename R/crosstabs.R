##### Public functions #####

#' Run a crosstabular frequency for a single variable and a single group.
#'
#' @param dataset A dataframe.
#' @param variable The unquoted name of a variable in the dataframe.
#' @param group The unquoted name of a grouping variable in the dataframe.
#' @param nas Boolean, whether or not to include NAs in the tabulation.
#' @param wt The unquoted name of a weighting variable in the dataframe.
#' @param prompt Boolean, whether or not to include the prompt in the dataframe.
#' @param digits Integer, number of significant digits for rounding. Default is 2,
#' which results in an integer percentage.
#' @return A dataframe with the group name, group prompt, group value, group
#' label, variable name, prompt, values, labels, counts, and percents.
#' @export

cross <- function(dataset,
                  variable,
                  group,
                  nas = T,
                  wt = NULL,
                  prompt = F,
                  digits = 2) {
  variable <- dplyr::enquo(variable)
  group <- dplyr::enquo(group)
  weight <- dplyr::enquo(wt)
  cross_ns(dataset, variable, group, weight, prompt) %>%
    cross_percents(nas, digits = digits) %>%
    dplyr::arrange(group_value, value)
}

cross_ns <- function(dataset, variable, group, weight, prompt) {
  counts <- base_cross_ns(dataset, variable, group, weight)
  counts <- label_cross_variable(dataset, variable, counts, prompt)
  counts <- label_cross_group(dataset, group, counts, prompt)
  # Reorder because Scotty is OCD
  if (prompt) {
    counts %>%
      dplyr::select(
        group,
        group_prompt,
        group_value,
        group_label,
        variable,
        prompt,
        value,
        label,
        n
      )
  } else {
    counts %>%
      dplyr::select(
        group,
        group_value,
        group_label,
        variable,
        value,
        label,
        n
      )
  }
}

label_cross_variable <- function(dataset, variable, counts, prompt) {
  if (class(dataset %>% dplyr::pull(!!variable)) == 'labelled') {
    if (prompt) {
      counts <- counts %>%
        dplyr::mutate(
          prompt = labelled::var_label(value)
        )
    }
    counts <- counts %>%
      dplyr::mutate(
        label = labelled::to_factor(value) %>% as.character,
        value = value %>% as.character
      )
    return(counts)
  } else {
    # Otherwise, use some sensible defaults
    if (prompt) {
      counts <- counts %>%
        dplyr::mutate(
          prompt = ''
        )
    }
    counts <- counts %>%
      dplyr::mutate(
        label = value %>% as.character,
        value = value %>% as.character
      )
    return(counts)
  }
}

label_cross_group <- function(dataset, group, counts, prompt) {
  if (class(dataset %>% dplyr::pull(!!group)) == 'labelled') {
    if (prompt) {
      counts <- counts %>%
        dplyr::mutate(
          group_prompt = labelled::var_label(group_value)
        )
    }
    counts <- counts %>%
      dplyr::mutate(
        group_label = labelled::to_factor(group_value) %>% as.character,
        group_value = group_value %>% as.character
      )
    return(counts)
  } else {
    # Otherwise, use some sensible defaults
    if (prompt) {
      counts <- counts %>%
        dplyr::mutate(
          group_prompt = ''
        )
    }
    counts <- counts %>%
      dplyr::mutate(
        group_label = group_value %>% as.character,
        group_value = group_value %>% as.character
      )
    return(counts)
  }
}

base_cross_ns <- function(dataset, variable, group, weight) {
  dataset %>%
    # When wt is NULL, it runs unweighted counts
    dplyr::count(!!variable, !!group, wt = !!weight) %>%
    dplyr::rename(
      value = !!variable,
      group_value = !!group
    ) %>%
    dplyr::mutate(
      variable = dplyr::quo_name(variable),
      group = dplyr::quo_name(group)
    )
}

cross_percents <- function(counts, include_nas, digits) {
  # Filter out NAs if requested
  if (! include_nas) {
    counts <- counts %>%
      dplyr::filter(
        !is.na(value)
      )
  }
  # Calculate and round to integer percentages
  counts %>%
    dplyr::group_by(group_value) %>%
    dplyr::mutate(
      percent = (n / sum(n)) %>% round(digits)
    ) %>%
    dplyr::ungroup()
}

