#### Final order_label Function ####
### Description of order_label
#' Order your data and add percent labels
#'
#' Takes a dataframe (frequencies) and orders the labels and groups while adding percent labels for use in ggplot.
#' @param dataset The name of the data frame that the mscharts pulls from, usually piped in after running freqs.
#' @param label_var DEFAULT = label; name of variable to be ordered
#' @param inherent_order_label DEFAULT = F; If F, puts labels in descending order. If T, puts labels in the inherent order from survey (Strongly agree to strongly disagree)
#' @param group_var DEFAULT = F; Add the name of the grouping variable if your data is grouped
#' @param inherent_order_group DEFAULT = F; If F, puts groups in descending order. If T, puts groups in the order they are factored (District 1, District 2...)
#' @param label_specific DEFAULT = NA; If T, puts the specified label first. ex: 'brand1' would put label called brand1 before all other labels
#' @param group_specific DEFAULT = NA; If T, puts the specified group first. ex: 'brand1' would put group called brand1 before all other groups
#' @param stacked DEFAULT = F; For stacked barcharts, use stacked = T (use ms_stacked for stacked mscharts)
#' @param ms_stacked DEFAULT = F; For stacked barcharts using mscharts, use ms_stacked = T. Specifying stacked = T automatically makes inherent_order_label = T
#' @param horizontal DEFAULT = F; For horizontal charts (grouped or ungrouped), use horizontal = T. Specifying ms_stacked = T automatically makes inherent_order_label = T
#' @param rev_label DEFAULT = F; To reverse the order of labels in a chart, use rev_label = T
#' @param rev_group DEFAULT = F; To reverse the order of groups in a chart, use rev_group = T
#' @param none_other DEFAULT = T; Automatically puts "Other" and "None of the above" options at the bottom. Change to F to let them stay ordered elsewhere in the chart
#' @keywords order label arrange
#' @export
#' @examples
#' frequencies %>% order_label(
#' horizontal = T,
#' inherent_order_label = T
#' )
#' OR
#' frequencies %>% order_label(
#' group_var = brand,
#' stacked = T,
#' group_specific = 'Y2 Analytics'
#' )

### Final Function
order_label <- function(
  dataset, #will likely be frequencies
  label_var = label,
  inherent_order_label = F,
  group_var = F,
  inherent_order_group = F,
  label_specific = NA,
  group_specific = NA,
  stacked = F,
  ms_stacked = F,
  horizontal = F,
  rev_label = F,
  rev_group = F,
  none_other = T
) {
  options(warn = -1)

  ###Flags
  #Enquo flags
  label_var_flag <- dplyr::enquo(label_var)
  group_var_flag <- dplyr::enquo(group_var)
  #Stacked flags: bars always inherently ordered
  inherent_order_label = ifelse(
    stacked == T | ms_stacked == T,
    T,
    inherent_order_label
  )
  #Grouping flags
  group_test <- dataset %>%
    dplyr::mutate(
      test = ifelse(
        !!group_var_flag == F,
        F,
        T
      )
    )
  grouped = ifelse(
    group_test$test == T,
    T,
    F
  )
  #Flags for putting a specific label/group first
  specifically_ordered = ifelse(
    is.character(label_specific) == T,
    T,
    F
  )
  specifically_ordered_group = ifelse(
    is.character(group_specific) == T,
    T,
    F
  )

  ### Prep work
  dataset <- reverse_label(dataset, grouped, !!group_var_flag, !!label_var_flag, rev_label)

  ### (1) ungrouped Section
  if(grouped == F){
    dataset <- section_ungrouped(dataset, grouped, specifically_ordered, inherent_order_label, stacked, label_specific)
    ### Arranging WITH grouping variables
  } else{
    ### (2) Grouped Section: arranging for specific group and label to be first
    dataset <- section_grouped_specifics(dataset, specifically_ordered, label_specific,inherent_order_label, group_var, inherent_order_group, group_specific, specifically_ordered_group, rev_group)
    ### (3) Grouped Section: arranging for specific group to be first
    dataset <- section_grouped_specifics_nolab(dataset, specifically_ordered, inherent_order_label, group_var, inherent_order_group, group_specific, specifically_ordered_group, rev_group, rev_label)
    ### (4) Grouped Section: inherent order of grouping variables
    dataset <- section_grouped_ordered(dataset, specifically_ordered, label_specific, inherent_order_label, group_var, inherent_order_group, group_specific, specifically_ordered_group, rev_group, rev_label)
    ### (5) Grouped Section: arranging grouping variables if group NOT inherently ordered
    dataset <- section_grouped_unordered(dataset, specifically_ordered, label_specific, inherent_order_label, group_var, inherent_order_group, group_specific, specifically_ordered_group, rev_group, rev_label)
  }
  ### Put "None" & "Other" at bottom
  dataset <- none_other(dataset, none_other, grouped)
  ### Horizontal
  dataset <- horizontal_chart(dataset, horizontal, grouped)
  ### Stacked
  dataset <- stacked_chart(dataset, stacked, grouped, inherent_order_group, specifically_ordered_group)
  dataset <- stacked_chart_ms(dataset, ms_stacked, grouped, inherent_order_group, specifically_ordered_group)
}


#### ***** ORDER LABEL FUNCTION PARTS ***** ####
#### Blank values ####
blank_values <- function(
  dataset
){
  #If no value, add in place filler
  if(
    any(names(dataset) == 'value') == F
  ) {
    dataset$value <- 'x'
  } else{
    dataset <- dataset
  }

  #If no label, add in place filler
  if(
    any(names(dataset) == 'label') == F
  ) {
    dataset$label <- 'x'
  } else{
    dataset <- dataset
  }
  return(dataset)
}

#### Add label ####
add_label <- function(
  dataset,
  label_var
){
  dataset <- blank_values(dataset)
  label_var_flag <- dplyr::enquo(label_var)
  if(dataset$label == 'x'
  ) {
    dataset <- dataset %>%
      dplyr::mutate(
        label = !!label_var_flag
      ) } else{
        dataset <- dataset
      }
  return(dataset)
}

#### Add group ####
add_group <- function(
  dataset,
  grouped,
  group_var,
  label_var
){
  #Frequencies with a grouping variable must be grouped for following section
  if(grouped == T){
    label_var_flag <- dplyr::enquo(label_var)
    dataset <- add_label(dataset, !!label_var_flag)
    group_var_flag <- dplyr::enquo(group_var)
    dataset <- dataset %>%
      dplyr::mutate(
        group_var = !!group_var_flag
      ) %>%
      group_by(
        group_var
      )
  } else{
    label_var_flag <- dplyr::enquo(label_var)
    dataset <- add_label(dataset, !!label_var_flag)
  }
}

#### Factors ####
#When inherent_order == T, labels are ordered by value #s
#But factored variables won't have #s in value column. Let's give them some
factors <- function(
  dataset,
  grouped,
  group_var,
  label_var
){
  label_var_flag <- dplyr::enquo(label_var)
  group_var_flag <- dplyr::enquo(group_var)
  dataset <- add_group(dataset, grouped, !!group_var_flag, !!label_var_flag)
  #When "value" is factored, value needs to be changed to .number
  #When "value" was completely missing or all the same, values needs to be changed to distinct .number
  if(dataset$value == dataset$label |
     length(unique(dataset$value)) == 1
  ){
    max_lab <- length(unique(dataset$label))
    dataset <- dataset %>%
      dplyr::mutate(
        value = 1:max_lab
      ) %>%
      dplyr::ungroup()
  } else {
    dataset <- dataset %>% dplyr::ungroup()
  }

  #Now convert value to numeric for inherent_orders
  dataset <- dataset %>%
    dplyr::mutate(
      value = gsub("[^0-9.]", "", value) %>% as.character() %>% as.numeric()
    )
  return(dataset)
}

#### Reverse label inherent order ####
reverse_label <- function(
  dataset,
  grouped,
  group_var,
  label_var,
  rev_label = F
) {
  label_var_flag <- dplyr::enquo(label_var)
  group_var_flag <- dplyr::enquo(group_var)
  dataset <- factors(dataset, grouped, !!group_var_flag, !!label_var_flag)
  if(rev_label == T){
    max_val <- max(dataset$value)
    min_val <- min(dataset$value)
    dataset <- dataset %>%
      dplyr::mutate(
        value = mapvalues(
          value,
          from = c(min_val:max_val),
          to = c(max_val:min_val)
        )
      )
  } else {
    dataset <- dataset
  }
}

#### Reverse label unordered ####
reverse_label_unordered <- function(
  dataset,
  rev_label
){
  if(rev_label == T){
    dataset <- dataset %>%
      dplyr::arrange(
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::arrange(
        label = forcats::fct_rev(label)
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        label = as.character.factor(label)
      )
  } else {
    dataset <- dataset
  }
}

#### ungrouped Section ####
section_ungrouped <- function(
  dataset,
  grouped,
  specifically_ordered,
  inherent_order_label,
  stacked,
  label_specific
) {
  if(grouped == F){
    dataset <- ungrouped1(dataset, specifically_ordered, inherent_order_label, label_specific)
    dataset <- ungrouped2(dataset, specifically_ordered, inherent_order_label, label_specific)
    dataset <- ungrouped3(dataset, stacked)
    dataset <- ungrouped4(dataset, specifically_ordered, inherent_order_label)
    dataset <- ungrouped5(dataset, specifically_ordered, inherent_order_label)
  } else {
    data <- dataset
  }
  return(dataset)
}

#### (1) ungrouped 1 ####
#Arranging with specific label first
ungrouped1 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  label_specific
){
  if(specifically_ordered == T &
     inherent_order_label == F) {
    freqs1 <- dataset %>%
      dplyr::filter(label == label_specific)
    freqs2 <- dataset %>%
      dplyr::filter(label != label_specific) %>%
      dplyr::arrange(
        desc(result)
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      )
  } else{
    dataset <- dataset
  }
}

#### ungrouped 2 ####
ungrouped2 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  label_specific
){
  if(specifically_ordered == T &
     inherent_order_label == T) { #Arranging with specific label first, then inherent
    freqs1 <- dataset %>%
      dplyr::filter(label == label_specific)
    freqs2 <- dataset %>%
      dplyr::filter(label != label_specific) %>%
      dplyr::arrange(
        value
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      )
  } else{
    dataset <- dataset
  }
}

#### ungrouped 3 ####
ungrouped3 <- function(
  dataset,
  stacked
){
  if(stacked == T){
    dataset <- dataset %>%
      dplyr::arrange(
        value, result
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        label = forcats::fct_rev(label)
      )
  } else{
    dataset <- dataset
  }
}

#### ungrouped 4 ####
#Arranging for non inherent order labels
ungrouped4 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label
){
  if(inherent_order_label == F &
     specifically_ordered == F
  ){
    dataset <- dataset %>%
      dplyr::arrange(
        desc(result)
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      )
  } else{
    dataset <- dataset
  }
}

#### ungrouped 5 ####
ungrouped5 <- function(
  dataset,
  specifically_ordered,
  inherent_order_label
){
  if(inherent_order_label == T &
     specifically_ordered == F
  ){ #Arranging for inherent order labels
    dataset <- dataset %>%
      dplyr::arrange(
        value, result
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      )
  } else{
    dataset <- dataset
  }
}




#### Reverse Group ####
reverse_group <- function(
  dataset,
  rev_group
){
  if(rev_group == T){
    dataset <- dataset %>%
      dplyr::arrange(
        group_var = forcats::fct_inorder(group_var)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      ) %>%
      dplyr::arrange(
        group_var = forcats::fct_rev(group_var)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var),
        group_var = as.character.factor(group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### Reverse label unordered pt 2 ####
reverse_label_unordered2 <- function(
  dataset,
  rev_label
){
  if(rev_label == T){
    dataset <- dataset %>%
      dplyr::arrange(
        group_var = forcats::fct_inorder(group_var)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      ) %>%
      dplyr::arrange(
        group_var = forcats::fct_rev(group_var)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var),
        group_var = as.character.factor(group_var)
      )
  } else {
    dataset <- dataset
  }
}

#### (2) Grouped Specifics for group and label ####
section_grouped_specifics <- function(
  dataset,
  specifically_ordered,
  label_specific,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_specific,
  specifically_ordered_group,
  rev_group #fix reversed groups in this section
) {
  if(specifically_ordered_group == T &
     specifically_ordered == T
  ) {
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        group_var = as_factor(group_var),
        group_var = as.character.factor(group_var)
      )

    dataset <- grouped_specific1(dataset, inherent_order_label, inherent_order_group, label_specific, group_specific)
    dataset <- grouped_specific2(dataset, inherent_order_label, inherent_order_group, label_specific, group_specific)
    dataset <- grouped_specific3(dataset, inherent_order_label, inherent_order_group, label_specific, group_specific)
    dataset <- grouped_specific4(dataset, inherent_order_label, inherent_order_group, label_specific, group_specific)
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Grouped specifics 1 ####
grouped_specific1 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_specific,
  group_specific
){
  if(inherent_order_label == F & #No inherent order for group/label
     inherent_order_group == F){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific &
          label == label_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label == label_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var == group_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs4 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label != label_specific
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3, freqs4) %>%
      dplyr::mutate(
        percent_label = ifelse(
          label == label_specific & group_var == group_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped specifics 2 ####
grouped_specific2 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_specific,
  group_specific
){
  if(inherent_order_label == T & #label inherent
     inherent_order_group == F){ #group NOT
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific &
          label == label_specific
      )
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label == label_specific
      ) %>%
      dplyr::arrange(
        value, desc(result)
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var == group_specific &
          label != label_specific
      ) %>%
      dplyr::arrange(
        value
      )
    freqs4 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label != label_specific
      ) %>%
      dplyr::arrange(
        value
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3, freqs4) %>%
      dplyr::mutate(
        percent_label = ifelse(
          group_var == group_specific & label == label_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped specifics 3 ####
grouped_specific3 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_specific,
  group_specific
){
  if(inherent_order_label == T & #Both label/group inherently ordered
     inherent_order_group == T){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific &
          label == label_specific
      )
    freqs2 <- dataset %>%
      dplyr::filter(
        label != label_specific &
          group_var == group_specific
      ) %>%
      dplyr::arrange(
        value
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var != group_specific
      ) %>% dplyr::arrange(
        value, group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label_specific & group_var == group_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped specifics 4 ####
grouped_specific4 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  label_specific,
  group_specific
){
  if(inherent_order_label == F &
     inherent_order_group == T){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific &
          label == label_specific
      )
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var == group_specific &
          label != label_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label == label_specific
      ) %>%
      dplyr::arrange(
        group_var
      )
    freqs4 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label != label_specific
      ) %>%
      dplyr::arrange(
        group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3, freqs4) %>%
      dplyr::mutate(
        percent_label = ifelse(
          group_var == group_specific & label == label_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### (3) Grouped, specific for group only ####
section_grouped_specifics_nolab <- function(
  dataset,
  specifically_ordered,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_specific,
  specifically_ordered_group,
  rev_group, #fix reversed groups in this section
  rev_label
) {
  if(specifically_ordered_group == T &
     specifically_ordered == F
  ) {
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        group_var = as_factor(group_var),
        group_var = as.character.factor(group_var)
      )

    dataset <- grouped_specific5(dataset, inherent_order_label, inherent_order_group, group_specific, rev_label, rev_group)
    dataset <- grouped_specific6(dataset, inherent_order_label, inherent_order_group, group_specific, rev_label, rev_group)
    dataset <- grouped_specific7(dataset, inherent_order_label, inherent_order_group, group_specific, rev_label, rev_group)
    dataset <- grouped_specific8(dataset, inherent_order_label, inherent_order_group, group_specific, rev_label, rev_group)
  } else {
    dataset <- dataset
  }
  return(dataset)
}


#### Grouped Specifics 5 ####
grouped_specific5 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_specific,
  rev_label,
  rev_group
){
  if(inherent_order_label == F &
     inherent_order_group == F){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      ) %>% reverse_label_unordered(rev_label)
    group1 <- freqs1$label[1]
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label == group1
      ) %>%
      dplyr::arrange(
        desc(result)
      ) %>% reverse_label_unordered2(rev_label)
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label != group1
      )
    freqs_combo <- dplyr::bind_rows(freqs2, freqs3)
    dataset <- dplyr::bind_rows(freqs1, freqs_combo) %>%
      dplyr::mutate(
        percent_label = ifelse(
          label == label[1] & group_var == group_specific,
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Specifics 6 ####
grouped_specific6 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_specific,
  rev_label,
  rev_group
){
  if(inherent_order_label == T & #No inherent order for group/label
     inherent_order_group == F){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific
      ) %>%
      dplyr::arrange(
        value, desc(result)
      )
    group1 <- freqs1$label[1]
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label == group1
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var != group_specific &
          label != group1
      )
    freqs_combo <- dplyr::bind_rows(freqs2, freqs3)
    dataset <- dplyr::bind_rows(freqs1, freqs_combo) %>%
      dplyr::mutate(
        percent_label = ifelse(
          group_var == group_specific & label == label[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Specifics 7 ####
grouped_specific7 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_specific,
  rev_label,
  rev_group
){
  if(inherent_order_label == T &
     inherent_order_group == T){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific
      ) %>%
      dplyr::arrange(
        value
      )
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var != group_specific
      ) %>% dplyr::arrange(
        value, group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1] & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Specifics 8 ####
grouped_specific8 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  group_specific,
  rev_label,
  rev_group
){
  if(inherent_order_label == F &
     inherent_order_group == T){
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      ) %>% reverse_label_unordered(rev_label)
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var != group_specific
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1] & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### (4) Grouped Ordered Section ####
section_grouped_ordered <- function(
  dataset,
  specifically_ordered,
  label_specific,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_specific,
  specifically_ordered_group,
  rev_group,
  rev_label
) {
  if(specifically_ordered_group == F &
     inherent_order_group == T
  ){
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate(group_var = forcats::fct_inorder(group_var))

    dataset <- grouped_ordered1(dataset, inherent_order_label, inherent_order_group, specifically_ordered, label_specific, rev_group)
    dataset <- grouped_ordered2(dataset, inherent_order_label, inherent_order_group, specifically_ordered, label_specific, rev_group, rev_label)
    dataset <- grouped_ordered3(dataset, inherent_order_label, inherent_order_group, specifically_ordered, rev_group, rev_label)
    dataset <- grouped_ordered4(dataset, inherent_order_label, inherent_order_group, specifically_ordered, rev_group)
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Grouped Ordered 1 ####
grouped_ordered1 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_specific,
  rev_group
){
  if(specifically_ordered == T &
     inherent_order_label == T){ #Arranging for specific label first
    freqs1 <- dataset %>%
      dplyr::filter(
        label == label_specific
      ) %>%
      dplyr::arrange(
        group_var
      ) %>% reverse_group(rev_group)
    freqs2 <- dataset %>%
      dplyr::filter(
        label != label_specific
      ) %>% dplyr::arrange(
        value, group_var
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1] & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Ordered 2 ####
grouped_ordered2 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_specific,
  rev_group,
  rev_label
){
  if(specifically_ordered == T &
     inherent_order_label == F){ #Arranging for specific label first
    dataset <- dataset %>% reverse_group(rev_group)
    freqs1 <- dataset %>%
      dplyr::filter(
        group_var == group_var[1] &
          label == label_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs2 <- dataset %>%
      dplyr::filter(
        group_var == group_var[1] &
          label != label_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      ) %>%
      reverse_label_unordered(rev_label)
    freqs3 <- dataset %>%
      dplyr::filter(
        group_var != group_var[1]
      )

    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label_specific & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Ordered 3 ####
grouped_ordered3 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group,
  rev_label
){
  if(inherent_order_label == F &
     specifically_ordered == F){
    dataset <- dataset %>%
      dplyr::arrange(
        group_var, desc(result)
      ) %>%
      reverse_label_unordered(rev_label) %>%
      reverse_group(rev_group) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1] & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Ordered 4 ####
grouped_ordered4 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group
){
  if(inherent_order_label == T &
     specifically_ordered == F
  ){
    dataset <- dataset %>%
      dplyr::arrange(
        group_var, value, result
      ) %>%
      reverse_group(rev_group) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1] & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}


#### (5) Grouped Unordered Section ####
section_grouped_unordered <- function(
  dataset,
  specifically_ordered,
  label_specific,
  inherent_order_label,
  group_var,
  inherent_order_group,
  group_specific,
  specifically_ordered_group,
  rev_group,
  rev_label
) {
  if(specifically_ordered_group == F &
     inherent_order_group == F
  ){
    dataset <- dataset %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        group_var = as_factor(group_var),
        group_var = as.character.factor(group_var)
      )

    dataset <- grouped_unordered1(dataset, inherent_order_label, inherent_order_group, specifically_ordered, label_specific, rev_group, rev_label)
    dataset <- grouped_unordered2(dataset, inherent_order_label, inherent_order_group, specifically_ordered, label_specific, rev_group)
    dataset <- grouped_unordered3(dataset, inherent_order_label, inherent_order_group, specifically_ordered, rev_group, rev_label)
    dataset <- grouped_unordered4(dataset, inherent_order_label, inherent_order_group, specifically_ordered, rev_group)
  } else {
    dataset <- dataset
  }
  return(dataset)
}

#### Grouped Unordered 1 ####
grouped_unordered1 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_specific,
  rev_group,
  rev_label
){
  if(specifically_ordered == T &
     inherent_order_label == F){
    freqs1 <- dataset %>%
      dplyr::filter(
        label == label_specific
      ) %>%
      dplyr::arrange(
        desc(result)
      ) %>%
      reverse_label_unordered(rev_label) %>%
      reverse_group(rev_group)
    group1 <- freqs1$group_var[1]
    freqs2 <- dataset %>%
      dplyr::filter(
        label != label_specific &
          group_var == group1
      ) %>%
      dplyr::arrange(
        desc(result)
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        label != label_specific &
          group_var != group1
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) %>%
      dplyr::mutate(
        percent_label = ifelse(
          label == label_specific & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Unordered 2 ####
grouped_unordered2 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  label_specific,
  rev_group
){
  if(specifically_ordered == T &
     inherent_order_label == T){ #Arranging for specific label first
    freqs1 <- dataset %>%
      dplyr::filter(
        label == label_specific
      ) %>%
      dplyr::arrange(
        value, desc(result), group_var
      ) %>% reverse_group(rev_group)
    group1 <- freqs1$group_var[1]
    freqs2 <- dataset %>%
      dplyr::filter(
        label != label_specific &
          group_var == group1
      ) %>%
      dplyr::arrange(
        value
      )
    freqs3 <- dataset %>%
      dplyr::filter(
        label != label_specific &
          group_var != group1
      )
    dataset <- dplyr::bind_rows(freqs1, freqs2, freqs3) %>%
      dplyr::mutate(
        percent_label = ifelse(
          label == label_specific & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        ),
        label = forcats::fct_inorder(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Unordered 3 ####
grouped_unordered3 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group,
  rev_label
){
  if(inherent_order_label == F &
     specifically_ordered == F
  ){
    freqs1 <- dataset %>%
      dplyr::arrange(
        desc(result)
      )  %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label)
      ) %>%
      reverse_label_unordered(rev_label) %>%
      dplyr::filter(
        label == label[1]
      ) %>% reverse_group(rev_group)
    group1 = freqs1$label[1]
    freqs2 <- dataset %>%
      dplyr::filter(
        label != group1
      ) %>%
      dplyr::arrange(
        desc(result)
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label)
      ) %>% reverse_label_unordered(rev_label)
    dataset <- dplyr::bind_rows(freqs1, freqs2) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var),
        percent_label = ifelse(
          label == group1 & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label)
      )
  } else{
    dataset <- dataset
  }
}

#### Grouped Unordered 4 ####
grouped_unordered4 <- function(
  dataset,
  inherent_order_label,
  inherent_order_group,
  specifically_ordered,
  rev_group
){
  if(inherent_order_label == T &
     specifically_ordered == F
  )
  {
    dataset <- dataset %>%
      dplyr::arrange(
        value, desc(result), group_var
      ) %>% reverse_group(rev_group) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label),
        percent_label = ifelse(
          label == label[1] & group_var == group_var[1],
          str_c(result * 100, '%'),
          str_c(result * 100)
        )
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_inorder(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### Horizontal ####
horizontal_chart <- function(
  dataset,
  horizontal,
  grouped
){
  if(horizontal == T){
    if(grouped == F){
      dataset <- dataset %>%
        dplyr::mutate(
          label = forcats::fct_rev(label)
        )
    } else{ #grouped == T
      dataset <- dataset %>%
        dplyr::mutate(
          label = forcats::fct_rev(label)
        ) %>%
        dplyr::mutate(
          group_var = forcats::fct_rev(group_var)
        )
    }
  } else{
    dataset <- dataset
  }
}
#### Stacked ####
stacked_chart <- function(
  dataset,
  stacked,
  grouped,
  inherent_order_group,
  specifically_ordered
){
  if(stacked == T &
     grouped == F){
    dataset <- dataset  %>%
      dplyr::mutate(
        label = forcats::fct_rev(label)
      )
  } else if(stacked == T &
            grouped == T){
    dataset <- dataset %>%
      dplyr::mutate(
        label = forcats::fct_rev(label)
      ) %>%
      dplyr::mutate(
        group_var = forcats::fct_rev(group_var)
      )
  } else{
    dataset <- dataset
  }
}

#### MS Stacked ####
stacked_chart_ms <- function(
  dataset,
  ms_stacked,
  grouped,
  inherent_order_group,
  specifically_ordered_group
){
  if(ms_stacked == T &
     grouped == T){
    dataset <- dataset %>%
      dplyr::mutate(
        group_var = forcats::fct_rev(group_var)
      )
  } else{
    dataset <- dataset
  }
}


#### None / Other ####
none_other <- function(
  dataset,
  none_other,
  grouped
){
  if(none_other == T){
    dataset <- dataset %>%
      dplyr::arrange(
        label = forcats::fct_relevel(
          label,
          "Other",
          'None of the above',
          after = Inf
        )
      ) %>%
      dplyr::mutate(
        label = forcats::fct_inorder(label)
      )
  } else{
    dataset <- dataset
  }
  #For grouped
  if(grouped == T & none_other == T){
    dataset <- dataset %>%
      dplyr::mutate(group_var = forcats::fct_inorder(group_var))
  } else{
    dataset <- dataset
  }
}

