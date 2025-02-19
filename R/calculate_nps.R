# calculate_nps() --------------------------------------------------------------------

#' Quickly extract the Net Promoter Score (NPS) from a frequencies object of formatted NPS survey questions
#'
#' @param frequencies A frequencies table as produced by y2clerk::freqs()
#' @param result DEFAULT = result; The column of NPS question results (proportions) used to calculate the final NPS
#' @param label DEFAULT = label; The column of NPS question labels (responses) used to calculate the final NPS (if `input_type` is set to "grouped", this column MUST include the values "Detractor", "Passive", and "Promoter" and no other values)
#' @param value DEFAULT = value; The column of NPS question values (levels) used to order the final output
#' @param input_type DEFAULT = "grouped"; The input type of the NPS questions. Must be one of either "grouped" or "numeric" corresponding to either pre-formatted or unformatted Qualtrics NPS questions, respectively
#' @param by_group DEFAULT = TRUE; Boolean, controls whether the function calculates the NPS for each unique specified `group_var` or for the frequencies as a whole
#' @param group_var DEFAULT = variable; The column of unique NPS questions and question names used to calculate the final NPS for each (if `by_group` is set to TRUE)
#' @param add_group DEFAULT = TRUE; Boolean, controls whether the function adds the specified `group_var` as a grouping variable to the frequencies object (if it is already grouped) or if the specified `group_var` will overwrite any and all previously applied grouping variables
#' @param get_brand DEFAULT = TRUE; Boolean, controls whether the function extracts the "brand" (NPS question subject) from the specified `prompt` column or does not
#' @param prompt DEFAULT = prompt; The column of NPS question prompts (question texts) from which to extract the "brand" (if `get_brand` is set to TRUE)
#' @param brand_var_name DEFAULT = "brand"; The name assigned to the output "brand" column (if `get_brand` is set to TRUE)
#' @param prompt_rm_pre DEFAULT = ".+\\,.+recommend "; String pattern in the specified `prompt` column before which everything in the column is scrubbed to obtain the "brand"
#' @param prompt_rm_post DEFAULT = " to a .+\\? \\-.+"; String pattern in the specified `prompt` column after which everything in the column is scrubbed to obtain the "brand"
#' @param arrange_nps DEFAULT = TRUE; Boolean, whether to arrange the final output by the NPS results, with previous frequencies arrangements/orderings still intact
#' @param append_nps_to_brand DEFAULT = TRUE; Boolean, whether to append the NPS values to the "brand" column, having the format "{Brand} (NPS = {NPS})"
#' @param brand_factor DEFAULT = TRUE; Boolean, whether to convert the "brand" variable (with appended NPS values) to a factor for ease of data visualization (argument specification only applied if `append_nps_to_brand` is set to TRUE)
#' @return An updated frequencies object with the new NPS column (and other specified
#' columns) attached, formatted as specified
#' @examples
#'
#' set.seed(1)
#' 
#' df <- data.frame(
#'   brand1_NPS_GROUP = sample(
#'     c(1:3, NA),
#'     size = 200,
#'     replace = TRUE
#'   ),
#'   brand2_NPS_GROUP = sample(
#'     c(1:3, NA),
#'     size = 200,
#'     replace = TRUE
#'   ),
#'   brand3_NPS_GROUP = sample(
#'     c(1:3, NA),
#'     size = 200,
#'     replace = TRUE
#'   )
#' ) %>% 
#'   labelled::set_value_labels(
#'     brand1_NPS_GROUP = c(
#'       'Promoter' = 3, 
#'       'Passive' = 2, 
#'       'Detractor' = 1
#'     ),
#'     brand2_NPS_GROUP = c(
#'       'Promoter' = 3,
#'       'Passive' = 2,
#'       'Detractor' = 1
#'     ),
#'     brand3_NPS_GROUP = c(
#'       'Promoter' = 3,
#'       'Passive' = 2,
#'       'Detractor' = 1
#'     )
#'   ) %>% 
#'   labelled::set_variable_labels(
#'     brand1_NPS_GROUP = "Based on your experience, how likely are you to recommend Brand1 to a friend or colleague? - Group",
#'     brand2_NPS_GROUP = "Based on your experience, how likely are you to recommend Brand2 to a friend or colleague? - Group",
#'     brand3_NPS_GROUP = "Based on your experience, how likely are you to recommend Brand3 to a friend or colleague? - Group"
#'   ) %>%
#'   tidyr::as_tibble()
#' 
#' # Frequencies
#' frequencies <- df %>%
#'   freqs(
#'     brand1_NPS_GROUP,
#'     brand2_NPS_GROUP,
#'     brand3_NPS_GROUP,
#'     nas = FALSE,
#'     prompt = TRUE
#'   )
#' 
#' # Calculate NPS
#' calculate_nps(frequencies)
#'
#' @export

calculate_nps <- function(
    frequencies,
    result = result,
    label = label,
    value = value,
    input_type = c('grouped', 'numeric'),
    by_group = TRUE,
    group_var = variable,
    add_group = TRUE,
    get_brand = TRUE,
    prompt = prompt,
    brand_var_name = 'brand',
    prompt_rm_pre = '.+\\,.+recommend ',
    prompt_rm_post = ' to a .+\\? \\-.+',
    arrange_nps = TRUE,
    append_nps_to_brand = TRUE, # WORKING Should arg be consolidated with brand_factor arg? Why else would an analyst order by NPS besides charting? Just for EDA?
    brand_factor = TRUE # WORKING Convert brand to factor for charting
) {
  
  ## Variable quosures, arg matches
  result_flag <- dplyr::enquo(result)
  label_flag <- dplyr::enquo(label)
  value_flag <- dplyr::enquo(value)
  group_flag <- dplyr::enquo(group_var)
  prompt_flag <- dplyr::enquo(prompt)
  brand_flag <- dplyr::sym(brand_var_name)
  
  input_type <- rlang::arg_match(input_type)
  
  ## Input freqs checks
  # Result
  result_exists <- deparse(substitute(result)) %in% colnames(frequencies)
  if (!result_exists) {
    stop('`result` variable not provided: please provide a result variable in input frequencies')
  }
  
  # Label
  label_exists <- deparse(substitute(label)) %in% colnames(frequencies)
  if (!label_exists) {
    stop('`label` variable not provided: please provide a label variable in input frequencies')
  }
  
  # Prompt
  prompt_exists <- deparse(substitute(prompt)) %in% colnames(frequencies)
  if (!prompt_exists & get_brand) {
    stop('`prompt` variable not provided: either specify a prompt variable or set `get_brand` to FALSE')
  }
  
  # Warning about `by_group` if freqs are not grouped but appear to need grouping
  if ((by_group == FALSE) & (nrow(frequencies) > 3)) {
    warning('Input frequencies appear to be grouped by brand/variable. Did you mean to set `by_group` to `TRUE`?')
  }
  
  # Checking for all the appropriate rollup label values
  if (input_type == 'rollup') {
    label_vals <- frequencies %>%
      dplyr::distinct(!!label_flag) %>%
      dplyr::pull(!!label_flag)
    
    if (!('Detractor' %in% label_vals) | !('Passive' %in% label_vals) | !('Promoter' %in% label_vals)) {
      stop('Input variables are not correctly formatted. Please use correctly formatted variables (labels reading "Promoter", "Passive", and "Detractor") or set `input_type` to `numeric`')
    }
  }
  
  ## Manual roll-up if inputs are numeric
  if (input_type == 'numeric') {
    
    frequencies <- frequencies %>% 
      dplyr::mutate(
        !!label_flag := dplyr::case_when(
          dplyr::between(as.numeric(!!value_flag), 0, 6) ~ 'Detractor',
          dplyr::between(as.numeric(!!value_flag), 7, 8) ~ 'Passive',
          dplyr::between(as.numeric(!!value_flag), 9, 10) ~ 'Promoter'
        ),
        !!value_flag := dplyr::case_when(
          !!label_flag == 'Detractor' ~ '1',
          !!label_flag == 'Passive' ~ '2',
          !!label_flag == 'Promoter' ~ '3'
        )
      ) %>% 
      dplyr::group_by(
        !!group_flag,
        !!label_flag
      ) %>% 
      dplyr::mutate(
        dplyr::across(
          .cols = c(n, result),
          .fns = ~sum(.x)
        )
      ) %>% 
      dplyr::distinct(
        !!group_flag,
        !!label_flag,
        .keep_all = TRUE
      ) %>% 
      dplyr::ungroup()
    
  }
  
  ## New columns
  # Grouping by specified var
  if (by_group == TRUE) {
    frequencies <- frequencies %>%
      dplyr::group_by(
        !!group_flag,
        .add = add_group
      )
  }
  
  # NPS col
  frequencies <- frequencies %>%
    dplyr::mutate(
      nps = dplyr::case_when(
        !!label_flag == 'Promoter' ~ !!result_flag,
        !!label_flag == 'Passive' ~ 0,
        !!label_flag == 'Detractor' ~ !!result_flag * -1
      ) %>%
        sum() %>%
        # (\(.x) .x * 100)() %>% # NOTE: Can also be done using this code BUT only necessary if using the native pipe operator
        (function(x) x * 100)() %>% # NOTE: This is done in-pipeline because an arithmetic vectorized transform in a pipeline throws off the next function
        round()
    ) %>%
    dplyr::ungroup()
  
  # Brand col
  if (get_brand) {
    frequencies <- frequencies %>%
      dplyr::mutate(
        !!brand_flag := stringr::str_remove(
          !!prompt_flag,
          prompt_rm_pre
        ),
        !!brand_flag := stringr::str_remove(
          !!brand_flag,
          prompt_rm_post
        )
      )
  }
  
  ## Final formatting
  # Arranging by NPS
  if (arrange_nps){
    frequencies <- frequencies %>%
      dplyr::arrange(
        dplyr::desc(nps),
        !!value_flag
      )
  }
  
  # Append NPS to brand
  if (append_nps_to_brand) {
    
    if (!get_brand) {
      stop('Cannot append NPS to brand if `get_brand` is set to FALSE. Please set `get_brand` to TRUE')
    }
    
    frequencies <- frequencies %>%
      dplyr::mutate(
        !!brand_flag := stringr::str_c(
          !!brand_flag,
          ' (NPS = ',
          nps,
          ')'
        )
      )
    
    # Convert brand to factor
    if (brand_factor) {
      frequencies <- frequencies %>%
        dplyr::mutate(!!brand_flag := forcats::as_factor(!!brand_flag))
    }
    
  }
  
  ## Output
  return(frequencies)
  
}