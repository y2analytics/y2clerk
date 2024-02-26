# sig_test_y2() ----------------------------------------------------------------

#' Significance testing
#'
#' Akin to crosstabs, run significance testing on a grouped frequencies
#'
#' @param frequencies A grouped frequencies table created using the freqs() function in y2clerk
#' @param dataset The original data frame that the frequencies table came from
#' @param banner_var This will be the banner variables for cross tabs. Must be the same as the grouping variable from the freqs() function.
#' @param wt The weight variable used in the frequencies function, if applicable
#' @param layout (default: 'tall') 'tall' formats the output to look like a basic grouped freqs table. 'wide' formats the output to look like the result from Q-formatted cross tabs
#' @return A table that matches the output of cross tabs, showing significance differences between different groups for any input variables
#' stats, cross tabs. Column comparison symbols: a, b, c... (p <= 0.05), A, B, C... (p <= 0.001); No symbol: not significant at at least (p <= 0.05)
#' @examples
#'
#' # Example Data
#'
#' df <- data.frame(
#'   V1 = c(
#'     rep('Agree', 100),
#'     rep('Neither', 100),
#'     rep('Disagree', 100),
#'     rep('Agree', 200),
#'     rep('Neither', 50),
#'     rep('Disagree', 50),
#'     rep('Agree', 50),
#'     rep('Neither', 125),
#'     rep('Disagree', 125)
#'   ),
#'   group = c(
#'     rep('Group 1', 300),
#'     rep('Group 2', 300),
#'     rep('Group 3', 300)
#'   ),
#'   weight = c(
#'     rnorm(900, 1, 0.25)
#'   )
#' )
#'
#' # Frequencies
#'
#' frequencies <- df %>%
#'   dplyr::group_by(group) %>%
#'   freqs(V1)
#'
#' # Frequencies with significance tests
#'
#' tested_freqs <- frequencies %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group)
#'
#' # Tested frequencies in crosstab format
#'
#' tested_freqs <- frequencies %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group,
#'               layout = 'wide')
#'
#' # Weighted tested frequencies
#'
#' tested_freqs <- df %>%
#'   dplyr::group_by(group) %>%
#'   freqs(V1,
#'         wt = weight) %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group,
#'               wt = weight)
#'
#' # Weighted tested frequencies in crosstab format
#'
#' tested_freqs <- df %>%
#'   dplyr::group_by(group) %>%
#'   freqs(V1,
#'         wt = weight) %>%
#'   sig_test_y2(dataset = df,
#'               banner_var = group,
#'               wt = weight,
#'               layout = 'wide')
#'
#' @export

sig_test_y2 <- function(
    frequencies,
    dataset,
    banner_var,
    wt = NULL,
    layout = c('tall', 'wide')
) {
  
  ## Error for missing dataset argument
  if (missing(dataset)) {
    
    stop(
      'argument "dataset" is missing, with no default'
    )
    
  }
  
  ## Error for missing banner_var argument
  if (missing(banner_var)) {
    
    stop(
      'argument "banner_var" is missing, with no default'
    )
    
  }
  
  ## Error for labelled double group_var
  if (haven::is.labelled(frequencies$group_var) == TRUE) {
    
    stop(
      stringr::str_c(
        'Banner variable "',
        deparse(substitute(banner_var)),
        '" is a labelled double; please set "factor_group" equal to TRUE in freqs() for this variable'
      )
    )
    
  }
  
  ## Create logical for if there are weights
  weight_null <- dplyr::enquo(wt)
  weight_exists <- !rlang::quo_is_null(weight_null)
  
  ## Test matching arguments
  layout <- rlang::arg_match(layout)
  
  ## Getting iterables
  # Define variable name for responses reference
  var_names <- frequencies %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$variable) %>%
    dplyr::pull()
  
  # Running list of currently filtered stems
  filtered_stems <- vector()
  
  # Define group_var levels from frequencies object
  group_levels <- frequencies %>%
    dplyr::count(.data$group_var) %>%
    dplyr::pull(.data$group_var) %>%
    as.character()
  
  # Letter references
  sig_codes <- data.frame(
    group_levels = group_levels,
    reference = letters[1:length(group_levels)]
  )
  
  # New column for test results
  tested_freqs <- frequencies %>%
    dplyr::mutate(sig = '')
  
  for (var_name in var_names) {
    
    # Define value levels from frequencies object
    if (haven::is.labelled(dataset[[var_name]])) {
      
      # Use value col for haven labelled vars
      value_levels <- frequencies %>%
        dplyr::ungroup() %>%
        dplyr::count(.data$value) %>%
        dplyr::pull(.data$value) %>%
        as.numeric()
      
    } else {
      
      # Label col works for everything else
      value_levels <- frequencies %>%
        dplyr::ungroup() %>%
        dplyr::count(.data$label) %>%
        dplyr::pull(.data$label)
      
    }
    
    ## Populate
    
    # Iterate through each value level (comparing ACROSS groups WITHIN values)
    for (i in value_levels) {
      
      # Process updates
      if (haven::is.labelled(dataset[[var_name]])) {
        
        haven_val_label <- frequencies %>%
          dplyr::ungroup() %>%
          dplyr::filter(
            .data$variable == var_name,
            .data$value == i
          ) %>%
          dplyr::distinct(.data$label) %>%
          dplyr::pull(.data$label)
        
        message(
          stringr::str_c(
            'Adding grouped pairwise significance tests for response "',
            haven_val_label,
            '" for group_var "',
            deparse(substitute(banner_var)),
            '"'
          )
        )
        
      } else {
        
        message(
          stringr::str_c(
            'Adding grouped pairwise significance tests for response "',
            i,
            '" for group_var "',
            deparse(substitute(banner_var)),
            '"'
          )
        )
        
      }
      
      # Filter dataset to non all NULL responses based on stem
      var_stem <- stringr::str_remove(var_name, '_[0-9]+$')

      if (!var_stem %in% filtered_stems) {
        
        # Filter all NA rows for each stem
        filtered_dataset <- dataset %>%
          dplyr::mutate(
            ns = rowSums(
              dplyr::across(
                .cols = dplyr::matches(stringr::str_c(var_stem, '_[0-9]+$')),
                .fns = ~ifelse(
                  is.na(.x),
                  FALSE,
                  TRUE
                )
              )
            ),
            # Set remaining NAs to zero as not to confuse test data
            dplyr::across(
              .cols = dplyr::matches(stringr::str_c(var_stem, '_[0-9]+$')),
              .fns = ~ifelse(
                is.na(.x),
                0,
                .x
              )
            )
          ) %>% 
          dplyr::filter(
            ns > 0
          ) %>%
          dplyr::select(
            -ns
          )
        
        # Add stem to list so it's not filtered again
        filtered_stems <- append(filtered_stems, var_stem)
        
      } else if (!exists("filtered_dataset")) {
        
        filtered_dataset <- dataset
        
      }
    
      # All group_level pairwise combinations
      for (j in group_levels) {
        for (k in group_levels) {
          
          # Only runs test for pairings of dissimilar groups
          if (j != k) {
            
            # Proportions check
            if (haven::is.labelled(dataset[[var_name]])) {
              
              # For haven labelled vars, reference value
              px <- frequencies %>%
                dplyr::filter(
                  .data$variable == var_name,
                  .data$group_var == j,
                  .data$value == i
                ) %>%
                dplyr::pull(.data$result)
              
              py <- frequencies %>%
                dplyr::filter(
                  .data$variable == var_name,
                  .data$group_var == k,
                  .data$value == i
                ) %>%
                dplyr::pull(.data$result)
              
            } else {
              
              # Reference label for all else
              px <- frequencies %>%
                dplyr::filter(
                  .data$variable == var_name,
                  .data$group_var == j,
                  .data$label == i
                ) %>%
                dplyr::pull(.data$result)
              
              py <- frequencies %>%
                dplyr::filter(
                  .data$variable == var_name,
                  .data$group_var == k,
                  .data$label == i
                ) %>%
                dplyr::pull(.data$result)
              
            }
            
            # In cases where no one in group answered
            if (rlang::is_empty(px)) {
              
              px <- 0
              
            }
            
            if (rlang::is_empty(py)) {
              
              py <- 0
              
            }
            
            # Only where px is greater than py
            if (px > py) {
              
              # Unweighted
              if (weight_exists == FALSE) {
                
                # Set up testing data
                test_data <- filtered_dataset %>%
                  dplyr::select(
                    test_var = tidyselect::all_of(var_name),
                    group = {{ banner_var }}
                  ) %>%
                  dplyr::mutate(
                    group = as.character(haven::as_factor(.data$group)),
                    test_var = dplyr::case_when(
                      test_var == i ~ 1,
                      is.na(test_var) ~ NA_real_,
                      TRUE ~ 0
                    )
                  ) %>%
                  dplyr::filter(
                    !is.na(.data$test_var),
                    .data$group == j |
                      .data$group == k
                  )
                
                # Create survey design object
                surv_object <- survey::svydesign(
                  id =~ 1,
                  weights = NULL,
                  data = test_data
                )
                
              }
              
              # Weighted
              if (weight_exists == TRUE) {
                
                # Set up testing data
                test_data <- filtered_dataset %>%
                  dplyr::select(
                    test_var = tidyselect::all_of(var_name),
                    group = {{ banner_var }},
                    weight = {{ wt }}
                  ) %>%
                  dplyr::mutate(
                    group = as.character(haven::as_factor(.data$group)),
                    test_var = dplyr::case_when(
                      test_var == i ~ 1,
                      is.na(test_var) ~ NA_real_,
                      TRUE ~ 0
                    )
                  ) %>%
                  dplyr::filter(
                    !is.na(.data$test_var),
                    .data$group == j |
                      .data$group == k
                  )
                
                # Create survey design object
                surv_object <- survey::svydesign(
                  id =~ 1,
                  weights =~ weight,
                  data = test_data
                )
                
              }
              
              # Get test results
              p_value <- survey::svychisq(
                ~test_var + group,
                surv_object
              ) %>%
                purrr::pluck('p.value')
              
              # Get legend code
              group_letter <- sig_codes %>%
                dplyr::filter(group_levels == k) %>%
                dplyr::pull(.data$reference)
              
              # FDR correction (default; used in Q crosstabs)
              p_value <- stats::p.adjust(
                p_value,
                method = 'fdr',
                n = choose(length(group_levels), 2)
              )
              
              code_result <- dplyr::case_when(
                # < 0.05 is lower-case code; < 0.001 is upper-case
                dplyr::between(p_value, 0.001, 0.05) ~ group_letter,
                p_value < 0.001 ~ toupper(group_letter),
                TRUE ~ ''
              )
              
            # Skip for any px < py
            } else {
              
              code_result = ''
              
            }
            
          # Skip if j == k
          } else {
            
            code_result = ''
            
          }
          
          
          ## Merge
          if (haven::is.labelled(dataset[[var_name]])) {
            
            # Onto value for haven labelled vars
            tested_freqs <- tested_freqs %>%
              dplyr::mutate(
                sig = ifelse(
                  # For the group_var, varbiable, and value just tested against,
                  .data$group_var == j &
                    .data$variable == var_name &
                    .data$value == i,
                  # Add the appropriate legend code to the sig column
                  stringr::str_c(.data$sig, code_result),
                  # Keep it the same for all else
                  .data$sig
                )
              )
            
          } else {
            
            # Onto label for all else
            tested_freqs <- tested_freqs %>%
              dplyr::mutate(
                sig = ifelse(
                  # For the group_var, varbiable, and value just tested against,
                  .data$group_var == j &
                    .data$variable == var_name &
                    .data$label == i,
                  # Add the appropriate legend code to the sig column
                  stringr::str_c(.data$sig, code_result),
                  # Keep it the same for all else
                  .data$sig
                )
              )
            
          }
          
        }
      }
      
    }
    
  }
  
  ## Final appending group level references
  sig_codes_refs <- sig_codes %>%
    dplyr::mutate(
      group_levels = unlist(
        lapply(
          group_levels, paste0('as.', class(tested_freqs$group_var))
        )
      ),
      reference = stringr::str_c(
        '[',
        toupper(.data$reference),
        ']'
      )
    )
  
  tested_freqs <- tested_freqs %>%
    dplyr::left_join(
      sig_codes_refs,
      by = c('group_var' = 'group_levels')) %>%
    dplyr::mutate(
      group_var = stringr::str_c(
        .data$group_var,
        ' ',
        .data$reference
      ),
      group_var = haven::as_factor(.data$group_var),
      group_var = forcats::fct_relevel(
        .data$group_var,
        stringr::str_c(
          group_levels,
          ' [',
          toupper(sig_codes$reference),
          ']'
        )
      )
    ) %>%
    dplyr::select(-'reference')
  
  ## Layout options
  
  # Normal Freqs Layout (tall)
  if (layout == 'tall') {
    
    return(tested_freqs)
    
  }
  
  # Crosstab Layout (wide)
  if (layout == 'wide') {
    
    # Get wide percentages
    xtab_results <- tested_freqs %>%
      tidyr::pivot_wider(
        id_cols = c(
          'variable', 
          'label'
        ),
        names_from = 'group_var',
        values_from = 'result'
      ) %>%
      dplyr::select(
        'variable',
        'label',
        stringr::str_c(
          group_levels,
          ' [',
          toupper(sig_codes$reference),
          ']'
        )
      ) %>%
      dplyr::mutate_all(~replace(., is.na(.), 0))
    
    # Get wide sig codes
    xtab_codes <- tested_freqs %>%
      tidyr::pivot_wider(
        id_cols = c(
          'variable', 
          'label'
        ),        
        names_from = 'group_var',
        values_from = 'sig'
      ) %>%
      dplyr::select(
        'variable',
        'label',
        stringr::str_c(
          group_levels,
          ' [',
          toupper(sig_codes$reference),
          ']'
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~replace(
            ., 
            is.na(.), 
            ''
          )
        )
      )
    
    # Combine
    xtab <- data.frame()
    
    for (i in 1:nrow(xtab_results)) {
      
      xtab <- rbind(
        xtab,
        xtab_results[i, ],
        xtab_codes[i, ]
      )
      
    }
    
    return(xtab)
    
  }
  
}
