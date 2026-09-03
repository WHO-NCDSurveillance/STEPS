# Load indicator matrix from Excel and filter for inclusion

indicator_matrix = read_excel(
  paste0('data_input/', country_ISO, '_input_matrix.xlsx'),
  'indicators'
) %>% dplyr::filter(include_in_analysis == 'Yes')

# Standardize column names and convert all values to character
colnames(indicator_matrix) = tolower(colnames(indicator_matrix))
eval(parse(text = paste0(
  'indicator_matrix$', colnames(indicator_matrix),
  '= as.character(indicator_matrix$', colnames(indicator_matrix), ')', sep = '\n'
)))

#########
# -------------------------------------------------------------------------
# Validation checks for indicator matrix
# -------------------------------------------------------------------------

# Helper function to return row numbers from the filtered indicator matrix
row_id = seq_len(nrow(indicator_matrix))


# -------------------------------------------------------------------------
# 1. Check 'indicator'
#
# Expected format:
#   indicator_id: description
#
# The first ':' must separate the indicator ID from its description.
# Indicator IDs must also be unique.
# -------------------------------------------------------------------------

indicator_check = tibble::tibble(
  row_id = row_id,
  indicator = indicator_matrix$indicator
) %>%
  dplyr::mutate(
    indicator = trimws(indicator),
    indicator_id = if_else(
      grepl(":", indicator, fixed = TRUE),
      trimws(sub(":.*$", "", indicator)),
      NA_character_
    ),
    indicator_description = if_else(
      grepl(":", indicator, fixed = TRUE),
      trimws(sub("^[^:]*:", "", indicator)),
      NA_character_
    ),
    
    # Valid only when:
    # - an indicator ID exists before ':'
    # - a description exists after ':'
    # - the ID itself does not contain another delimiter
    indicator_format_valid =
      !is.na(indicator_id) &
      nzchar(indicator_id) &
      !is.na(indicator_description) &
      nzchar(indicator_description)
  )


# Report incorrectly formatted indicators
indicator_format_errors = indicator_check %>%
                          dplyr::filter(!indicator_format_valid)

if (nrow(indicator_format_errors) > 0) {
  
  message(
    "\nERROR: The following values in 'indicator' are not correctly formatted.\n",
    "Expected format: indicator_id: description\n"
  )
  
  print(
    indicator_format_errors %>%
      dplyr::select(row_id, indicator)
  )
  
  stop(
    "Execution stopped because one or more 'indicator' values are incorrectly formatted."
  )
}


# Check uniqueness of indicator IDs
duplicate_indicators = indicator_check %>%
  dplyr::group_by(indicator_id) %>%
  dplyr::filter(dplyr::n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(indicator_id, row_id)

if (nrow(duplicate_indicators) > 0) {
  
  message(
    "\nERROR: Duplicate indicator IDs have been found.\n"
  )
  
  print(
    duplicate_indicators %>%
      dplyr::select(row_id, indicator, indicator_id)
  )
  
  # IMPORTANT: stop execution if duplicate indicator IDs are found
  stop(
    "Execution stopped because duplicate indicator IDs were found."
  )
}


# -------------------------------------------------------------------------
# 2. Helper functions for fields using ';' as a separator
# -------------------------------------------------------------------------

# Check whether a value has a valid semicolon-separated structure.
#
# Invalid examples:
#   a;b;
#   ;a;b
#   a;;b
#
# Valid examples:
#   a
#   a;b
#   a;b;c
#
check_semicolon_structure = function(x) {
  
  x = trimws(as.character(x))
  
  # NA / blank values are allowed
  if (is.na(x) || !nzchar(x)) {
    return(TRUE)
  }
  
  # No leading or trailing ';'
  if (grepl("^;|;$", x)) {
    return(FALSE)
  }
  
  # No consecutive ';'
  if (grepl(";;", x, fixed = TRUE)) {
    return(FALSE)
  }
  
  TRUE
}


# Split a semicolon-separated field into its individual elements
split_semicolon = function(x) {
  
  if (is.na(x) || !nzchar(trimws(x))) {
    return(character(0))
  }
  
  trimws(strsplit(as.character(x), ";", fixed = TRUE)[[1]])
}


# -------------------------------------------------------------------------
# 3. Check semicolon separators in:
#    - logic_condition_var
#    - pop_subset
#    - primary_variables
#    - subtitle1
# -------------------------------------------------------------------------

semicolon_fields = c(
  "logic_condition_var",
  "pop_subset",
  "primary_variables",
  "subtitle1"
)

semicolon_errors = list()

for (field in semicolon_fields) {
  
  invalid_rows = which(
    !vapply(
      indicator_matrix[[field]],
      check_semicolon_structure,
      logical(1)
    )
  )
  
  if (length(invalid_rows) > 0) {
    
    semicolon_errors[[field]] <- tibble::tibble(
      row_id = invalid_rows,
      field = field,
      value = indicator_matrix[[field]][invalid_rows]
    )
  }
}

semicolon_errors = dplyr::bind_rows(semicolon_errors)


if (nrow(semicolon_errors) > 0) {
  
  message(
    "\nERROR: Invalid ';' separator structure found.\n",
    "Leading, trailing, or consecutive ';' separators are not permitted.\n"
  )
  
  print(semicolon_errors)
  
  stop(
    "Execution stopped because invalid ';' separators were found."
  )
}


# -------------------------------------------------------------------------
# 4. Check logical expressions in logic_condition_var and pop_subset
#
# Expressions are checked using R's parser.
#
# Examples that should be valid:
#   t1 == 1
#   t1 == 1 & t2 == 1
#   c8 == 4 | c8 == 5
#   !is.na(t5a)
#   t5a > 0 | t5aw > 0
#
# An expression such as:
#   t1 = 1
#
# is syntactically valid R, but is NOT a valid logical comparison for
# these fields, so it is explicitly flagged.
#
# Bare variable names such as 'c1' or 'tob_initiation_age' are allowed,
# because these already occur in the existing indicator matrix and can
# represent a variable used directly or as a logical condition.
# -------------------------------------------------------------------------

check_logical_expression = function(expr) {
  
  expr = trimws(as.character(expr))
  
  # Blank/NA values are allowed
  if (is.na(expr) || !nzchar(expr)) {
    return(list(valid = TRUE, reason = NA_character_))
  }
  
  # Explicitly reject single '=' used as an assignment.
  #
  # Allowed:
  #   ==
  #   >=
  #   <=
  #   !=
  #
  # Not allowed:
  #   =
  #
  if (grepl("(?<![<>=!])=(?!=)", expr, perl = TRUE)) {
    return(
      list(
        valid = FALSE,
        reason = "Single '=' found. Use '==' for logical equality."
      )
    )
  }
  
  # Parse the expression using R's parser 
  parsed = tryCatch(
    {
      parse(text = expr)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
  
  if (!parsed) {
    return(
      list(
        valid = FALSE,
        reason = "Expression cannot be parsed as valid R syntax."
      )
    )
  }
  
  list(
    valid = TRUE,
    reason = NA_character_
  )
}


# Apply logical-expression checks to logic_condition_var and pop_subset
logical_expression_errors = list()

for (field in c("logic_condition_var", "pop_subset")) {
  
  for (i in row_id) {
    
    value = indicator_matrix[[field]][i]
    
    # Split on ';' first so each logical expression is checked separately
    expressions = split_semicolon(value)
    
    if (length(expressions) == 0) {
      next
    }
    
    for (j in seq_along(expressions)) {
      
      result = check_logical_expression(expressions[j])
      
      if (!result$valid) {
        
        logical_expression_errors[[length(logical_expression_errors) + 1]] <-
          tibble::tibble(
            row_id = i,
            field = field,
            element = j,
            expression = expressions[j],
            error = result$reason
          )
      }
    }
  }
}

logical_expression_errors = dplyr::bind_rows(logical_expression_errors)


if (nrow(logical_expression_errors) > 0) {
  
  message(
    "\nERROR: Invalid logical expressions found in ",
    "'logic_condition_var' or 'pop_subset'.\n"
  )
  
  print(logical_expression_errors)
  
  stop(
    "Execution stopped because invalid logical expressions were found."
  )
}


# -------------------------------------------------------------------------
# 5. Check the number of ';'-separated elements in logic_condition_var
#    against subtitle1
#
# If either field contains multiple ';'-separated elements, the number
# of elements must be identical.
#
# Examples:
#
#   logic_condition_var:
#       t1 == 1;t1 == 2;t1 == 3
#
#   subtitle1:
#       Indicator subtitle 1;Indicator subtitle 2;Indicator subtitle 3
#
#   --> valid (3 vs 3)
#
#   logic_condition_var:
#       t1 == 1;t1 == 2
#
#   subtitle1:
#       Indicator subtitle 1;Indicator subtitle 2;Indicator subtitle 3
#
#   --> invalid (2 vs 3)
# -------------------------------------------------------------------------
ids_without_subtitle2 = indicator_matrix %>%
                        mutate(
                          row_id = row_number(),
                          exempt_rows = grepl(
                            "drink_level",
                            logic_condition_var,
                            fixed = TRUE
                          )
                        ) %>%
                        dplyr::filter(
                          is.na(subtitle2),
                          !exempt_rows
                        ) %>% pull(row_id)


length_errors = list()

for (i in ids_without_subtitle2) {
  
  logic_elements = split_semicolon(
    indicator_matrix$logic_condition_var[i]
  )
  
  subtitle_elements = split_semicolon(
    indicator_matrix$subtitle1[i]
  )
  
  n_logic = length(logic_elements)
  n_subtitle = length(subtitle_elements)
  
  # Only perform the comparison when either field contains
  # multiple elements.
  if (n_logic > 1 || n_subtitle > 1) {
    
    if (n_logic != n_subtitle) {
      
      length_errors[[length(length_errors) + 1]] <-
        tibble::tibble(
          row_id = i,
          indicator = indicator_matrix$indicator[i],
          logic_condition_var =
            indicator_matrix$logic_condition_var[i],
          subtitle1 =
            indicator_matrix$subtitle1[i],
          n_logic_condition_var = n_logic,
          n_subtitle1 = n_subtitle
        )
    }
  }
}

length_errors = dplyr::bind_rows(length_errors)


if (nrow(length_errors) > 0) {
  
  print(length_errors)
  
  message(
    "\nERROR: The number of ';'-separated elements in ",
    "'logic_condition_var' and 'subtitle1' does not match.\n"
  )
  
  
  stop(
    "Execution stopped because 'logic_condition_var' and 'subtitle1' have different numbers of elements.
    See the printed table above."
  )
}


# -------------------------------------------------------------------------
# 6. Final validation message
# -------------------------------------------------------------------------

message(
  "\nIndicator matrix validation completed successfully.\n",
  "- Indicator format checked\n",
  "- Indicator uniqueness checked\n",
  "- Semicolon separators checked\n",
  "- Logical expressions checked\n",
  "- logic_condition_var/subtitle1 element counts checked\n"
)

#########
# -------------------------------------------------------------------------
# Validation checks for derived variables
# -------------------------------------------------------------------------
#### Reading derived variables matrix ####
dervar_matrix = read_excel(
  paste0('data_input/', country_ISO, '_input_matrix.xlsx'),
  sheet = 'derivedvars'
)

colnames(dervar_matrix) = tolower(colnames(dervar_matrix))

# -------------------------------------------------------------------------
# Check delimiters in derived_vars and primary_vars
# -------------------------------------------------------------------------

delimiter_errors = lapply(
  c("derived_vars", "primary_vars"),
  function(field) {
    
    invalid_rows = which(
      !vapply(
        dervar_matrix[[field]],
        check_semicolon_structure,
        logical(1)
      )
    )
    
    if (length(invalid_rows) == 0) {
      return(NULL)
    }
    
    tibble::tibble(
      row_id = invalid_rows,
      field = field,
      value = dervar_matrix[[field]][invalid_rows]
    )
  }
)

delimiter_errors = dplyr::bind_rows(delimiter_errors)


if (nrow(delimiter_errors) > 0) {
  
  print(delimiter_errors)
  
  message(
    "\nERROR: Invalid delimiter structure found in ",
    "'derived_vars' or 'primary_vars'.\n",
    "Values must use ';' as the delimiter between variables.\n",
    "Leading, trailing, or consecutive ';' are not permitted.\n"
  )
  
  stop(
    "Execution stopped because invalid delimiters were found in ",
    "'derived_vars' or 'primary_vars'."
  )
}


# -------------------------------------------------------------------------
# Check that logic_exp is valid R code
# -------------------------------------------------------------------------
check_r_code = function(x) {
  
  x = trimws(as.character(x))
  
  if (is.na(x) || !nzchar(x)) {
    return(list(
      valid = TRUE,
      error = NA_character_
    ))
  }
  
  # Convert line endings (\r\n) to line endings (\n)
  x = gsub("\r\n", "\n", x, fixed = TRUE)
  
  tryCatch(
    {
      parse(text = x)
      
      list(
        valid = TRUE,
        error = NA_character_
      )
    },
    error = function(e) {
      list(
        valid = FALSE,
        error = conditionMessage(e)
      )
    }
  )
}

logic_exp_results = lapply(
  seq_len(nrow(dervar_matrix)),
  function(i) {
    
    result = check_r_code(
      dervar_matrix$logic_exp[i]
    )
    
    if (result$valid) {
      return(NULL)
    }
    
    tibble::tibble(
      row_id = i,
      logic_exp = dervar_matrix$logic_exp[i],
      error = result$error
    )
  }
)

logic_exp_errors = dplyr::bind_rows(logic_exp_results)


if (nrow(logic_exp_errors) > 0) {
  
  print(logic_exp_errors)
  
  message(
    "\nERROR: Invalid R code found in 'logic_exp'. See printed table above."
  )
  
  stop(
    "Execution stopped because one or more values in 'logic_exp' ",
    "are not valid R code."
  )
}
