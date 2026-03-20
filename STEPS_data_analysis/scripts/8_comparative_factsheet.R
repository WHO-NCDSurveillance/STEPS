############################################################
## SCRIPT FOR GENERATING COMPARATIVE FACT SHEET
############################################################

# Only execute this section if the comparative fact sheet matrix
# contains at least one row (i.e., if fact sheet indicators exist)
if(nrow(comparative_fact_sheet_matrix)>0)
{
  
  ##########################################################
  ## CREATE FACT SHEET TABLE
  ##########################################################
  
  # Extract survey years from the combined dataset and sort them
  survey_yrs = sort(as.numeric(unique(combined_dataset$svy_year)))
  
  # Generate results for all fact sheet sections by applying the
  # factsheet_section_fn function to each section and stacking results
  factsheet_table = do.call(
    'rbind',
    lapply(unique(comparative_fact_sheet_matrix$section),
           comp_factsheet_section_fn)
  )
  
  # Assign column names to the final table
  colnames(factsheet_table) = c(
    other_language[11,language],
    survey_yrs[1],
    survey_yrs[2],
    other_language[12,language]
  )
  
  # Convert table to matrix for easier manipulation
  factsheet_table = as.matrix(factsheet_table)
  
  # Remove numbering prefixes (e.g., "1. ", "2. ") from indicator labels
  factsheet_table[,1] = gsub("^\\d+\\.\\s*", "", factsheet_table[,1])
  
  
  ##########################################################
  ## IDENTIFY SECTION HEADER ROWS
  ##########################################################
  
  # Convert to dataframe and create row index variable
  factsheet_table = factsheet_table %>%
    as.data.frame() %>%
    mutate(row_numbers = 1:n())
  
  # Remove default row names
  rownames(factsheet_table) = NULL
  
  # Identify rows where the second survey year column is empty.
  # These correspond to section headers in the factsheet.
  eval(parse(text = paste0(
    "extract_rows = (factsheet_table %>%",
    " dplyr::filter(`",survey_yrs[2],"`=='') %>%",
    " dplyr::select(row_numbers))$row_numbers"
  )))
  
  
  ##########################################################
  ## FINAL TABLE CLEANING
  ##########################################################
  
  # Remove temporary row_numbers column
  factsheet_table = factsheet_table %>%
    dplyr::select(-row_numbers)
  
  # change * footnote markers to † since * is used to indicate 
  # significant differences in comparison fact sheet
  factsheet_table[, 1] <- gsub("\\*", "†", factsheet_table[, 1])
  
  ##########################################################
  ## FORMAT TABLE USING FLEXTABLE
  ##########################################################
  
  # Convert the results table into a formatted flextable
  flex_fact_sheet =
    factsheet_table %>%
    flextable() %>%
    autofit() %>%
    
    # Apply consistent font style
    flextable::style(
      pr_t = fp_text(font.size=10,
                     font.family='Source Sans Pro'),
      part = 'all'
    ) %>%
    
    # Bold section headers
    bold(i = c(1,extract_rows)) %>%
    # Header styling
    bg(bg="white",i=1,part="header") %>%
    theme_box() %>%
    # Center numeric columns
    align(align = "center", j = 2:4, part = "all") %>%
    # Reduce font size slightly for compact layout
    fontsize(size = 9 ,part = "all") %>%
    # Merge section header rows across all columns
    merge_h_range(i=extract_rows, j1=1,j2=4) %>%
    # Adjust column widths
    width(j = 2:4, 4.3, unit = "in") %>%
    # Apply header background color
    bg(bg="#339966",i=1,part="header") %>%
    # Apply background color to section rows
    bg(bg="#C9DDF3",i=extract_rows,part="body") %>%
    # Set header text color
    color(color = "white", part = 'header') %>%
    # Reduce cell padding
    padding(padding = 0, part = "all") %>%
    # Allow table pagination in Word
    paginate()
  
  
  ##########################################################
  ## INSERT TABLE INTO WORD TEMPLATE
  ##########################################################
  
  # Read Word template containing placeholders
  doc = officer::read_docx(
    paste0(getwd(),'/templates/comparative_factsheet_template.docx')
  )
  
  # Replace placeholder text in the template with
  # the actual survey year and country name
  final_sample_size <- nrow(data)
  age_range <- paste0(min(data$age, na.rm = TRUE), "–", max(data$age, na.rm = TRUE))
  doc <- doc %>%
    body_replace_all_text(old_value = "survey_year",new_value = as.character(survey_year),only_at_cursor = FALSE) %>%
    body_replace_all_text(old_value = "country_name",new_value = as.character(country),only_at_cursor = FALSE) %>%
    body_replace_all_text(old_value = "final_sample_size", new_value = as.character(final_sample_size), only_at_cursor = FALSE) %>%
    body_replace_all_text(old_value = "age_range", new_value = as.character(age_range), only_at_cursor = FALSE)
  
  
  ##########################################################
  ## ADD TABLE TO TEMPLATE BOOKMARK
  ##########################################################
  
  # Move cursor to bookmark location and insert table
  doc = doc %>%
    cursor_bookmark(id  = "bmk1") %>%
    body_add_flextable(
      width(
        flex_fact_sheet,
        width = dim(flex_fact_sheet)$widths *
          7.25 / (flextable_dim(flex_fact_sheet)$widths)
      ),
      pos = "on",
      align = 'left'
    )
  
  
  ##########################################################
  ## EXPORT FINAL FACTSHEET
  ##########################################################
  
  # Save the completed factsheet to the outputs directory
  print(
    doc,
    target = paste0(
      'outputs/',
      country_ISO,'-',
      survey_year,
      '_Comparative_Factsheet_',
      format(Sys.time(), "%d-%b-%y_%H-%M-%S"),
      '.docx'
    )
  )
  
}else{
  # Do nothing if no factsheet indicators are available
}

