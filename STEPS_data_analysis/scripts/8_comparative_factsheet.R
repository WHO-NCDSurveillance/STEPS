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
  
  ##Factsheet table for men
  factsheet_table_men = do.call(
    'rbind',
    lapply(
      unique(comparative_fact_sheet_matrix$section),
      function(x) comp_factsheet_section_fn(
        sect = x,
        sex_val = 'Men'
      )
    )
  )
  
  ##Factsheet table for women
  factsheet_table_women = do.call(
    'rbind',
    lapply(
      unique(comparative_fact_sheet_matrix$section),
      function(x) comp_factsheet_section_fn(
        sect = x,
        sex_val = 'Women'
      )
    )
  )
  
  ##Calling the formatting flextable function
  #Overall
  flex_fact_sheet = comp_factsheet_formatting()
  #Males
  flex_fact_sheet_men = comp_factsheet_formatting(fct_table = factsheet_table_men, 
                                                  fact_label = other_language[13,language])
  #Females
  flex_fact_sheet_women = comp_factsheet_formatting(fct_table = factsheet_table_women, 
                                                    fact_label = other_language[14,language])
  
  
  ##########################################################
  ## INSERT TABLE INTO WORD TEMPLATE
  ##########################################################
  
  # Read Word template containing placeholders
  doc = officer::read_docx(
    paste0(getwd(),'/templates/comparative_factsheet_template.docx')
  )
  #
  doc_by_sex = officer::read_docx(
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
  #
  doc_by_sex <- doc_by_sex %>%
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
  #
  doc_by_sex = doc_by_sex %>%
    cursor_bookmark(id  = "bmk1") %>%
    body_add_flextable(
      width(
        flex_fact_sheet_men,
        width = dim(flex_fact_sheet_men)$widths *
          7.25 / (flextable_dim(flex_fact_sheet_men)$widths)
      ),
      pos = "on",
      align = 'left'
    ) %>%    
    body_add_break()%>% 
    body_add_flextable(
    width(
      flex_fact_sheet_women,
      width = dim(flex_fact_sheet_women)$widths *
        7.25 / (flextable_dim(flex_fact_sheet_women)$widths)
    )
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
      '_Comparative_Fact_Sheet_',
      format(Sys.time(), "%d-%b-%y_%H-%M-%S"),
      '.docx'
    )
  )
  #
  print(
    doc_by_sex,
    target = paste0(
      'outputs/',
      country_ISO,'-',
      survey_year,
      '_Comparative_Fact_Sheet_by_sex_',
      format(Sys.time(), "%d-%b-%y_%H-%M-%S"),
      '.docx'
    )
  )
  
}else{
  # Do nothing if no factsheet indicators are available
}






