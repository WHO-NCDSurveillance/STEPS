############################################################
## SCRIPT FOR GENERATING REPORT NARRATIVE PER SECTION BASED
## ON SINGLE/CURRENT SURVEY ROUND
############################################################

# -------------------------------
# Parallel computation for all sections
# -------------------------------

# Combine column and row stratification variables for narrative interpretation
narrative_strat = c(col_strat_variable, row_strat_variables)

# Run the computation function across all reporting sections in parallel
rev_comp_indicator_results_list = future_lapply(
  unique(reporting_matrix$section),
  FUN = rev_comp_numbers
)

# Combine all section-level results into a single dataframe
rev_comp_indicator_results = do.call(rbind, rev_comp_indicator_results_list) 

# Dropping indicators with less than denom_limit (see 0_main script for the setting)
rev_comp_indicator_results = rev_comp_indicator_results %>%
                             dplyr::filter(total_n >= denom_limit) %>%
                             dplyr::select(-total_n)

### Rounding numerical estimates to 1 decimal place
# Identify columns containing estimates and confidence intervals
estimate_lo_hi = grep('estimate|low|high', names(rev_comp_indicator_results), v=T)

##
rev_comp_indicator_results = rev_comp_indicator_results %>%
  mutate(across(all_of(estimate_lo_hi), ~ round(.x, 1)))%>%
  unique()

# -----------------------------------------------------------
# Cleaning the output directory before generating new sections
# -----------------------------------------------------------

# Path to the folder where section outputs will be saved
folder_path = file.path(getwd(), "outputs", "report sections")

# List all existing files and subfolders
contents = list.files(folder_path, full.names = TRUE)

# Delete all existing contents to avoid duplication
unlink(contents, recursive = TRUE)

####
# Call function that translates report headers if needed
translated_header_fn()

########
# Initialize loop variables
i = NULL
sect_no = 1
all_references = NULL

# -----------------------------------------------------------
# Loop through each report section to generate narrative reports
# -----------------------------------------------------------
for(i in unique(reporting_matrix$section_title))
{
  
  ### Subset computed results for this section
  section_data_tables = rev_comp_indicator_results %>% dplyr::filter(section_title == i) %>%
    dplyr::select(-background_text)
  
  ## Extract metadata for this section from reporting matrix
  sec_report_matrix = reporting_matrix %>% dplyr::filter(section_title == i)%>% arrange(order_ind)
  
  ##
  bacground_text = unique(sec_report_matrix$background)[1]
  survey_measures = unique(sec_report_matrix$survey_measures)[1]
  
  ##---------------------------------------------------------
  ## Adjust background text using LLM to include statistics
  ##---------------------------------------------------------
  
  adj_background_text = llm_wrapper_connect(
    paste0("Adjust the following text, adding critical statistics in the background for ",
           country, ": ", bacground_text,
           ". Just provide the output without any notes, explanations, introductions, or extra words. Insert 
           academic style references within the text, but do not list them."))
  
  ##---------------------------------------------------------
  ## Generate reference list from background text
  ##---------------------------------------------------------
  
  # Ask LLM to extract full reference details in APA format
  list_reference = llm_wrapper_connect(
    paste0(
      "From the following text, generate a clean list of full references with all publication details (authors, year, title, journal, volume, pages). ",
      "Output only the references, one per line, in APA format.\n\n",
      adj_background_text
    )
  )  
  
  # Split references by line breaks
  reference_list = unlist(strsplit(list_reference, "\n\n"))
  reference_list = unlist(strsplit(reference_list, "\n"))
  
  # Remove extra whitespace
  reference_list = trimws(reference_list)
  
  ##---------------------------------------------------------
  ## Adjust survey measures text to include country name
  ##---------------------------------------------------------
  
  adj_survey_measures = llm_wrapper_connect(
    paste0("Insert the country name into the following text without changing it. ",
           "Output only the result: ", survey_measures,
           " Country name is: ", country,
           ". Just provide the output without any notes, explanations, introductions, or extra words.")
  )
  
  # ---------------------------------------------------------
  # Translation step if report language is not English
  # ---------------------------------------------------------
  
  if(language!='english')
  {
    ## Translate background and measures text
    translated_background_text = llm_translate(adj_background_text)
    translated_survey_measures = llm_translate(adj_survey_measures)
    
    # Translate references
    refs = paste(capture.output(write.csv(reference_list, row.names=FALSE)), collapse="\n")
    translated_ref = llm_translate(refs)
    translated_ref = unlist(strsplit(translated_ref, "\n"))[-1]
    
    ## Translate section header
    translated_section_header = llm_translate(i)
    
  }else{
    
    # Use original English text
    translated_background_text = adj_background_text
    translated_survey_measures = adj_survey_measures
    translated_section_header = i
    translated_ref = reference_list
  }
  
  ###-------------------------------------------------------
  ### Formatting survey measures text for bullet display
  ###-------------------------------------------------------
  
  text_parts = str_split(translated_survey_measures, "•")[[1]]
  
  # Introductory paragraph before bullets
  intro_text = str_trim(text_parts[1])
  
  # Bullet points
  bullet_items = text_parts[-1] %>% str_trim()
  
  ###
  # Create Word document from template and populate sections
  sec_doc = officer::read_docx('templates/section_template.docx') %>%
    body_add_par(translated_section_header,   style = "heading 1", pos = 'on') %>%
    body_add_par(translated_background_header, style = "heading 2") %>%
    body_add_par(translated_background_text,   style = "JustifiedNormal") %>%
    body_add_par(translated_measures_header,   style = "heading 2")%>%
    body_add_par(intro_text, style = "JustifiedNormal")
  
  # Add survey measures bullet points
  for (item in bullet_items) {
    sec_doc = sec_doc %>% body_add_par(item, style = "bullet")
  }
  
  # Add findings section header
  sec_doc = sec_doc %>% body_add_par(translated_findings_header,   style = "heading 2")
  
  ##
  # Extract subsection titles
  sub_sec_titles = unique(sec_report_matrix$sub_section_text)
  
  j = NULL
  
  # ---------------------------------------------------------
  # Loop through subsections within each section
  # ---------------------------------------------------------
  
  for(j in sub_sec_titles)
  {
    
    # Subset reporting matrix for this subsection
    sub_sec_report_matrix = sec_report_matrix %>% 
      dplyr::filter(sub_section_text == j)
    
    sub_sec_tab = (sub_sec_report_matrix %>% 
                     arrange(table_order))$arrange_num
    
    ##
    # Extract subsection data tables
    sub_section_data_tables = section_data_tables %>% 
      dplyr::filter(sub_section_text == j)%>%
      dplyr::select(-c(arrange_num,sub_section_text))
    
    #### Cleaning dataframe to reduce token usage in LLM prompts
    cols_clean = c("sect","grp_tab_title", "ind_subtitle", "stratifier")
    
    ##
    sub_section_data_tables[cols_clean] = lapply(sub_section_data_tables[cols_clean], 
                                                 blank_consecutive_duplicates)    
    
    ## Further cleaning
    sub_section_data_tables = sub_section_data_tables %>%
      mutate(p_value = ifelse(stratifier!='',p_value,NA),
             significance = ifelse(stratifier!='',significance,NA))
    
    ## Remove significance columns if not required
    subset_names = c(grep('ci_high|ci_low',names(sub_section_data_tables), v=T),
                     'p_value','significance')
    
    if(report_signf == 'No'){
      sub_section_data_tables = sub_section_data_tables %>% 
        dplyr::select(-all_of(subset_names))
      }
    
    ##
    # Convert table to CSV text to reduce prompt size
    all_tables_text = paste(capture.output(write.csv(sub_section_data_tables, row.names=FALSE)), collapse="\n")
    
    #
    sample_narration = na.omit(unique(sub_sec_report_matrix$text_example)[1])
    
    # Prompt template when significance reporting is required
    prompt1 <- paste0(
      "You are provided with the following tables:\n\n",
      all_tables_text, "\n\n",
      "Below is an example of how a similar section was written:\n\n",
      sample_narration, "\n\n",
      "Task:\n",
      "Write a concise narrative summary for this section for ", country, " in ", language, ".\n\n",
      "Instructions:\n",
      "1. Follow the same structure, tone, and format as the example narration.\n",
      "2. Interpret all tables and describe the main findings clearly.\n",
      "3. Highlight only statistically significant differences (p < 0.05).\n",
      "4. Report p-values to four decimal places.\n",
      "5. Report overall differences across stratifiers.\n",
      "6. Include 95% confidence intervals for each estimate.\n",
      "7. Keep the summary concise and non-repetitive.\n",
      "8. Maintain meaningful narrative flow.\n",
      "9. Do not start a sentence with a number.\n",
      "10. If related indicators exist, interpret only one.\n",
      "11. Interpret overall estimate before subgroup results.\n",
      "Write the final narrative directly."
    )
    
    # Simplified prompt when significance reporting is disabled
    prompt2 <- paste0(
      "You are provided with the following tables:\n\n",
      all_tables_text, "\n\n",
      "Task:\n",
      "Write a concise narrative summary for this section for ", country, " in ", language, ".\n\n",
      "Instructions:\n",
      "Interpret all tables and summarize key findings clearly.\n",
      "Keep the narrative concise and meaningful.\n",
      "Do not start a sentence with a number."
    )
    
    ### Select appropriate prompt
    if(report_signf == 'No'){prompt = prompt2} else{ prompt = prompt1}
    
    ###
    prompt = enc2utf8(paste(prompt, collapse = "\n"))
    
    ## Call LLM wrapper to generate narrative
    complete_narrative = llm_wrapper_connect(prompt)
    
    ##
    # Translate subsection header if needed
    if(language!='english')
    {
      tanslated_sub_header = llm_translate(j)
    }else{tanslated_sub_header = j}
    
    #
    sec_doc = sec_doc %>% body_add_par(tanslated_sub_header, style = "heading 3")
    sec_doc = sec_doc %>% body_add_par(complete_narrative, style = "JustifiedNormal")
  }
  
  # Save section document
  print(sec_doc,target=paste0(getwd(),'/outputs/report sections/',sect_no,'_section_file.docx')) 
  
  sect_no=sect_no+1
  
  # Append references
  all_references = c(all_references, translated_ref)
}

#####
## Combine all section reports into one document

all_section_reports = eval(parse(text = paste0('c(',paste0('"outputs/report sections/',
                                                           1:length(unique(reporting_matrix$section_title)),
                                                           '_section_file.docx"',collapse = ','),')')))

###
combined_report = officer::read_docx('templates/section_template.docx')

# Loop through each section file and append it
for(i in all_section_reports) {
  
  combined_report = combined_report %>% 
    officer::body_add_docx(i, pos = "after")
  
  # Add page break between sections
  if (i != all_section_reports[length(all_section_reports)]) {
    combined_report = combined_report %>% 
      body_add_break(pos = "after")
  }
}

#### Adding reference list

combined_report = combined_report %>%  
  body_add_par(translated_ref_list,   style = "heading 1")

####
item = NULL

# Add references as bullet points
for (item in all_references) {
  combined_report = combined_report %>% body_add_par(item, style = "bullet")
}

#######
# Save final combined narrative report
print(combined_report, target = paste0('outputs/', country_ISO, '-', survey_year, '_Combined_Narrative_Report_', format(Sys.time(), "%d-%b-%y_%H-%M-%S"), '.docx'))

