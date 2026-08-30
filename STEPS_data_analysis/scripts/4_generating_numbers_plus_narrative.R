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
#all_references = NULL
all_background_texts = NULL
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
  #
  all_background_texts = c(all_background_texts,adj_background_text)

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
    # refs = paste(capture.output(write.csv(reference_list, row.names=FALSE)), collapse="\n")
    # translated_ref = llm_translate(refs)
    # translated_ref = unlist(strsplit(translated_ref, "\n"))[-1]
    
    ## Translate section header
    translated_section_header = llm_translate(i)
    
  }else{
    
    # Use original English text
    translated_background_text = adj_background_text
    translated_survey_measures = adj_survey_measures
    translated_section_header = i
    #translated_ref = reference_list
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
    #
    prompt1 = paste0(
      "You are provided with the following tables:\n\n",
      all_tables_text, "\n\n",
      "Below is an example of how a similar section was written:\n\n",
      sample_narration, "\n\n",
      
      "Task:\n",
      "Write a report-ready narrative summary for this section for ", country,
      " in ", language, ". The narrative should synthesise the findings across all tables, ",
      "identify the most important messages, and explain what the results show rather than simply listing numbers.\n\n",
      
      "Instructions:\n",
      "1. Use the example narration as a guide for structure, tone, level of detail, and reporting style, ",
      "but do not copy its wording unless appropriate.\n",
      
      "2. Critically review all tables before writing. Identify the most important findings, patterns, ",
      "differences, and trends, and prioritise these in the narrative. Do not describe every number or table unnecessarily.\n",
      
      "3. Begin with the overall estimate or population-level finding before presenting subgroup or stratified results. ",
      "Use subgroup findings to explain, qualify, or add context to the overall result.\n",
      
      "4. Highlight statistically significant differences only when p < 0.05. Do not describe non-significant ",
      "differences as meaningful or suggest that an association exists when statistical evidence is lacking.\n",
      
      "5. Where an overall comparison across stratifiers is available, report and interpret it before discussing ",
      "specific subgroup comparisons.\n",
      
      "6. For statistically significant comparisons, report the relevant p-value to four decimal places and include ",
      "95% confidence intervals where available. Ensure that confidence intervals are clearly linked to the corresponding estimates.\n",
      
      "7. Use the numbers to explain the magnitude and direction of findings. Where appropriate, describe which ",
      "groups had higher or lower estimates, the extent of the difference, and which findings are most notable.\n",
      
      "8. Go beyond numerical repetition. Translate the statistical results into clear, meaningful messages that help ",
      "the reader understand what the findings indicate. Where the tables support a clear pattern, describe that pattern explicitly.\n",
      
      "9. Distinguish clearly between descriptive differences and statistically significant differences. Do not imply ",
      "causality, explanations, or conclusions that are not supported by the data.\n",
      
      "10. Where several tables or indicators address the same underlying concept, synthesise them into one coherent ",
      "interpretation rather than repeating similar findings. If related indicators are available, focus on the indicator ",
      "that provides the clearest and most informative message unless the additional indicator adds materially different information.\n",
      
      "11. Prioritise findings according to their importance to the reader. Focus on results that show substantial ",
      "differences, important inequalities, notable patterns, or findings that materially contribute to understanding ",
      "the section. Minor or redundant numerical differences should be omitted.\n",
      
      "12. Ensure that the narrative has a logical flow: overall finding → important differences → key subgroup findings ",
      "→ interpretation and implications supported by the data.\n",
      
      "13. Do not simply reproduce table values in sequence. Integrate related findings into sentences and paragraphs ",
      "so that the narrative reads as an analytical interpretation rather than a description of the tables.\n",
      
      "14. Do not start a sentence with a number. Integrate percentages, estimates, counts, confidence intervals, ",
      "and p-values naturally within sentences.\n",
      
      "15. Use precise statistical language. Avoid words such as 'significant' unless the difference meets the ",
      "specified p < 0.05 threshold. Use 'higher', 'lower', 'increased', 'decreased', or 'differed' where appropriate.\n",
      
      "16. Do not overstate findings. Avoid causal language such as 'led to', 'caused', or 'resulted in' unless the ",
      "study design and tables explicitly support a causal interpretation.\n",
      
      "17. Keep the narrative concise but sufficiently detailed to be informative for a formal report. Avoid repetition ",
      "of the same finding, unnecessary methodological explanation, and commentary that does not add interpretation.\n",
      
      "18. End the section with the main takeaway or key message emerging from the results, where appropriate. ",
      "The reader should be able to understand the principal finding without referring back to the tables.\n",
      
      "19. Before finalising, check that all reported numbers, percentages, confidence intervals, and p-values exactly ",
      "match the supplied tables. Do not calculate, infer, or invent values that are not supported by the tables.\n",
      
      "20. Write the final narrative directly in ", language,
      ". Do not provide an introduction, explanation of your approach, bullet-point summary, or commentary about the task. \n",
      "Note - The reported p-values are based on tests of overall differences across variable levels (e.g., chi-square tests for categorical variables and t-tests for comparisons of continuous variables). Interpret these p-values 
       according to the test provided and do not imply that they represent a test of a specific level or subgroup.\n",
      'If p-value = 0.0000 then you need to rewrite it as <0.0001 in the narrative.\n',
      "Do not bold, underline, or otherwise format the text. Produce polished, report-ready prose only.",
      "Do not show 95% CIs that are neither interpretable nor plausible"
    )
    
    # Simplified prompt when significance reporting is disabled
    prompt2 = paste0(
      "You are provided with the following tables:\n\n",
      all_tables_text, "\n\n",
      "Write a report-ready narrative summary for this section for ", country,
      " in ", language, ". The narrative should synthesise the findings across all tables, ",
      "identify the most important messages, and explain what the results show rather than simply listing numbers.\n\n",
      
      "Instructions:\n",
      "1. Critically review all tables before writing. Identify the most important findings, patterns, ",
      "differences, and trends, and prioritise these in the narrative. Do not describe every number or table unnecessarily.\n",
      
      "2. Begin with the overall estimate or population-level finding before presenting subgroup or stratified results. ",
      "Use subgroup findings to explain, qualify, or add context to the overall result.\n",
      
      "3. Where an overall comparison across stratifiers is available, report and interpret it before discussing ",
      "specific subgroup comparisons.\n",
      
      "4. Use the numbers to explain the magnitude and direction of findings. Where appropriate, describe which ",
      "groups had higher or lower estimates, the extent of the difference, and which findings are most notable.\n",
      
      "5. Go beyond numerical repetition. Translate the statistical results into clear, meaningful messages that help ",
      "the reader understand what the findings indicate. Where the tables support a clear pattern, describe that pattern explicitly.\n",
      
      "6. Distinguish clearly between descriptive differences. Do not imply ",
      "causality, explanations, or conclusions that are not supported by the data.\n",
      
      "7. Where several tables or indicators address the same underlying concept, synthesise them into one coherent ",
      "interpretation rather than repeating similar findings. If related indicators are available, focus on the indicator ",
      "that provides the clearest and most informative message unless the additional indicator adds materially different information.\n",
      
      "8. Prioritise findings according to their importance to the reader. Focus on results that show substantial ",
      "differences, important inequalities, notable patterns, or findings that materially contribute to understanding ",
      "the section. Minor or redundant numerical differences should be omitted.\n",
      
      "9. Ensure that the narrative has a logical flow: overall finding → important differences → key subgroup findings ",
      "→ interpretation and implications supported by the data.\n",
      
      "10. Do not simply reproduce table values in sequence. Integrate related findings into sentences and paragraphs ",
      "so that the narrative reads as an analytical interpretation rather than a description of the tables.\n",
      
      "11. Do not start a sentence with a number. Integrate percentages, estimates, and counts ",
      " naturally within sentences.\n",
      
      "12. Use precise language. Use 'higher', 'lower', 'increased', 'decreased', or 'differed' where appropriate.\n",
      
      "13. Do not overstate findings. Avoid causal language such as 'led to', 'caused', or 'resulted in' unless the ",
      "study design and tables explicitly support a causal interpretation.\n",
      
      "14. Keep the narrative concise but sufficiently detailed to be informative for a formal report. Avoid repetition ",
      "of the same finding, unnecessary methodological explanation, and commentary that does not add interpretation.\n",
      
      "15. End the section with the main takeaway or key message emerging from the results, where appropriate. ",
      "The reader should be able to understand the principal finding without referring back to the tables.\n",
      
      "16. Before finalising, check that all reported numbers and percentages exactly ",
      "match the supplied tables. Do not calculate, infer, or invent values that are not supported by the tables.\n",
      
      "17. Write the final narrative directly in ", language,
      "Do not provide an introduction, explanation of your approach, bullet-point summary, or commentary about the task. ",
      "Do not bold, underline, or otherwise format the text. Produce polished, report-ready prose only."
      
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
    #
    paragraphs = strsplit(complete_narrative, "\\n\\n")[[1]]
    #
    for (paragraph in paragraphs) {

      sec_doc = sec_doc %>%
        body_add_par(
          value = trimws(paragraph),
          style = "JustifiedNormal"
        ) %>% body_add_par('\n')
    }

    
  }
  
  # Save section document
  print(sec_doc,target=paste0(getwd(),'/outputs/report sections/',sect_no,'_section_file.docx')) 
  
  sect_no=sect_no+1
  
  # Append references
  #all_references = c(all_references, translated_ref)
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
#
##---------------------------------------------------------
## Generate reference list from background text
##---------------------------------------------------------

# Ask LLM to extract full reference details in APA format
adj_list_reference = llm_wrapper_connect(
  paste0(
    "Review the following text and identify the publications, reports, surveys, ",
    "or other authoritative sources that are relevant to the information presented. ",
    "Generate a clean reference list for the sources that are relevant to the text.\n\n",
    
    "For each reference, provide the bibliographic information available or identifiable, ",
    "including authors or organisation, year, title, journal or publisher, volume, issue, ",
    "pages, and DOI or URL where applicable.\n\n",
    
    "OUTPUT REQUIREMENTS:\n",
    "- Output ONLY the reference list.\n",
    "- Provide one reference per line.\n",
    "- Use APA-style formatting.\n",
    "- Output references as plain text only.\n",
    "- Do NOT use Markdown formatting.\n",
    "- Do NOT use asterisks (*) or double asterisks (**).\n",
    "- Do NOT use underscores for formatting.\n",
    "- Do NOT use HTML or XML tags.\n",
    "- Do NOT bold, italicise, or underline any text.\n",
    "- Do NOT include headings, bullets, numbering, explanations, or commentary.\n",
    "- Do NOT include statements such as 'I cannot provide', 'insufficient information', ",
    "'please provide more information', or similar messages.\n\n",
    
    "ACCURACY REQUIREMENTS:\n",
    "- Select references that are directly relevant to the claims in the text.\n",
    "- Do not include unrelated references.\n",
    "- Do not invent references or bibliographic details.\n",
    "- Do not invent DOIs or URLs.\n",
    "- If some bibliographic details are unavailable, provide the reference using the ",
    "information that can be reliably identified rather than returning an explanation.\n",
    "- Remove duplicate references.\n
       - Provide url links where feasible.\n",
    
    "TEXT TO REVIEW:\n\n",
    paste0(all_background_texts, collapse = '.')
  )
)

# Split references by line breaks
reference_list = unlist(strsplit(adj_list_reference, "\n\n"))
reference_list = unlist(strsplit(reference_list, "\n"))

# Remove extra whitespace
reference_list = trimws(reference_list)
#
if(language!='english')
{
  # Translate references
  refs = paste(capture.output(write.csv(reference_list, row.names=FALSE)), collapse="\n")
  translated_ref = llm_translate(refs)
  translated_ref = unlist(strsplit(reference_list, "\n"))[-1]
  
}else{
  
  # Use original English text
  translated_ref = reference_list
}

#
item = NULL

# Add references as bullet points
for (item in translated_ref) {
  combined_report = combined_report %>% body_add_par(item, style = "bullet")
}

#######
# Save final combined narrative report
print(combined_report, target = paste0('outputs/', country_ISO, '-', survey_year, '_Combined_Narrative_Report_', format(Sys.time(), "%d-%b-%y_%H-%M-%S"), '.docx'))
