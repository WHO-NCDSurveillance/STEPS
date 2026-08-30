############################################################
## SCRIPT FOR GENERATING COMPARATIVE NARRATIVE FOR @SECTION
############################################################


############################################################
## DEFINE STRATIFIERS FOR COMPARATIVE ANALYSIS
############################################################

# Combine all stratifiers used when generating comparative results.
# These include:
# - the column stratifier
# - row stratifiers
# - a combined sex-age stratifier
comp_stratifiers = c(col_strat_variable, row_strat_variables, "sex_age")

############################################################
## GENERATE INDICATOR RESULTS FOR ALL SECTIONS
############################################################

# Run indicator calculations in parallel across all sections
# defined in the comparative reporting matrix.
# Each section is processed by the function `comp_numbers`.
comp_indicator_results_list = future_lapply(
  unique(comparative_reporting_matrix$section),
  FUN = comp_numbers
)

# Combine the list of results into a single dataframe
# and order rows using the predefined arrangement variable.
comp_indicator_results = do.call(rbind, comp_indicator_results_list) %>% 
  arrange(arrange_num)

# Dropping indicators with less than denom_limit (see 0_main script for the setting)
comp_indicator_results = comp_indicator_results %>%
                              dplyr::filter(total_n >= denom_limit) %>%
                              dplyr::select(-total_n)


############################################################
## PREPARE OUTPUT DIRECTORY
############################################################

# Remove any previously generated comparative reports.
# This ensures the output directory contains only newly
# generated factsheets.
unlink(
  list.files(
    paste0(getwd(),'/outputs/comparative'),
    full.names = TRUE,
    recursive = TRUE
  ),
  recursive = TRUE
)


############################################################
## PREPARE TRANSLATED HEADERS
############################################################

# Call function that prepares translated headers used in
# the factsheets (e.g., background, findings, charts).
translated_header_fn()


############################################################
## GENERATE COMPARATIVE NARRATIVE PER SECTION
############################################################

i = NULL

# Loop through each section title and generate a separate
# comparative narrative document.
for(i in unique(comp_indicator_results$section_title))
{
  
  ##########################################################
  ## EXTRACT SECTION DATA
  ##########################################################
  
  # Subset the indicator results corresponding to the current section
  sec_report_matrix = comp_indicator_results %>% 
                      dplyr::filter(section_title == i) 
  
  # Combine all background text for the section into a single string
  bacground_text = paste0(
    unique(sec_report_matrix$background_text),
    collapse = ' '
  )
  
  
  ##########################################################
  ## IMPROVE BACKGROUND TEXT USING LLM
  ##########################################################
  
  # Send the background text to the LLM to refine wording
  # and optionally include contextual statistics.
  adj_background_text = llm_wrapper_connect(
    paste0(
      "Adjust the following text, adding critical statistics in the background for ",
      country, ": ",
      bacground_text,
      "Just provide the output without any notes, explanations, introductions, or extra words."
    )
  )
  
  
  ##########################################################
  ## GENERATE HEADER TITLE
  ##########################################################
  
  # Create the header title containing country and survey years
  header_title = paste0(
    'Comparison Fact Sheet: ',
    country,
    ' ',
    previous_survey_year,
    ' & ',
    survey_year
  )
  
  
  ##########################################################
  ## HANDLE LANGUAGE TRANSLATION
  ##########################################################
  
  # If reporting language is not English, translate the
  # background text and section headers.
  if(language!='english')
  {
    translated_background_text = llm_translate(adj_background_text)
    translated_section_header = llm_translate(gsub('/',' or ',i))
    translated_header_title = llm_translate(header_title)
    
  } else{
    
    translated_background_text = adj_background_text
    translated_section_header = gsub('/',' or ',i)
    translated_header_title = header_title
  }
  
  
  ##########################################################
  ## INITIALIZE WORD DOCUMENT
  ##########################################################
  
  # Load Word template and add background and findings headers
  sec_doc = officer::read_docx(
    'templates/comparative_report_template.docx'
  ) %>%
    body_add_flextable(add_grey_header(translated_background_header)) %>%
    body_add_par(translated_background_text, style = "Normal") %>%
    body_add_par('\n') %>%
    body_add_flextable(add_grey_header(translated_findings_header))
  
  
  ##########################################################
  ## INSERT HEADER TITLES INTO TEMPLATE
  ##########################################################
  
  sec_doc = sec_doc %>%
    headers_replace_text_at_bkm(
      bookmark = "header",
      value = translated_header_title
    ) %>%
    headers_replace_text_at_bkm(
      bookmark = "header2",
      value = translated_section_header
    )
  
  
  ##########################################################
  ## GENERATE SUBSECTION NARRATIVES
  ##########################################################
  
  # Identify all subsection titles within the section
  sub_sec_titles = unique(sec_report_matrix$sub_section_text)
  
  j = NULL
  
  for(j in sub_sec_titles)
  {
    
    # Extract results corresponding to the current subsection
    sub_sec_report_matrix = sec_report_matrix %>%
      dplyr::filter(sub_section_text == j) %>%
      dplyr::select(-c(sect,arrange_num, sub_section_text, background_text))
    
    
    ########################################################
    ## CLEAN TABLE BEFORE SENDING TO LLM
    ########################################################
    
    # Remove repeated labels to reduce token usage
    cols_to_clean = c("grp_tab_title", "ind_subtitle", "stratifier")
    
    sub_sec_report_matrix[cols_to_clean] =
      lapply(sub_sec_report_matrix[cols_to_clean],
             blank_consecutive_duplicates)
    
    
    ########################################################
    ## REMOVE SIGNIFICANCE VARIABLES IF NOT REQUIRED
    ########################################################
    
    if(report_signf == 'No'){
      sub_sec_report_matrix =
        sub_sec_report_matrix %>%
        dplyr::select(
          -all_of(c('p_value','significance_of_change'))
        )
    }
    
    
    ########################################################
    ## CONVERT TABLE TO CSV TEXT FOR LLM INPUT
    ########################################################
    
    csv_text = paste(
      capture.output(
        write.csv(sub_sec_report_matrix, row.names=FALSE)
      ),
      collapse="\n"
    )
    
    
    ########################################################
    ## CREATE PROMPTS FOR NARRATIVE GENERATION
    ########################################################
    
    #
    prompt1 = paste0(
      "Task:\n",
      "Write a report-ready narrative summary for ", country,
      " in ", language, ". The narrative should synthesise the findings across all tables, ",
      "identify the most important messages, and explain what the results show rather than simply listing numbers.\n\n",
      
      "Instructions:\n",
      "1. Critically review all tables before writing. Identify the most important findings, patterns, ",
      "differences, and trends, and prioritise these in the narrative. Do not describe every number or table unnecessarily.\n",
      
      "2. Highlight statistically significant differences only when p < 0.05. Do not describe non-significant ",
      "differences as meaningful or suggest that an association exists when statistical evidence is lacking.\n",
      
      "3. Where an overall comparison across stratifiers is available, report and interpret it before discussing ",
      "specific subgroup comparisons.\n",
      
      "4. For statistically significant results, report the relevant p-value to four decimal places and include ",
      "95% confidence intervals where available. Ensure that confidence intervals are clearly linked to the corresponding estimates.\n",
      
      "5. Use the numbers to explain the magnitude and direction of findings. Where appropriate, describe which ",
      "groups had higher or lower estimates, the extent of the change, and which findings are most notable.\n",
      
      "6. Go beyond numerical repetition. Translate the statistical results into clear, meaningful messages that help ",
      "the reader understand what the findings indicate. Where the tables support a clear pattern, describe that pattern explicitly.\n",
      
      "7. Distinguish clearly between descriptive differences and statistically significant differences. Do not imply ",
      "explanations, or conclusions that are not supported by the data.\n",
      
      "8. Where several tables or indicators address the same underlying concept, synthesise them into one coherent ",
      "interpretation rather than repeating similar findings. If related indicators are available, focus on the indicator ",
      "that provides the clearest and most informative message unless the additional indicator adds materially different information.\n",
      
      "9. Prioritise findings according to their importance to the reader. Focus on results that show substantial ",
      "changes, important inequalities, notable patterns, or findings that materially contribute to understanding ",
      "the section. Minor or redundant numerical differences should be omitted.\n",
      
      "10. Ensure that the narrative has a logical flow: overall finding → important differences → key subgroup findings ",
      "→ interpretation and implications supported by the data.\n",
      
      "11. Do not simply reproduce table values in sequence. Integrate related findings into sentences and paragraphs ",
      "so that the narrative reads as an analytical interpretation rather than a description of the tables.\n",
      
      "12. Do not start a sentence with a number. Integrate percentages, estimates, counts, confidence intervals, ",
      "and p-values naturally within sentences.\n",
      
      "13. Use precise statistical language. Avoid words such as 'significant' unless the difference meets the ",
      "specified p < 0.05 threshold. Use 'higher', 'lower', 'increased', 'decreased', or 'differed' where appropriate.\n",
      
      "14. Do not overstate findings. Avoid causal language such as 'led to', 'caused', or 'resulted in' unless the ",
      "study design and tables explicitly support a causal interpretation.\n",
      
      "15. Keep the narrative concise but sufficiently detailed to be informative for a formal report. Avoid repetition ",
      "of the same finding, unnecessary methodological explanation, and commentary that does not add interpretation.\n",
      
      "16. End the section with the main takeaway or key message emerging from the results, where appropriate. ",
      "The reader should be able to understand the principal finding without referring back to the tables.\n",
      
      "17. Before finalising, check that all reported numbers, percentages, confidence intervals, and p-values exactly ",
      "match the supplied tables. Do not calculate, infer, or invent values that are not supported by the tables.\n",
      
      "18. Write the final narrative directly in ", language,
      ". Do not provide an introduction, explanation of your approach, bullet-point summary, or commentary about the task. \n",
      'If p-value = 0.0000 then you need to rewrite it as <0.0001 in the narrative.\n',
      "Do not bold, underline, or otherwise format the text. Produce polished, report-ready prose only.",
      "Do not show 95% CIs that are neither interpretable nor plausible"
    )
    
    # Simplified prompt when significance reporting is disabled
    prompt2 <- paste0(
      "Task:\n",
      "Write a report-ready narrative summary for ", country,
      " in ", language, ". The narrative should synthesise the findings across all tables, ",
      "identify the most important messages, and explain what the results show rather than simply listing numbers.\n\n",
      
      "Instructions:\n",
      "1. Critically review all tables before writing. Identify the most important findings, patterns, ",
      "differences, and trends, and prioritise these in the narrative. Do not describe every number or table unnecessarily.\n",
      
      "2. Where an overall comparison across stratifiers is available, report and interpret it before discussing ",
      "specific subgroup comparisons.\n",
      
      "3. Use the numbers to explain the magnitude and direction of findings. Where appropriate, describe which ",
      "groups had higher or lower estimates, the extent of the change, and which findings are most notable.\n",
      
      "4. Go beyond numerical repetition. Translate the statistical results into clear, meaningful messages that help ",
      "the reader understand what the findings indicate. Where the tables support a clear pattern, describe that pattern explicitly.\n",
      
      "5. Do not imply ",
      "explanations, or conclusions that are not supported by the data.\n",
      
      "6. Where several tables or indicators address the same underlying concept, synthesise them into one coherent ",
      "interpretation rather than repeating similar findings. If related indicators are available, focus on the indicator ",
      "that provides the clearest and most informative message unless the additional indicator adds materially different information.\n",
      
      "7. Prioritise findings according to their importance to the reader. Focus on results that show substantial ",
      "changes, important inequalities, notable patterns, or findings that materially contribute to understanding ",
      "the section. Minor or redundant numerical differences should be omitted.\n",
      
      "8. Ensure that the narrative has a logical flow: overall finding → important differences → key subgroup findings ",
      "→ interpretation and implications supported by the data.\n",
      
      "9. Do not simply reproduce table values in sequence. Integrate related findings into sentences and paragraphs ",
      "so that the narrative reads as an analytical interpretation rather than a description of the tables.\n",
      
      "10. Do not start a sentence with a number. Integrate percentages, estimates, counts, confidence intervals, ",
      "and p-values naturally within sentences.\n",
      
      "11. Do not overstate findings. Avoid causal language such as 'led to', 'caused', or 'resulted in' unless the ",
      "study design and tables explicitly support a causal interpretation.\n",
      
      "12. Keep the narrative concise but sufficiently detailed to be informative for a formal report. Avoid repetition ",
      "of the same finding, unnecessary methodological explanation, and commentary that does not add interpretation.\n",
      
      "13. End the section with the main takeaway or key message emerging from the results, where appropriate. ",
      "The reader should be able to understand the principal finding without referring back to the tables.\n",
      
      "14. Before finalising, check that all reported numbers and percentages exactly ",
      "match the supplied tables. Do not calculate, infer, or invent values that are not supported by the tables.\n",
      
      "15. Write the final narrative directly in ", language,
      ". Do not provide an introduction, explanation of your approach, bullet-point summary, or commentary about the task. \n",
      "Do not bold, underline, or otherwise format the text. Produce polished, report-ready prose only."
    )
    
    if(report_signf == 'No'){
      prompt = prompt2
    } else{
      prompt = prompt1
    }
    
    
    ########################################################
    ## GENERATE NARRATIVE USING LLM
    ########################################################
    
    complete_narrative = llm_wrapper_connect(prompt)
    
    
    ########################################################
    ## TRANSLATE SUBSECTION TITLE IF REQUIRED
    ########################################################
    
    if(language!='english'){
      tanslated_sub_header = llm_translate(j)
    } else{
      tanslated_sub_header = j
    }
    
    
    ########################################################
    ## INSERT BULLET PARAGRAPH INTO DOCUMENT
    ########################################################
    
    combined_fpar = fpar(
      ftext(tanslated_sub_header, prop = fp_text(bold = TRUE)),
      ftext(": ", prop = fp_text(bold = TRUE)),
      ftext(complete_narrative)
    )
    
    sec_doc = sec_doc %>%
      body_add_fpar(combined_fpar, style = "bullet")
  }
  
  
  ##########################################################
  ## GENERATE CHARTS FOR SECTION
  ##########################################################
  
  sec_doc = sec_doc %>% body_add_par('\n')
  
  # Identify indicator groups used to generate charts
  indicator_groups = unique(sec_report_matrix$grp_tab_title)
  
  # Generate charts for each indicator group
  chart_list = lapply(
    indicator_groups,
    function(grp) chart_function(indicator_group = grp)
  )
  
  # Split charts into groups of two for layout
  split_groups = split(
    chart_list,
    ceiling(seq_along(chart_list) / 2)
  )
  
  sec_doc = sec_doc %>%
    body_add_break(pos = 'on') %>%
    body_add_flextable(add_grey_header(translates_charts_headr))
  
  
  ##########################################################
  ## INSERT CHARTS INTO WORD DOCUMENT
  ##########################################################
  
  m = NULL
  
  for (m in seq_along(split_groups)) {
    
    gg_charts = plot_grid(
      plotlist = split_groups[[m]],
      ncol = 2,
      rel_heights = rep(
        1,
        ceiling(length(split_groups[[m]]) / 2)
      )
    )
    
    chart_path = paste0(
      getwd(),
      "/outputs/comparative/temp_plots/charts_group_",
      m,
      ".png"
    )
    
    ggsave(
      filename = chart_path,
      plot = gg_charts,
      width = 15,
      height = 11,
      dpi = 600
    )
    
    sec_doc <- sec_doc %>%
      body_add_img(src = chart_path,
                   width = 7.25,
                   height = 4.5)
  }
  
  
  ##########################################################
  ## CLEAN TEMPORARY CHART FILES
  ##########################################################
  
  unlink(
    list.files(
      paste0(getwd(),'/outputs/comparative/temp_plots/'),
      full.names = TRUE,
      recursive = TRUE
    ),
    recursive = TRUE
  )
  
  
  ##########################################################
  ## SAVE SECTION FACTSHEET
  ##########################################################
  
  print(
    sec_doc,
    target = paste0(
      getwd(),
      '/outputs/comparative/',
      translated_section_header,
      '.docx'
    )
  )
}

