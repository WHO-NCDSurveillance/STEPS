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
      "\n OUTPUT REQUIREMENTS:\n",
      "Write the final text directly in ", language, ". Produce concise, polished, factsheet-ready ",
      "prose only. Do not provide an introduction to the task, headings, bullet points, explanations ",
      "of your approach, or commentary. Do not use bold, italics, underlining, Markdown, or any ",
      "other text formatting."
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
    prompt1 <- paste0(
      "Based on the following tables:\n\n",
      csv_text,
      "\n\n",
      
      "TASK:\n",
      "Write a concise, factsheet-ready summary of the findings for ", country,
      " in ", language, ". The purpose of this text is to communicate the most important ",
      "numbers, findings, and takeaways clearly and quickly to a non-technical reader.\n\n",
      
      "The summary should be selective and number-led. Identify the key findings across all ",
      "tables and use the most important statistics to support the main messages. Do not attempt ",
      "to describe every result, category, or table.\n\n",
      
      "KEY PRINCIPLES:\n",
      
      "1. Focus on the most important findings. Select the results that provide the clearest ",
      "and most useful picture of the issue, including headline prevalence estimates, substantial ",
      "differences between groups, important inequalities, notable trends, or particularly large ",
      "or concerning findings.\n",
      
      "2. Summarise key numbers rather than reproducing tables. Include the most important ",
      "percentages, estimates, counts, or other statistics needed to support each main message, ",
      "but omit minor, repetitive, or less informative numbers.\n",
      
      "3. Make the main takeaway clear. The reader should quickly understand what the most ",
      "important finding is and why it matters from the numbers presented.\n",
      
      "4. Prioritise approximately 2 to 5 key messages across the tables, depending on the ",
      "amount and importance of the information available. Do not force every table to contribute ",
      "a finding if it does not add an important message.\n",
      
      "5. Where an overall estimate is available, present the headline finding first before ",
      "describing important differences across population groups.\n",
      
      "6. Where statistically significant overall differences across categories are available, ",
      "report and interpret the overall test before discussing subgroup patterns. Remember that ",
      "an overall p-value tests variation across the levels of a variable and does not identify ",
      "a specific individual category as statistically different.\n",
      
      "7. Highlight statistically significant differences only when p < 0.05. Do not describe ",
      "non-significant differences as statistically meaningful or imply evidence of an association ",
      "when the statistical evidence does not support this.\n",
      
      "8. For important statistically significant findings, report the relevant p-value and ",
      "95% confidence interval where these add useful information. Report p-values to four decimal ",
      "places. If p = 0.0000 in the table, report it as p < 0.0001.\n",
      
      "9. Use confidence intervals selectively. Include them when they help communicate the ",
      "precision of an important estimate, but do not show 95% confidence intervals that are ",
      "not interpretable, plausible, or informative.\n",
      
      "10. Explain the magnitude and direction of important findings. Clearly indicate which ",
      "groups have higher or lower estimates and quantify meaningful differences using the key ",
      "numbers from the tables.\n",
      
      "11. Where several indicators describe the same underlying issue, synthesise them into ",
      "one clear message rather than repeating similar statistics. Prioritise the indicator that ",
      "provides the strongest and clearest evidence.\n",
      
      "12. Translate statistics into clear messages. The output should explain what the numbers ",
      "show, rather than simply listing percentages in the order in which they appear in the tables.\n",
      
      "13. Distinguish between descriptive patterns and statistically supported differences. ",
      "Do not overinterpret small numerical differences.\n",
      
      "14. Do not imply causes, explanations, or consequences that cannot be supported by the ",
      "supplied data. Avoid causal language such as 'caused', 'led to', or 'resulted in'.\n",
      
      "15. Keep the writing concise and suitable for a factsheet. Prefer short, clear paragraphs ",
      "and direct language. Avoid lengthy methodological explanations, unnecessary detail, ",
      "repetition, and technical jargon.\n",
      
      "16. Do not start a sentence with a number. Integrate statistics naturally into sentences.\n",
      
      "17. End with a clear overall takeaway that summarises the principal message emerging ",
      "from the findings.\n",
      
      "ACCURACY CHECK:\n",
      "Before finalising, verify that every reported number, percentage, confidence interval, ",
      "and p-value exactly matches the supplied tables. Do not calculate, infer, estimate, ",
      "or invent values that are not explicitly supported by the tables.\n\n",
      
      "OUTPUT REQUIREMENTS:\n",
      "Write the final text directly in ", language, ". Produce concise, polished, factsheet-ready ",
      "prose only. Do not provide an introduction to the task, headings, bullet points, explanations ",
      "of your approach, or commentary. Do not use bold, italics, underlining, Markdown, or any ",
      "other text formatting."
    )
    
    
    # Simplified prompt when significance reporting is disabled
    prompt2 <- paste0(
      "Based on the following tables:\n\n",
      csv_text,
      "\n\n",
      
      "TASK:\n",
      "Write a concise, factsheet-ready summary of the findings for ", country,
      " in ", language, ". The purpose of this text is to communicate the most important ",
      "numbers, findings, and takeaways clearly and quickly to a non-technical reader.\n\n",
      
      "The summary should be selective and number-led. Identify the key findings across all ",
      "tables and use the most important statistics to support the main messages. Do not attempt ",
      "to describe every result, category, or table.\n\n",
      
      "KEY PRINCIPLES:\n",
      
      "1. Focus on the most important findings. Select the results that provide the clearest ",
      "and most useful picture of the issue, including headline prevalence estimates, substantial ",
      "differences between groups, important inequalities, notable trends, or particularly important findings.\n",
      
      "2. Summarise key numbers rather than reproducing tables. Include the most important ",
      "percentages, estimates, counts, or other statistics needed to support each main message, ",
      "but omit minor, repetitive, or less informative numbers.\n",
      
      "3. Make the main takeaway clear. The reader should quickly understand what the most ",
      "important finding is and what the numbers show.\n",
      
      "4. Prioritise approximately 2 to 5 key messages across the tables, depending on the ",
      "amount and importance of the information available. Do not force every table to contribute ",
      "a finding if it does not add an important message.\n",
      
      "5. Where an overall estimate is available, present the headline finding first before ",
      "describing important differences across population groups.\n",
      
      "6. Use the numbers to explain the magnitude and direction of important findings. Clearly ",
      "indicate which groups have higher or lower estimates and quantify meaningful differences ",
      "using the key numbers from the tables.\n",
      
      "7. Where several indicators describe the same underlying issue, synthesise them into ",
      "one clear message rather than repeating similar statistics. Prioritise the indicator that ",
      "provides the strongest and clearest message.\n",
      
      "8. Translate statistics into clear messages. Explain what the numbers show rather than ",
      "simply listing percentages or describing the tables in sequence.\n",
      
      "9. Prioritise substantial differences, important inequalities, clear patterns, and findings ",
      "that materially improve understanding of the topic. Minor or redundant numerical differences ",
      "should normally be omitted.\n",
      
      "10. Do not imply causes, explanations, or consequences that cannot be supported by the ",
      "supplied data. Avoid causal language such as 'caused', 'led to', or 'resulted in'.\n",
      
      "11. Keep the writing concise and suitable for a factsheet. Prefer short, clear paragraphs ",
      "and direct language. Avoid lengthy methodological explanations, unnecessary detail, ",
      "repetition, and technical jargon.\n",
      
      "12. Do not start a sentence with a number. Integrate statistics naturally into sentences.\n",
      
      "13. End with a clear overall takeaway that summarises the principal message emerging ",
      "from the findings. The reader should be able to understand the main message without ",
      "referring back to the tables.\n",
      
      "ACCURACY CHECK:\n",
      "Before finalising, verify that every reported number, percentage, or other estimate ",
      "exactly matches the supplied tables. Do not calculate, infer, estimate, or invent values ",
      "that are not explicitly supported by the tables.\n\n",
      
      "OUTPUT REQUIREMENTS:\n",
      "Write the final text directly in ", language, ". Produce concise, polished, factsheet-ready ",
      "prose only. Do not provide an introduction to the task, headings, bullet points, explanations ",
      "of your approach, or commentary. Do not use bold, italics, underlining, Markdown, or any ",
      "other text formatting."
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




