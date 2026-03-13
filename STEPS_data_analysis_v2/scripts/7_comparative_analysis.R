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
    'templates/template_comparative_factsheet.docx'
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
    
    prompt1 = paste0(
      "Tables (in CSV format):\n", csv_text, "\n",
      "Now write a concise narrative summary for this section for ",
      country,' in ',language,
      ". Include p-values when significant changes occur (p<0.05)."
    )
    
    prompt2 = paste0(
      "Tables (in CSV format):\n", csv_text, "\n",
      "Now write a concise narrative summary for this section for ",
      country,' in ',language,"."
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