########################################################################
#https://console.groq.com/keys;models - llama3-8b-8192, gemma2-9b-it, meta-llama/llama-4-scout-17b-16e-instruct
#https://console.groq.com/docs/rate-limits


llm_wrapper_groq = function(prompt_text, model = "llama-3.3-70B-Versatile", max_tokens = 8192) {
  # API key
  api_key = Sys.getenv("GROQ_API_KEY")
  
  # Ensuring UTF-8 encoding to avoid warning
  prompt_text = enc2utf8(prompt_text)
  
  # Rough token count estimate (~4 chars per token)
  #estimated_tokens = ceiling(nchar(prompt_text) / 4)
  
  # Switch model if estimated tokens > 6000
  # if (estimated_tokens > 6000) {
  #   message("Estimated tokens exceed 6000, switching model to llama-4-scout...")
  #   model = "meta-llama/llama-4-scout-17b-16e-instruct"
  # }
  # Function to send request
  make_request = function() {
    httr::POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = model,
        messages = list(list(role = "user", content = prompt_text)),
        max_tokens = max_tokens,
        temperature = 0.7,
        top_p = 0.9
      ), auto_unbox = TRUE)
    )
  }
  
  attempt = 1
  repeat {
    response = make_request()
    #
    if (response$status_code == 200) {
      result = httr::content(response, as = "parsed", encoding = "UTF-8")
      return(result$choices[[1]]$message$content)
    }
    
    error_msg = httr::content(response, as = "parsed", encoding = "UTF-8")
    
    # Handling rate limit exceeded dynamically
    if (!is.null(error_msg$error$code) && error_msg$error$code == "rate_limit_exceeded") {
      wait_time = 10 # fallback wait in case parsing fails
      if (grepl("try again in ([0-9.]+)s", error_msg$error$message)) {
        wait_time = as.numeric(sub(".*try again in ([0-9.]+)s.*", "\\1", error_msg$error$message))
      }
      message(sprintf("Rate limit hit. Waiting %.2f seconds before retry (attempt %d)...", wait_time, attempt))
      Sys.sleep(wait_time)
      attempt = attempt + 1
      next
    }
    
    stop("API call failed: ", jsonlite::toJSON(error_msg, auto_unbox = TRUE))
  }
}

##################
## Helper functions
llm_translate = function(text) {
  llm_wrapper_groq(
    paste0(
      "Translate into ", language, 
      #" only if the text is not already in English. ",
      #"If it is already in English, return it exactly as provided. ",
      "The text to be translated is:\n",
      text,'\n\n',
      #"Otherwise return the text exactly the way it is if already in English.
      "Do not add any notes, explanations, introductions, or extra words. 
      Provided just the needed text only."
    )
  )
}

#######
if(language!='english')
{
  translated_background_header = llm_translate("Background")
  translated_measures_header   = llm_translate("Survey Measures")
  translated_findings_header   = llm_translate("Findings")
  translates_charts_headr = llm_translate("Charts")
  translated_ref_list = llm_translate("Reference List")
} else{
  translated_background_header = "Background"
  translated_measures_header   = "Survey Measures"
  translated_findings_header   = "Findings"
  translated_ref_list = "Reference List"
  translates_charts_headr = "Charts"
}
####

####
i = NULL
sect_no = 1
all_references = NULL
for(i in unique(reporting_matrix$section_title))
{
  #print(i)
  ###Data subset for reporting
  section_data_tables = rev_comp_indicator_results %>% dplyr::filter(section_title == i) %>%
                        dplyr::select(-background_text)
  ##Linking with matrix
  sec_report_matrix = reporting_matrix %>% dplyr::filter(section_title == i)%>% arrange(order_ind)
  ##
  bacground_text = unique(sec_report_matrix$background)
  survey_measures = unique(sec_report_matrix$survey_measures)
  ##Adjusting background text and survey measures using LLM
  adj_background_text = llm_wrapper_groq(
    paste0("Adjust the following text, adding critical statistics in the background for ",
           country, ": ", bacground_text,
           ". Just provide the output without any notes, explanations, introductions, or extra words. Insert 
           academic style references within the text, but do not list them."))
  
  ##Reference list
  # Step 2: Ask the LLM for a full reference list
  list_reference = llm_wrapper_groq(
    paste0(
      "From the following text, generate a clean list of full references with all publication details (authors, year, title, journal, volume, pages). ",
      "Output only the references, one per line, in APA format.\n\n",
      adj_background_text
    )
  )  
  #Split by double line breaks
  reference_list = unlist(strsplit(list_reference, "\n\n"))
  reference_list = unlist(strsplit(reference_list, "\n"))
  
  # Trim whitespace (optional)
  reference_list = trimws(reference_list)
  
  adj_survey_measures = llm_wrapper_groq(
    paste0("Insert the country name into the following text without changing it. ",
           "Output only the result: ", survey_measures,
           " Country name is: ", country,
           ". Just provide the output without any notes, explanations, introductions, or extra words.")
  )
  
  if(language!='english')
  {
    ## Translate adjusted text
    translated_background_text = llm_translate(adj_background_text)
    translated_survey_measures = llm_translate(adj_survey_measures)
    #
    refs = paste(capture.output(write.csv(reference_list, row.names=FALSE)), collapse="\n")
    translated_ref = llm_translate(refs)
    translated_ref = unlist(strsplit(translated_ref, "\n"))[-1]
    
    ## Translate section header
    translated_section_header = llm_translate(i)
  }else{
    translated_background_text = adj_background_text
    translated_survey_measures = adj_survey_measures
    translated_section_header = i
    translated_ref = reference_list
  }
  
  ###Formatting survey measures text####
  text_parts = str_split(translated_survey_measures, "•")[[1]]
  intro_text = str_trim(text_parts[1])          # text before first bullet
  #bullet_items = paste0("• ",text_parts[-1] %>% str_trim())  # all bullets 
  bullet_items = text_parts[-1] %>% str_trim()  # all bullets 
  
  ###
  sec_doc = officer::read_docx('section_templates/section_template.docx') %>%
    body_add_par(translated_section_header,   style = "heading 1", pos = 'on') %>%
    body_add_par(translated_background_header, style = "heading 2") %>%
    body_add_par(translated_background_text,   style = "JustifiedNormal") %>%
    body_add_par(translated_measures_header,   style = "heading 2")%>%
    body_add_par(intro_text, style = "JustifiedNormal")
  
  # adding bullets one by one
  for (item in bullet_items) {
    sec_doc = sec_doc %>% body_add_par(item, style = "bullet")
  }
  
  sec_doc = sec_doc %>% body_add_par(translated_findings_header,   style = "heading 2")
  
  
  ##
  sub_sec_titles = unique(sec_report_matrix$sub_section_text)
  j = NULL
  for(j in sub_sec_titles)
  {
    sub_sec_report_matrix = sec_report_matrix %>% dplyr::filter(sub_section_text == j)
    sub_sec_tab = (sub_sec_report_matrix %>% arrange(table_order))$arrange_num
    ##
    sub_section_data_tables = section_data_tables %>% dplyr::filter(sub_section_text == j)%>%
                              dplyr::select(-c(arrange_num,sub_section_text))
    
    ##Cleaning p - values
    # sub_section_data_tables = sub_section_data_tables %>%
    #   group_by(stratifier) %>%
    #   mutate(
    #     p_value = if (any(!is.na(p_value))) {
    #       ifelse(row_number() == min(which(!is.na(p_value))), p_value, NA_real_)
    #     } else {
    #       NA_real_
    #     },
    #     p_value = round(p_value, 4)
    #   ) %>%
    #   ungroup()
    
    ####Cleaning dataframe to reduce the use of tokens
    cols_clean = c("sect","grp_tab_title", "ind_subtitle", "stratifier")
    
    blank_consecutive_duplicates = function(x) {
      x[c(FALSE, x[-1] == x[-length(x)])] = ""
      x
    }
    sub_section_data_tables[cols_clean] = lapply(sub_section_data_tables[cols_clean], blank_consecutive_duplicates)    
    ##Further cleaning
    sub_section_data_tables = sub_section_data_tables %>%
                              mutate(p_value = ifelse(stratifier!='',p_value,NA),
                                     significance = ifelse(stratifier!='',significance,NA))
    ##subsetting table based on the need to report significance
    subset_names = c(grep('ci_high|ci_low',names(sub_section_data_tables), v=T),'p_value','significance')
    if(report_signf == 'No'){sub_section_data_tables = sub_section_data_tables %>% dplyr::select(-all_of(subset_names))}
    
    ##
    all_tables_text = paste(capture.output(write.csv(sub_section_data_tables, row.names=FALSE)), collapse="\n")
    #
    #all_tables_text = enc2utf8(paste(all_tables_text, collapse = "\n"))
    #
    sample_narration = na.omit(unique(sub_sec_report_matrix$text_example))
    ##
    # prompt = paste0(
    #   "Tables:\n", all_tables_text, "\n",
    #   "Here is an example of how a similar section was narrated:\n",
    #   sample_narration, "\n",
    #   "Now write a concise narrative summary for this section for ",country,' in ',language,
    #   ", covering all tables and in the same format as the text example (and covering all aspects and key messages in the text example). Please write the
    #    narrative directly, and do not start with here is a concise narrative summary.Interpret all the tables
    #    and please examine significance of any differences (only highlight what is significant - whenever there are significant differences). Provide p-values
    #    for these in four decimal places. For p-values = 0.0000, show them as <0.0001. P-values indicate whether ther is a difference overall each stratifier, but not specific
    #    to stratifier level, for example, the p - values would show whether there is a difference by age as a variable. Difference is considered significant if p-value < 0.05.
    #    Together with percentage estimates, provide the corresponding 95% CIs. Let the summary be as concise as possible without repetitions.")
    # 
    
    
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
      "4. Report p-values to four decimal places. If a p-value equals 0.0000, show it as <0.0001 (always report p-values in 4 decimal points).\n",
      "5a. P-values indicate overall differences across each stratifier (e.g., age, sex), not within specific levels. Only one p-value should be reported per test.\n",
      "5b. When a significant difference exists, state it clearly without repetition. For example, instead of: \n",
      "   'Prevalence was significantly higher among men (19.8%, 95% CI: 17.3–22.6%) compared with women (2.4%, 95% CI: 1.7–3.3%), with a significant difference (p < 0.0001)',\n",
      "   write: \n",
      "   'Prevalence was significantly higher among men (19.8%, 95% CI: 17.3–22.6%) compared with women (2.4%, 95% CI: 1.7–3.3%) (p < 0.0001)'.\n",
      "6. For each percentage estimate, report the 95% confidence interval (CI).\n",
      "7. Keep the summary concise, non-repetitive, and focused on key messages and insights.\n\n",
      "Write the final narrative directly — do not preface it with phrases like 'Here is the summary'."
    )
    
    #
    prompt2 <- paste0(
      "You are provided with the following tables:\n\n",
      all_tables_text, "\n\n",
      "Task:\n",
      "Write a concise narrative summary for this section for ", country, " in ", language, ".\n\n",
      "Instructions:\n",
      "1. Interpret all tables and describe the main findings clearly.\n",
      "2. Keep the summary concise, non-repetitive, and focused on key messages and insights.\n\n",
      "Write the final narrative directly — do not preface it with phrases like 'Here is the summary'."
    )
    ###selecting prompt based on the need to report significance
    if(report_signf == 'No'){prompt = prompt2} else{ prompt = prompt1}
    
    ###
    prompt = enc2utf8(paste(prompt, collapse = "\n"))
    ##Calling the wrapper
    complete_narrative = llm_wrapper_groq(prompt)
    ##
    #translated_narrative = llm_translate(complete_narrative)
    if(language!='english')
    {
      tanslated_sub_header = llm_translate(j)
    }else{tanslated_sub_header = j}
    #
    sec_doc = sec_doc %>% body_add_par(tanslated_sub_header, style = "heading 3")
    sec_doc = sec_doc %>% body_add_par(complete_narrative, style = "JustifiedNormal")##translated_narrative
  }
  
  print(sec_doc,target=paste0(getwd(),'/report outputs/report sections/',sect_no,'_section_file.docx')) 
  sect_no=sect_no+1
  all_references = c(all_references, translated_ref)
}

#####
##Combining sections
all_section_reports = eval(parse(text = paste0('c(',paste0('"report outputs/report sections/',1:length(unique(reporting_matrix$section_title)),
                                                           '_section_file.docx"',collapse = ','),')')))
###
combined_report = officer::read_docx('section_templates/section_template.docx')
# Loop through each section and add content to databook
for(i in all_section_reports) {
  
  # Add the content of the current document
  combined_report = combined_report %>% officer::body_add_docx(i, pos = "after")  # Insert the document content
  
  # Add a page break after each part, except the last one
  if (i != all_section_reports[length(all_section_reports)]) {
    combined_report = combined_report %>% body_add_break(pos = "after") # Start a new section (on a new page)
  }
}

####Adding reference list

combined_report = combined_report %>% #body_add_break(pos = "after")%>% 
                  body_add_par(translated_ref_list,   style = "heading 1")
####
item = NULL
for (item in all_references) {
  combined_report = combined_report %>% body_add_par(item, style = "bullet")
}


#######
print(combined_report, target = paste0('report outputs/combined_report', '.docx'))

