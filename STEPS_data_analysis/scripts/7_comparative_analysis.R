# -------------------------------
# Preparing datasets
# -------------------------------
dataset1 = analysis_data
source('scripts/functions/processing_dataset2.R', local = TRUE)
dataset2 = analysis_dataset2
# 
common_variables = intersect(names(dataset1), names(dataset2))

###Conversion to common variable types
# Create a list of classes from dataset1 for the common variables
target_classes = sapply(dataset1[common_variables], class)

# Function to safely convert a column(x) to a target class (target_class)
convert_to_target_class = function(x, target_class) {
  # Get the appropriate 'as.' function (eg as.numeric, as.factor)
  conversion_function = match.fun(paste0("as.", target_class))
  # Apply the conversion
  return(conversion_function(x))
}

# Apply the conversion across all common columns in dataset2 using mutate and across
dataset2 = dataset2 %>%
  mutate(across(
    all_of(common_variables),
    ~ convert_to_target_class(., target_classes[cur_column()])
  ))

#############Normalisation######################
# Function to normalise wstep variables in any dataset
normalize_wstep_vars = function(datum, prefix = "wstep", postfix = "_norm") {
  
  # Find all columns starting with the prefix
  wstep_vars <- grep(paste0("^", prefix), names(datum), value = TRUE)
  # If no matching columns, return dataset as is
  if (length(wstep_vars) == 0) return(datum)
  # Normalise and store with postfix
  datum %>%
    dplyr::mutate(
      dplyr::across(
        all_of(wstep_vars),
        ~ .x / sum(.x, na.rm = TRUE),
        .names = "{.col}{postfix}"
      )
    )
}

###
dataset1 = dataset1 %>%
            dplyr::select(all_of(common_variables)) %>%
            dplyr::mutate(svy_year = survey_year) %>%
            normalize_wstep_vars(postfix = "_norm")
##
dataset2 = dataset2 %>%
            dplyr::select(all_of(common_variables)) %>%
            dplyr::mutate(svy_year = previous_survey_year) %>%
            normalize_wstep_vars(postfix = "_norm")

# dataset1 = dataset1 %>%
#   dplyr::select(all_of(common_variables)) %>%
#   mutate(svy_year = survey_year,
#          wstep1_norm = wstep1 / sum(wstep1, na.rm = TRUE),
#          wstep2_norm = wstep2 / sum(wstep2, na.rm = TRUE),
#          wstep3_norm = wstep3 / sum(wstep3, na.rm = TRUE))
# 
# dataset2 = dataset2 %>%
#   dplyr::select(all_of(common_variables)) %>%
#   mutate(svy_year = previous_survey_year,
#          wstep1_norm = wstep1 / sum(wstep1, na.rm = TRUE),
#          wstep2_norm = wstep2 / sum(wstep2, na.rm = TRUE),
#          wstep3_norm = wstep3 / sum(wstep3, na.rm = TRUE))

combined_dataset = full_join(dataset1, dataset2)

###NOTE: Case by case: agerange should be similar between two surveys
combined_dataset = combined_dataset %>%
                    mutate(
                           ##Overwriting agerange variable
                           agerange = case_when(age>=18 & age <29 ~1,age>=30 & age <44 ~2,
                                                age>=45 & age <59 ~3,age>=60 & age <69 ~4),
                           agerange = factor(agerange,levels=1:4, labels=c('18-29','30-44','45-59','60-69')),
                           ######
                           sex_age = case_when(sex == 'Men' & (agerange=='18-29'|agerange=='30-44') ~ 1,
                                               sex == 'Men' & (agerange=='45-59'|agerange=='60-69') ~ 2,
                                               sex == 'Women' & (agerange=='18-29'|agerange=='30-44') ~ 3,
                                               sex == 'Women' & (agerange=='45-59'|agerange=='60-69') ~ 4),
                           sex_age = factor(sex_age, levels = 1:4, 
                                            labels = c('Men 18 - 44','Men 45 - 69','Women 18 - 44','Women 45 - 69')),
                           bin_age = case_when(agerange=='18-29'|agerange=='30-44' ~ 1,
                                               agerange=='45-59'|agerange=='60-69' ~ 2),
                           bin_age = factor(bin_age,levels = 1:2, labels = c('18-44','45-69')))%>%
                  dplyr::filter(!is.na(agerange))

###Deriving comparative_reporting_matrix
reporting_matrix_v2 = indicator_matrix_v2 %>% dplyr::filter(!is.na(section_title))
common_ind_desc = intersect(reporting_matrix$indicator,reporting_matrix_v2$indicator)
#
comparative_reporting_matrix = reporting_matrix %>%
                               dplyr::filter(eval(parse(text = paste0('indicator == "',common_ind_desc,'"', collapse = '|'))))


###Run the script to process the combined dataset: deriving indicators
#######
comp_stratifiers = c(col_strat_variable, row_strat_variables, "sex_age")

#################################Helper functions#################################
# -------------------------------
# Indicator computation function
# -------------------------------
compute_indicator = function(ind_level, type_indicators, subset_indicators, design) {
  ind_type = tolower(type_indicators[grep(ind_level, subset_indicators)][1])
  if (is.na(ind_type)) stop("Indicator type not found for: ", ind_level)
  
  formula = as.formula(paste0("~", ind_level))
  fun = switch(ind_type,
                "mean" = function(x, design, ...) svymean(x, design = design, na.rm = TRUE),
                "median" = function(x, design, ...) svyquantile(x, design = design, quantiles = 0.5, ci = FALSE, na.rm = TRUE),
                "categorical" = function(x, design, ...) svyciprop(x, design = design, method = "lo", level = 0, na.rm = TRUE),
                stop("Unknown indicator type: ", ind_type)
  )
  
  list(fun = fun, formula = formula, type = ind_type)
}

# -------------------------------
# Wide table computation function
# -------------------------------
compute_wide_tab = function(indicator, svy_datum, strat = NULL) {
  by_formula = as.formula(if (is.null(strat)) "~svy_year" else paste0("~", strat, "+ svy_year"))
  
  df = svyby(
    indicator$formula,
    by = by_formula,
    design = svy_datum,
    FUN = indicator$fun,
    keep.var = FALSE
  ) %>% as.data.frame()
  
  is_categorical = indicator$type == "categorical"
  
  if (!is.null(strat)) {
    colnames(df)[1:3] = c("category", "svy_year", "statistic")
    df = mutate(df, stratifier = strat)
  } else {
    colnames(df)[1:2] = c("svy_year", "statistic")
    df = mutate(df, stratifier = "Total", category = "Total")
  }
  
  if (is_categorical) {
    df = df %>% mutate(statistic = as.numeric(statistic) * 100)
  }
  
  df_wide = df %>%
    pivot_wider(names_from = svy_year, values_from = statistic) %>%
    mutate(across(tail(names(.), 2), ~ round(as.numeric(.), 0)))
  
  last_cols = tail(names(df_wide), 2)
  df_wide = df_wide %>%
    mutate(change = as.numeric(.data[[last_cols[2]]]) - as.numeric(.data[[last_cols[1]]]))
  
  df_wide %>% mutate(across(everything(), as.character))
}

# -------------------------------
# Function to compute p-values
# -------------------------------
compute_pvalue = function(ind_level, indicator, svy_datum, strat_col = NULL, strat_val = NULL) {
  # Subset survey design if stratifier provided
  subset_design = if (!is.null(strat_col) && !is.null(strat_val)) {
    subset(svy_datum, get(strat_col) == strat_val)
  } else {
    svy_datum
  }
  
  # Checking if subset has any observations
  if (nrow(subset_design$variables) == 0) return(NA_real_)
  
  # Compute p-value
  p = tryCatch({
    if (indicator$type %in% c("mean", "median")) {
      subset_design$variables[[ind_level]] = as.numeric(subset_design$variables[[ind_level]])
      test = svyttest(as.formula(paste0(ind_level, " ~ svy_year")), design = subset_design)
      test$p.value
    } else if (indicator$type == "categorical") {
      subset_design$variables[[ind_level]] = factor(subset_design$variables[[ind_level]])
      test = svychisq(as.formula(paste0("~", ind_level, "+ svy_year")), design = subset_design, statistic = "Chisq",simulate.p.value = TRUE)
      #test = chisq.test(svytable(as.formula(paste0("~", ind_level, " + ", svy_year)),design = subset_design, statistic = "Chisq"),correct = TRUE) 
      test$p.value
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  
  round(p, 4)
}

# -------------------------------
# Wrapper function to compute indicator with stratifiers, totals, p-values (showing significance)
# -------------------------------
analyse_indicator = function(ind_level, type_indicators, subset_indicators, svy_datum, sect,section_title, grp_tab_title, 
                             ind_subtitle,arrange_num,sub_section_text,background_text) {
  indicator = compute_indicator(ind_level, type_indicators, subset_indicators, svy_datum)
  
  # Stratified results
  strat_results = lapply(comp_stratifiers, function(s) {
    df = compute_wide_tab(indicator, svy_datum, strat = s)
    df = df %>% rowwise() %>%
      mutate(
        p_value = compute_pvalue(ind_level, indicator, svy_datum, strat_col = s, strat_val = category),
        significance_of_change = case_when(
          #change == 0 ~ "No change",
          !is.na(p_value) & p_value < 0.05 ~ "Significant",
           is.na(p_value) | p_value >= 0.05 |change == 0 ~ "Not significant",
          TRUE ~ NA_character_
        )
      ) %>% ungroup()
    df
  }) %>% bind_rows()
  
  # Totals
  total_results = compute_wide_tab(indicator, svy_datum)
  total_results = total_results %>% rowwise() %>%
    mutate(
      p_value = compute_pvalue(ind_level, indicator, svy_datum),
      significance_of_change = case_when(
        #change == 0 ~ "No change",
        !is.na(p_value) & p_value < 0.05 ~ "Significant",
         is.na(p_value) | p_value >= 0.05 |change == 0 ~ "Not significant",
        TRUE ~ NA_character_
      )
    ) %>% ungroup()
  
  bind_rows(total_results, strat_results) %>%
    mutate(
      sect = sect,
      section_title = section_title,
      grp_tab_title = grp_tab_title,
      ind_subtitle = ind_subtitle,
      arrange_num = arrange_num,
      sub_section_text = sub_section_text,
      background_text = background_text
    ) %>%
    dplyr::select(sect, grp_tab_title, ind_subtitle, stratifier, category, everything())
}

# -------------------------------
# Main function to compute numbers for indicators per section
# -------------------------------
comp_numbers = function(sect) {
  data = combined_dataset
  section_matrix = comparative_reporting_matrix %>% filter(section == sect)
  # wt_step = unique(section_matrix$weight_step)[1]
  # data = data %>% filter(!is.na(get(wt_step)))
  # svy_data = svydesign(id = ~psu, weights = ~get(wt_step), strata = ~stratum, data = data, nest = TRUE)
  #### Section-specific age grouping
  # if (sect == "Cardiovascular disease risk") {
  #   data = data %>%
  #     mutate(agerange = case_when(age >= 40 & age < 55 ~ 1,
  #                                 age >= 55 & age < 70 ~ 2),
  #            agerange = factor(agerange, levels = 1:2, labels = c("40-54","55-69")))
  #   svy_data = svydesign(id = ~psu, weights = ~wstep3, strata = ~stratum, data = data, nest = TRUE)
  # } else if (sect == "Summary of Combined Risk Factors") {
  #   data = data %>%
  #     mutate(agerange = case_when(age >= 18 & age < 45 ~ 1,
  #                                 age >= 45 & age < 70 ~ 2),
  #            agerange = factor(agerange, levels = 1:2, labels = c("18-44","45-69")))
  #   svy_data = svydesign(id = ~psu, weights = ~wstep2, strata = ~stratum, data = data, nest = TRUE)
  # }
  
  #########
  section_results = NULL
  
  for (i in 1:nrow(section_matrix)) {
    sub_matrix = section_matrix[i,]
    subset_indicators = strsplit(sub_matrix$indicator_var, ";")[[1]]
    type_indicators = strsplit(sub_matrix$type, ";")[[1]]
    denom_logic = strsplit(sub_matrix$pop_subset, ";")[[1]]
    
    grp_tab_title = sub_matrix$table_title
    tab_subtitle1 = strsplit(sub_matrix$subtitle1, ";")[[1]]
    tab_subtitle2 = sub_matrix$subtitle2 %>% strsplit(";") %>% unlist() %>% strsplit(":") %>% unlist()
    arrange_num = sub_matrix$arrange_num
    sub_section_text = sub_matrix$sub_section_text
    section_title = sub_matrix$section_title
    background_text = sub_matrix$background
    
    if (!all(is.na(tab_subtitle2))) tab_subtitle1 = tab_subtitle2
    ###
    ####Defining survey design structure
    wt_step = unique(sub_matrix$weight_step)[1]
    data[,wt_step] = as.numeric(as.character(data[,wt_step]))
    ##Setting arbitrary weights 0 to missing survey weights: This is later to preserve the design during analysis
    data[,wt_step][is.na(data[,wt_step])] = 0
    svy_data = svydesign(id=~psu, weights=~get(wt_step),strata=~stratum, data=data,nest = T)
    ###
    
    for (ind_level in subset_indicators) {
      if(!all(is.na(data[[ind_level]])))
      {
      #print(ind_level)
      ind_position = grep(ind_level, subset_indicators)
      denom_condition = denom_logic[ind_position]
      ind_subtitle = tab_subtitle1[ind_position]
      
      # Subset data
      if (denom_condition == "all") {
        svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange))
      } else {
        svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange) &
                              eval(parse(text = paste0("(", denom_condition, ")"))) & get(wt_step)!=0)
      }
      
      comb_rslts = analyse_indicator(ind_level, type_indicators, subset_indicators, svy_datum,
                                      sect,section_title, grp_tab_title, ind_subtitle,arrange_num,sub_section_text,background_text)
      section_results = bind_rows(section_results, comb_rslts)
      }else{}
    }
  }
  
  return(section_results)
}

# -------------------------------
# Generating numbers for all sections
# -------------------------------
#comp_indicator_results = do.call(rbind, lapply(unique(comparative_reporting_matrix$section), comp_numbers))
# Using multisession for cross-platform compatibility (Windows, Mac, Linux)
# cores_detected = parallel::detectCores()
# analysis_cores = ifelse(cores_detected == 1, 1, cores_detected - 4) # leave 1 core free
# plan(multisession, workers = analysis_cores)  

# Parallel computation of indicator results
comp_indicator_results_list = future_lapply(
  unique(comparative_reporting_matrix$section),
  FUN = comp_numbers
)
# Combine results into a single data frame
comp_indicator_results = do.call(rbind, comp_indicator_results_list) %>% arrange(arrange_num)
# system.time({
#   comp_indicator_results = do.call(rbind, comp_indicator_results_list)
# })


# -------------------------------
# Generating comparative factsheet per section
# -------------------------------
##-Helper for adding headings to the comparative factsheet
add_grey_header = function(text, doc_width_in_inches = 7.3, min_row_height = NULL) {
  df = data.frame(text = text, stringsAsFactors = FALSE)
  
  header_row = flextable(df) %>%
    delete_part(part = "header") %>%
    set_table_properties(layout = "fixed", width = 1) %>%
    width(j = 1, width = doc_width_in_inches) %>%
    align(i = 1, j = 1, align = "left", part = "body") %>%
    bg(i = 1, j = 1, bg = "#D9D9D9") %>%
    bold(i = 1, j = 1) %>%
    fontsize(i = 1, j = 1, size = 12) %>%
    padding(i = 1, j = 1, padding.top = 2, padding.bottom = 2) %>%
    border_remove()  # REMOVE ALL BORDERS
  
  if (!is.null(min_row_height)) {
    header_row = height(header_row, height = min_row_height, part = "body")
  }
  return(header_row)
}
#
#clear contents of the folder with comparative factsheets/reports
unlink(list.files(paste0(getwd(),'/outputs/comparative'), full.names = TRUE, recursive = TRUE), recursive = TRUE)

#############################Helper function###########################
chart_function = function(indicator_group = unique(sec_report_matrix$grp_tab_title)[1])
{
  ###Indicator type
  type_data = comparative_reporting_matrix %>% dplyr::filter(table_title == indicator_group)%>%
              dplyr::select(type)
  ind_type = strsplit(type_data$type, ";")[[1]][1]
  ##### 
  test_data <- sec_report_matrix %>%
    dplyr::filter(grp_tab_title == indicator_group,
           stratifier %in% c("Total", "sex")) %>%
    mutate(across(matches("^\\d{4}$"), as.numeric),
           change = as.numeric(change),
           max_value = pmax(!!!select(., matches("^\\d{4}$")), na.rm = TRUE),
           num_category = recode(category, "Total" = 1, "Men" = 2, "Women" = 3),
           category_level = paste0(ind_subtitle, ": ", num_category),
           category_label = paste0(ind_subtitle, ": ", category)
           ) %>%
    rowwise() %>%
    mutate(category = factor(category_level,
      levels = category_level,
      #labels = category_label
      labels = ifelse(language == "english", category_label, llm_translate(category_label))
      ),
      category = gsub('NA:|NA :|NA :| NA:| NA :| NA :','',category),
    ) %>%
    ungroup()%>%
    mutate(category = factor(category, 
                             levels = unique(category),
                             labels = unique(category)))
  
  # === Prepare Plot Data ===
  main_plot_data <- select(test_data, category, matches("^\\d{4}$"), change)
  
  test_df_long <- main_plot_data %>%
    select(-change) %>%
    pivot_longer(-category)
  
  # === Plot 1: Trend Plot ===
  nudge_value=.6
  num_height = length(unique(test_df_long$category))
  
  p1 <- ggplot(test_df_long, aes(x = value, y = category)) +
    geom_line(aes(group = category), color = "#E7E7E7", linewidth = 5) +
    geom_point(aes(color = name), size = 6.5) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 15),
      axis.text.y = element_text(color = "black", size = 15),
      axis.text.x = element_text(color = "#989898", size = 13.5),
      #plot.title = element_text(size = 25, face = "bold"),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    scale_color_manual(values = c("#436685", "purple")) +
    #scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"))+
    guides(color = guide_legend(title = NULL))
  
  if(ind_type == 'categorical')
  {
    p1 = p1 + scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%"))
  } else{}
  
  # === Prepare Gap Data ===
  df_gap <- test_data %>%
    mutate(
      change_label = ifelse(change <= 0, change, paste0("+", change))#,
      #change_label = ifelse(significance_of_change=='Significant',paste0(change_label,'*'),change_label),
      #
      # chart_color = case_when(
      #   change < 0 & p_value < 0.05 ~ 1,
      #   change > 0 & p_value < 0.05 ~ 2,
      #   TRUE ~ 3
      # )
      
    )
  
  # === Plot 2: Change Labels ===
  #lab_change <- "▲"
  lab_change <- ""
  
  p_gap <- ggplot(df_gap, aes(x = change, y = category)) +
    geom_text(aes(x = 0, label = change_label),
              fontface = "bold", size = 5) +
    geom_text(aes(x = 0, y = num_height), label = lab_change,
              nudge_y = .5, fontface = "bold", size = 5) +
    theme_void() +
    coord_cartesian(xlim = c(-.0, 0.0), ylim = c(1, num_height)) +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      panel.background = element_rect(fill = "#EFEFE3", color = "#EFEFE3"),
      legend.position = "none"
    ) #+
    #scale_color_manual(values = c("darkgoldenrod", "red", "green"))
  
  # === Combine Plots ===
  chart_title <- ifelse(language == "english",
                        unique(test_data$grp_tab_title),
                        llm_translate(unique(test_data$grp_tab_title)))
  
  ##Chart position
  #chart_position = grep(paste0("^",unique(test_data$grp_tab_title),"$"),indicator_groups)
  chart_position = which(indicator_groups %in% unique(test_data$grp_tab_title))
  mod_title = as.character(paste0(chart_position,': ',chart_title))
  #p1 <- p1 + coord_cartesian(clip = "off")
  p_whole <- p1 + p_gap +
    plot_layout(design = c(
      area(l = 0, r = 43, t = 0, b = 1),
      area(l = 44, r = 52, t = 0, b = 1)
    )) +
    plot_annotation(
      title = mod_title,
      theme = theme(
        plot.title = element_textbox_simple(
          halign = 0.5,         # center align
          size = 20,
          face = "bold",
          lineheight = 1.1,
          margin = margin(t = 6, b = 6),
          width = unit(1, "npc")  # ensures it fits within the plot width
        ),
        plot.margin = margin(15, 15, 10, 15),
        plot.title.position = "plot"  # keeps title within the chart area
      )
    ) &
    theme(plot.margin = margin(15, 15, 15, 15))
  
  print(p_whole)
}


##############################################
i = NULL
for(i in unique(comp_indicator_results$section_title))#sect
{
  sec_report_matrix = comp_indicator_results %>% dplyr::filter(section_title == i)
  ##
  bacground_text = paste0(unique(sec_report_matrix$background_text), collapse = ' ')
  ##Adjusting background text and survey measures using LLM
  adj_background_text = llm_wrapper_groq(
    paste0("Adjust the following text, adding critical statistics in the background for ",
           country, ": ", bacground_text,
           "Just provide the output without any notes, explanations, introductions, or extra words.")
  )
  
  ##Header title
  header_title = paste0('Comparison Fact Sheet: ',country, ' ', previous_survey_year,' & ',survey_year)
  
  if(language!='english')
  {
    ## Translate adjusted text
    translated_background_text = llm_translate(adj_background_text)
    ## Translate section header
    translated_section_header = llm_translate(gsub('/',' or ',i))
    ##
    translated_header_title = llm_translate(header_title)
    
  }else{
    translated_background_text = adj_background_text
    translated_section_header = gsub('/',' or ',i)
    translated_header_title = header_title
  }
 
  ###
  sec_doc = officer::read_docx('templates/template_comparative_factsheet.docx') %>%
    #body_add_par(translated_section_header,   style = "heading 1") %>%
    body_add_flextable(add_grey_header(translated_background_header)) %>%
    body_add_par(translated_background_text,   style = "Normal")  %>%
    body_add_par('\n') %>%
    body_add_flextable(add_grey_header(translated_findings_header))  
     
  #Adding header title
  sec_doc = sec_doc %>% headers_replace_text_at_bkm(bookmark = "header", value = translated_header_title)%>%
             headers_replace_text_at_bkm(bookmark = "header2", value = translated_section_header)
  
  # 
  sub_sec_titles = unique(sec_report_matrix$sub_section_text)
  j = NULL
  for(j in sub_sec_titles)
  {
    sub_sec_report_matrix = sec_report_matrix %>% dplyr::filter(sub_section_text == j) %>%
                            dplyr::select(-c(sect,arrange_num, sub_section_text, background_text))
    
    ###Cleaning dataframe to reduce the use of tokens
    cols_to_clean = c("grp_tab_title", "ind_subtitle", "stratifier")
    
    blank_consecutive_duplicates = function(x) {
      x[c(FALSE, x[-1] == x[-length(x)])] = ""
      x
    }
    sub_sec_report_matrix[cols_to_clean] = lapply(sub_sec_report_matrix[cols_to_clean], blank_consecutive_duplicates)    
    ##selecting out p - value and significance
    if(report_signf == 'No'){sub_sec_report_matrix = sub_sec_report_matrix %>% dplyr::select(-all_of(c('p_value','significance_of_change')))} else{}

    csv_text = paste(capture.output(write.csv(sub_sec_report_matrix, row.names=FALSE)), collapse="\n")
    
    prompt1 = paste0(
      "Tables (in CSV format):\n", csv_text, "\n",
      "Now write a concise narrative summary for this section for ",country,' in ',language,
      ", Please write the 
       narrative directly, and do not start with here is a concise narrative summary.
      Let the summary be as concise as possible without repetitions. Include p - values whenever changes were considered significant.
      Significant changes are interpreted as those with p-values < 0.05.\n
      Also avoid starting paragraphs in the same way (For example, avoiding always starting the results with: In country x...(this extends to all languages).")
    #
    prompt2 = paste0(
      "Tables (in CSV format):\n", csv_text, "\n",
      "Now write a concise narrative summary for this section for ",country,' in ',language,
      ", Please write the 
       narrative directly, and do not start with here is a concise narrative summary.
      Let the summary be as concise as possible without repetitions. \n
      Also avoid starting paragraphs in the same way (For example, avoiding always starting the results with: In country x...(this extends to all languages).")
    
    ##
    if(report_signf == 'No'){prompt = prompt2} else{ prompt = prompt1}

    ##Calling the wrapper
    complete_narrative = llm_wrapper_groq(prompt)
    ##
    if(language!='english')
    {
      tanslated_sub_header = llm_translate(j)
    }else{tanslated_sub_header = j}
    #
    #sec_doc = sec_doc %>% body_add_par(fpar(ftext(point, prop = bold_fp)), style = "bullet")
    
    # Create combined formatted paragraph:
    combined_fpar = fpar(
      ftext(tanslated_sub_header, prop = fp_text(bold = TRUE)),
      ftext(": ", prop = fp_text(bold = TRUE)),
      ftext(complete_narrative))
    
    # Add as a bullet point (using your bullet style)
    sec_doc = sec_doc %>% body_add_fpar(combined_fpar, style = "bullet") 
        
    # sec_doc = sec_doc %>% body_add_par(tanslated_sub_header, style = "heading 2")%>%
    #           body_add_par(complete_narrative, style = "Normal")
  }
  
  sec_doc = sec_doc %>% body_add_par('\n')
  # ###Developing charts
  # Create list of all unique indicator groups
  indicator_groups <- unique(sec_report_matrix$grp_tab_title)
  
  # Generate all charts in a list (no eval/parse)
  chart_list <- lapply(indicator_groups, function(grp) chart_function(indicator_group = grp))
  
  # Split charts into groups of up to 2
  split_groups <- split(chart_list, ceiling(seq_along(chart_list) / 2))
  
  sec_doc = sec_doc %>% body_add_break(pos = 'on') %>%
    body_add_flextable(add_grey_header(translates_charts_headr))
  # Add charts to Word document
  m = NULL
  for (m in seq_along(split_groups)) {
    gg_charts = plot_grid(
      plotlist = split_groups[[m]],
      ncol = 2,
      rel_heights = rep(1, ceiling(length(split_groups[[m]]) / 2)))
    ##
    ##
    # Now save it:
    chart_path = paste0(getwd(),"/outputs/comparative/temp_plots/charts_group_", m, ".png")
    ggsave(filename = chart_path, plot = gg_charts,width = 15,height = 11,dpi = 600)
    ####
    sec_doc <- sec_doc %>% body_add_img(src = chart_path, width = 7.25, height = 4.5)
  }
  
  ##deleting temporary plot files
  unlink(list.files(paste0(getwd(),'/outputs/comparative/temp_plots/'), full.names = TRUE, recursive = TRUE), recursive = TRUE)
  
  #########
  print(sec_doc,target=paste0(getwd(),'/outputs/comparative/',translated_section_header,'.docx')) 
}




