###Editing the matrix
#Addition section number
section_part_edit = cbind(section_title = unique(indicator_matrix$section_title))%>% 
                    as.data.frame()%>% 
                    mutate(Part = paste0('Part',1:n()))%>% as.data.frame()
##
indicator_matrix = indicator_matrix %>% left_join(section_part_edit)%>% 
                    group_by(Part)%>% 
                    mutate(indicator_der_num = paste0('Indicator',1:n()),
                           part_ind_no = paste0(Part,'_',indicator_der_num)) 

####Reducing the matrix to required fields for written report
reporting_matrix = indicator_matrix %>% dplyr::filter(!is.na(section_title))%>%
                    mutate(order_ind  = as.numeric(sub(".*_", "", sub_section_title)),
                           arrange_num = paste0(order_ind,'_',part_ind_no),
                           sub_section_text = sub("_.*$", "", sub_section_title))#%>% arrange(arrange_num,table_order)

###Converting temporary files to text files
#relevant_files = reporting_matrix$part_ind_no
#relevant_name = reporting_matrix$arrange_num
###########Function to generate numbers for reporting
##Generate stratifier
analysis_data = analysis_data %>%
                mutate(svy_year = survey_year,
                       sex_age = case_when(sex == 'Men' & (agerange=='18-29'|agerange=='30-44') ~ 1,
                                           sex == 'Men' & (agerange=='45-59'|agerange=='60-69') ~ 2,
                                           sex == 'Women' & (agerange=='18-29'|agerange=='30-44') ~ 3,
                                           sex == 'Women' & (agerange=='45-59'|agerange=='60-69') ~ 4),
                       sex_age = factor(sex_age, levels = 1:4, 
                                        labels = c('Men 18 - 44','Men 45 - 69','Women 18 - 44','Women 45 - 69')),
                       bin_age = case_when(agerange=='18-29'|agerange=='30-44' ~ 1,
                                           agerange=='45-59'|agerange=='60-69' ~ 2),
                       bin_age = factor(bin_age,levels = 1:2, labels = c('18-44','45-69')))

###
#all_stratifiers = c(col_strat_variable,row_strat_variables,'sex_age','bin_age')
narrative_strat = c(col_strat_variable,row_strat_variables)
# -------------------------------
# Indicator computation function
# -------------------------------
rev_compute_indicator <- function(ind_level, type_indicators, subset_indicators) {
  ind_type <- tolower(type_indicators[grep(ind_level, subset_indicators)][1])
  if (is.na(ind_type)) stop("Indicator type not found for: ", ind_level)
  
  formula <- as.formula(paste0("~", ind_level))
  fun <- switch(ind_type,
                "mean" = function(x, design, ...) svymean(x, design = design, na.rm = TRUE),#, deff = TRUE
                "median" = function(x, design, ...) svyquantile(x, design = design, quantiles = 0.5, ci = TRUE, na.rm = TRUE),
                "categorical" = function(x, design, ...) svyciprop(x, design = design, method = "logit", level = 0.95, na.rm = TRUE),
                stop("Unknown indicator type: ", ind_type)
  )
  
  list(fun = fun, formula = formula, type = ind_type)
}

# -------------------------------
# Compute wide table with 95% CI
# -------------------------------
rev_compute_wide_tab <- function(indicator, svy_datum, strat = NULL) {
  by_formula <- as.formula(if (is.null(strat)) "~svy_year" else paste0("~", strat, "+ svy_year"))
  
  df <- svyby(
    indicator$formula,
    by = by_formula,
    design = svy_datum,
    FUN = indicator$fun,
    vartype = c("ci"),
    keep.var = TRUE,
    level = 0.95,
    na.rm = TRUE
  ) %>% as.data.frame()
  
  # Add stratifier/category columns
  if (!is.null(strat)) {
    names(df)[1] <- "category"
    df <- df %>% mutate(stratifier = strat)
  } else {
    df <- df %>% mutate(stratifier = "Total", category = "Total")
  }
  
  # Identify estimate and CI columns dynamically
  ind_name <- as.character(indicator$formula[[2]])
  est_col <- grep(paste0("^", ind_name, "$"), names(df), value = TRUE)
  lower_col <- grep("ci_l|ci.low|ci_lw|ci_lwr|ci_lower", names(df), value = TRUE, ignore.case = TRUE)
  upper_col <- grep("ci_u|ci.high|ci_upp|ci_upper", names(df), value = TRUE, ignore.case = TRUE)
  
  if (length(lower_col) == 0) lower_col <- NA
  if (length(upper_col) == 0) upper_col <- NA
  
  df <- df %>%
    mutate(
      estimate = if (!is.na(est_col)) .data[[est_col]] else NA_real_,
      ci_low = if (!is.na(lower_col)) .data[[lower_col]] else NA_real_,
      ci_high = if (!is.na(upper_col)) .data[[upper_col]] else NA_real_
    )
  
  # Convert categorical to percentage
  if (indicator$type == "categorical") {
    df <- df %>% mutate(across(c(estimate, ci_low, ci_high), ~ . * 100)#,
                        # estimate = round(estimate,1),ci_low = round(ci_low,1),
                        # ci_high = round(ci_high,1)
                        )
  }
  
  # Keep only relevant columns
  df <- df %>% select(stratifier, category, svy_year, estimate, ci_low, ci_high)
  
  # Pivot to wide format by survey year
  df_wide <- df %>%
    pivot_wider(
      names_from = svy_year,
      values_from = c(estimate, ci_low, ci_high),
      names_sep = "_"
    )
  
  df_wide
}

# -------------------------------
# Compute p-value for association with stratifier
# -------------------------------
rev_compute_pvalue <- function(ind_level, indicator, svy_datum, strat_col = NULL) {
  if (is.null(strat_col)) return(NA_real_)
  if (length(unique(svy_datum$variables[[strat_col]])) <= 1) return(NA_real_)
  
  p <- tryCatch({
    if (indicator$type %in% c("mean", "median")) {
      formula <- as.formula(paste0(ind_level, " ~ ", strat_col))
      fit <- svyglm(formula, design = svy_datum)
      coef_test <- regTermTest(fit, strat_col)
      coef_test$p
    } else if (indicator$type == "categorical") {
      svy_datum$variables[[ind_level]] <- factor(svy_datum$variables[[ind_level]])
      #test <- svychisq(as.formula(paste0("~", ind_level, " + ", strat_col)), design = svy_datum)
      # test <- chisq.test(svytable(as.formula(paste0("~", ind_level, " + ", strat_col)),design = svy_datum),
      #            correct = TRUE) 
      test <- svychisq(as.formula(paste0("~", ind_level, " + ", strat_col)),design = svy_datum,statistic = "Chisq",simulate.p.value = TRUE)
      test$p.value
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)
  
  round(p, 4)
}

# -------------------------------
# Analyse indicator across stratifiers and totals
# -------------------------------
rev_analyse_indicator <- function(ind_level, type_indicators, subset_indicators, svy_datum,
                              sect, section_title, grp_tab_title, ind_subtitle, arrange_num,
                              sub_section_text, background_text) {
  indicator <- rev_compute_indicator(ind_level, type_indicators, subset_indicators)
  
  # Stratified results
  strat_results <- lapply(narrative_strat, function(s) {
    df <- rev_compute_wide_tab(indicator, svy_datum, strat = s)
    pval <- rev_compute_pvalue(ind_level, indicator, svy_datum, strat_col = s)
    
    df %>% mutate(
      p_value = pval,
      significance = ifelse(!is.na(pval) & pval < 0.05, "Significant", "Not significant")
    )
  }) %>% bind_rows()
  
  # Total (no stratifier)
  total_results <- rev_compute_wide_tab(indicator, svy_datum) %>%
    mutate(p_value = NA_real_, significance = NA_character_)
  
  # Combine and add metadata
  bind_rows(total_results, strat_results) %>%
    mutate(
      sect = sect,###
      section_title = section_title,
      grp_tab_title = grp_tab_title,
      ind_subtitle = ind_subtitle,
      arrange_num = arrange_num,
      sub_section_text = sub_section_text,
      background_text = background_text
    ) %>%
    select(sect, section_title, grp_tab_title, ind_subtitle, stratifier, category, everything())
}

# -------------------------------
# Compute all indicators for a section
# -------------------------------
rev_comp_numbers <- function(sect) {
  data <- analysis_data ##combined_dataset
  section_matrix <- reporting_matrix %>% filter(section == sect)
  wt_step <- unique(section_matrix$weight_step)[1]
  data <- data %>% filter(!is.na(get(wt_step)))
  svy_data <- svydesign(id = ~psu, weights = ~get(wt_step), strata = ~stratum, data = data, nest = TRUE)
  
  section_results <- NULL
  
  for (i in 1:nrow(section_matrix)) {
    #print(i)
    sub_matrix <- section_matrix[i,]
    subset_indicators <- strsplit(sub_matrix$indicator_var, ";")[[1]]
    type_indicators <- strsplit(sub_matrix$type, ";")[[1]]
    denom_logic <- strsplit(sub_matrix$pop_subset, ";")[[1]]
    
    grp_tab_title <- sub_matrix$table_title
    tab_subtitle1 <- strsplit(sub_matrix$subtitle1, ";")[[1]]
    #tab_subtitle2 <- strsplit(sub_matrix$subtitle2, ";")[[1]]
    tab_subtitle2 = sub_matrix$subtitle2 %>% strsplit(";") %>% unlist() %>% strsplit(":") %>% unlist()
    arrange_num <- sub_matrix$arrange_num
    sub_section_text <- sub_matrix$sub_section_text
    background_text <- sub_matrix$background
    section_title <- sub_matrix$section_title#[1]
    
    if (!all(is.na(tab_subtitle2))) tab_subtitle1 <- tab_subtitle2
    
    for (ind_level in subset_indicators) {
      #print(ind_level)
      if(!all(is.na(analysis_data[[ind_level]])))
      {
      ind_position <- grep(ind_level, subset_indicators)
      denom_condition <- denom_logic[ind_position]
      ind_subtitle <- tab_subtitle1[ind_position]
      
      # Section-specific age grouping
      if (sect == "Cardiovascular disease risk") {
        data <- data %>%
          mutate(agerange = case_when(age >= 40 & age < 55 ~ 1,
                                      age >= 55 & age < 70 ~ 2),
                 agerange = factor(agerange, levels = 1:2, labels = c("40-54","55-69")))
        svy_data <- svydesign(id = ~psu, weights = ~wstep3, strata = ~stratum, data = data, nest = TRUE)
      } else if (sect == "Summary of Combined Risk Factors") {
        data <- data %>%
          mutate(agerange = case_when(age >= 18 & age < 45 ~ 1,
                                      age >= 45 & age < 70 ~ 2),
                 agerange = factor(agerange, levels = 1:2, labels = c("18-44","45-69")))
        svy_data <- svydesign(id = ~psu, weights = ~wstep2, strata = ~stratum, data = data, nest = TRUE)
      }
      
      # Subset data
      if (denom_condition == "all") {
        svy_datum <- subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange))
      } else {
        svy_datum <- subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange) &
                              eval(parse(text = paste0("(", denom_condition, ")"))))
      }
      
      comb_rslts <- rev_analyse_indicator(ind_level, type_indicators, subset_indicators, svy_datum,
                                      sect, section_title, grp_tab_title, ind_subtitle, arrange_num,
                                      sub_section_text, background_text)
      section_results <- bind_rows(section_results, comb_rslts)
      }else{}
    } 
  }
  
  return(section_results)
}

# -------------------------------
# Parallel computation for all sections
# -------------------------------
cores_detected <- parallel::detectCores()
analysis_cores <- ifelse(cores_detected == 1, 1, cores_detected - round(cores_detected/2)) # leave cores free
plan(multisession, workers = analysis_cores)  

rev_comp_indicator_results_list <- future_lapply(
  unique(reporting_matrix$section),
  FUN = rev_comp_numbers
)

rev_comp_indicator_results = do.call(rbind, rev_comp_indicator_results_list) #%>% arrange(arrange_num)

###rounding to 1 dp
estimate_lo_hi = grep('estimate|low|high', names(rev_comp_indicator_results), v=T)
##
rev_comp_indicator_results <- rev_comp_indicator_results %>%
                              mutate(across(all_of(estimate_lo_hi), ~ round(.x, 1)))






