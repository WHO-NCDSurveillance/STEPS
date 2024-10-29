# Filter out rows from `indicator_matrix` where `fsctsheet_desc` is NA
fact_sheet_matrix = indicator_matrix %>% dplyr::filter(!is.na(fsctsheet_desc)) 
# Initialize an empty variable to hold section data
sect = NULL
# Define a function to generate factsheet section results
factsheet_section_fn = function(sect = unique(fact_sheet_matrix$section)[4])
{
  # Access global variable `analysis_data`
  data = analysis_data
  # Filter `fact_sheet_matrix` for the specified section and arrange by `fsctsheet_desc`
  section_matrix = fact_sheet_matrix %>% dplyr::filter(section == sect)%>% arrange(fsctsheet_desc)
  # Extract weight step and convert it to numeric
  wt_step = unique(section_matrix$weight_step)
  numeric_step = as.numeric(str_extract(wt_step, "\\d+"))
  # Filter `data` for non-NA values in the specified weight step column
  data = data %>% dplyr::filter(!is.na(get(wt_step)))
  # Create a survey design object with specified id, weights, and strata
  svy_data = svydesign(id=~psu, weights=~get(wt_step),strata=~stratum, data=data,nest = T)
  # Set section title based on numeric step and section name
  section_title = c(paste0('Step ',numeric_step,' ',unique(section_matrix$section)),'','','')
  
  # Adjust section title for specific sections
  if(sect=='Summary of Combined Risk Factors'|sect=='Cardiovascular disease risk')
  {
    section_title = c(unique(section_matrix$section),'','','')
  }
  # Initialize empty variables for storing results
  section_results = NULL
  ind = NULL
  # Loop through each row in `section_matrix`
  for (ind in 1:nrow(section_matrix))
  {
    sub_matrix = section_matrix[ind,]
    # Extract indicator positions, variables, types, and descriptions
    ind_position = as.numeric(do.call('c',strsplit(sub_matrix$ind_position, "[;]")))
    subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))[ind_position]
    type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))[ind_position]
    denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))[ind_position]
    ind_desc = do.call('c',strsplit(sub_matrix$fsctsheet_desc, "[;]"))
    output_level = sub_matrix$column_strat
    # Initialize variables for storing intermediate results
    ind_level = NULL
    sub_section_results = NULL
    # Loop through each indicator level
    for(ind_level in subset_indicators)
    {
      # Determine the condition and description for the indicator
      denom_condition = denom_logic[grep(ind_level,subset_indicators)]
      full_ind_desc = ind_desc[grep(ind_level,subset_indicators)]
      # Adjust age ranges and survey design based on the section
      if(sect=="Cardiovascular disease risk")
      {
        data = data %>%mutate(agerange = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
                              agerange = factor(agerange,levels=1:2, labels=c('40-54','55-69')))
        
        svy_data = svydesign(id=~psu, weights=~wstep3,strata=~stratum, data=data,nest = T)
        
      } else if(sect=="Summary of Combined Risk Factors")
      {
        data = data %>%mutate(agerange = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
                              agerange = factor(agerange,levels=1:2, labels=c('18-44','45-69')))
        svy_data = svydesign(id=~psu, weights=~wstep2,strata=~stratum, data=data,nest = T)
        
      }else{data = data}
      # Filter data based on the denominator condition
      if (denom_condition == 'all') {
        datum = data %>% filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange))
        svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange))
      } else {
        datum = data %>% dplyr::filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))))
        svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))))
      }
      if(nrow(datum)==0)
      {
        svy_datum = svy_data
        svy_datum$variables[,ind_level]=0# Set the key variable to 0 if no data matches
      }
      
      
      # Compute degrees of freedom for survey data
      degrees_freedom = degf(svy_datum)-1
      # Handle numeric indicators
      if(type_indicators[grep(ind_level,subset_indicators)] == 'numeric')
      {
        data[,ind_level] = as.numeric(data[,ind_level])
        # Create a formula for the indicator variable
        eval(parse(text = paste0('formula = ~', ind_level)))
        # Calculate number of participants by sex and total
        n_participants = datum %>% group_by(sex,.drop=FALSE) %>% reframe(participants=n())%>%
          pivot_wider(names_from = c(sex), values_from = participants) %>%
          mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)), Total=Men+Women)
        # Estimate means and confidence intervals
        men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci')%>%
          mutate(ci_l = ifelse(ci_l<0,0,ci_l))
        total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", df = degrees_freedom) 
        conf_interval = confint(total_est_ciprop) 
        conf_interval[conf_interval<0] = 0
        # Compute median and interquartile range (IQR) if specified
        median_compute = sub_matrix$median_computation =='yes' & !is.na(sub_matrix$median_computation)
        if(median_compute==TRUE)
        {
          men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svyquantile, quantiles = c(.5,.25,.75), method = "lo", df = degrees_freedom)[,1:4]
          median_total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", quantiles = c(.5,.25,.75), df = degrees_freedom, ci=FALSE) 
          total_est_ciprop = as.vector(unlist(median_total_est_ciprop)[1])
          conf_interval = as.vector(unlist(median_total_est_ciprop)[2:3])
        }
        # Combine estimates into a formatted string
        total_est = paste0(formatC(round(as.vector(total_est_ciprop),1),format = "f", digits = 1),'\n(',
                           formatC(round(as.numeric(conf_interval[1]),1),format = "f", digits = 1), ' - ',
                           formatC(round(as.numeric(conf_interval[2]),1),format = "f", digits = 1),')')
        
        # Define delimiter and multiplier for formatting
        delim_char = '\n('
        mult_n = 1
        
      } else if(type_indicators[grep(ind_level,subset_indicators)] == 'categorical'){
        # Create formula for categorical indicator variable
        eval(parse(text=paste0('formula = ~I(',ind_level, '=="',1,'")')))
        # Calculate number of participants by sex and total
        n_participants = datum %>% group_by(sex, .drop = FALSE) %>% reframe(participants=n())%>%
          pivot_wider(names_from = c(sex), values_from = participants) %>%
          mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)), Total=Men+Women)
        
        # Estimate proportions and confidence intervals
        men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci')
        total_est_ciprop = svyciprop(formula, design = svy_datum, method = "lo", df = degrees_freedom) 
        ###
        men_women_est_ciprop[is.na(men_women_est_ciprop)]=0
        
        total_lci = as.numeric(attr(total_est_ciprop, "ci")[1])
        total_uci = as.numeric(attr(total_est_ciprop, "ci")[2])
        #
        total_lci = ifelse(is.na(total_lci),0,total_lci)
        total_uci = ifelse(is.na(total_uci),0,total_uci)
        
        # Combine estimates into a formatted string
        total_est = paste0(formatC(round(100*as.vector(total_est_ciprop),1),format = "f", digits = 1),'%\n(',
                           formatC(round(100*total_lci,1),format = "f", digits = 1), ' - ',
                           formatC(round(100*total_uci,1),format = "f", digits = 1),')')
        # Define delimiter and multiplier for formatting
        delim_char = '%\n('
        mult_n = 100
      }
      # Combine estimates for males and females into formatted strings
      males_est = paste0(formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',2]),1),format = "f", digits = 1),delim_char,
                         formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',3]),1),format = "f", digits = 1), ' - ',
                         formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',4]),1),format = "f", digits = 1),')')
      
      females_est = paste0(formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',2]),1),format = "f", digits = 1),delim_char,
                           formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',3]),1),format = "f", digits = 1), ' - ',
                           formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',4]),1),format = "f", digits = 1),')')
      ###
      if(output_level =='collevel1'){females_est = '-'}
      if(output_level =='collevel2'){males_est = '-'}
      if(output_level =='total_col'){
        males_est = '-'
        females_est = '-'
      }
      #
      if(collevel1_2 =='total_col'){total_est = '-'}
      ##
        
      # Combine results for each indicator
      combined_results = c(full_ind_desc, total_est,males_est,females_est)
      sub_section_results = rbind(sub_section_results,combined_results)    
    }
    section_results = rbind(section_results,sub_section_results) 
    
  }
  # Add section title and sort results by indicator description
  section_results = rbind(section_title,section_results[order(section_results[,1]),])
  return(section_results )  
}


# Generate factsheet table for all indicators by applying `factsheet_section_fn` to each unique section
factsheet_table = do.call('rbind',sapply(unique(fact_sheet_matrix$section),factsheet_section_fn))
# Set column names for the factsheet table
colnames(factsheet_table)=c('Results for adults aged 18-69 years (incl. 95% CI)','Both Sexes',
                            'Males','Females')
# Convert the factsheet table to a matrix and clean up indicator descriptions
factsheet_table = as.matrix(factsheet_table)
factsheet_table[,1] = gsub("^\\d+\\.\\s*", "", factsheet_table[,1])

# Convert matrix to data frame and add row numbers
factsheet_table = factsheet_table %>% as.data.frame()%>% mutate(row_numbers = 1:n())
rownames(factsheet_table)=NULL
# Extract rows where 'Females' column is empty for merging
extract_rows = (factsheet_table %>% dplyr::filter(Females=='') %>% dplyr::select(row_numbers))$row_numbers
# Remove the row numbers column
factsheet_table = factsheet_table %>% dplyr::select(-row_numbers)
# Create a flextable from the factsheet table and format it
flex_fact_sheet = factsheet_table%>% flextable() %>% autofit() %>%
                  flextable::style(pr_t=fp_text(font.size=10,font.family='Source Sans Pro'), part = 'all')%>%
                  bold(i = c(1,extract_rows))%>%
                  bg(bg="white",i=1,part="header")%>%  
                  theme_box()%>% 
                  align(align = "center", j = 2:4, part = "all") %>%
                  fontsize(size = 9 ,part = "all")%>%
                  merge_h_range(i=extract_rows, j1=1,j2=4)%>%
                  width(j = 2:4, 4.3, unit = "in")%>% 
                  bg(bg="#339966",i=1,part="header")%>%
                  bg(bg="#CCFFFF",i=extract_rows,part="body")%>%
                  color(color = "white", part = 'header')%>%
                  padding(padding = 0, part = "all") %>%
                  paginate()


# Read the factsheet template document and add the formatted flextable to it
doc = read_docx(paste0(getwd(),'/templates/factsheet_template.docx'))
doc=doc %>% cursor_bookmark(id  = "bmk1") %>%
  body_add_flextable(width(flex_fact_sheet, width = dim(flex_fact_sheet)$widths*7.25/(flextable_dim(flex_fact_sheet)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/outputs/Factsheet.docx')) 

