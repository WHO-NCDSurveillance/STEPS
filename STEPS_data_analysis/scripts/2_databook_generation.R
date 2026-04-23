############################################################
## SCRIPT FOR GENERATING DATABOOK
############################################################
# Initialize loop variables for sections, indicators, and sub-indicators
i=NULL
j=NULL
k=NULL
#########################################################################
## Loop through sections defined in the indicator matrix
#########################################################################

for (i in unique(indicator_matrix$section))
{
  ########################################
  ## Section: Indicator Analysis Loop
  ########################################
  
  # Filter the indicator matrix to only include indicators belonging
  # to the current section.
  section_matrix = indicator_matrix %>% dplyr::filter(section == i)
  
  # Initialize a list that will store all output tables generated
  # for indicators within the section.
  output_table = list()
  
  
  ########################################
  ## Loop through indicator descriptions
  ########################################
  # Each indicator group is identified using indicator_short_desc
  for(j in unique(section_matrix$indicator_short_desc))
  {
    # Filter rows belonging to the current indicator group
    pre_sub_matrix = indicator_matrix %>% 
      dplyr::filter(indicator_short_desc == j)
    # Initialize sub-row iterator
    sub_row = NULL
    ########################################
    ## Loop through sub-indicators
    ########################################
    for(sub_row in 1:nrow(pre_sub_matrix))
    {
      # Extract the specific row describing the current sub-indicator
      sub_matrix = pre_sub_matrix[sub_row,]
      # Extract indicator variable names separated by semicolons
      subset_indicators = do.call('c',
                                  strsplit(sub_matrix$indicator_var, "[;]"))
      # Extract indicator types (e.g., categorical, median, mean)
      type_indicators = do.call('c',
                                strsplit(sub_matrix$type, "[;]"))
      # Extract denominator logic defining the population subset
      denom_logic = do.call('c',
                            strsplit(sub_matrix$pop_subset, "[;]"))
      
      
      ########################################
      ## Defining survey design structure
      ########################################
      
      # Identify the survey weight variable for the current indicator
      wt_step = unique(sub_matrix$weight_step)[1]
      
      # Use analysis dataset prepared earlier
      data = analysis_data
      # Ensure weights are numeric
      data[,wt_step] = as.numeric(as.character(data[,wt_step]))
      # Replace missing weights with zero so that survey design
      # object creation does not fail
      data[,wt_step][is.na(data[,wt_step])] = 0
      # Update age range variable if it exists
      if(!is.null(data[,"agerange"])){data[,"agerange"] = data[,unique(sub_matrix$agevar)]}
      # Define survey design object using PSU, weights, and strata
      svy_data = svydesign(id=~psu, 
                           weights=~get(wt_step),
                           strata=~stratum, 
                           data=data,nest = T)
      # Correct for finite sample size if fpc exists in the data
      if(fpc_var_check){
        svy_data = svydesign(id=~psu, 
                             weights=~get(wt_step),
                             strata=~stratum, 
                             fpc = ~fpc, 
                             data=data,nest = T)
      }
      ########################################
      ## Indicator characteristics
      ########################################
      
      # Identify if the indicator type is median
      median_compute = unique(type_indicators)=='median'
      # Extract numerator/denominator population definitions if provided
      if(is.na(sub_matrix$pop_level_num_denom))
      {
        pop_num_denom = NA
      }else{pop_num_denom = do.call('c',strsplit(sub_matrix$pop_level_num_denom, "[;]"))}
      ########################################
      ## Determine if indicators should be combined
      ########################################
      
      # Remove numeric suffixes to identify indicators with similar base names
      similarity_ind = unique(gsub("\\d{1,2}$", "", na.omit(subset_indicators[1:9])))
      # Flag indicating whether multiple indicators should be displayed
      # in the same table
      combine_ind = length(similarity_ind)==1 & length(subset_indicators)>1
      
      ########################################
      ## Identify column stratification levels
      ########################################
      
      # Determine levels for the column stratification variable
      col_strat_var_levels = names(table(data[,col_strat_variable]))
      # Initialize lists dynamically to store stratified tables
      eval(parse(text=paste0('sublist_', 1:length(col_strat_var_levels),'= list()', sep='\n')))
      # Initialize list for total column tables
      total_tab = list()
      
      ########################################
      ## Loop through individual indicators
      ########################################
      for(k in subset_indicators)
      {
        print(k)
        ##################################################
        ## Numeric indicators (mean / median)
        ##################################################
        if(type_indicators[which(subset_indicators %in%k)] == 'median'|
           type_indicators[which(subset_indicators %in%k)] == 'mean')
        {
          # Ensure variable is numeric
          data[,k] = as.numeric(data[,k])
          ####################################
          ## Demographics indicators
          ####################################
          if(i ==demog_section_header)
          {
            # Apply denominator filter if needed
            if(denom_logic[which(subset_indicators %in%k)] =='all')
            {
              datum = data%>%dplyr::filter(!is.na(eval(parse(text = k))))
            }else{
              datum = data%>%dplyr::filter(eval(parse(text=paste0('(',denom_logic[which(subset_indicators %in%k)],')'))))
            }
            
            # Variables used for demographic summaries
            list_demog_vars = c("agerange", setdiff(row_strat_variables,"agerange"))
            num_demog = NULL
            summary_table = NULL
            # Generate numeric summaries for each demographic variable
            for(num_demog in list_demog_vars)
            {
              summary_table = rbind(summary_table, demog_numeric(num_demog))
            }
            # Replace missing stratification labels with "Total"
            summary_table = summary_table %>%
              mutate(`eval(parse(text = strat_variable))` = ifelse(is.na(`eval(parse(text = strat_variable))`),
                                                                   'Total',
                                                                   as.character(`eval(parse(text = strat_variable))`)))
            
          } else{
            
            ####################################
            ## Non-demographic numeric indicators
            ####################################
            
            # Create formula for survey analysis
            eval(parse(text = paste0('formula = ~', k)))
            #denom_condition = denom_logic[which(subset_indicators %in%k)]
            denom_condition = if (length(denom_logic[subset_indicators %in% k]) == 1) {denom_logic[1]} else {denom_logic[which(subset_indicators %in% k)]}
            sub_pop_num_denom = pop_num_denom[which(subset_indicators %in%k)]
            # Initialize objects for stratified summaries
            s = NULL
            int_summary_table = list()
            # Perform stratified analysis for each row stratification variable
            for(s in na.omit(unique(c(row_strat_variables))))
            {
              s_position = grep(s, row_strat_variables)
              sub_label=row_strat_variable_titles[s_position]
              
              res_table = analysis_numeric_fn(row_strat = s, col_strat = col_strat_variable)%>%
                as.data.frame()
              # Add row headers for stratification variables
              if(s_position!=1)
              {
                res_table = rbind(c(sub_label, rep('',ncol(res_table)-1)),res_table)%>%
                  as.data.frame()
                colnames(res_table) = all_tabcolnames
              } else{}
              int_summary_table[[s]]= res_table
              all_tabcolnames = colnames(int_summary_table[[1]])
            }
            # Combine stratified tables
            summary_table = do.call('rbind', int_summary_table) %>% 
              as.data.frame()
            ####################################
            ## Formatting output tables
            ####################################
            
            # Mask percentages and confidence intervals if denominator is small
            eval(parse(text=paste0('summary_table$`',
                                   paste0(col_strat_var_levels,'_',c('b`')),
                                   '[as.numeric(summary_table$`',
                                   paste0(col_strat_var_levels,'_',c('a`)')),
                                   '<',denom_limit,']="-"', sep = '\n')))
            eval(parse(text=paste0('summary_table$`',
                                   paste0(col_strat_var_levels,'_',c('ci`')),
                                   '[as.numeric(summary_table$`',
                                   paste0(col_strat_var_levels,'_',c('a`)')),
                                   '<',denom_limit,']="-"', sep = '\n')))
            summary_table$Total_b[as.numeric(summary_table$Total_a) < denom_limit] = '-'
            summary_table$Total_ci[as.numeric(summary_table$Total_a) < denom_limit] = '-'
          }
        }
        ##################################################
        ## Categorical indicators
        ##################################################
        else if(type_indicators[which(subset_indicators %in%k)] == 'categorical')
        {
          
          ####################################
          ## Demographic categorical indicators
          ####################################
          if(i ==demog_section_header)
          {
            # Apply denominator condition
            if(denom_logic[which(subset_indicators %in%k)] =='all')
            {
              datum = data
            }else{
              datum = data%>%dplyr::filter(eval(parse(text=paste0('(',denom_logic[which(subset_indicators %in%k)],')'))))
            }
            
            ####################################
            ## Generate demographic summary tables
            ####################################
            list_demog_vars = c("agerange",'', setdiff(row_strat_variables,"agerange")) 
            num_demog = NULL
            summary_table = NULL
            ##
            summary_table = demog_cat()
            ###
            for(num_demog in list_demog_vars[-1]) #
            {
              summary_table = full_join(summary_table %>% mutate_all(as.character),
                                        demog_cat(num_demog)%>% mutate_all(as.character))
            }
            # Replace NA stratification values with Total
            summary_table = summary_table %>%
              mutate(`eval(parse(text = strat_variable))` = ifelse(is.na(`eval(parse(text = strat_variable))`),
                                                                   'Total',as.character(`eval(parse(text = strat_variable))`)))
            #Editing summary_table
            summ_table_names = sort(names(summary_table))
            if(sub_matrix$logic_condition_var=='c1'){
              names_to_select = c("eval(parse(text = strat_variable))",'Men_1',
                                  'sex_Total_1_Percentage','Women_2','sex_Total_2_Percentage',
                                  'Total_Count','Men_1_Percentage')##Note Men_1_Percentage will be 100% throughout
              summary_table = summary_table %>% dplyr::select(all_of(names_to_select))
              colnames(summary_table)=c("eval(parse(text = strat_variable))",'Men_a','Men_b','Women_a','Women_b','Total_a','Total_b')
              #
              summary_table = summary_table %>% dplyr::mutate(Men_b = replace(Men_b,as.numeric(Men_a)<denom_limit,'-'),
                                                              Women_b = replace(Women_b,as.numeric(Women_a)<denom_limit,'-'),
                                                              Total_b = replace(Total_b,as.numeric(Total_a)<denom_limit,'-'))
              
              ####################################
              ## Non-demographic categorical indicators
              ####################################
            } else {
              # Define formula for categorical indicator
              names_to_select = c("eval(parse(text = strat_variable))",'men_Total_Count', 
                                  grep('Percentage',grep('Men_',summ_table_names,v=T), v = T)
                                  ,'women_Total_Count', 
                                  grep('Percentage',grep('Women_',summ_table_names,v=T), v = T)
                                  ,'Total_Count', 
                                  grep('Percentage',grep('sex_',summ_table_names,v=T), v = T))
              summary_table = summary_table %>% dplyr::select(all_of(names_to_select)) %>% as.data.frame()
              
              ####Editing summary_table
              eval(parse(text=paste0('summary_table$',
                                     grep('Percentage',
                                          grep('Men_',summ_table_names,v=T), v = T),
                                     '[as.numeric(summary_table$men_Total_Count)<',
                                     denom_limit,']="-"', sep = '\n')))
              eval(parse(text=paste0('summary_table$',
                                     grep('Percentage',
                                          grep('Women_',summ_table_names,v=T), v = T),
                                     '[as.numeric(summary_table$women_Total_Count)<',
                                     denom_limit,']="-"', sep = '\n')))
              eval(parse(text=paste0('summary_table$',
                                     grep('Percentage',
                                          grep('sex_',summ_table_names,v=T), v = T),
                                     '[as.numeric(summary_table$Total_Count)<',
                                     denom_limit,']="-"', sep = '\n')))
              
              ########Calling custom sort functions
              summary_table = list(
                summary_table %>%
                  dplyr::select(c("eval(parse(text = strat_variable))",'men_Total_Count', 
                                  custom_sort1(grep('Percentage',grep('Men_',summ_table_names,v=T), v = T)))),
                summary_table %>%
                  dplyr::select(c("eval(parse(text = strat_variable))",'women_Total_Count', 
                                  custom_sort2(grep('Percentage',grep('Women_',summ_table_names,v=T), v = T)))),
                summary_table %>%
                  dplyr::select(c("eval(parse(text = strat_variable))",'Total_Count', 
                                  custom_sort3(grep('Percentage',grep('sex_',summ_table_names,v=T), v = T)))))
            }
            
            
          }else{
            eval(parse(text=paste0('formula = ~I(',k, '=="',1,'")')))
            #denom_condition = denom_logic[which(subset_indicators %in%k)]
            denom_condition = if (length(denom_logic[subset_indicators %in% k]) == 1) {denom_logic[1]} else {denom_logic[which(subset_indicators %in% k)]}
            # Stratified categorical analysis
            s = NULL
            int_summary_table = list()
            
            for(s in na.omit(unique(c(row_strat_variables))))
            {
              s_position = grep(s, row_strat_variables)
              sub_label = row_strat_variable_titles[s_position]
              
              res_table = analysis_categorical_fn(row_strat = s, col_strat = col_strat_variable)%>%
                as.data.frame()
              if(s_position!=1)
              {
                res_table = rbind(c(sub_label, rep('',ncol(res_table)-1)),res_table)%>%as.data.frame()
                colnames(res_table) = all_tabcolnames
              } else{}
              int_summary_table[[s]]= res_table
              all_tabcolnames = colnames(int_summary_table[[1]])
            }
            # Combine categorical tables
            summary_table = do.call('rbind', int_summary_table) %>% as.data.frame()
            ####################################
            ## Apply denominator threshold formatting
            ####################################
            eval(parse(text=paste0(
              'summary_table$`',paste0(col_strat_var_levels,'_',c('b`')),
              '[as.numeric(summary_table$`',
              paste0(col_strat_var_levels,'_',c('a`)')),
              '<',denom_limit,']="-"', sep = '\n')))
            eval(parse(text=paste0(
              'summary_table$`',paste0(col_strat_var_levels,'_',c('ci`')),
              '[as.numeric(summary_table$`',
              paste0(col_strat_var_levels,'_',c('a`)')),
              '<',denom_limit,']="-"', sep = '\n')))
            summary_table$Total_b[as.numeric(summary_table$Total_a) < denom_limit] = '-'
            summary_table$Total_ci[as.numeric(summary_table$Total_a) < denom_limit] = '-'
          }
        } else{}
        ########################################
        ## Combining outputs if indicators share table
        ########################################
        if (combine_ind) {
          # Combine multiple indicators into the same output table
          # depending on stratification levels.
          if (i == demog_section_header) {
            if (any(class(summary_table) != 'list')) {
              sublist_1[[k]] = summary_table %>% 
                dplyr::select(all_of(c("eval(parse(text = strat_variable))", 'Men_a', 'Men_b')))
              sublist_2[[k]] = summary_table %>% 
                dplyr::select(all_of(c("eval(parse(text = strat_variable))", 'Women_a', 'Women_b')))
              total_tab[[k]] = summary_table %>% 
                dplyr::select(all_of(c("eval(parse(text = strat_variable))", 'Total_a', 'Total_b')))
            } else {
              # When summary_table already contains multiple stratified tables 
              sublist_1[[k]] = summary_table[[1]]
              sublist_2[[k]] = summary_table[[2]]
              total_tab[[k]] = summary_table[[3]]
            }
          } else {
            # Prepare list of column combinations for stratified tables
            all_colmames = c(sort(c(paste0(col_strat_var_levels,'_',c('a')),
                                    paste0(col_strat_var_levels,'_',c('b')),
                                    paste0(col_strat_var_levels,'_',c('ci')))),
                             'Total_a','Total_b','Total_ci')
            ###
            col_list = list()
            llist = NULL
            for(llist in col_strat_var_levels)
            {
              list_names = grep(llist,all_colmames, v=T)
              col_list[[llist]]=c(colnames(summary_table)[1],list_names)
            }
            # Store subtables for each column stratification level
            eval(parse(text=paste0('sublist_', 
                                   1:length(col_strat_var_levels),
                                   '[[k]] = summary_table %>% dplyr::select(all_of(c(col_list[["',
                                   col_strat_var_levels,'"]])))', sep='\n')))
            total_tab[[k]] = summary_table %>% 
              dplyr::select(all_of(c(colnames(summary_table)[1], 'Total_a', 'Total_b', 'Total_ci')))
          }
        } else {
          # Store individual indicator table
          output_table[[paste0(j, '__', k)]] = summary_table
        }
        
      }# end indicator loop
      
      ########################################
      ## Combine tables when required
      ########################################
      if(combine_ind)
      {
        # Merge subtables for each column stratification level
        eval(parse(text=paste0('merged_sublist_', 
                               1:length(col_strat_var_levels),
                               ' = reduce(sublist_',1:length(col_strat_var_levels),
                               ', full_join, by = "',colnames(as.data.frame(sublist_1[[1]]))[1],'")', 
                               sep='\n')))
        # Merge total tables
        merged_total_tab = reduce(total_tab, full_join, by = colnames(as.data.frame(sublist_1[[1]]))[1])
        # Store merged result
        output_table[[paste0(j,'__',k)]] = eval(parse(text=paste0('list(',
                                                                  paste0('merged_sublist_', 1:length(col_strat_var_levels), collapse = ','),
                                                                  ', merged_total_tab)')))
        
      } else{}
    }# end sub-indicator loop
  }# end indicator group loop
  
  
  #########################################################################
  ## Formatting Output Tables and Writing to Word Documents
  #########################################################################
  
  # Loop through generated output tables and format them using flextable
  # before exporting them to Word documents.
  g = NULL
  ind_no = 1
  for(g in names(output_table))
  {
    # Initialize Word document
    doc = officer::read_docx()
    
    #
    tab_length = length(grep('_a',names(output_table[[g]][[1]]), v=T))
    if(tab_length < 10)
    {
      matching_indicator = gsub("\\d{1}$", "", do.call('c',strsplit(g, "[__]"))[[3]])
    }else{matching_indicator = gsub("\\d{1,2}$", "", do.call('c',strsplit(g, "[__]"))[[3]])}
    # Retrieve indicator description from matrix
    sub_formatrix = indicator_matrix[grep(paste0('\\b',matching_indicator,'\\b'), 
                                          tolower(indicator_matrix$indicator)),] 
    
    indicator = sub_formatrix$indicator
    indicator_short_desc = unique(sub_formatrix$indicator_short_desc)
    description = sub_formatrix$description
    instrument_questions = sub_formatrix$instrument_questions
    agevar = sub_formatrix$agevar ## ADDED 
    group_type = unique(do.call('c',strsplit(sub_formatrix$type, "[;]")))[1]
    #
    table_title = sub_formatrix$table_title
    ########################################
    ## Subtitle handling
    ########################################
    subtitle1 = do.call('c',strsplit(as.character(sub_formatrix$subtitle1), "[;]"))
    if(length(sub_formatrix$subtitle2)>0)
    {
      subtitle2 = do.call('c',
                          strsplit(do.call('c',
                                           strsplit(as.character(sub_formatrix$subtitle2), "[:]")),
                                   "[;]"))
    } else{}
    #
    subtitle1 = ifelse(is.na(subtitle1) & sub_formatrix$section!=demog_section_header,'',subtitle1)
    subtitle2 = ifelse(is.na(subtitle2),'',subtitle2)
    ########################################
    ## Special handling for demographic section
    ########################################
    if(sub_formatrix$section==demog_section_header)
    {
      if(all(is.na(subtitle1)))
      {
        subtitle1 = eval(parse(text=paste0('names(table(data$demog_',sub_formatrix$logic_condition_var,'))')))
      } else{subtitle1 = do.call('c',strsplit(as.character(sub_formatrix$subtitle1), "[;]"))}
    } else{}
    
    ##
    column_strat = tolower(sub_formatrix$column_strat)
    ##
    median_title = grep('median',g)
    #########################################################################
    ## ---------------- Indicator Definition Table -------------------------
    #########################################################################
    
    # This section generates a formatted indicator definition table that
    # includes the indicator description and the instrument questions.
    # The layout is adapted depending on the language used in the report,
    # particularly to support right-to-left languages such as Arabic.
    if(language == 'arabic')
    {
      # For Arabic, the rows are reversed so that the content appears
      # correctly in right-to-left reading order.
      def_indicator = as.data.frame(
        rev(rbind(cbind(indicator_short_desc,
                        paste0(other_language[6,language],': ',description)),
                  cbind(indicator_short_desc,
                        paste0(other_language[7,language],':\n',
                               instrument_questions))) %>%
              as.data.frame())
      ) %>% 
        flextable() %>% autofit() %>% 
        delete_part(part = "header")%>%# remove default header
        merge_at(j=2)%>% # merge definition column
        valign(j = 2,valign = 'top')%>% # align text vertically
        width(j = 1, 10, unit = "in") %>%# control column width
        flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
        bold(j = 2) %>% 
        fontsize(size = 9 ,part = "all") %>% 
        border_remove()
      
    }else{
      # For all other languages, standard left-to-right formatting is used.
      def_indicator = as.data.frame(
        rbind(cbind(indicator_short_desc,
                    paste0(other_language[6,language],': ',description)),
              cbind(indicator_short_desc,
                    paste0(other_language[7,language],':\n',
                           instrument_questions)))) %>%
        flextable() %>% autofit() %>% 
        delete_part(part = "header")%>%
        merge_at(j=1)%>% # merge indicator column
        valign(j = 1,valign = 'top')%>%
        width(j = 2, 10, unit = "in") %>%
        flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
        bold(j = 1) %>% 
        fontsize(size = 9 ,part = "all") %>% 
        border_remove()
    }
    #########################################################################
    ## Determining Indicator Position within Section
    #########################################################################
    
    # Identify the position of the current indicator within the section.
    indicator_position = grep(paste0('\\b',
                                     matching_indicator,'\\b'),
                              tolower(section_matrix$indicator))
    # Identify the previously processed indicator.
    prev_indicator = ifelse(indicator_position>1,
                            names(output_table)[indicator_position-1],
                            names(output_table)[1])
    # Extract short description from previous indicator name.
    prev_short_desc = do.call('c',strsplit(prev_indicator, "[__]"))[[1]]
    #########################################################################
    ## Conditional Addition of Indicator Definition to Word Document
    #########################################################################
    
    # Insert the indicator definition block only when a new indicator group
    # begins or when processing the first indicator.
    if(any(!(indicator_short_desc %in% prev_short_desc) | indicator_position==1))
    {
      doc = doc %>% 
        body_add_flextable(width(def_indicator, 
                                 width = dim(def_indicator)$widths*6.4/(flextable_dim(def_indicator)$widths)))%>%
        body_add('\n', style = "Normal")
    }else{}
    #########################################################################
    ## Extracting Indicator Table for Formatting
    #########################################################################
    
    # Retrieve computed summary table from output list.
    extract_table = output_table[[g]]
    
    #########################################################################
    ## Determining Stratification Structure
    #########################################################################
    
    # Identify number of age group levels used in the table.
    #range_levels = length(names(table(data[,unique(sub_formatrix$agevar)])))
    range_levels = length(names(table(analysis_data[,unique(sub_formatrix$agevar)]))) ## ADDED
    # Initialize vector for storing row group boundaries.
    all_hlines = c()
    # Determine number of levels in row stratification variables.
    strat_var_levels = if(any(!is.na(row_strat_variables)))
    {
      length_strat = length(row_strat_variables)
      sub_var_levels = eval(parse(text=paste0('c(',
                                              paste0('length(names(table(analysis_data[,"', ## ADDED - changed "data" to "analysis_data"
                                                     unique(sub_formatrix$agevar),'"])))', collapse = ','),
                                              ')')))
      all_hlines = c(all_hlines,sub_var_levels)
    } else{NA}
    
    #########################################################################
    ## Determine Table Row Lines for Formatting
    #########################################################################
    
    # These calculations determine row ranges used to highlight
    # sections of the table or apply formatting rules.
    ln = NULL
    if(all(is.na(subtitle2))|all(subtitle2==''))
    {
      first_ln = 4:(all_hlines[1]+2)
      if(range_levels==2){first_ln = 4}
    } else{
      first_ln = 5:(all_hlines[1]+3)
      if(range_levels==2){first_ln = 5}
    }
    # Special rule for Demographic section
    if(i==demog_section_header){first_ln = 4:(length(names(table(data[,"agerange"])))+2)}
    final_hlines = c(first_ln)
    #########################################################################
    ## Compute Additional Row Group Boundaries
    #########################################################################
    
    next_no = 1
    if(any(!is.na(row_strat_variables))) #
    {
      for(ln in all_hlines[-1])
      {
        if(next_no==1)
        {
          next_ln = (4+max(final_hlines)):(ln+max(final_hlines)+2)
        } else {
          next_ln = (3+max(final_hlines)):(ln+max(final_hlines)+1)
        }
        final_hlines = c(final_hlines, next_ln)
        next_no = next_no+1
      }
    }
    
    #########################################################################
    ## Define Table Border Styles
    #########################################################################
    
    std_border = fp_border(color = "grey",width = 5)
    white_border = fp_border(color = "white")
    #########################################################################
    ## Handling Output Tables that are Data Frames
    #########################################################################
    
    # This branch handles summary tables generated as data frames
    # (typically simple indicator outputs).
    if(any(class(extract_table)=="data.frame"))
    {
      #######################################################################
      ## Language-Specific Number Formatting
      #######################################################################
      
      # Replace decimal points with commas for French reports.
      if(language == 'french')
      {
        extract_table = extract_table %>% as.matrix
        extract_table = gsub('[.]',',',extract_table)
        extract_table = extract_table %>% as.data.frame()
      } else{}
      #######################################################################
      ## Identify Total Rows
      #######################################################################
      total_pos = grep('Total',extract_table[,1])
      if(i==demog_section_header){total_pos = grep('Total',as.data.frame(extract_table)[,1])}
      #######################################################################
      ## Construct Column Labels for Table
      #######################################################################
      inline_text = if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical'){
        paste0('% ',do.call('c',strsplit(subtitle1, "[;]")))
        
      }else{do.call('c',strsplit(subtitle1, "[;]"))}
      #######################################################################
      ## Define Header Structure
      #######################################################################
      
      if(length(inline_text)>1)
      {
        label_subtitle1 = c(row_strat_variable_titles[1],'n',
                            inline_text[1],other_language[5,language],'n',
                            inline_text[2],other_language[5,language],'n',inline_text[3],
                            other_language[5,language])
        
      } else{
        label_subtitle1 = c(row_strat_variable_titles[1],
                            rep(c("n",inline_text,other_language[5,language]),
                                (ncol(extract_table)-1)/3))
        
      }
      #######################################################################
      ## Special Formatting for Demographic Tables
      #######################################################################
      # For the "Demographics" section, the column headers are customized
      # depending on the type of variable (categorical vs continuous).
      if(i==demog_section_header){
        # For categorical demographic variables, the table headers include 
        # repeated 'n' and '%' columns along with translated column titles.
        label_subtitle1=if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical')
        {c(other_language[8,language],# Header title for the demographic category
           'n','%','n','%','n','%')# Column labels for counts and percentages
        } else{
          # For continuous variables, headers include 'n' and mean/95 CIs or median/IQR
          (c(other_language[8,language],'n',other_language[9,language],
             'n',other_language[9,language],'n',other_language[9,language]))}}
      
      ###########################################################################
      ## ---------------- Prepare Indicator Table ------------------------------
      ###########################################################################
      
      # Determine the next indicator for grouping logic
      next_ind = names(output_table)[indicator_position+1]
      next_short_desc = do.call('c',strsplit(next_ind, "[__]"))[[1]]
      # Replace 'Total' in the first column with language-specific translation
      extract_table[,1][extract_table[,1]=='Total'] = other_language[4,language]
      ###########################################################################
      ## ---------------- Formatting Demographic Table ------------------------
      ###########################################################################
      
      if(i==demog_section_header)
      {
        if(language =='arabic')
        {
          # For Arabic, reverse the table for right-to-left layout
          pre_edited_table = rbind(rep(table_title,7),# Table title repeated across columns
                                   c('',rep(other_language[1,language],2),# First row of header with subcategories
                                     rep(other_language[2,language],2),
                                     rep(other_language[3,language],2)),
                                   label_subtitle1,# Column labels
                                   as.matrix(extract_table))%>%# Data rows
            as.data.frame() %>%rev() # Reverse for RTL
          vline_num = c(2,4)# Vertical line positions for formatting
        }else{
          # Standard LTR formatting
          pre_edited_table =  rbind(rep(table_title,7),
                                    c('',rep(other_language[1,language],2),
                                      rep(other_language[2,language],2),
                                      rep(other_language[3,language],2)),
                                    label_subtitle1,# Column labels
                                    as.matrix(extract_table)) %>%# Data rows
            as.data.frame()
          vline_num = c(3,5)
        }
        
        # Call external function to apply detailed Demographic table formatting
        source('scripts/functions/table_formatting_function_demog.R')
        
        # Insert table into Word document
        if(indicator_short_desc %in% next_short_desc)
        {
          doc = doc %>% 
            body_add_flextable(width(edited_table, 
                                     width = dim(edited_table)$widths*6.5/(flextable_dim(edited_table)$widths)))%>%
            body_add('\n', style = "Normal")
        }else{
          # Add a page break if it's the last indicator in this group
          doc = doc %>% 
            body_add_flextable(width(edited_table, 
                                     width = dim(edited_table)$widths*6.5/(flextable_dim(edited_table)$widths))) %>% 
            body_add_break()}
        
      } else{
        #########################################################################
        ## Non-Demographic Table Formatting and Column Splitting
        #########################################################################
        
        ext_ncols = ncol(extract_table) # Number of columns in the table
        lt = NULL
        inc_num=0
        frame_split_tables = list() # Store subtables after splitting wide tables
        colnosplit = cumsum(frame_split_into_parts(num_cols = ext_ncols)) # Determine column break points
        ##
        for(lt in colnosplit)
        {
          # Determine column range for current subtable
          col_range <- unique(c(1, (inc_num + 1):lt))
          split_tab <- extract_table[, col_range]# Subset the table
          sub_edited_inline_text <- label_subtitle1[col_range]# Subset headers
          # Determine stratification labels for subtable
          #col_strat_var_levels = names(table(data[,'demog_c1']))
          col_strat_var_levels = other_language[1:2,language]
          
          sub_labels <- c('',rep(c(col_strat_var_levels,
                                   other_language[3,language]), each = 3))[col_range]
          #########
          total_pos = grep(other_language[4,language],split_tab[,1])
          # Replace median title placeholder if needed
          if(length(median_title)==1){
            sub_edited_inline_text = gsub(other_language[5,language],
                                          other_language[10,language],
                                          sub_edited_inline_text)
          }
          # Determine vertical lines for column borders
          sub_ext_ncols = ncol(split_tab)%/%3
          vertical_lines = (4+c(0, seq(3,999,3)))[1:(sub_ext_ncols-1)]
          # Reverse table for Arabic
          if(language =='arabic')
          {
            pre_sub_edited_table =  rbind(rep(table_title,ncol(split_tab)),
                                          sub_labels,
                                          sub_edited_inline_text,as.matrix(split_tab))%>%
              as.data.frame()%>%rev()
            vline_num = ncol(split_tab) - vertical_lines
          }else{
            pre_sub_edited_table =  rbind(rep(table_title,ncol(split_tab)),
                                          sub_labels,
                                          sub_edited_inline_text,as.matrix(split_tab))%>%
              as.data.frame()
            vline_num = vertical_lines
          }
          
          # Apply formatting using external function
          source('scripts/functions/table_formatting_function_nondemog.R')
          # Store formatted subtable
          frame_split_tables[[lt]] = sub_edited_table
          inc_num=lt
        }
        
        # Filter out any NULL subtables
        filtered_sub_framelist = Filter(function(x) !is.null(x), frame_split_tables)
        # Add subtables to Word document sequentially
        if(indicator_short_desc %in% next_short_desc)
        {
          eval(parse(text=paste0('doc = doc %>% body_add_flextable(width(filtered_sub_framelist[[',1:length(filtered_sub_framelist),']],
          width = dim(filtered_sub_framelist[[',1:length(filtered_sub_framelist),']])$widths*6.5/(flextable_dim(filtered_sub_framelist',
                                 '[[',1:length(filtered_sub_framelist),']])$widths)))%>%
          body_add("\n", style = "Normal")', sep='\n')))
        } else{
          eval(parse(text=paste0('doc = doc %>% body_add_flextable(width(filtered_sub_framelist[[',1:length(filtered_sub_framelist),']],
          width = dim(filtered_sub_framelist[[',1:length(filtered_sub_framelist),']])$widths*6.5/(flextable_dim(filtered_sub_framelist',
                                 '[[',1:length(filtered_sub_framelist),']])$widths)))%>%
          body_add("\n", style = "Normal")', sep='\n')))
          doc = doc %>% body_add_break()
        }
        
      }
      ###########################################################################
      ## ---------------- Handling Output Tables as Lists ----------------------
      ###########################################################################
      
      # Some indicators return a list of tables (stratified by a column)
      # rather than a single data frame. This branch handles these.
      
    } else if(any(class(extract_table)=="list")){
      #########################################################################
      ## Handling Indicators Where extract_table is a List of Tables
      #########################################################################
      
      # Some indicators return a list of tables (e.g., stratified by a column)
      # rather than a single data frame. This branch handles those cases.
      
      # ---------------- Handle Row Merging and Inline Subtitles ----------------
      if(all(is.na(subtitle2))|all(subtitle2==''))
      {
        # If subtitle2 is missing or blank, we only need a simple 2-row header
        row_merge = 2
        # Determine inline text depending on whether the variable is categorical
        inline_text = if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical'){
          paste0('% ',do.call('c',strsplit(subtitle1, "[;]")))
        }else{do.call('c',strsplit(subtitle1, "[;]"))}
        # Construct the header row for the flextable
        edited_inline_text = c(row_strat_variable_titles[1],'n',
                               strsplit(paste0(inline_text, collapse=paste0('; ',
                                                                            other_language[5,language],'; n; ')),'[;]')[[1]],
                               paste0(' ',other_language[5,language]))
        # Special handling for Demographics: simpler header
        if(i==demog_section_header)
        {
          edited_inline_text = c(row_strat_variable_titles[1],'n',paste0(inline_text))
        }
        
      } else{
        # If subtitle2 is present, we use a more complex 3-row header
        row_merge = 3
        inline_text = if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical'){
          paste0('% ',do.call('c',strsplit(subtitle2, "[;]")))
          
        }else{do.call('c',strsplit(subtitle2, "[;]"))}
        # Calculate how many times to replicate subtitle1 across columns
        notimes = eval(parse(text = paste0('c(',
                                           paste0('length(do.call("c",strsplit(strsplit(sub_formatrix$subtitle2, "[:]")[[1]][',
                                                  1:length(subtitle1),'], "[;]")))', collapse = ','),
                                           ')')))
        # Construct a two-row header for the flextable
        edited_inline_text = rbind(c('',rep(subtitle1,notimes*3)),# First row: subtitle1 replicated
                                   c(row_strat_variable_titles[1],'n',
                                     strsplit(paste0(inline_text,
                                                     collapse=paste0('; ',other_language[5,language],'; n; ')),'[;]')[[1]],
                                     paste0(' ',other_language[5,language])))
      }
      
      # ---------------- Create Flextable Objects for Each Stratified Column ----------------
      #col_strat_var_levels = names(table(data[,'demog_c1']))
      col_strat_var_levels = other_language[1:2,language]
      
      # Dynamically generate flextable objects for each stratification level + total column
      eval(parse(text = paste0('flex_output',1:(length(col_strat_var_levels)+1), 
                               ' = flextab_function(index =',1:(length(col_strat_var_levels)+1),
                               ',table_label ="',c(col_strat_var_levels,other_language[3,language]),'")', 
                               sep = '\n')))
      
      # ---------------- Determine Column Stratification Levels ----------------
      extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))
      collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))
      # Remove "_full" suffix if present
      column_strat = ifelse(length(grep('_full',tolower(column_strat),v=T))>0, gsub('_full','',column_strat),column_strat)
      # Determine which columns (levels) to report based on column stratification choice
      levels_to_report = if(column_strat == 'all'){
        1:((length(col_strat_var_levels))+1) # All stratification levels
      } else if (column_strat==collevel){
        extracted_integers # Only specific levels
      } else if (column_strat=='total_col'){
        (length(col_strat_var_levels))+1 # Only total column
      } else{}
      
      #
      levels_to_report = as.numeric(as.character(levels_to_report))
      # ---------------- Determine Next Indicator for Table Grouping ----------------
      next_ind = names(output_table)[indicator_position+1]
      next_short_desc = do.call('c',strsplit(next_ind, "[__]"))[[1]]
      # ---------------- Determine Number of Flextables for Each Column ----------------
      output_length = eval(parse(text = paste0('c(',paste0('length(flex_output',
                                                           1:(length(col_strat_var_levels)+1),')', 
                                                           collapse = ','),')')))
      # ---------------- Insert Flextables into Word Document ----------------
      if(indicator_short_desc %in% next_short_desc)
      {
        # If next indicator is in same group, insert tables sequentially without page break        
        out_levels = NULL
        for(out_levels in levels_to_report)
        {
          sub_out_levels = output_length[out_levels]
          subnote = NULL
          for(subnote in 1:sub_out_levels)
          {
            eval(parse(text=paste0('doc = doc %>% body_add_flextable(width(flex_output',out_levels,'[[',subnote,']],
          width = dim(flex_output',out_levels,'[[',subnote,']])$widths*6.5/(flextable_dim(flex_output',
                                   out_levels,'[[',subnote,']])$widths)))%>%
          body_add("\n", style = "Normal")', sep='\n')))
            ###
            if(out_levels <(length(col_strat_var_levels)+1))
            {
              
            } else{
              
              doc = doc %>% body_add_break()
            }
            
          }
        }
        
      } else{
        
        out_levels = NULL
        # If next indicator is different, add page break after inserting tables
        for(out_levels in levels_to_report)
        {
          sub_out_levels = output_length[out_levels]
          subnote = NULL
          for(subnote in 1:sub_out_levels)
          {
            eval(parse(text=paste0('doc = doc %>% body_add_flextable(width(flex_output',out_levels,'[[',subnote,']],
          width = dim(flex_output',out_levels,'[[',subnote,']])$widths*6.5/(flextable_dim(flex_output',
                                   out_levels,'[[',subnote,']])$widths)))%>%
          body_add("\n", style = "Normal")', sep='\n')))
          }
        }
        doc = doc %>% body_add_break()
      }
      
    } else{}
    # ---------------- Write Individual Indicator Document ----------------
    section_position = grep(i, unique(indicator_matrix$section))
    print(doc, target =paste0('temp/Part',section_position,'_Indicator',ind_no,'.docx'))
    ind_no = ind_no+1
  }
  
  # ---------------- Consolidate Individual Indicators into Section Document ----------------
  section_header = unique(section_matrix$section)[1]
  title_text = ftext(section_header) %>% fpar()
  docx = officer::read_docx()%>%
    body_add(title_text, style = "heading 1")
  
  # Append all individual indicator documents
  p=NULL
  for(p in 1:length(names(output_table))){
    path = paste0(getwd(),'/temp/Part',section_position,'_Indicator',p,'.docx')
    docx = body_add_docx(docx, path, pos = "after")
  }
  # Export consolidated section document
  print(docx, target =paste0('temp/Part',section_position,'.docx'))
}


#########################################################################
# Initialize databook using the style template templates/databook_template.docx
databook = officer::read_docx(path = "templates/databook_template.docx")
databook = databook %>%
  body_replace_all_text(old_value = "survey_year", 
                        new_value = as.character(survey_year), 
                        only_at_cursor = FALSE) %>%
  body_replace_all_text(old_value = "country_name", 
                        new_value = as.character(country), 
                        only_at_cursor = FALSE)


###########################################################################
## Combine All Section Documents into the Final Databook
###########################################################################

# Loop through each section that exists in the indicator matrix.
# Each section has previously been exported as an intermediate
# Word document stored in the "temp" folder (e.g., Part1.docx, Part2.docx, etc.).

i=NULL
for(i in 1:(length(unique(indicator_matrix$section)))) {
  
  # ----------------------------------------------------------------------
  # Define the file path for the current section document
  # Each section was previously saved as: temp/Part{i}.docx
  # ----------------------------------------------------------------------
  path <- paste0(getwd(), '/temp/Part', i, '.docx')
  # ----------------------------------------------------------------------
  # Insert the content of the section document into the main databook
  # `body_add_docx()` appends the external Word document at the current
  # cursor position of the databook object.
  # ----------------------------------------------------------------------
  databook <- databook %>%
    body_add_docx(path, pos = "after")  # Insert the document content
  
  # ----------------------------------------------------------------------
  # Insert a page break after each section so that the next section
  # starts on a new page. This improves readability of the final report.
  #
  # The break is added only if the current section is not the last one.
  # ----------------------------------------------------------------------
  if (i < (1 + length(unique(indicator_matrix$section)))) {
    databook = databook %>% body_add_break(pos = "after") # Start a new section (on a new page)
  }
}
###########################################################################
## Export Final Databook Document
###########################################################################

# Save the assembled databook to the "outputs" directory.
# The file name includes:
# - Country ISO code
# - Survey year
# - Timestamp to ensure unique filenames and version tracking.

print(databook, 
      target = paste0('outputs/', 
                      country_ISO, '-', 
                      survey_year, '_Databook_', 
                      format(Sys.time(), "%d-%b-%y_%H-%M-%S"), 
                      '.docx'))
