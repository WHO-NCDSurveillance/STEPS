i=NULL
j=NULL
k=NULL
for (i in unique(indicator_matrix$section))
{
  data = analysis_data
  section_matrix = indicator_matrix %>% dplyr::filter(section == i)
  
  wt_step = unique(section_matrix$weight_step)[1]
  data[,wt_step] = as.numeric(as.character(data[,wt_step]))
  data = data %>% dplyr::filter(!is.na(get(wt_step)))
  svy_data = svydesign(id=~psu, weights=~get(wt_step),strata=~stratum, data=data,nest = T)
  
  output_table = list()
  
  for(j in unique(section_matrix$indicator_short_desc))
  {
    pre_sub_matrix = indicator_matrix %>% dplyr::filter(indicator_short_desc == j)
      sub_row = NULL
      for(sub_row in 1:nrow(pre_sub_matrix))
      {
      sub_matrix = pre_sub_matrix[sub_row,]
      subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))
      type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))
      denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))
      #median_compute = sub_matrix$median_computation =='yes' & !is.na(sub_matrix$median_computation)
      median_compute = unique(type_indicators)=='median'
      
      ###population level indicators
      if(is.na(sub_matrix$pop_level_num_denom))
      {
        pop_num_denom = NA
      }else{pop_num_denom = do.call('c',strsplit(sub_matrix$pop_level_num_denom, "[;]"))}
      #checking if the indicators are to be combined in the same table
      similarity_ind = unique(gsub("\\d{1,2}$", "", na.omit(subset_indicators[1:9])))
      ##Flag for combining outputs
      combine_ind = length(similarity_ind)==1 & length(subset_indicators)>1 
      #combined_tables = list()
      if(i == "Demographic"){col_strat_var_levels = names(table(data[,'demog_c1']))} else{col_strat_var_levels = names(table(data[,col_strat_variable]))}
      
      ##
      eval(parse(text=paste0('sublist_', 1:length(col_strat_var_levels),'= list()', sep='\n')))
      ##
      total_tab = list() 
      for(k in subset_indicators)
      {
       #data[,"agerange"] = data[,unique(sub_matrix$agevar)]
      #svy_data = svydesign(id=~psu, weights=~get(wt_step),strata=~stratum, data=data,nest = T)
       
  
        if(type_indicators[which(subset_indicators %in%k)] == 'median'|type_indicators[which(subset_indicators %in%k)] == 'mean') 
        {
          data[,k] = as.numeric(data[,k])
          if(i =="Demographic")
          {
            if(denom_logic[which(subset_indicators %in%k)] =='all')
            {
              datum = data%>%dplyr::filter(!is.na(eval(parse(text = k)))) 
            }else{
              datum = data%>%dplyr::filter(eval(parse(text=paste0('(',denom_logic[which(subset_indicators %in%k)],')'))))
            }
            
            ###Calling analyses functions
            list_demog_vars = c("agerange", setdiff(row_strat_variables,"agerange"))
            num_demog = NULL
            summary_table = NULL
            ##
            for(num_demog in list_demog_vars)
            {
              summary_table = rbind(summary_table, demog_numeric(num_demog))
            }
            ###
            summary_table = summary_table %>% 
                            mutate(`eval(parse(text = strat_variable))` = ifelse(is.na(`eval(parse(text = strat_variable))`),'Total',as.character(`eval(parse(text = strat_variable))`)))
            
          } else{
            eval(parse(text = paste0('formula = ~', k)))
            denom_condition = denom_logic[which(subset_indicators %in%k)]
            sub_pop_num_denom = pop_num_denom[which(subset_indicators %in%k)]
            ###Stratified analysis
            s = NULL
            int_summary_table = list()
            
            for(s in na.omit(unique(c(row_strat_variables)))) 
            {
              s_position = grep(s, row_strat_variables)
              sub_label=row_strat_variable_titles[s_position]
              
              res_table = analysis_numeric_fn(row_strat = s, col_strat = col_strat_variable)%>%as.data.frame()
              if(s_position!=1) 
              {
                res_table = rbind(c(sub_label, rep('',ncol(res_table)-1)),res_table)%>%as.data.frame()
                colnames(res_table) = all_tabcolnames
              } else{}
              int_summary_table[[s]]= res_table
              all_tabcolnames = colnames(int_summary_table[[1]])
            }
            ##
            summary_table = do.call('rbind', int_summary_table) %>% as.data.frame()
            ###Formatting summary_table
            eval(parse(text=paste0('summary_table$`',paste0(col_strat_var_levels,'_',c('b`')),'[as.numeric(summary_table$`',paste0(col_strat_var_levels,'_',c('a`)')),'<',denom_limit,']="-"', sep = '\n')))
            eval(parse(text=paste0('summary_table$`',paste0(col_strat_var_levels,'_',c('ci`')),'[as.numeric(summary_table$`',paste0(col_strat_var_levels,'_',c('a`)')),'<',denom_limit,']="-"', sep = '\n')))
            summary_table$Total_b[as.numeric(summary_table$Total_a) < denom_limit] = '-'
            summary_table$Total_ci[as.numeric(summary_table$Total_a) < denom_limit] = '-'
          }
        }
        #
        else if(type_indicators[which(subset_indicators %in%k)] == 'categorical')
        {
          if(i =="Demographic")
          { 
            if(denom_logic[which(subset_indicators %in%k)] =='all')
            {
              datum = data #%>%dplyr::filter(!is.na(agerange))
            }else{
              datum = data%>%dplyr::filter(eval(parse(text=paste0('(',denom_logic[which(subset_indicators %in%k)],')'))))
            }
             #####Calling summary table here
            #Adding computations across the age categories to the table above
            list_demog_vars = c("agerange",'', setdiff(row_strat_variables,"agerange")) ####To recheck that '' not coming after 'Agerange"
            num_demog = NULL
            summary_table = NULL
            ##
            summary_table = demog_cat() 
            ###
            for(num_demog in list_demog_vars[-1]) #####Recheck the edits[-1]
            {
              summary_table = full_join(summary_table %>% mutate_all(as.character), 
                                        demog_cat(num_demog)%>% mutate_all(as.character))
            }
            
            summary_table = summary_table %>%
                            mutate(`eval(parse(text = strat_variable))` = ifelse(is.na(`eval(parse(text = strat_variable))`),'Total',as.character(`eval(parse(text = strat_variable))`)))
            ####
            ###Editing summary_table
            summ_table_names = sort(names(summary_table))
            if(sub_matrix$logic_condition_var=='c1'){
              names_to_select = c("eval(parse(text = strat_variable))",'Men_1','sex_Total_1_Percentage','Women_2','sex_Total_2_Percentage','Total_Count','Men_1_Percentage')##Note Men_1_Percentage will be 100% throughout
              summary_table = summary_table %>% dplyr::select(all_of(names_to_select))
              colnames(summary_table)=c("eval(parse(text = strat_variable))",'Men_a','Men_b','Women_a','Women_b','Total_a','Total_b')
              ####
              ###Editing summary_table
              summary_table = summary_table %>% dplyr::mutate(Men_b = replace(Men_b,as.numeric(Men_a)<denom_limit,'-'),
                                                              Women_b = replace(Women_b,as.numeric(Women_a)<denom_limit,'-'),
                                                              Total_b = replace(Total_b,as.numeric(Total_a)<denom_limit,'-'))
              #####
            } else {
              names_to_select = c("eval(parse(text = strat_variable))",'men_Total_Count', grep('Percentage',grep('Men_',summ_table_names,v=T), v = T)
                                  ,'women_Total_Count', grep('Percentage',grep('Women_',summ_table_names,v=T), v = T)
                                  ,'Total_Count', grep('Percentage',grep('sex_',summ_table_names,v=T), v = T))
              summary_table = summary_table %>% dplyr::select(all_of(names_to_select)) %>% as.data.frame()
              
              ####Editing summary_table
              eval(parse(text=paste0('summary_table$',grep('Percentage',grep('Men_',summ_table_names,v=T), v = T),'[as.numeric(summary_table$men_Total_Count)<',denom_limit,']="-"', sep = '\n')))
              eval(parse(text=paste0('summary_table$',grep('Percentage',grep('Women_',summ_table_names,v=T), v = T),'[as.numeric(summary_table$women_Total_Count)<',denom_limit,']="-"', sep = '\n')))
              eval(parse(text=paste0('summary_table$',grep('Percentage',grep('sex_',summ_table_names,v=T), v = T),'[as.numeric(summary_table$Total_Count)<',denom_limit,']="-"', sep = '\n')))
              
              ########Calling custom sort functions
              summary_table = list(
                summary_table %>%dplyr::select(c("eval(parse(text = strat_variable))",'men_Total_Count', custom_sort1(grep('Percentage',grep('Men_',summ_table_names,v=T), v = T)))),
                summary_table %>%dplyr::select(c("eval(parse(text = strat_variable))",'women_Total_Count', custom_sort2(grep('Percentage',grep('Women_',summ_table_names,v=T), v = T)))),
                summary_table %>%dplyr::select(c("eval(parse(text = strat_variable))",'Total_Count', custom_sort3(grep('Percentage',grep('sex_',summ_table_names,v=T), v = T)))))
            }
   
            
          }else{
            eval(parse(text=paste0('formula = ~I(',k, '=="',1,'")')))
            denom_condition = denom_logic[which(subset_indicators %in%k)]
            ########
            s = NULL
            int_summary_table = list()
            
            for(s in na.omit(unique(c(row_strat_variables)))) 
            {
              s_position = grep(s, row_strat_variables)
              sub_label = row_strat_variable_titles[s_position]
              
              res_table = analysis_categorical_fn(row_strat = s, col_strat = col_strat_variable)%>%as.data.frame()
              if(s_position!=1)
              {
                res_table = rbind(c(sub_label, rep('',ncol(res_table)-1)),res_table)%>%as.data.frame()
                colnames(res_table) = all_tabcolnames
              } else{}
              int_summary_table[[s]]= res_table
              all_tabcolnames = colnames(int_summary_table[[1]])
            }
            ##
            summary_table = do.call('rbind', int_summary_table) %>% as.data.frame()
            ###Formatting summary_table
            eval(parse(text=paste0('summary_table$`',paste0(col_strat_var_levels,'_',c('b`')),'[as.numeric(summary_table$`',paste0(col_strat_var_levels,'_',c('a`)')),'<',denom_limit,']="-"', sep = '\n')))
            eval(parse(text=paste0('summary_table$`',paste0(col_strat_var_levels,'_',c('ci`')),'[as.numeric(summary_table$`',paste0(col_strat_var_levels,'_',c('a`)')),'<',denom_limit,']="-"', sep = '\n')))
            summary_table$Total_b[as.numeric(summary_table$Total_a) < denom_limit] = '-'
            summary_table$Total_ci[as.numeric(summary_table$Total_a) < denom_limit] = '-'
          }
        } else{}
        ###Combining outputs using the flag above
        if (combine_ind) {
          if (i == "Demographic") {
            if (any(class(summary_table) != 'list')) {
              sublist_1[[k]] = summary_table %>% dplyr::select(all_of(c("eval(parse(text = strat_variable))", 'Men_a', 'Men_b')))
              sublist_2[[k]] = summary_table %>% dplyr::select(all_of(c("eval(parse(text = strat_variable))", 'Women_a', 'Women_b')))
              total_tab[[k]] = summary_table %>% dplyr::select(all_of(c("eval(parse(text = strat_variable))", 'Total_a', 'Total_b')))
            } else {
              sublist_1[[k]] = summary_table[[1]]
              sublist_2[[k]] = summary_table[[2]]
              total_tab[[k]] = summary_table[[3]]
            }
          } else {
            all_colmames = c(sort(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
                                            paste0(col_strat_var_levels,'_',c('ci')))),'Total_a','Total_b','Total_ci')
            ###
            col_list = list()
            llist = NULL
            for(llist in col_strat_var_levels)
            {
              list_names = grep(llist,all_colmames, v=T)
              col_list[[llist]]=c(colnames(summary_table)[1],list_names)
            }
            #####
            eval(parse(text=paste0('sublist_', 1:length(col_strat_var_levels),'[[k]] = summary_table %>% dplyr::select(all_of(c(col_list[["',col_strat_var_levels,'"]])))', sep='\n')))
            total_tab[[k]] = summary_table %>% dplyr::select(all_of(c(colnames(summary_table)[1], 'Total_a', 'Total_b', 'Total_ci')))
          }
        } else {
          output_table[[paste0(j, '__', k)]] = summary_table
        }
        
        
      }
      ###Organising outputs for indicators combined into one table
      if(combine_ind)
      {
          eval(parse(text=paste0('merged_sublist_', 1:length(col_strat_var_levels),' = reduce(sublist_',1:length(col_strat_var_levels),', full_join, by = "',colnames(as.data.frame(sublist_1[[1]]))[1],'")', sep='\n')))
          merged_total_tab = reduce(total_tab, full_join, by = colnames(as.data.frame(sublist_1[[1]]))[1])
          output_table[[paste0(j,'__',k)]] = eval(parse(text=paste0('list(',paste0('merged_sublist_', 1:length(col_strat_var_levels), collapse = ','),', merged_total_tab)')))
        
      } else{}
    }
  } 
  ###Formatting of the output
  g = NULL
  ind_no = 1
  for(g in names(output_table))
  {
    doc = read_docx()
    
    #
    tab_length = length(grep('_a',names(output_table[[g]][[1]]), v=T))
    if(tab_length < 10)
    {
    matching_indicator = gsub("\\d{1}$", "", do.call('c',strsplit(g, "[__]"))[[3]])
    }else{matching_indicator = gsub("\\d{1,2}$", "", do.call('c',strsplit(g, "[__]"))[[3]])}
    
    sub_formatrix = indicator_matrix[grep(paste0('\\b',matching_indicator,'\\b'), tolower(indicator_matrix$indicator)),] #%>% dplyr::filter(add_factsheet=='No')
    #
    indicator = sub_formatrix$indicator
    indicator_short_desc = unique(sub_formatrix$indicator_short_desc)
    description = sub_formatrix$descritpiton
    instrument_questions = sub_formatrix$instrument_questions
    group_type = unique(do.call('c',strsplit(sub_formatrix$type, "[;]")))[1]
    #
    table_title = sub_formatrix$table_title
    subtitle1 = do.call('c',strsplit(sub_formatrix$subtitle1, "[;]"))
    if(length(sub_formatrix$subtitle2)>0)
    {
    subtitle2 = do.call('c',strsplit(do.call('c',strsplit(as.character(sub_formatrix$subtitle2), "[:]")), "[;]"))
    } else{}
    #
    subtitle1 = ifelse(is.na(subtitle1) & sub_formatrix$section!="Demographic",'',subtitle1)
    subtitle2 = ifelse(is.na(subtitle2),'',subtitle2)
    ##
    if(sub_formatrix$section=="Demographic")
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
    ####Indicator title
    ###
    if(language == 'arabic')
    {
      def_indicator = as.data.frame(rev(rbind(cbind(indicator_short_desc,paste0(other_language[6,language],': ',description)),
                                          cbind(indicator_short_desc,paste0(other_language[7,language],':\n',instrument_questions))) %>%as.data.frame())) %>%
        flextable() %>% autofit() %>% delete_part(part = "header")%>%merge_at(j=2)%>% valign(j = 2,valign = 'top')%>%
        width(j = 1, 10, unit = "in") %>%
        flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
        bold(j = 2) %>% fontsize(size = 9 ,part = "all") %>% border_remove()


    }else{
      def_indicator = as.data.frame(rbind(cbind(indicator_short_desc,paste0(other_language[6,language],': ',description)),
                                          cbind(indicator_short_desc,paste0(other_language[7,language],':\n',instrument_questions)))) %>%
        flextable() %>% autofit() %>% delete_part(part = "header")%>%merge_at(j=1)%>% valign(j = 1,valign = 'top')%>%
        width(j = 2, 10, unit = "in") %>%
        flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
        bold(j = 1) %>% fontsize(size = 9 ,part = "all") %>% border_remove() 
    }
    ###indicator_position = grep(g,names(output_table))
    indicator_position = grep(paste0('\\b',matching_indicator,'\\b'),tolower(section_matrix$indicator))
    
    prev_indicator = ifelse(indicator_position>1,names(output_table)[indicator_position-1],names(output_table)[1])
    prev_short_desc = do.call('c',strsplit(prev_indicator, "[__]"))[[1]]
    
    if(any(!(indicator_short_desc %in% prev_short_desc) | indicator_position==1))
    {
      doc = doc %>% body_add_flextable(width(def_indicator, width = dim(def_indicator)$widths*6.4/(flextable_dim(def_indicator)$widths)))%>%
        body_add('\n', style = "Normal") 
    }else{}
    ###
    extract_table = output_table[[g]]
    ###
    range_levels = length(names(table(data[,unique(sub_formatrix$agevar)])))

    # if(i=="Cardiovascular disease risk")
    # {
    #   datax = data %>%mutate(agerange = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
    #                         agerange = factor(agerange,levels=1:2, labels=c('40-54','55-69')))
    #   range_levels = length(names(table(datax[,row_strat_variables[1]])))
    #   
    # } else if(i=="Summary of Combined Risk Factors")
    # {
    #   datax = data %>%mutate(agerange = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
    #                         agerange = factor(agerange,levels=1:2, labels=c('18-44','45-69')))
    #   range_levels = length(names(table(datax[,row_strat_variables[1]])))
    #   
    # }else{range_levels = length(names(table(data[,row_strat_variables[1]])))}
    
    #####
    all_hlines = c()
    strat_var_levels = if(any(!is.na(row_strat_variables)))
    {
      length_strat = length(row_strat_variables)
      sub_var_levels = eval(parse(text=paste0('c(',paste0('length(names(table(data[,"',unique(sub_formatrix$agevar),'"])))', collapse = ','),')')))
      all_hlines = c(all_hlines,sub_var_levels)
    } else{NA}
    ###Determining lines to colour white
    ##
    ln = NULL
    if(all(is.na(subtitle2))|all(subtitle2==''))
    {
      first_ln = 4:(all_hlines[1]+2)
      if(range_levels==2){first_ln = 4}
    } else{
      first_ln = 5:(all_hlines[1]+3)
      if(range_levels==2){first_ln = 5}
      }
    #
    if(i=="Demographic"){first_ln = 4:(length(names(table(data[,"agerange"])))+2)}
    final_hlines = c(first_ln)
    
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
   
    #if(i == "Cervical Cancer Screening"){final_hlines = final_hlines[-c(1:4)]-4}
    ####
    std_border = fp_border(color = "grey",width = 5)
    white_border = fp_border(color = "white")
    #
    if(any(class(extract_table)=="data.frame"))
    {
      ##
      if(language == 'french')
      {
        extract_table = extract_table %>% as.matrix
        extract_table = gsub('[.]',',',extract_table)
        extract_table = extract_table %>% as.data.frame()
      } else{}
      ##
      total_pos = grep('Total',extract_table[,1])
      if(i=="Demographic"){total_pos = grep('Total',as.data.frame(extract_table)[,1])}
      ###
      inline_text = if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical'){
        paste0('% ',do.call('c',strsplit(subtitle1, "[;]")))
        
      }else{do.call('c',strsplit(subtitle1, "[;]"))}
      #
      if(length(inline_text)>1)
      {
        label_subtitle1 = c(row_strat_variable_titles[1],'n',inline_text[1],other_language[5,language],'n',inline_text[2],other_language[5,language],'n',inline_text[3],other_language[5,language])
        
      } else{
        label_subtitle1 = c(row_strat_variable_titles[1],rep(c("n",inline_text,other_language[5,language]),(ncol(extract_table)-1)/3))
        
      }
      if(i=="Demographic"){
        label_subtitle1=if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical')
        {c(other_language[8,language],'n','%','n','%','n','%')} else{(c(other_language[8,language],'n',other_language[9,language],'n',other_language[9,language],'n',other_language[9,language]))}}
      
      ########Outputs
      next_ind = names(output_table)[indicator_position+1]
      next_short_desc = do.call('c',strsplit(next_ind, "[__]"))[[1]]
      #
      extract_table[,1][extract_table[,1]=='Total'] = other_language[4,language]
      ###
      if(i=="Demographic")
      {
        if(language =='arabic')
        {
          pre_edited_table = rbind(rep(table_title,7),
                                   c('',rep(other_language[1,language],2),rep(other_language[2,language],2),rep(other_language[3,language],2)),label_subtitle1,as.matrix(extract_table))%>%
                             as.data.frame() %>%rev()
        vline_num = c(2,4)  
        }else{
          pre_edited_table =  rbind(rep(table_title,7),
                c('',rep(other_language[1,language],2),rep(other_language[2,language],2),rep(other_language[3,language],2)),label_subtitle1,as.matrix(extract_table)) %>% 
            as.data.frame() 
          vline_num = c(3,5) 
        }
        
        ####Editing of extract_table
        source('scripts/functions/table_formatting_function_demog.R')
       
        ####
        if(indicator_short_desc %in% next_short_desc)
        {
          doc = doc %>% body_add_flextable(width(edited_table, width = dim(edited_table)$widths*6.5/(flextable_dim(edited_table)$widths)))%>%body_add('\n', style = "Normal")
        }else{doc = doc %>% body_add_flextable(width(edited_table, width = dim(edited_table)$widths*6.5/(flextable_dim(edited_table)$widths))) %>% body_add_break()}
        
      } else{
        
        ext_ncols = ncol(extract_table)
        lt = NULL
        inc_num=0
        frame_split_tables = list()
        colnosplit = cumsum(frame_split_into_parts(num_cols = ext_ncols))
        ##
        for(lt in colnosplit)
        {
          # Determine the column range based on the condition
          col_range <- unique(c(1, (inc_num + 1):lt))
          # Subset the extract_table using the determined column range
          split_tab <- extract_table[, col_range]
          # Subset the label_subtitle1 based on the condition
          sub_edited_inline_text <- label_subtitle1[col_range]
          if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_c1']))}
          sub_labels <- c('',rep(c(col_strat_var_levels,other_language[4,language]), each = 3))[col_range]
          #########
          total_pos = grep(other_language[4,language],split_tab[,1])
          ###formatting the sub table
          if(length(median_title)==1){sub_edited_inline_text = gsub(other_language[5,language],other_language[10,language],sub_edited_inline_text)}
          #
          sub_ext_ncols = ncol(split_tab)%/%3
          vertical_lines = (4+c(0, seq(3,999,3)))[1:(sub_ext_ncols-1)]
          ###
          if(language =='arabic')
          {
            pre_sub_edited_table =  rbind(rep(table_title,ncol(split_tab)),
                                          sub_labels,
                                          sub_edited_inline_text,as.matrix(split_tab))%>%as.data.frame()%>%rev()
            vline_num = ncol(split_tab) - vertical_lines  
          }else{
            pre_sub_edited_table =  rbind(rep(table_title,ncol(split_tab)),
                                          sub_labels,
                                          sub_edited_inline_text,as.matrix(split_tab))%>%as.data.frame()
            vline_num = vertical_lines 
          }
          
          ###
          source('scripts/functions/table_formatting_function_nondemog.R')
          #####
          frame_split_tables[[lt]] = sub_edited_table
          inc_num=lt
        }
        
      ####
      filtered_sub_framelist = Filter(function(x) !is.null(x), frame_split_tables)
      ####
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
      ######Write out the table on the word document
    } else if(any(class(extract_table)=="list")){
      ##
      if(all(is.na(subtitle2))|all(subtitle2==''))
      {
        row_merge = 2
        inline_text = if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical'){
          paste0('% ',do.call('c',strsplit(subtitle1, "[;]")))
        }else{do.call('c',strsplit(subtitle1, "[;]"))}
        #
        edited_inline_text = c(row_strat_variable_titles[1],'n',strsplit(paste0(inline_text, collapse=paste0('; ',other_language[5,language],'; n; ')),'[;]')[[1]],paste0(' ',other_language[5,language]))
        #
        if(i=="Demographic")
        {
          edited_inline_text = c(row_strat_variable_titles[1],'n',paste0(inline_text))
        } 
        
      } else{
        row_merge = 3
        inline_text = if (unique(strsplit(sub_formatrix$type,'[;]')[[1]])=='categorical'){
          paste0('% ',do.call('c',strsplit(subtitle2, "[;]")))
          
        }else{do.call('c',strsplit(subtitle2, "[;]"))}
        #number of times to replicate subtitle 1
        notimes = eval(parse(text = paste0('c(',paste0('length(do.call("c",strsplit(strsplit(sub_formatrix$subtitle2, "[:]")[[1]][',
                                                       1:length(subtitle1),'], "[;]")))', collapse = ','),')')))
        #
        edited_inline_text = rbind(c('',rep(subtitle1,notimes*3)),
                                   c(row_strat_variable_titles[1],'n',
                                     strsplit(paste0(inline_text, 
                                                     collapse=paste0('; ',other_language[5,language],'; n; ')),'[;]')[[1]],paste0(' ',other_language[5,language])))
      }
      
       ###start of a function
      ###Flex table outputs::Calling flextable outputs
      if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_c1']))}
      
      eval(parse(text = paste0('flex_output',1:(length(col_strat_var_levels)+1), ' = flextab_function(index =',1:(length(col_strat_var_levels)+1),
                               ',table_label ="',c(col_strat_var_levels,other_language[4,language]),'")', sep = '\n')))
      
      ####
      extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))
      collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))
      column_strat = ifelse(length(grep('_full',tolower(column_strat),v=T))>0, gsub('_full','',column_strat),column_strat)
      
      
      
      levels_to_report = if(column_strat == 'all'){
        1:((length(col_strat_var_levels))+1)
      } else if (column_strat==collevel){
        extracted_integers
      } else if (column_strat=='total_col'){
        (length(col_strat_var_levels))+1
      } else{}
      
      ##
      levels_to_report = as.numeric(as.character(levels_to_report))
      ####
      next_ind = names(output_table)[indicator_position+1]
      next_short_desc = do.call('c',strsplit(next_ind, "[__]"))[[1]]
      ###
      output_length = eval(parse(text = paste0('c(',paste0('length(flex_output',1:(length(col_strat_var_levels)+1),')', collapse = ','),')')))
      
      if(indicator_short_desc %in% next_short_desc)
      {
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
    
    section_position = grep(i, unique(indicator_matrix$section))
    print(doc, target =paste0('temp/Part',section_position,'_Indicator',ind_no,'.docx'))
    ind_no = ind_no+1
  }
  
  ########Consolidating printed pieces per indicator
  section_header_eng = unique(section_matrix$section)[1]
  lang_translation = with(language_translation, cbind(english, get(language)))
  translated_text = lang_translation[,2][which(lang_translation[,1] %in% section_header_eng)]
  #######
  title_text = ftext(translated_text) %>% fpar()
  docx = read_docx()%>% 
         body_add(title_text, style = "heading 1")
  
  ##
  p=NULL
  for(p in 1:length(names(output_table))){
    path = paste0(getwd(),'/temp/Part',section_position,'_Indicator',p,'.docx')
    docx = body_add_docx(docx, path, pos = "after") 
  }
  ##
  print(docx, target =paste0('temp/Part',section_position,'.docx'))
}


#########################################################################
i=NULL
library(officer)
library(magrittr)


# Initialize databook using the style template templates/databook_template.docx
databook <- read_docx(path = "templates/databook_template.docx")
databook <- databook %>%
  body_replace_all_text(old_value = "survey_year", new_value = as.character(survey_year), only_at_cursor = FALSE) %>%
  body_replace_all_text(old_value = "country_name", new_value = as.character(country), only_at_cursor = FALSE)


# Loop through each section and add content to databook
for(i in 1:(length(unique(indicator_matrix$section)))) {
  
  # Specify the path for the current Part file
  path <- paste0(getwd(), '/temp/Part', i, '.docx')
  
  
  # Add the content of the current Part document
  databook <- databook %>%
    body_add_docx(path, pos = "after")  # Insert the document content
  
  # Add a page break after each part, except the last one
  if (i < (1 + length(unique(indicator_matrix$section)))) {
    databook <- databook %>% body_add_break(pos = "after") # Start a new section (on a new page)
  }
}
####
###
print(databook, target = paste0('outputs/', country_ISO, '-', survey_year, '_Databook_', format(Sys.time(), "%d-%b-%y_%H-%M-%S"), '.docx'))
