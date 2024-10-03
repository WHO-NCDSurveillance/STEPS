#column_strat = c('all','collevel1_2','collevel2','total_col', 'total_overall','total_row')
extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))
collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))
if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_c1']))}

col_strat_var_levels[col_strat_var_levels==other_language[1,2]]='Men'
col_strat_var_levels[col_strat_var_levels==other_language[2,2]]='Women'

if(column_strat =='all')
{
  sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% vline(j=vline_num, border = std_border)%>%
    hline(i=final_hlines, border = white_border)
}else if(column_strat == collevel)
{
  incl_level_names = col_strat_var_levels[extracted_integers]
  pre_sub_edited_table = pre_sub_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_sub_edited_table)))]
  #########
  if(length(row_strat_variables)>1)
  {
    all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,tail(1:nrow(pre_sub_edited_table),all_additional_levels+2)),]

  }else{
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),]
  }
  
  #########
  sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")#%>%
    #hline(i=final_hlines, border = white_border)
}else if(column_strat == 'total_col')
{
  pre_sub_edited_table = pre_sub_edited_table[,-c(grep(paste0(col_strat_var_levels, collapse = '|'),names(pre_sub_edited_table)))]
  
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% 
    hline(i=final_hlines, border = white_border)
  
}else if(column_strat == 'total_row')
{
  pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),]
  
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body") 
  
} else if (column_strat == 'total_overall')
{
  
  if(length(row_strat_variables)>1)
  {
    all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,tail(1:nrow(pre_sub_edited_table),all_additional_levels+2)),-c(grep(paste0(col_strat_var_levels, collapse = '|'),names(pre_sub_edited_table)))]
    
  }else{
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),-c(grep(paste0(col_strat_var_levels, collapse = '|'),names(pre_sub_edited_table)))]
  }
  
  
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  
  
}else{}