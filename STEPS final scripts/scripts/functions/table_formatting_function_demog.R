#column_strat = c('all','collevel1_2','collevel2','total_col', 'total_overall','total_row')
# Extract unique integers from the column_strat string by finding all digit sequences using regex
# and converting them into unique integers.
extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))
# Generate a column level string based on the extracted integers.
collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))
# Check if 'sex' is part of the column stratification variables and extract the levels accordingly.
if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_c1']))}
##
full_tab = ifelse(length(grep('full',column_strat, v=T))==1,TRUE, FALSE)
# Replace specific level names in the column stratification variables with 'Men' and 'Women'
# based on the language settings.
col_strat_var_levels[col_strat_var_levels==other_language[1,2]]='Men'
col_strat_var_levels[col_strat_var_levels==other_language[2,2]]='Women'

# If column_strat is set to 'all', generate a formatted table without dropping any levels of stratification.
if(column_strat =='all')
  {
edited_table = pre_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
  flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
  bold(i = c(1:2,total_pos+3))%>% 
  fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
  theme_vanilla() %>% 
  merge_h(i = 1:2) %>%
  padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
  align(align = "center", j=1:7, part = "body")  %>% vline(j=vline_num, border = std_border)%>%
  hline(i=final_hlines, border = white_border)
# If column_strat matches collevel, apply specific stratification based on the extracted levels.
}else if(column_strat == collevel & full_tab == FALSE)
{
  incl_level_names = col_strat_var_levels[extracted_integers]
  # Filter columns based on the included level names.
  pre_edited_table = pre_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_edited_table)))]
  #############
  # Handle additional levels if row stratification involves more than one variable.
  if(length(row_strat_variables)>1)
  {
    all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
    pre_edited_table = pre_edited_table[c(1:3,tail(1:nrow(pre_edited_table),all_additional_levels+2)),]
  }else{
    pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),]
  }
  # Generate the edited table with the specified formatting options.
  edited_table = pre_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_edited_table), part = "body")%>%
    vline(j=3, border = std_border)#%>%
    #hline(i=final_hlines, border = white_border)
  # Handle case where column_strat is 'total_col', which removes certain columns from the table.
  }else if(column_strat == collevel & full_tab == TRUE)
{
    incl_level_names = col_strat_var_levels[extracted_integers]
    # Filter columns based on the included level names.
    pre_edited_table = pre_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_edited_table)))]
    # Format the table with the specified options.
    edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,total_pos+3))%>% 
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>% 
      merge_h(i = 1:2) %>%
      padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      align(align = "center", j=1:ncol(pre_edited_table), part = "body")  %>% 
      hline(i=final_hlines, border = white_border)
    # Handle case where column_strat is 'total_row', which limits the table to certain rows.
  }else if(column_strat == 'total_col')
{
    pre_edited_table = pre_edited_table[,-c(grep(paste0(c(col_strat_var_levels,'Men','Women'), collapse = '|'),names(pre_edited_table)))]
    # Format the table with the specified options.
    edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,total_pos+3))%>% 
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>% 
      merge_h(i = 1:2) %>%
      padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      align(align = "center", j=1:ncol(pre_edited_table), part = "body")  %>% 
      hline(i=final_hlines, border = white_border)
    # Handle case where column_strat is 'total_row', which limits the table to certain rows.
  }else if(column_strat == 'total_row')
{
    pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),]
    # Format the table with the specified options.
    edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,4))%>% 
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>% 
      merge_h(i = 1:2) %>%
      padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      align(align = "center", j=1:ncol(pre_edited_table), part = "body") 
    # Handle case where column_strat is 'total_overall', applying specific stratification and column removal.
  } else if (column_strat == 'total_overall')
  {
    if(length(row_strat_variables)>1)
    {
      all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
      pre_edited_table = pre_edited_table[c(1:3,tail(1:nrow(pre_edited_table),all_additional_levels+2)),-c(grep(paste0(c(col_strat_var_levels,'Men','Women'), collapse = '|'),names(pre_edited_table)))]
    }else{
      pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),-c(grep(paste0(c(col_strat_var_levels,'Men','Women'), collapse = '|'),names(pre_edited_table)))]
    }
    # Format the table with the specified options.
    edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,4))%>% 
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>% 
      merge_h(i = 1:2) %>%
      padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      align(align = "center", j=1:ncol(pre_edited_table), part = "body")  
    
  }else{}
