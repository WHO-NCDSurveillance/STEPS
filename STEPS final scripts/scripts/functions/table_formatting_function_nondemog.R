#trial_table = pre_sub_edited_table
#column_strat = c('all','collevel1_2','collevel2','total_col', 'total_overall','total_row')
# Extract unique integers from the 'column_strat' string (e.g., 'collevel1_2') using a regex pattern.
# These integers will be used to determine specific levels for stratification.
extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))
# Create a string 'collevel' that represents a specific column stratification level based on the extracted integers.
# For example, if 'column_strat' contains "collevel1_2", 'collevel' will be set to "collevel1_2".
collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))
##
full_tab = ifelse(length(grep('full',column_strat, v=T))==1,TRUE, FALSE)
# If 'sex' is among the stratification variables, retrieve the levels for the 'demog_c1' variable from the data.
if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_c1']))}
# Map specific language strings in 'col_strat_var_levels' to 'Men' and 'Women'.
# For example, replace 'Homme' (in another language) with 'Men'.

col_strat_var_levels[col_strat_var_levels==other_language[1,2]]='Men'
col_strat_var_levels[col_strat_var_levels==other_language[2,2]]='Women'
# Case 1: If 'column_strat' is set to 'all', apply a specific table style to format the entire table.
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
  # Case 2: If 'column_strat' matches 'collevel', filter and format specific columns based on the extracted level names.
}else if(column_strat == collevel & full_tab == FALSE)
{
  # Extract the stratification levels based on the integers obtained earlier.
  incl_level_names = col_strat_var_levels[extracted_integers]
  # Subset the table to only include relevant columns based on the stratification levels.
  pre_sub_edited_table = pre_sub_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_sub_edited_table)))]
  # Adjust the table row selection based on the presence of additional stratification variables.
  if(length(row_strat_variables)>1)
  {
    all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,tail(1:nrow(pre_sub_edited_table),all_additional_levels+2)),]

  }else{
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),]
  }
  
  # Apply table styling similar to Case 1 but with different settings for specific levels.
  sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")#%>%
    #hline(i=final_hlines, border = white_border)
  # Case 3: If 'column_strat' is set to 'total_col', exclude specific stratification columns and format the table.
}else if(column_strat == collevel & full_tab == TRUE)
{
  # Extract the stratification levels based on the integers obtained earlier.
  incl_level_names = col_strat_var_levels[extracted_integers]
  # Subset the table to only include relevant columns based on the stratification levels.
  pre_sub_edited_table = pre_sub_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_sub_edited_table)))]
  # Apply table styling for the 'total_col' case.
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% 
    hline(i=final_hlines, border = white_border)
  # Case 4: If 'column_strat' is set to 'total_row', subset rows to include only relevant summary information.
}else if(column_strat == 'total_col')
{
  # Remove columns that match any of the specified stratification levels.
  pre_sub_edited_table = pre_sub_edited_table[,-c(grep(paste0(col_strat_var_levels, collapse = '|'),names(pre_sub_edited_table)))]
  # Apply table styling for the 'total_col' case.
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% 
    hline(i=final_hlines, border = white_border)
  # Case 4: If 'column_strat' is set to 'total_row', subset rows to include only relevant summary information.
}else if(column_strat == 'total_row')
{
  pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),]
  # Apply table styling for the 'total_row' case.
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body") 
  # Case 5: If 'column_strat' is set to 'total_overall', exclude both column stratification levels and subset relevant rows.
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
  # Default case: If none of the above conditions are met, do nothing.
}else{}
