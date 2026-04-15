### trial_table = pre_sub_edited_table
# column_strat = c('all','collevel1_2','collevel2','total_col', 'total_overall','total_row')

# -----------------------------------------------------------
# Extract numeric levels embedded in the column stratification string
# Example:
#   "collevel1_2" → extract integers 1 and 2
# These integers determine which stratification levels to display.
# -----------------------------------------------------------
extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))

# Construct a reference stratification string based on extracted integers
# Example:
#   extracted_integers = c(1,2) → "collevel1_2"
collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))

# -----------------------------------------------------------
# Check whether a "_full" option exists in the column stratifier
# This determines whether full tables or summary rows are displayed
# -----------------------------------------------------------
has_full = length(grep('_full',tolower(column_strat),v=T))>0 

# Remove "_full" from column_strat for downstream matching
column_strat = ifelse(length(grep('_full',tolower(column_strat),v=T))>0, gsub('_full','',column_strat),column_strat)
# Levels of sex in English
col_strat_var_levels_eng = names(table(data[,'demog_sex']))
# -----------------------------------------------------------
# Identify stratification levels for the column variable
# -----------------------------------------------------------

# If 'sex' is a column stratifier, retrieve levels from the dataset
#if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_sex']))}

# Map translated language values back to standard labels
# Example: French "Homme" → "Men"
#col_strat_var_levels[col_strat_var_levels==other_language[1,2]]='Men'
#col_strat_var_levels[col_strat_var_levels==other_language[2,2]]='Women'

# -----------------------------------------------------------
# CASE 1: Entire table (no column filtering)
# -----------------------------------------------------------
if(column_strat =='all')
{
  sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% 
    vline(j=vline_num, border = std_border)%>%
    hline(i=final_hlines, border = white_border)
  
  # -----------------------------------------------------------
  # CASE 2: Specific column levels (e.g., collevel1_2)
  # -----------------------------------------------------------
}else if(column_strat == collevel)
{
  # Determine which column levels should be included
  incl_level_names = col_strat_var_levels_eng[extracted_integers]
  
  # Keep only columns corresponding to selected stratification levels
  pre_sub_edited_table <- pre_sub_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_sub_edited_table)))]
  
  #
  if(has_full)
  {
    # Identify rows for horizontal line formatting
    #no_lines = 4:(length(names(table(data[,"agerange"])))+2)
    no_lines = 4:(nrow(pre_sub_edited_table)-2)
    
    #
    sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,total_pos+3))%>% 
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>% 
      merge_h(i = 1:2) %>%
      padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")%>%
      hline(i=no_lines, border = white_border)
    
  }else{
    
    # Subset table to show summary rows only
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),]
    
    sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,4))%>% 
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>% 
      merge_h(i = 1:2) %>%
      padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")
    
  }
  
  # -----------------------------------------------------------
  # CASE 3: Total column only (remove stratified columns)
  # -----------------------------------------------------------
}else if(column_strat == 'total_col')
{
  # Remove all columns corresponding to stratification levels
  pre_sub_edited_table = pre_sub_edited_table[,-c(grep(paste0(col_strat_var_levels_eng, collapse = '|'),names(pre_sub_edited_table)))]
  
  # Apply formatting
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% 
    hline(i=final_hlines, border = white_border)
  
  # -----------------------------------------------------------
  # CASE 4: Total row only (overall row summary)
  # -----------------------------------------------------------
}else if(column_strat == 'total_row')
{
  # Keep only header rows and final total row
  pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),]
  
  # Apply formatting
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body") 
  
  # -----------------------------------------------------------
  # CASE 5: Total overall summary
  # (remove column stratifiers and subset key rows)
  # -----------------------------------------------------------
} else if (column_strat == 'total_overall')
{
  
  if(length(row_strat_variables)>1)
  {
    # Determine number of additional stratification levels
    all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
    
    # Subset rows and remove column stratification columns
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,tail(1:nrow(pre_sub_edited_table),all_additional_levels+2)),-c(grep(paste0(col_strat_var_levels_eng, collapse = '|'),names(pre_sub_edited_table)))]
    
  }else{
    
    # Simpler case when only one row stratifier exists
    pre_sub_edited_table = pre_sub_edited_table[c(1:3,nrow(pre_sub_edited_table)),-c(grep(paste0(col_strat_var_levels_eng, collapse = '|'),names(pre_sub_edited_table)))]
  }
  
  # Apply formatting
  sub_edited_table = pre_sub_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,4))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  
  
  # -----------------------------------------------------------
  # DEFAULT CASE
  # Apply standard table formatting when no specific rule applies
  # -----------------------------------------------------------
}else{
  
  sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
    bold(i = c(1:2,total_pos+3))%>% 
    fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
    theme_vanilla() %>% 
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
    align(align = "center", j=1:ncol(pre_sub_edited_table), part = "body")  %>% 
    vline(j=vline_num, border = std_border)%>%
    hline(i=final_hlines, border = white_border)
}
