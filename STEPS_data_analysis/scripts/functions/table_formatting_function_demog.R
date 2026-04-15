############################################################
## PROCESS COLUMN STRATIFICATION AND FORMAT TABLE OUTPUT
############################################################

# column_strat = c('all','collevel1_2','collevel2','total_col', 'total_overall','total_row')

############################################################
## EXTRACT COLUMN LEVELS FROM STRATIFICATION STRING
############################################################

# Extract unique integers from the column_strat string.
# The regex "\\d+" finds all digit sequences (e.g., 1, 2, 3 in "collevel1_2"),
# which are then converted to integers and stored uniquely.
extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))

# Reconstruct the expected column stratification label (e.g., collevel1_2)
# based on the extracted integer values.
collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))

############################################################
## CHECK IF FULL STRATIFICATION IS REQUESTED
############################################################

# Identify whether the column stratification includes "_full"
# which indicates that all levels should be retained.
has_full = length(grep('_full',tolower(column_strat),v=T))>0 

# Remove "_full" from the column stratification label
# so it can be processed normally in subsequent conditions.
column_strat = ifelse(
  length(grep('_full',tolower(column_strat),v=T))>0,
  gsub('_full','',column_strat),
  column_strat
)

# Levels of sex in English
col_strat_var_levels_eng = names(table(data[,'demog_sex']))

############################################################
## EXTRACT COLUMN STRATIFICATION LEVEL NAMES
############################################################

# If sex is part of the column stratification variables,
# retrieve the level names from the dataset.
# if('sex' %in% col_strat_variable){
#   col_strat_var_levels = names(table(data[,'demog_sex']))
# }

# Replace language-specific level labels with standardized
# English labels ("Men" and "Women") to ensure consistency.
#col_strat_var_levels[col_strat_var_levels==other_language[1,2]]='Men'
#col_strat_var_levels[col_strat_var_levels==other_language[2,2]]='Women'


############################################################
## CASE 1: DISPLAY ALL COLUMN STRATIFICATION LEVELS
############################################################

# If column_strat is "all", retain the entire table without
# dropping any stratification levels.
if(column_strat =='all')
{
  
  edited_table = pre_edited_table %>%
    flextable() %>%
    autofit() %>%
    # Remove default header section
    delete_part(part = "header") %>%
    # Apply global font styling
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all') %>%
    # Bold header rows and total rows
    bold(i = c(1:2,total_pos+3)) %>%
    # Adjust font size
    fontsize(size = 8.5 ,part = "all") %>%
    # Remove default borders
    border_remove() %>%
    # Apply vanilla table theme
    theme_vanilla() %>%
    # Merge header rows horizontally
    merge_h(i = 1:2) %>%
    # Remove padding for compact layout
    padding(padding = 0, part = "all") %>%
    # Enable pagination when table spans multiple pages
    paginate(init = TRUE, hdr_ftr = TRUE) %>%
    # Center-align table values
    align(align = "center", j=1:7, part = "body") %>%
    # Add vertical separator lines
    vline(j=vline_num, border = std_border) %>%
    # Apply horizontal separators
    hline(i=final_hlines, border = white_border)
  
  
  ############################################################
  ## CASE 2: DISPLAY SPECIFIC COLUMN LEVELS
  ############################################################
  
}else if(column_strat == collevel)
{
  
  # Identify column names corresponding to requested stratification levels
  incl_level_names = col_strat_var_levels_eng[extracted_integers]
  # Filter columns in the table based on selected stratification levels
  pre_edited_table = pre_edited_table[,c(
    1,
    grep(paste0(incl_level_names, collapse = '|'),names(pre_edited_table))
  )]
  
  ##########################################################
  ## HANDLE MULTIPLE ROW STRATIFICATION VARIABLES
  ##########################################################
  
  if(length(row_strat_variables)>1)
  {
    # Calculate total additional levels from secondary row stratifiers
    all_additional_levels = eval(parse(text = paste0(
      'sum(c(',
      paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),
      '))'
    )))
    
    # Retain header rows plus final aggregated rows
    pre_edited_table =
      pre_edited_table[c(1:3,tail(1:nrow(pre_edited_table),all_additional_levels+2)),]
    
  }else{
    
    # Keep only main header rows and total row
    pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),]
  }
  
  
  ##########################################################
  ## FORMAT TABLE OUTPUT
  ##########################################################
  
  edited_table = pre_edited_table %>%
    flextable() %>%
    autofit() %>%
    delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all') %>%
    bold(i = c(1:2,total_pos+3)) %>%
    fontsize(size = 8.5 ,part = "all") %>%
    border_remove() %>%
    theme_vanilla() %>%
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all") %>%
    paginate(init = TRUE, hdr_ftr = TRUE) %>%
    align(align = "center", j=1:ncol(pre_edited_table), part = "body") %>%
    # Add vertical line separating indicator column from results
    vline(j=3, border = std_border)
  
  
  ############################################################
  ## CASE 3: TOTAL COLUMN VALUES ONLY
  ############################################################
  
}else if(column_strat == 'total_col')
{
  
  # Remove all stratified columns and keep only total columns
  pre_edited_table =
    pre_edited_table[
      ,-c(grep(paste0(c(col_strat_var_levels_eng,'Men','Women'), collapse = '|'),
               names(pre_edited_table)))
    ]
  
  edited_table = pre_edited_table %>%
    flextable() %>%
    autofit() %>%
    delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all') %>%
    bold(i = c(1:2,total_pos+3)) %>%
    fontsize(size = 8.5 ,part = "all") %>%
    border_remove() %>%
    theme_vanilla() %>%
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all") %>%
    paginate(init = TRUE, hdr_ftr = TRUE) %>%
    align(align = "center", j=1:ncol(pre_edited_table), part = "body") %>%
    # Apply horizontal separators
    hline(i=final_hlines, border = white_border)
  
  
  ############################################################
  ## CASE 4: TOTAL ROW ONLY
  ############################################################
  
}else if(column_strat == 'total_row')
{
  
  # Keep only header rows and final total row
  pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),]
  
  edited_table = pre_edited_table %>%
    flextable() %>%
    autofit() %>%
    delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all') %>%
    bold(i = c(1:2,4)) %>%
    fontsize(size = 8.5 ,part = "all") %>%
    border_remove() %>%
    theme_vanilla() %>%
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all") %>%
    paginate(init = TRUE, hdr_ftr = TRUE) %>%
    align(align = "center", j=1:ncol(pre_edited_table), part = "body")
  
  
  ############################################################
  ## CASE 5: TOTAL OVERALL
  ############################################################
  
}else if (column_strat == 'total_overall')
{
  
  # Handle multiple row stratifiers
  if(length(row_strat_variables)>1)
  {
    all_additional_levels = eval(parse(text = paste0(
      'sum(c(',
      paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),
      '))'
    )))
    
    pre_edited_table =
      pre_edited_table[
        c(1:3,tail(1:nrow(pre_edited_table),all_additional_levels+2)),
        -c(grep(paste0(c(col_strat_var_levels_eng,'Men','Women'), collapse = '|'),
                names(pre_edited_table)))
      ]
    
  }else{
    
    pre_edited_table =
      pre_edited_table[
        c(1:3,nrow(pre_edited_table)),
        -c(grep(paste0(c(col_strat_var_levels_eng,'Men','Women'), collapse = '|'),
                names(pre_edited_table)))
      ]
  }
  
  edited_table = pre_edited_table %>%
    flextable() %>%
    autofit() %>%
    delete_part(part = "header") %>%
    flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all') %>%
    bold(i = c(1:2,4)) %>%
    fontsize(size = 8.5 ,part = "all") %>%
    border_remove() %>%
    theme_vanilla() %>%
    merge_h(i = 1:2) %>%
    padding(padding = 0, part = "all") %>%
    paginate(init = TRUE, hdr_ftr = TRUE) %>%
    align(align = "center", j=1:ncol(pre_edited_table), part = "body")
  
}else{}


# #column_strat = c('all','collevel1_2','collevel2','total_col', 'total_overall','total_row')
# # Extract unique integers from the column_strat string by finding all digit sequences using regex
# # and converting them into unique integers.
# extracted_integers = unique(as.integer(unlist(str_extract_all(column_strat, "\\d+"))))
# # Generate a column level string based on the extracted integers.
# collevel = paste0('collevel',paste0(extracted_integers, collapse = '_'))
# #
# has_full = length(grep('_full',tolower(column_strat),v=T))>0 
# #
# column_strat = ifelse(length(grep('_full',tolower(column_strat),v=T))>0, gsub('_full','',column_strat),column_strat)
# # Check if 'sex' is part of the column stratification variables and extract the levels accordingly.
# if('sex' %in% col_strat_variable){col_strat_var_levels = names(table(data[,'demog_sex']))}
# # Replace specific level names in the column stratification variables with 'Men' and 'Women'
# # based on the language settings.
# col_strat_var_levels[col_strat_var_levels==other_language[1,2]]='Men'
# col_strat_var_levels[col_strat_var_levels==other_language[2,2]]='Women'
# 
# # If column_strat is set to 'all', generate a formatted table without dropping any levels of stratification.
# if(column_strat =='all')
#   {
# edited_table = pre_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
#   flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
#   bold(i = c(1:2,total_pos+3))%>% 
#   fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
#   theme_vanilla() %>% 
#   merge_h(i = 1:2) %>%
#   padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
#   align(align = "center", j=1:7, part = "body")  %>% vline(j=vline_num, border = std_border)%>%
#   hline(i=final_hlines, border = white_border)
# # If column_strat matches collevel, apply specific stratification based on the extracted levels.
# }else if(column_strat == collevel)
# {
#   incl_level_names = col_strat_var_levels[extracted_integers]
#   # Filter columns based on the included level names.
#   pre_edited_table = pre_edited_table[,c(1,grep(paste0(incl_level_names, collapse = '|'),names(pre_edited_table)))]
#   #############
#   # Handle additional levels if row stratification involves more than one variable.
#   if(length(row_strat_variables)>1)
#   {
#     all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
#     pre_edited_table = pre_edited_table[c(1:3,tail(1:nrow(pre_edited_table),all_additional_levels+2)),]
#   }else{
#     pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),]
#   }
#   # Generate the edited table with the specified formatting options.
#   edited_table = pre_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
#     flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
#     bold(i = c(1:2,total_pos+3))%>% 
#     fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
#     theme_vanilla() %>% 
#     merge_h(i = 1:2) %>%
#     padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
#     align(align = "center", j=1:ncol(pre_edited_table), part = "body")%>%
#     vline(j=3, border = std_border)#%>%
#     #hline(i=final_hlines, border = white_border)
#   # Handle case where column_strat is 'total_col', which removes certain columns from the table.
#   }else if(column_strat == 'total_col')
# {
#     pre_edited_table = pre_edited_table[,-c(grep(paste0(c(col_strat_var_levels,'Men','Women'), collapse = '|'),names(pre_edited_table)))]
#     # Format the table with the specified options.
#     edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
#       flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
#       bold(i = c(1:2,total_pos+3))%>% 
#       fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
#       theme_vanilla() %>% 
#       merge_h(i = 1:2) %>%
#       padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
#       align(align = "center", j=1:ncol(pre_edited_table), part = "body")  %>% 
#       hline(i=final_hlines, border = white_border)
#     # Handle case where column_strat is 'total_row', which limits the table to certain rows.
#   }else if(column_strat == 'total_row')
# {
#     pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),]
#     # Format the table with the specified options.
#     edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
#       flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
#       bold(i = c(1:2,4))%>% 
#       fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
#       theme_vanilla() %>% 
#       merge_h(i = 1:2) %>%
#       padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
#       align(align = "center", j=1:ncol(pre_edited_table), part = "body") 
#     # Handle case where column_strat is 'total_overall', applying specific stratification and column removal.
#   } else if (column_strat == 'total_overall')
#   {
#     if(length(row_strat_variables)>1)
#     {
#       all_additional_levels = eval(parse(text = paste0('sum(c(',paste0('length(names(table(analysis_data$',row_strat_variables[-1],')))', collapse = ','),'))')))
#       pre_edited_table = pre_edited_table[c(1:3,tail(1:nrow(pre_edited_table),all_additional_levels+2)),-c(grep(paste0(c(col_strat_var_levels,'Men','Women'), collapse = '|'),names(pre_edited_table)))]
#     }else{
#       pre_edited_table = pre_edited_table[c(1:3,nrow(pre_edited_table)),-c(grep(paste0(c(col_strat_var_levels,'Men','Women'), collapse = '|'),names(pre_edited_table)))]
#     }
#     # Format the table with the specified options.
#     edited_table = pre_edited_table%>%flextable() %>% autofit() %>% delete_part(part = "header") %>%
#       flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
#       bold(i = c(1:2,4))%>% 
#       fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
#       theme_vanilla() %>% 
#       merge_h(i = 1:2) %>%
#       padding(padding = 0, part = "all")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
#       align(align = "center", j=1:ncol(pre_edited_table), part = "body")  
#     
#   }else{}
# 
