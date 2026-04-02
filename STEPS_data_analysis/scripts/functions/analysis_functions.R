################### ------------------------------- ###################
# FUNCTIONS: DATA PROCESSING
################### ------------------------------- ###################

# Function to search whether a set of variables appears inside a logical expression.
# This is useful when checking whether a user-defined denominator condition
# references particular variables before evaluating the condition.
search_vars = function(nonexist_vars = 'selected', logic_denom) {
  
  # Create a regex pattern by collapsing variable names with OR operator
  pattern = paste(tolower(nonexist_vars), collapse="|")
  
  # Search for pattern within the logical expression string
  found = grepl(pattern, logic_denom)
  
  # Return TRUE if any variable is detected, otherwise FALSE
  return(found)
}

# Helper function used to clean semicolon-separated fields.
# Some datasets store multiple responses in one column separated by ';'.
# This function removes a specified element by index and reconstructs the string.
clean_field = function(field, remove_idx) {
  
  # Split string into individual components
  parts = strsplit(as.character(field), ";")[[1]]
  
  # Remove the specified index unless sentinel value (999) is supplied
  if (!identical(remove_idx, 999)) parts = parts[-remove_idx]
  
  # If nothing remains return NA, otherwise recombine values
  if (length(parts) == 0) NA_character_ else paste(parts, collapse = ";")
}

################### ------------------------------- ###################
# FUNCTIONS FOR GENERATING THE DATABOOK
################### ------------------------------- ###################

#######################################################################
# NUMERIC DEMOGRAPHIC SUMMARY TABLE
#######################################################################

# Function generating a demographic summary for numeric variables.
# Results are stratified by a specified variable (default: agerange)
# and presented separately for men, women, and totals.

demog_numeric = function(strat_variable = 'agerange')
{
  # Create a summary table by grouping data based on the specified stratification variable and sex
  summary_table = datum %>%
    # Group by the stratification variable and sex, and calculate counts and means
    group_by(eval(parse(text=strat_variable)), sex,.drop = FALSE) %>%
    summarise(count = n(), # Count of observations
              mean_var = round(mean(eval(parse(text = k)), na.rm = TRUE),1)) %>%# Mean of the variable (rounded to 1 decimal place)
    pivot_wider(names_from = sex, values_from = c(count, mean_var)) %>%    # Reshape data to wide format, with separate columns for counts and means by sex
    # Handle missing values and calculate total counts
    mutate(
      across(contains(c("Men", "Women")), ~coalesce(., 0)),# Replace NA values with 0 for counts
      Total_Count = rowSums(across(contains(c("count_Men", "count_Women")), as.numeric), na.rm = TRUE) # Total count of observations
    ) %>%
    ungroup() %>%
    left_join(# Join with data aggregated by stratification variable to get total mean values
      datum %>%
        group_by(eval(parse(text=strat_variable))) %>%
        summarise(Total_mean = round(mean(eval(parse(text = k)), na.rm = TRUE),1))
    ) %>% full_join(    # Join with overall data aggregated by sex to get overall totals
      datum %>%
        group_by(sex) %>%
        summarise(count = n(), # Overall count of observations by sex
                  mean_var = round(mean(eval(parse(text = k)), na.rm = TRUE),1)) %>% # Overall mean by sex
        pivot_wider(names_from = sex, values_from = c(count, mean_var)) %>%
        mutate(agerange = 'Total') %>% # Add a row for total counts
        add_column(datum %>% summarise(Total_Count = n(),  # Overall total count
                                       Total_mean=round(mean(eval(parse(text = k)), na.rm=T),1))))# Overall mean

  # Define the columns to select for the final summary table
  names_to_select = c("eval(parse(text = strat_variable))",
                      'count_Men','mean_var_Men','count_Women',
                      'mean_var_Women','Total_Count', 'Total_mean')
  # Select the columns from the summary table
  summary_table = summary_table %>% dplyr::select(all_of(names_to_select))
  ###Editing summary_table
  summary_table = summary_table %>% dplyr::mutate(
    # Format means with one decimal place
    mean_var_Men = formatC(round(mean_var_Men,1),format = "f", digits = 1),
    mean_var_Women = formatC(round(mean_var_Women,1),format = "f", digits = 1),
    Total_mean = formatC(round(Total_mean,1),format = "f", digits = 1),
    # Replace values with '-' if counts are below the denominator limit
    mean_var_Men = replace(mean_var_Men,count_Men<denom_limit,'-'),
    mean_var_Women = replace(mean_var_Women,count_Women<denom_limit,'-'),
    Total_mean = replace(Total_mean,Total_Count<denom_limit,'-')
  )
  # Adjust summary table if the stratification variable is not 'agerange'
  if(strat_variable != 'agerange')
  {
    strat_position = grep(strat_variable,row_strat_variables)
    # Exclude rows with NA in the stratification variable column
    summary_table = summary_table %>% 
                    dplyr::filter(!is.na(`eval(parse(text = strat_variable))`)) ###Excluding the totals row
    # Add a title row for the stratification variable
    summary_table = rbind(c(row_strat_variable_titles[strat_position], 
                            rep('',ncol(summary_table)-1)),summary_table %>% 
                            as.matrix())%>%as.data.frame()
  } else{}

  return(summary_table)
}

#######################################################################
# CATEGORICAL DEMOGRAPHIC SUMMARY TABLE
#######################################################################

# Function generating summary tables for categorical variables.
# Counts and percentages are calculated by sex and stratification group.
demog_cat = function (strat_variable = 'agerange')
{
  # Create a count table by filtering and grouping data
  count_table = datum %>% 
                dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(agerange))%>%# Filter out rows with NA values in the specified variable and 'agerange'
    group_by(eval(parse(text=strat_variable)), eval(parse(text=k)), sex, .drop = FALSE) %>%# Group data by stratification variable, the categorical variable (k), and sex
    summarise(n = n()) %>%# Summarize data to get counts
    rename(variable = `eval(parse(text = k))`) %>%# Rename the column for the categorical variable to 'variable'
    # Reshape data to a wide format with separate columns for counts by sex and variable
    pivot_wider(names_from = c(sex, variable), values_from = n) %>%
    mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)))# Replace missing values with 0 for counts
  # Determine unique levels for the categorical variable
  if(sub_matrix$has_log_exp==TRUE)
  {
    unique_levels = c(0,1)# If log_exp is true, use specific levels
  } else{unique_levels = sort(unique(data[,k]))}# Otherwise, use unique levels from the data


  # Ensure that all combinations of sex and unique levels are present in the count_table
  eval(parse(text=paste0('count_table$Men_',unique_levels,'[is.null(count_table$Men_',unique_levels,')]=0', sep='\n')))
  eval(parse(text=paste0('count_table$Women_',unique_levels,'[is.null(count_table$Women_',unique_levels,')]=0', sep='\n')))
  # Calculate total counts for each unique level
  eval(parse(text=paste0('count_table$sex_Total_',
                         unique_levels,'= count_table$Men_',
                         unique_levels,'+ count_table$Women_',unique_levels)))

  # Further mutate the count_table to add total counts and percentages
  count_table = count_table %>%
    mutate(
      # Total counts across all categories
      Total_Count = rowSums(across(contains(c("Men", "Women")), as.numeric), na.rm = TRUE),
      # Percentages for men and women by category
      men_Total_Count = rowSums(across(contains(c("Men"),ignore.case = F), as.numeric), na.rm = TRUE),
      women_Total_Count = rowSums(across(contains(c("Women"),ignore.case = F), as.numeric), na.rm = TRUE),
      #Percentages
      across(contains(c("Men"), ignore.case = F), list(Percentage = ~scales::percent(. / men_Total_Count,accuracy = 0.1, suffix = ''))),
      across(contains(c("Women"), ignore.case = F), list(Percentage = ~scales::percent(. / women_Total_Count,accuracy = 0.1, suffix = ''))),
      # Overall percentage
      across(contains(c("sex"), ignore.case = F), list(Percentage = ~scales::percent(. / Total_Count,accuracy = 0.1, suffix = ''))))

  # Exclude columns related to specific levels if log_exp is true
  if(sub_matrix$has_log_exp==TRUE)
  {
    names_to_excl = grep('_0', names(count_table), v=T)# Identify columns to exclude
    count_table = count_table %>% dplyr::select(-all_of(names_to_excl)) # Drop identified columns
  } else{}

  ###########
  # Adjust the count_table if the stratification variable is not 'agerange' or empty
  if(strat_variable != 'agerange' & strat_variable!='')
  {
    strat_position = grep(strat_variable,row_strat_variables)
    # Filter out rows with NA values in the stratification variable column
    count_table = count_table %>% 
                  dplyr::filter(!is.na(`eval(parse(text = strat_variable))`)) ###Excluding the any row with NAs
    # Add a title row for the stratification variable and convert to a data frame
    count_table = rbind(c(row_strat_variable_titles[strat_position], 
                          rep('',ncol(count_table)-1)),count_table %>% 
                          as.matrix())%>%as.data.frame()
  } else{count_table = count_table %>% as.matrix()%>%as.data.frame()}    # If the stratification variable is 'agerange' or empty, just convert the table to a data frame


  return(count_table)
}

#########################################
# Function: analysis_numeric_fn
# Purpose: Analyze numeric (non-demographic) survey variables with row and column stratification
# Inputs:
#   row_strat - variable for row stratification (default 'agerange')
#   col_strat - variable for column stratification (default 'sex')
# Assumptions:
#   - `data` and `svy_data` are defined in the global environment
#   - `k` is the current variable being analyzed
#   - `denom_condition`, `wt_step`, `median_compute`, `combine_ind`, `row_strat_variables`, and `sub_matrix` are defined

analysis_numeric_fn = function(row_strat = 'agerange', col_strat = 'sex')
{
  # Filtering and processing data based on condition
if (denom_condition == 'all') {
  # If denominator condition is 'all', include all data with non-NA values
  datum = data %>% 
          dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
  datum[,k] = as.numeric(datum[,k])

  svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
  eval(parse(text = paste0('svy_datum$variables$',k,' = as.numeric(as.character(svy_datum$variables$',k,'))')))
  svy_datum = subset(svy_datum, !is.na(eval(parse(text = k))) & get(wt_step)!=0)
  # If no rows in the data, set survey data to have zero counts for the variable
  if(nrow(datum)==0)
  {
    svy_datum = svy_data
    svy_datum$variables[,k]=0
  }

} else {    # Apply a specific condition to filter the data
  datum = data %>% 
          dplyr::filter(!is.na(eval(parse(text = k))) & 
                          !is.na(eval(parse(text=row_strat))) & 
                          eval(parse(text = paste0('(',denom_condition,')'))))
  datum[,k] = as.numeric(datum[,k])
  svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & 
                       !is.na(eval(parse(text=row_strat))) & 
                       eval(parse(text = paste0('(',denom_condition,')'))))
  ##
  eval(parse(text = paste0('svy_datum$variables$',k,' = as.numeric(as.character(svy_datum$variables$',k,'))')))
  svy_datum = subset(svy_datum, !is.na(eval(parse(text = k))) & get(wt_step)!=0)
  # If no rows in the data, set survey data to have zero counts for the variable
  if(nrow(datum)==0)
  {
    svy_datum = svy_data
    svy_datum$variables[,k]=0
  }
}

# Determine degrees of freedom for survey analysis
degrees_freedom = ifelse(degf(svy_datum)==0,1,degf(svy_datum)-1)
# Get levels of the column stratification variable
col_strat_var_levels = names(table(data[,col_strat]))

# Calculate unweighted participant numbers
n_participants = datum %>%
  group_by(eval(parse(text=col_strat)),eval(parse(text=row_strat)), .drop = FALSE) %>%
  reframe(participants=n())%>%
  pivot_wider(names_from = `eval(parse(text = col_strat))`, values_from = participants) %>%
  mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`),
         across(contains(col_strat_var_levels), ~coalesce(., 0)),
         Total=eval(parse(text = paste0("`",col_strat_var_levels,'`' , collapse = '+'))))%>%
  full_join(
    datum %>%
      group_by(eval(parse(text=col_strat)), .drop = FALSE) %>% 
      reframe(participants=n())%>%
      pivot_wider(names_from = `eval(parse(text = col_strat))`, values_from = participants) %>%
      mutate(`eval(parse(text = row_strat))`='Total',across(contains(col_strat_var_levels), ~coalesce(., 0)),
             Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", collapse = '+')))))%>%
  dplyr::filter(!is.na(`eval(parse(text = row_strat))`))
# Rename columns for clarity
colnames(n_participants) = c(paste0('0',row_strat),paste0(colnames(n_participants)[-1],'_a'))
#Calculate weighted participant percentages
est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+ eval(parse(text=row_strat)), 
                   design = svy_datum, FUN = svymean, method = "lo", 
                   df = degrees_freedom, vartype = 'ci', na.rm = T)%>%
            full_join(svyby(formula, by = ~eval(parse(text=row_strat)), 
                            design = svy_datum, FUN = svymean, method = "lo", 
                            df = degrees_freedom, vartype = 'ci', na.rm = T) %>%
                        mutate(`eval(parse(text = col_strat))` = 'Total'))%>%
  mutate(ci_l = ifelse(ci_l<0,0,ci_l))
##Totals
# Calculate total weighted percentages
colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat)), 
                                design = svy_datum, FUN = svymean, method = "lo", 
                                df = degrees_freedom, vartype = 'ci', na.rm = T)%>%
                       mutate(ci_l = ifelse(ci_l<0,0,ci_l))
total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", 
                           df = degrees_freedom, na.rm = T)
conf_interval = confint(total_est_ciprop, df = degrees_freedom, na.rm=T)
conf_interval[conf_interval<0] = 0

total_est_ciprop = cbind(as.vector(total_est_ciprop),ci_l = as.numeric(conf_interval[1]),ci_u=as.numeric(conf_interval[2]))%>%
                   as.data.frame() %>% 
  add_column(`eval(parse(text = col_strat))` = 'Total')
# Rename column for consistency
colnames(total_est_ciprop)[1] = colnames(colstrat_var_est_ciprop)[2]
# Combine estimates and confidence intervals
combined_est_ciprop = est_ciprop %>% 
  mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>%
                        full_join(colstrat_var_est_ciprop %>% 
                                    mutate(`eval(parse(text = row_strat))`='Total')) %>%
                        full_join(total_est_ciprop %>% 
                                    mutate(`eval(parse(text = row_strat))`='Total'))

#  # Population level estimates if specified
if(any(sub_matrix$pop_level_num_denom!=''& !is.na(sub_matrix$pop_level_num_denom)))
{
  est_ciprop = datum %>%
    group_by(eval(parse(text=col_strat)),eval(parse(text=row_strat)))%>%
               reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), ci_l = NA, ci_u = NA)%>%
               full_join(datum %>%
                           group_by(eval(parse(text=row_strat)))%>%
                           reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), ci_l = NA, ci_u = NA)%>%
                           mutate(`eval(parse(text = col_strat))` = 'Total'))
  #####
  colstrat_var_est_ciprop = datum %>%
    group_by(eval(parse(text=col_strat)))%>%
                            reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), 
                                    ci_l = NA, ci_u = NA)

  ####
  total_est_ciprop = datum %>%
                     reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), 
                             ci_l = NA, ci_u = NA)%>%
                     add_column(`eval(parse(text = col_strat))` = 'Total')
  ###
  combined_est_ciprop = est_ciprop %>% 
    mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>%
                        full_join(colstrat_var_est_ciprop %>% 
                                    mutate(`eval(parse(text = row_strat))`='Total')) %>%
                        full_join(total_est_ciprop %>% 
                                    mutate(`eval(parse(text = row_strat))`='Total'))%>%
                        mutate(ind_avg=ind_avg*100)%>% as.data.frame()
}

# Compute median and interquartile range (IQR) if specified
if(median_compute==TRUE)
{
  est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+ eval(parse(text=row_strat)), 
                     design = svy_datum, FUN = svyquantile, 
                     quantiles = c(.5,.25,.75), method = "lo", 
                     df = degrees_freedom, na.rm.all = T)[,-c(6:8)]%>%
    full_join(svyby(formula, by = ~eval(parse(text=row_strat)), 
                    design = svy_datum, FUN = svyquantile, 
                    quantiles = c(.5,.25,.75), method = "lo", 
                    df = degrees_freedom, na.rm.all = T)[,-c(5:7)]%>%
                mutate(`eval(parse(text = col_strat))` = 'Total'))
  ##Totals
  colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat)), 
                                  design = svy_datum, FUN = svyquantile, 
                                  quantiles = c(.5,.25,.75), method = "lo", 
                                  df = degrees_freedom, na.rm.all = T)[,-c(5:7)]
  total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", 
                                 quantiles = c(.5,.25,.75), 
                                 df = degrees_freedom, ci=FALSE)
  total_est_ciprop = do.call('c',total_est_ciprop)
  #
  total_est_ciprop = cbind(total_est_ciprop[1],total_est_ciprop[2],total_est_ciprop[3])%>%
    as.data.frame() %>% add_column(`eval(parse(text = col_strat))` = 'Total')
  colnames(total_est_ciprop)[1:3] = colnames(colstrat_var_est_ciprop)[2:4]
  ##Combined est_ciprop
  combined_est_ciprop = est_ciprop %>% 
    mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>%
    full_join(colstrat_var_est_ciprop %>% 
                mutate(`eval(parse(text = row_strat))`='Total')) %>%
    full_join(total_est_ciprop %>% 
                mutate(`eval(parse(text = row_strat))`='Total'))

}
#####
combined_est_ciprop[,3:5] = round(combined_est_ciprop[,3:5],1)
eval(parse(text = paste0('combined_est_ciprop[,',3:5,'] = paste0(formatC(combined_est_ciprop[,',3:5,'],format = "f", digits = 1))')))
##Point estimate
point_estimate = combined_est_ciprop[,1:3]
colnames(point_estimate)=c(col_strat,row_strat,'point_est')
ci_estimate = cbind(combined_est_ciprop[,1:2], 
                    paste0(combined_est_ciprop[,4],' - ',
                           combined_est_ciprop[,5]))
colnames(ci_estimate)=c(col_strat,row_strat,'interval')
##conversion to wider format
point_estimate=point_estimate%>%
  pivot_wider(names_from = all_of(col_strat), values_from = point_est)
colnames(point_estimate) = c(paste0('0',row_strat),paste0(colnames(point_estimate)[-1],'_b'))
#
ci_estimate=ci_estimate%>%
  pivot_wider(names_from = all_of(col_strat), values_from = interval)
colnames(ci_estimate) = c(paste0('0',row_strat),paste0(colnames(ci_estimate)[-1],'_ci'))

# Further formatting and combining results
computed_results = n_participants %>% mutate_all(as.character) %>%
                   left_join(point_estimate %>% 
                               mutate_all(as.character)) %>%
                   left_join(ci_estimate %>% 
                               mutate_all(as.character))
summary_table = computed_results %>% 
  dplyr::select(sort(colnames(computed_results)))
# Handle any missing columns
names_diff = setdiff(c(paste0(col_strat_var_levels,'_',c('a')),
                       paste0(col_strat_var_levels,'_',c('b')),
                       paste0(col_strat_var_levels,'_',c('ci')),
                       'Total_a','Total_b','Total_ci'), 
                     names(summary_table))
if(length(names_diff)>0)
{
  eval(parse(text=paste0('summary_table$`',names_diff,'`=NA')))
}
# Adjust summary table based on row stratification
s_position = grep(row_strat, row_strat_variables)

if(!combine_ind)
{
  if(s_position==1)
  {
  summary_table = summary_table%>%
    dplyr::select(all_of(c(paste0('0',row_strat),
                           sort(c(paste0(col_strat_var_levels,'_',c('a')),
                                  paste0(col_strat_var_levels,'_',c('b')),
                                  paste0(col_strat_var_levels,'_',c('ci')))),'Total_a','Total_b','Total_ci')))
  } else{
  summary_table = summary_table[-nrow(summary_table),]%>%
    dplyr::select(all_of(c(paste0('0',row_strat),
                           sort(c(paste0(col_strat_var_levels,'_',c('a')),
                                  paste0(col_strat_var_levels,'_',c('b')),
                                  paste0(col_strat_var_levels,'_',c('ci')))),'Total_a','Total_b','Total_ci')))
  }
} else{
  if(s_position==1)
  {
    summary_table = summary_table
  } else {
    summary_table = summary_table[-nrow(summary_table),]
    }

}
####Further editing of summary tables
summary_table[summary_table=='NA - NA'] = '-'

return(summary_table)
# }else{}
}

#########################################
# Function: analysis_categorical_fn
# Purpose: Analyze categorical (non-demographic) survey variables with row and column stratification
# Inputs:
#   row_strat - variable for row stratification (default 'agerange')
#   col_strat - variable for column stratification (default 'sex')
# Assumptions:
#   - `data` and `svy_data` exist in the global environment
#   - `k` is the key categorical variable to analyze
#   - `denom_condition`, `wt_step`, `combine_ind`, and `row_strat_variables` are defined globally

analysis_categorical_fn = function(row_strat = 'agerange', col_strat = 'sex')
{
  # Convert the row stratification variable to a factor, ensuring consistent levels and labels
  data[,row_strat] = factor(data[,row_strat], 
                            levels = names(table(data[,row_strat])), 
                            labels = names(table(data[,row_strat])))
  # Conditional filtering based on the specified denominator condition
  if (denom_condition == 'all') {
    # Filter data where the key variable (k) and the row stratification variable are not missing
    datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & 
                                     !is.na(eval(parse(text=row_strat))))
    # Subset the survey design data similarly
    svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & 
                         !is.na(eval(parse(text=row_strat)))& get(wt_step)!=0)
    # Handle cases where no rows match the filter criteria
    if(nrow(datum)==0)
    {
      svy_datum = svy_data
      svy_datum$variables[,k]=0# Set the key variable to 0 if no data matches
    }

  } else {
    # Apply custom filtering condition along with the default criteria
    datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & 
                                     !is.na(eval(parse(text=row_strat))) & 
                                     eval(parse(text = paste0('(',denom_condition,')'))))
    svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & 
                         !is.na(eval(parse(text=row_strat))) & 
                         eval(parse(text = paste0('(',denom_condition,')'))) & 
                         get(wt_step)!=0)
    # Handle cases where no rows match the filter criteria
    if(nrow(datum)==0)
    {
      svy_datum = svy_data
      svy_datum$variables[,k]=0# Set the key variable to 0 if no data matches
    }
  }

  # Calculate degrees of freedom for survey-based estimates, ensuring a minimum value of 1
  degrees_freedom = ifelse(degf(svy_datum)==0,1,degf(svy_datum)-1)

  # Determine the levels for the column stratification variable
  col_strat_var_levels = names(table(data[,col_strat]))
  # Calculate unweighted participant counts
  n_participants = datum %>%
    group_by(eval(parse(text=col_strat)),
             eval(parse(text=row_strat)), .drop = FALSE) %>%
    reframe(participants=n())%>%
    pivot_wider(names_from = `eval(parse(text = col_strat))`, 
                values_from = participants) %>%
    mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`),
           across(contains(col_strat_var_levels), ~coalesce(., 0)),
           Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", 
                                          collapse = '+'))))%>%
    full_join(
      datum %>%
        group_by(eval(parse(text=col_strat)), .drop = FALSE) %>% 
        reframe(participants=n())%>%
        pivot_wider(names_from = `eval(parse(text = col_strat))`, 
                    values_from = participants) %>%
        # Ensure row stratification is treated as a character variable for consistency
        # Replace missing values with 0 for participant counts
        mutate(`eval(parse(text = row_strat))`='Total',
               across(contains(col_strat_var_levels), ~coalesce(., 0)),
               # Calculate the total participants across all column stratification levels
               Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", collapse = '+')))))%>%
    dplyr::filter(!is.na(`eval(parse(text = row_strat))`))# Filter out rows where the row stratification variable is missing
  # Rename the columns to differentiate participant counts
  colnames(n_participants) = c(paste0('0',row_strat),
                               paste0(colnames(n_participants)[-1],'_a'))
  # Calculate weighted participant percentages and confidence intervals
  est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+eval(parse(text=row_strat)), 
                     design = svy_datum, FUN = svyciprop, method = "lo", 
                     df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
    # Add a row for the total across all column stratification levels
    full_join(svyby(formula, by = ~eval(parse(text=row_strat)), 
                    design = svy_datum, FUN = svyciprop, method = "lo", 
                    df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
                mutate(`eval(parse(text = col_strat))` = 'Total'))
  # Calculate overall totals for the column stratification variable
  colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text = col_strat)), 
                                  design = svy_datum, FUN = svyciprop, 
                                  method = "lo", df = degrees_freedom, vartype = 'ci', 
                                  na.rm.all = T)#%>%
  # Calculate the overall total estimate and confidence intervals
  total_est_ciprop = svyciprop(formula, design = svy_datum, 
                               method = "lo", df = degrees_freedom, na.rm=T)

  total_est_ciprop = cbind(as.vector(total_est_ciprop),
                           ci_l = as.numeric(attr(total_est_ciprop, "ci")[1]),
                           ci_u=as.numeric(attr(total_est_ciprop, "ci")[2]))%>%
    as.data.frame() %>% 
    add_column(`eval(parse(text = col_strat))` = 'Total')
  colnames(total_est_ciprop)[1] = colnames(colstrat_var_est_ciprop)[2]
  # Combine all the calculated estimates
  combined_est_ciprop = est_ciprop %>% 
    mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>%
    full_join(colstrat_var_est_ciprop %>% 
                mutate(`eval(parse(text = row_strat))`='Total')) %>%
    full_join(total_est_ciprop %>% 
                mutate(`eval(parse(text = row_strat))`='Total')) #%>%
  # Convert the estimates to percentages and format the output
  combined_est_ciprop[,3:5] = round(100*combined_est_ciprop[,3:5],1)
  eval(parse(text = paste0('combined_est_ciprop[,',3:5,'] = paste0(formatC(combined_est_ciprop[,',3:5,'],format = "f", digits = 1))')))
  # Extract and reshape the point estimates
  point_estimate = combined_est_ciprop[,1:3]
  colnames(point_estimate)=c(col_strat,row_strat,'point_est')
  ci_estimate = cbind(combined_est_ciprop[,1:2], 
                      paste0(combined_est_ciprop[,4],' - ',combined_est_ciprop[,5]))
  colnames(ci_estimate)=c(col_strat,row_strat,'interval')
  ##conversion to wider format
  point_estimate=point_estimate%>%
    pivot_wider(names_from = all_of(col_strat), values_from = point_est)
  colnames(point_estimate) = c(paste0('0',row_strat),paste0(colnames(point_estimate)[-1],'_b'))
  #
  ci_estimate=ci_estimate%>%
    pivot_wider(names_from = all_of(col_strat), values_from = interval)
  colnames(ci_estimate) = c(paste0('0',row_strat),paste0(colnames(ci_estimate)[-1],'_ci'))

  ##Further formatting
  computed_results = n_participants %>%
    mutate_all(as.character)%>% left_join(point_estimate) %>% 
    left_join(ci_estimate)
  summary_table = computed_results %>% dplyr::select(sort(colnames(computed_results)))
  ###
  names_diff = setdiff(c(paste0(col_strat_var_levels,'_',c('a')),
                         paste0(col_strat_var_levels,'_',c('b')),
                         paste0(col_strat_var_levels,'_',c('ci')),
                         'Total_a','Total_b','Total_ci'), 
                       names(summary_table))
  if(length(names_diff)>0)
  {
    eval(parse(text=paste0('summary_table$`',names_diff,'`= NA')))
  }

  s_position = grep(row_strat, row_strat_variables)
  ###
  if(!combine_ind)
  {
    if(s_position==1)
    {
      summary_table = summary_table %>%
                      dplyr::select(all_of(c(paste0('0',row_strat),
                                             sort(c(paste0(col_strat_var_levels,'_',c('a')),
                                                                          paste0(col_strat_var_levels,'_',c('b')),
                                                                          paste0(col_strat_var_levels,'_',c('ci')))),
                                             'Total_a','Total_b','Total_ci')))
    } else{
      summary_table = summary_table[-nrow(summary_table),] %>%
                      dplyr::select(all_of(c(paste0('0',row_strat),
                                             sort(c(paste0(col_strat_var_levels,'_',c('a')),
                                                                          paste0(col_strat_var_levels,'_',c('b')),
                                                                          paste0(col_strat_var_levels,'_',c('ci')))),
                                             'Total_a','Total_b','Total_ci')))
    }
  } else{
    if(s_position==1)
    {
      summary_table = summary_table
    } else {
      summary_table = summary_table[-nrow(summary_table),]
    }

  }
  return(summary_table)
}

# -------------------------------------------------------------------
# Function to split a given number of columns into visually balanced parts
# Used when indicator tables become too wide for a single page.
# The function returns a numeric vector indicating how many columns
# should appear in each split table.
# -------------------------------------------------------------------
frame_split_into_parts = function(num_cols) {
  # If the number of columns is exactly 10, simply return 10 (no further splitting needed)
  if(num_cols==10){return(num_cols)}
  # Initialize an empty vector to store the size of each part
  parts = numeric()
  ## Determine the size of the first part based on the total number of columns
  # If the number of columns is greater than 13, the first part is 10 columns
  # Otherwise, the first part is 7 columns

  first_part = if (num_cols > 13) 10 else 7
  # Add the size of the first part to the parts vector
  parts = c(parts, first_part)
  # Calculate the remaining columns after allocating the first part
  remaining_cols = num_cols - first_part
  ## Determine the number of additional parts required
  # Calculate the number of parts needed by dividing the remaining columns by 9 and rounding up
  num_parts = ceiling(remaining_cols / 9)
  # Calculate the size of each additional part by evenly distributing the remaining columns
  part_size = floor(remaining_cols / num_parts)
  # Loop through each additional part (excluding the last one) to determine its size
  for (i in 1:(num_parts - 1)) {
    # The size of the next part is the smaller value between the calculated part_size and 9
    next_part = min(part_size, 9)
    # Ensure the size of each part is a multiple of 3 (for better visual balance or organization)
    # If the next_part is not a multiple of 3, adjust it upward to the nearest multiple of 3
    next_part = ifelse(next_part %% 3 == 0, next_part, next_part + (3 - next_part %% 3))
    # Add the size of the next part to the parts vector
    parts = c(parts, next_part)
    # Subtract the size of the next part from the remaining columns
    remaining_cols = remaining_cols - next_part
  }
  # The last part takes all the remaining columns (whatever is left)
  parts = c(parts, remaining_cols)
  ## Determine if the number of total parts should be limited based on the number of columns
  # Calculate the ceiling of the number of columns divided by 10 (helps control the number of parts)
  index_ceil = ceiling(num_cols/10)

  if(index_ceil<3){parts = parts[1:index_ceil]}
  return(parts)
}

####################################################################
# Function generating formatted flextables for grouped indicator tables
# Each table may be automatically split into multiple parts when
# the number of columns exceeds page width limits.
####################################################################
flextab_function = function(index =1, table_label ='Men')
{
  ####Defining list split function first::
  list_split_number = function(num = number_columns, max_part = 10) {
    if (num <= max_part) {
      # If num is less than or equal to max_part, return num
      return(num)
    } else {
      # If num is greater than max_part, apply the distribution logic

      # If num is greater than max_part, distribute parts evenly
      times = ceiling(num / max_part)
      quotient = num %/% times
      remainder = num %% times

      # Initialize list to store parts
      parts = numeric(times)

      # Calculate evenly distributed parts
      if (remainder == 0) {
        parts = rep(quotient, times)
      } else {
        parts[1:remainder] = quotient + 1
        parts[(remainder + 1):times] = quotient
      }

      # Ensure preceding parts are multiples of 2 plus 2
      if(length(rev_n_colnames)==1 & i !="Demographics")
      {
        for (i in 1:(times - 1)) {
          excess = parts[i] %% 2
          if (excess != 2) {
            parts[i] = parts[i] + (2 - excess)
            parts[times] = parts[times] - (2 - excess)
          }
        }
      }
      # Ensure preceding parts are multiples of 3 plus 1
      if((length(rev_n_colnames)>1 & i !="Demographics")|(number_columns>=20 & length(rev_n_colnames)>1))#(number_columns>=20 & length(rev_n_colnames)>1)
      {
        for (i in 1:(times - 1)) {
          excess = parts[i] %% 3
          if (excess != 1) {
            parts[i] = parts[i] + (1 - excess)
            parts[times] = parts[times] - (1 - excess)
          }
        }
      }
      # Adjust the last part to ensure the sum equals num
      parts[times] = parts[times] + num - sum(parts)

      return(parts)
    }
  }
  ##
  frame_split_into_parts = function(num_cols) {

    if(num_cols==10){return(num_cols)}
    parts = numeric()
    ## Determine the size of the first part
    first_part = if (num_cols > 13) 10 else 7
    parts = c(parts, first_part)
    remaining_cols = num_cols - first_part
    ## Determine the number of additional parts
    num_parts = ceiling(remaining_cols / 9)
    # Calculate the size of each part
    part_size = floor(remaining_cols / num_parts)
    # Distribute remaining columns among the parts
    for (i in 1:(num_parts - 1)) {
      next_part = min(part_size, 9)
      # Ensure subsequent parts are multiples of three
      next_part = ifelse(next_part %% 3 == 0, next_part, next_part + (3 - next_part %% 3))
      parts = c(parts, next_part)
      remaining_cols = remaining_cols - next_part
    }
    # The last part takes all remaining columns
    parts = c(parts, remaining_cols)
    ##
    index_ceil = ceiling(num_cols/10)

    if(index_ceil<3){parts = parts[1:index_ceil]}
    return(parts)
  }

  ###########
  extracted_table = extract_table[[index]]
  #
  # if(i!='Demographics')
  #{
  n_colnames = grep('_a|Total_Count', colnames(extracted_table),v=T)
  ####
  if(length(n_colnames)>1)
  {
    pairwise_combs = combn(n_colnames,2)
    #
    all_combs = list()
    col_idx = NULL
    for(col_idx in 1:ncol(pairwise_combs))
    {
      all_combs[[col_idx]] = paste0('all(',paste0('extracted_table$`',pairwise_combs[,col_idx],"`", collapse = ' == '),')')
    }

    equivalent_check = eval(parse(text=paste0(do.call('c',all_combs), collapse = ' & ')))
    cols_exclude = n_colnames

    if(equivalent_check)
    {
      excl_colpos = grep(paste0(cols_exclude, collapse = '|'),names(extracted_table))[-1]
      extracted_table = extracted_table %>%dplyr::select(-all_of(cols_exclude[-1]))
      #exc column positions
      if((all(is.na(subtitle2))|all(subtitle2=='')) & i != "Demographics")
      {
        edited_inline_text = edited_inline_text[-excl_colpos]

      } else if((all(is.na(subtitle2))|all(subtitle2=='')) & i == "Demographics")
      {
        edited_inline_text = edited_inline_text
      }else{edited_inline_text = edited_inline_text[,-excl_colpos]}

    } else{}
  } else{}
  #}
  ###################Splitting long tables
  number_columns = ncol(extracted_table)
  rev_n_colnames = grep('_a|Total_Count', colnames(extracted_table),v=T)
  #######
  lt = NULL
  inc_num=0
  prev_lt = 0
  split_tables = list()

  split_numbers = list_split_number(num = number_columns)
  ###

  if((number_columns>=20 & length(rev_n_colnames)>1))
  {
    split_numbers = frame_split_into_parts(num = number_columns)
  }
  ###
  colnosplit = cumsum(split_numbers)

  #######
  for(lt in colnosplit)
  {
    # Determine the column range based on the condition
    col_range = if (length(rev_n_colnames) == 1 & number_columns<20) {
      unique(c(1:2, (inc_num + 1):lt))
    } else if(number_columns>=20)
    {
      if(length(rev_n_colnames) == 1)
      {
      unique(c(1:2, (prev_lt + 1):lt))
      }else{unique(c(1, (prev_lt + 1):lt))}
    }else {
      unique(c(1, (inc_num + 1):lt))
    }
    #
    col_range = sort(col_range)
    # Subset the extracted_table using the determined column range
    split_tab = extracted_table[, col_range]

    # Subset the edited_inline_text based on the condition
    if (all(is.na(subtitle2)) | all(subtitle2 == '')) {
      sub_edited_inline_text = edited_inline_text[col_range]
    } else {
      sub_edited_inline_text = edited_inline_text[, col_range]
    }

    #########
    if(all(is.na(subtitle2))|all(subtitle2==''))
    {
      total_pos = grep('Total',split_tab[,1])+3
    } else{total_pos = grep('Total',split_tab[,1])+4}
    ###formatting the sub table
    if(median_compute==TRUE){sub_edited_inline_text = gsub(other_language[10,language],sub_edited_inline_text)}
    #
    split_tab[,1][split_tab[,1]=='Total'] = other_language[4,language]

    if(language == 'french')
    {
      split_tab = split_tab %>% as.matrix
      split_tab = gsub('[.]',',',split_tab)
    } else{}
    ##
    ##
    if(language =='arabic')
    {
      pre_sub_edited_table =  rbind(rep(table_title,ncol(split_tab)),
                                    rep(table_label,ncol(split_tab)),sub_edited_inline_text,as.matrix(split_tab)) %>%as.data.frame()%>%rev()
    }else{
      pre_sub_edited_table =  rbind(rep(table_title,ncol(split_tab)),
                                    rep(table_label,ncol(split_tab)),sub_edited_inline_text,as.matrix(split_tab)) %>%as.data.frame()
    }
    ####
    sub_edited_table = pre_sub_edited_table %>% flextable() %>% autofit() %>% delete_part(part = "header") %>%
      flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
      bold(i = c(1:2,total_pos))%>%
      fontsize(size = 8.5 ,part = "all") %>% border_remove() %>%
      theme_vanilla() %>%
      merge_h(i=1:row_merge) %>%padding(padding = 0, part = "all")%>%
      align(align = "center", j=1:ncol(split_tab), part = "body")%>%paginate(init = TRUE, hdr_ftr = TRUE)%>%
      hline(i=final_hlines, border = white_border)
    #####
    split_tables [[lt]] = sub_edited_table
    inc_num=inc_num+lt
    prev_lt = lt
  }

  # Filtering out NULL elements
  filtered_list = Filter(function(x) !is.null(x), split_tables)
  return(filtered_list)
}

####################################################################
# Helper functions to sort percentage columns correctly
# Ensures columns appear in numerical order of indicator categories
####################################################################

custom_sort1 = function(name) {
  number = sort(as.numeric(sub("Men_(\\d+)_Percentage", "\\1", name)))
  return(paste0('Men_',number,'_Percentage'))
}
custom_sort2 = function(name) {
  number = sort(as.numeric(sub("Women_(\\d+)_Percentage", "\\1", name)))
  return(paste0('Women_',number,'_Percentage'))
}
custom_sort3 = function(name) {
  number = sort(as.numeric(sub("sex_Total_(\\d+)_Percentage", "\\1", name)))
  return(paste0('sex_Total_',number,'_Percentage'))
}

####################################################################
# Helper function to adjust demographic stratification variables
# Matches user-supplied variables to those in a metadata lookup table
####################################################################
adjust_strat_vars <- function(strat_vars, matched_df) {
  map_chr(strat_vars, function(var) {

    var_match <- matched_df %>%
      filter(str_detect(matched_var, fixed(var))) %>%
      pull(matched_var)

    if (length(var_match) > 0) {
      paste0("demog_", var_match[1])  # take first match (same logic as grep)
    } else {
      var
    }
  })
}

#-----------------------------------------------------------
# Function to generate the factsheet results for one section
# for a single survey round
#-----------------------------------------------------------
factsheet_section_fn = function(sect = unique(fact_sheet_matrix$section)[8])
{
  # Copy analysis dataset
  data = analysis_data
  
  # Subset factsheet matrix for the selected section and order indicators
  section_matrix = fact_sheet_matrix %>% dplyr::filter(section == sect)%>% arrange(factsheet_desc)
  
  ##
  # Extract weight variable name for the section
  no_wt_step = unique(section_matrix$weight_step)[1]
  
  # Extract numeric step identifier (e.g., "Step 1", "Step 2")
  numeric_step = as.numeric(str_extract(no_wt_step, "\\d+"))
  
  # Construct section title with step number
  section_title = c(paste0('Step ',numeric_step,' ',unique(section_matrix$section)),'','','')
  
  # For certain sections remove the "Step" prefix
  if(sect=='Summary of Combined Risk Factors'|sect=='Cardiovascular disease risk')
  {
    section_title = c(unique(section_matrix$section),'','','')
  }
  
  # Initialise containers
  section_results = NULL
  ind = NULL
  
  #-----------------------------------------------------------
  # Loop through each indicator row in the section matrix
  #-----------------------------------------------------------
  for (ind in 1:nrow(section_matrix))
  {
    sub_matrix = section_matrix[ind,]
    
    # Extract indicator position indexes
    ind_position = as.numeric(do.call('c',strsplit(sub_matrix$ind_position, "[;]")))
    
    # Extract indicator variables
    subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))[ind_position]
    
    # Indicator types (mean / median / categorical)
    type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))[ind_position]
    
    # Denominator condition logic
    #denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))[ind_position]
    denom_logic = if (length(do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))) == 1) {sub_matrix$pop_subset[1]} else {do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))[ind_position]}
    
    # Indicator description
    ind_desc = do.call('c',strsplit(sub_matrix$factsheet_desc, "[;]"))
    
    ###-------------------------------------------------------
    ### Defining survey design structure
    ###-------------------------------------------------------
    
    # Extract survey weight variable
    wt_step = unique(sub_matrix$weight_step)[1]
    
    # Ensure weight variable is numeric
    data[,wt_step] = as.numeric(as.character(data[,wt_step]))
    
    # Replace missing weights with zero to preserve survey design
    data[,wt_step][is.na(data[,wt_step])] = 0
    
    # Create survey design object
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
    ###
    ind_level = NULL
    sub_section_results = NULL
    
    #-----------------------------------------------------------
    # Loop through indicators belonging to the same row
    #-----------------------------------------------------------
    for(ind_level in subset_indicators)
    {
      # Skip indicators with only missing values
      if(!all(is.na(data[[ind_level]])))
      {
        # Extract denominator logic for this indicator
        #denom_condition = denom_logic[grep(ind_level,subset_indicators)]
        denom_condition = if (length(denom_logic) == 1) {denom_logic[1]} else {denom_logic[grep(ind_level,subset_indicators)]}
        
        # Extract full indicator description
        full_ind_desc = ind_desc[grep(ind_level,subset_indicators)]
        
        ##
        # Apply denominator filtering conditions
        if (denom_condition == 'all') {
          
          # Dataset for counting participants
          datum = data %>% filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange))
          
          # Survey subset used for estimation
          svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange) & get(wt_step)!=0)
          
        } else {
          
          # Apply additional denominator restriction
          datum = data %>% dplyr::filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))))
          
          svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))) & get(wt_step)!=0)
        }
        
        #####
        # Compute degrees of freedom for survey variance estimation
        degrees_freedom = degf(svy_datum)-1
        
        #-----------------------------------------------------------
        # Continuous indicators (mean or median)
        #-----------------------------------------------------------
        if(type_indicators[grep(ind_level,subset_indicators)] == 'mean'|type_indicators[grep(ind_level,subset_indicators)] == 'median')
        {
          #
          # Create formula dynamically
          eval(parse(text = paste0('formula = ~', ind_level)))
          
          # Compute number of participants by sex
          n_participants = datum %>% group_by(sex,.drop=FALSE) %>% reframe(participants=n())%>%
            pivot_wider(names_from = c(sex), values_from = participants) %>%
            mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)), Total=Men+Women)
          
          ##
          # Compute mean and CI by sex
          men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm = TRUE)%>%
            mutate(ci_l = ifelse(ci_l<0,0,ci_l))
          
          # Compute total estimate
          total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", df = degrees_freedom, na.rm = TRUE) 
          
          # Extract confidence interval
          conf_interval = confint(total_est_ciprop) 
          
          # Prevent negative CI bounds
          conf_interval[conf_interval<0] = 0
          
          ###
          #### Computation of median and IQR
          median_compute = unique(type_indicators)=='median'
          
          if(median_compute==TRUE)
          {
            # Median and IQR by sex
            men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svyquantile, quantiles = c(.5,.25,.75), method = "lo", df = degrees_freedom, na.rm = TRUE)[,1:4]
            
            # Median and IQR for total
            median_total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", quantiles = c(.5,.25,.75), df = degrees_freedom, ci=FALSE, na.rm = TRUE) 
            
            total_est_ciprop = as.vector(unlist(median_total_est_ciprop)[1])
            
            conf_interval = as.vector(unlist(median_total_est_ciprop)[2:3])
          }
          
          ### Combining estimates into formatted string
          total_est = paste0(formatC(round(as.vector(total_est_ciprop),1),format = "f", digits = 1),'\n(',
                             formatC(round(as.numeric(conf_interval[1]),1),format = "f", digits = 1), ' - ',
                             formatC(round(as.numeric(conf_interval[2]),1),format = "f", digits = 1),')')
          
          #####
          delim_char = '\n('
          mult_n = 1
          
        } else if(type_indicators[grep(ind_level,subset_indicators)] == 'categorical'){
          
          ##
          # Define binary indicator formula
          eval(parse(text=paste0('formula = ~I(',ind_level, '=="',1,'")')))
          
          # Count participants by sex
          n_participants = datum %>% group_by(sex, .drop = FALSE) %>% reframe(participants=n())%>%
            pivot_wider(names_from = c(sex), values_from = participants) %>%
            mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)), Total=Men+Women)
          
          ###
          # Proportion estimates by sex
          men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm = TRUE)
          
          # Total estimate
          total_est_ciprop = svyciprop(formula, design = svy_datum, method = "lo", df = degrees_freedom, na.rm = TRUE) 
          
          ##
          # Format total estimate with CI
          total_est = paste0(formatC(round(100*as.vector(total_est_ciprop),1),format = "f", digits = 1),'%\n(',
                             formatC(round(100*as.numeric(attr(total_est_ciprop, "ci")[1]),1),format = "f", digits = 1), ' - ',
                             formatC(round(100*as.numeric(attr(total_est_ciprop, "ci")[2]),1),format = "f", digits = 1),')')
          
          ##
          delim_char = '%\n('
          mult_n = 100
        }
        
        ########Combining estimates by sex
        males_est = paste0(formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',2]),1),format = "f", digits = 1),delim_char,
                           formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',3]),1),format = "f", digits = 1), ' - ',
                           formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',4]),1),format = "f", digits = 1),')')
        
        females_est = paste0(formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',2]),1),format = "f", digits = 1),delim_char,
                             formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',3]),1),format = "f", digits = 1), ' - ',
                             formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',4]),1),format = "f", digits = 1),')')
        
        # Combine indicator description and estimates
        combined_results = c(full_ind_desc, total_est,males_est,females_est)
        
        sub_section_results = rbind(sub_section_results,combined_results)    
      }
    } 
    
    # Append subsection results
    section_results = rbind(section_results,sub_section_results) 
    
  }
  
  # Add section title and order indicators alphabetically
  section_results = rbind(section_title,section_results[order(section_results[,1]),])
  
  return(section_results )  
}

#################### Extracting reference parameters for calibrating CVD risk
# The reference dataset is stored as risk_ref_data.dta and contains
# calibration parameters (intercepts and slopes) used to adjust CVD risk models.
# These parameters are country-specific, sex-specific, and year-specific.

# Define the reference year used for calibration
ref_year = 2017 # 2017 is the latest possible year in the CVD risk database and thus used by default

# Load the reference dataset and filter to the country of interest (CVD_ISO)
# and the defined reference year. Only the relevant calibration variables are kept.
risk_ref_data = haven::read_dta('scripts/functions/risk_ref_data.dta') %>%
  dplyr::filter(ccode == CVD_ISO & year == ref_year) %>%
  dplyr::select(
    sex,
    cal2_m1_cons_ep_crbv,   # intercept for cerebrovascular disease risk model
    cal2_m1_slope_ep_crbv,  # slope for cerebrovascular disease risk model
    cal2_m1_cons_ep_chdmi,  # intercept for coronary heart disease / myocardial infarction model
    cal2_m1_slope_ep_chdmi  # slope for coronary heart disease / myocardial infarction model
  )

####
# Extract calibration parameters for CRBV (cerebrovascular disease) models

# Intercept for males
int_males_crbv = risk_ref_data %>%
  dplyr::filter(sex == 1) %>%
  pull(cal2_m1_cons_ep_crbv)

# Intercept for females
int_females_crbv = risk_ref_data %>%
  dplyr::filter(sex == 2) %>%
  pull(cal2_m1_cons_ep_crbv)

# Slope for males
slope_males_crbv = risk_ref_data %>%
  dplyr::filter(sex == 1) %>%
  pull(cal2_m1_slope_ep_crbv)

# Slope for females
slope_females_crbv = risk_ref_data %>%
  dplyr::filter(sex == 2) %>%
  pull(cal2_m1_slope_ep_crbv)

# Extract calibration parameters for CHDMI (coronary heart disease / myocardial infarction)

# Intercept for males
int_males_chdmi = risk_ref_data %>%
  dplyr::filter(sex == 1) %>%
  pull(cal2_m1_cons_ep_chdmi)

# Intercept for females
int_females_chdmi = risk_ref_data %>%
  dplyr::filter(sex == 2) %>%
  pull(cal2_m1_cons_ep_chdmi)

# Slope for males
slope_males_chdmi = risk_ref_data %>%
  dplyr::filter(sex == 1) %>%
  pull(cal2_m1_slope_ep_chdmi)

# Slope for females
slope_females_chdmi = risk_ref_data %>%
  dplyr::filter(sex == 2) %>%
  pull(cal2_m1_slope_ep_chdmi)

################### -------------------------------###################
# FUNCTIONS TO SUPPORT GENERATION OF NUMBERS FOR NARRATIVES IN LLMS
# These functions are used in scripts 4–8 to compute numerical summaries
# and generate automated narrative descriptions of survey results.
################### -------------------------------###################

## NOTE:
# The models defined here may become deprecated over time.
# Users should check the Groq documentation for currently supported models:
# https://console.groq.com/keys
# https://console.groq.com/docs/rate-limits

# Example models currently available:
# meta-llama/llama-4-scout-17b-16e-instruct
# llama-3.3-70B-Versatile (default here due to better table interpretation)

##########################################################
## LLM wrapper function to connect to Groq servers and 
## plug into the LLMs to generate narrative text
##########################################################
llm_wrapper_connect = function(prompt_text, model = "llama-3.3-70b-versatile", max_tokens = 8192) {
  # API key
  api_key = Sys.getenv("API_KEY")

  # Ensuring UTF-8 encoding to avoid warning
  prompt_text = enc2utf8(prompt_text)

  # Function to send request
  make_request = function() {
    httr::POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = model,
        messages = list(list(role = "user", content = prompt_text)),
        max_tokens = max_tokens,
        temperature = 0.7,
        top_p = 0.9
      ), auto_unbox = TRUE)
    )
  }

  attempt = 1
  repeat {
    response = make_request()
    #
    if (response$status_code == 200) {
      result = httr::content(response, as = "parsed", encoding = "UTF-8")
      return(result$choices[[1]]$message$content)
    }

    error_msg = httr::content(response, as = "parsed", encoding = "UTF-8")

    # Handling rate limit exceeded dynamically
    if (!is.null(error_msg$error$code) && error_msg$error$code == "rate_limit_exceeded") {
      wait_time = 10 # fallback wait in case parsing fails
      if (grepl("try again in ([0-9.]+)s", error_msg$error$message)) {
        wait_time = as.numeric(sub(".*try again in ([0-9.]+)s.*", "\\1", error_msg$error$message))
      }
      message(sprintf("Rate limit hit. Waiting %.2f seconds before retry (attempt %d)...", wait_time, attempt))
      Sys.sleep(wait_time)
      attempt = attempt + 1
      next
    }

    stop("API call failed: ", jsonlite::toJSON(error_msg, auto_unbox = TRUE))
  }
}

##################
## Helper function for translation into another language
##################
llm_translate = function(text) {
  llm_wrapper_connect(
    paste0(
      "Translate into ", language,
      #" only if the text is not already in English. ",
      #"If it is already in English, return it exactly as provided. ",
      "The text to be translated is:\n",
      text,'\n\n',
      #"Otherwise return the text exactly the way it is if already in English.
      "Do not add any notes, explanations, introductions, or extra words.
      Provided just the needed text only."
    )
  )
}

#######
# Function to generate translated section headers used in the report
translated_header_fn = function(lang = language){
if(lang!='english')
{
  translated_background_header <<- llm_translate("Background")
  translated_measures_header   <<- llm_translate("Survey Measures")
  translated_findings_header   <<- llm_translate("Findings")
  translates_charts_headr <<- llm_translate("Charts")
  translated_ref_list <<- llm_translate("Reference List")
} else{
  translated_background_header <<- "Background"
  translated_measures_header   <<- "Survey Measures"
  translated_findings_header   <<- "Findings"
  translated_ref_list <<- "Reference List"
  translates_charts_headr <<- "Charts"
}
}

########### Function to generate numbers for reporting for single/current survey round
# These functions are used in script 4 to compute indicator estimates
# and produce structured outputs used for narrative generation.

# Supporting functions:
# rev_compute_indicator
# rev_compute_wide_tab
# rev_compute_pvalue
# rev_analyse_indicator

# -------------------------------
# Indicator computation function
# -------------------------------
rev_compute_indicator = function(ind_level, type_indicators, subset_indicators) {
  ind_type = tolower(type_indicators[grep(ind_level, subset_indicators)][1])
  if (is.na(ind_type)) stop("Indicator type not found for: ", ind_level)

  formula = as.formula(paste0("~", ind_level))
  fun = switch(ind_type,
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
rev_compute_wide_tab = function(indicator, svy_datum, strat = NULL) {
  #
  by_formula = as.formula(if (is.null(strat)) "~svy_year" else paste0("~", strat, "+ svy_year"))

  df = svyby(
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
    names(df)[1] = "category"
    df = df %>% mutate(stratifier = strat)
  } else {
    df = df %>% mutate(stratifier = "Total", category = "Total")
  }

  # Identify estimate and CI columns dynamically
  ind_name = as.character(indicator$formula[[2]])
  est_col = grep(paste0("^", ind_name, "$"), names(df), value = TRUE)
  lower_col = grep("ci_l|ci.low|ci_lw|ci_lwr|ci_lower", names(df), value = TRUE, ignore.case = TRUE)
  upper_col = grep("ci_u|ci.high|ci_upp|ci_upper", names(df), value = TRUE, ignore.case = TRUE)

  if (length(lower_col) == 0) lower_col = NA
  if (length(upper_col) == 0) upper_col = NA

  df = df %>%
    mutate(
      estimate = if (!is.na(est_col)) .data[[est_col]] else NA_real_,
      ci_low = if (!is.na(lower_col)) .data[[lower_col]] else NA_real_,
      ci_high = if (!is.na(upper_col)) .data[[upper_col]] else NA_real_
    )

  # Convert categorical to percentage
  if (indicator$type == "categorical") {
    df = df %>% mutate(across(c(estimate, ci_low, ci_high), ~ . * 100))
  }

  # Keep only relevant columns
  df = df %>% select(stratifier, category, svy_year, estimate, ci_low, ci_high)

  # Pivot to wide format by survey year
  df_wide = df %>%
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
rev_compute_pvalue = function(ind_level, indicator, svy_datum, strat_col = NULL) {
  if (is.null(strat_col)) return(NA_real_)
  if (length(unique(svy_datum$variables[[strat_col]])) <= 1) return(NA_real_)

  p = tryCatch({
    if (indicator$type %in% c("mean", "median")) {
      formula = as.formula(paste0(ind_level, " ~ ", strat_col))
      fit = svyglm(formula, design = svy_datum)
      coef_test = regTermTest(fit, strat_col)
      coef_test$p
    } else if (indicator$type == "categorical") {
      svy_datum$variables[[ind_level]] = factor(svy_datum$variables[[ind_level]])
      test = svychisq(as.formula(paste0("~", ind_level, " + ", strat_col)),
                      design = svy_datum,statistic = "Chisq",simulate.p.value = TRUE)
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
rev_analyse_indicator = function(ind_level, type_indicators, subset_indicators, svy_datum,
                                  sect, section_title, grp_tab_title, ind_subtitle, arrange_num,
                                  sub_section_text, background_text) {
  indicator = rev_compute_indicator(ind_level, type_indicators, subset_indicators)

  # Stratified results
  strat_results = lapply(narrative_strat, function(s) {
    ##NOTE Recheck this stratification substitution for agerange
    if(s == "agerange"){strat = s}
    ##
    df = rev_compute_wide_tab(indicator, svy_datum, strat = s)
    pval = rev_compute_pvalue(ind_level, indicator, svy_datum, strat_col = s)

    df %>% mutate(
      p_value = pval,
      significance = ifelse(!is.na(pval) & pval < 0.05, "Significant", "Not significant")
    )
  }) %>% bind_rows()

  # Total (no stratifier)
  total_results = rev_compute_wide_tab(indicator, svy_datum) %>%
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
    dplyr::select(sect, section_title, grp_tab_title, ind_subtitle, stratifier, category, everything())
}

# -------------------------------
# Compute all indicators for a section
# -------------------------------
rev_comp_numbers = function(sect) {
  data = analysis_data #
  section_matrix = reporting_matrix %>% filter(section == sect)
  section_results = NULL

  for (i in 1:nrow(section_matrix)) {
    #print(i)
    sub_matrix = section_matrix[i,]
    subset_indicators = strsplit(sub_matrix$indicator_var, ";")[[1]]
    type_indicators = strsplit(sub_matrix$type, ";")[[1]]
    denom_logic = strsplit(sub_matrix$pop_subset, ";")[[1]]

    grp_tab_title = sub_matrix$table_title
    tab_subtitle1 = strsplit(sub_matrix$subtitle1, ";")[[1]]
    tab_subtitle2 = sub_matrix$subtitle2 %>% strsplit(";") %>% 
      unlist() %>% 
      strsplit(":") %>% 
      unlist()
    arrange_num = sub_matrix$arrange_num
    sub_section_text = sub_matrix$sub_section_text
    background_text = sub_matrix$background
    section_title = sub_matrix$section_title#[1]
    ##
    agegrp_var = unique(sub_matrix$agevar)
    ##

    if (!all(is.na(tab_subtitle2))) tab_subtitle1 = tab_subtitle2

    ####Defining survey design structure
    wt_step = unique(sub_matrix$weight_step)[1]
    data[,wt_step] = as.numeric(as.character(data[,wt_step]))
    ##Setting arbitrary weights 0 to missing survey weights: This is later to preserve the design during analysis
    data[,wt_step][is.na(data[,wt_step])] = 0
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
    
    for (ind_level in subset_indicators) {
      #print(ind_level)
      if(!all(is.na(analysis_data[[ind_level]])))
      {
        ind_position = grep(ind_level, subset_indicators)
        #denom_condition = denom_logic[ind_position]
        denom_condition = if (length(denom_logic) == 1) {denom_logic[1]} else {denom_logic[grep(ind_level,subset_indicators)]}
        ind_subtitle = tab_subtitle1[ind_position]
        # Subset data
        if (denom_condition == "all") {
          svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & 
                               !is.na(get(agegrp_var)) & get(wt_step)!=0)
        } else {
          svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & 
                               !is.na(get(agegrp_var)) & get(wt_step)!=0 &
                                eval(parse(text = paste0("(", denom_condition, ")"))))
        }
        # Denominator computation
        total_n = nrow(svy_datum$variables)
        #
        comb_rslts = rev_analyse_indicator(ind_level, type_indicators, subset_indicators, svy_datum,
                                            sect, section_title, grp_tab_title, ind_subtitle, arrange_num,
                                            sub_section_text, background_text)%>%
        # Adding total per indicator to assess robustness of the computed estimates
                     mutate(total_n = total_n)
        #
        section_results = bind_rows(section_results, comb_rslts)
      }else{}
    }
  }

  return(section_results)
}

# ============================================================
# FUNCTION: Replace consecutive duplicate values with blanks
# Useful for table formatting
# ============================================================
blank_consecutive_duplicates = function(x) {
  x[c(FALSE, x[-1] == x[-length(x)])] = ""
  x
}

# ============================================================
# FUNCTION: Generate indicator numbers for infographics
# ============================================================
gen_numbers_fn = function(sect = unique(infographic_matrix$section)[2])
{
  data = analysis_data

  section_matrix = infographic_matrix %>% dplyr::filter(section == sect)
  ##
  no_wt_step = unique(section_matrix$weight_step)[1]
  numeric_step = as.numeric(str_extract(no_wt_step, "\\d+"))

  ####
  section_results = NULL
  ind = NULL

  for (ind in 1:nrow(section_matrix))
  {
    #print(ind)
    sub_matrix = section_matrix[ind,]

    #
    subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))
    type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))
    denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))
    ####Defining survey design structure
    wt_step = unique(sub_matrix$weight_step)[1]
    data[,wt_step] = as.numeric(as.character(data[,wt_step]))
    ##Setting arbitrary weights 0 to missing survey weights: This is later to preserve the design during analysis
    data[,wt_step][is.na(data[,wt_step])] = 0
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
    
    ###
    ind_level = NULL
    sub_section_results = NULL

    for(ind_level in subset_indicators)
    {
      if(!all(is.na(analysis_data[[ind_level]])))
      {
        #denom_condition = denom_logic[grep(ind_level,subset_indicators)]
        denom_condition = if (length(denom_logic) == 1) {denom_logic[1]} else {denom_logic[grep(ind_level,subset_indicators)]}
        #
        if (denom_condition == 'all') {
          datum = data %>% 
            dplyr::filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange))
          svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & 
                               !is.na(agerange) & get(wt_step)!=0)
        } else {
          datum = data %>% 
            dplyr::filter(!is.na(eval(parse(text = ind_level))) & 
                            !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))))
          svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & 
                               !is.na(agerange) & get(wt_step)!=0 & 
                               eval(parse(text = paste0('(',denom_condition,')'))))
        }

        #####
        degrees_freedom = degf(svy_data)-1

        if(type_indicators[grep(ind_level,subset_indicators)] == 'mean'|type_indicators[grep(ind_level,subset_indicators)] == 'median')
        {
          #
          eval(parse(text = paste0('formula = ~', ind_level)))
          ####
          eval(parse(text = paste0('results_',all_stratifiers,' = svyby(formula, by = ~',all_stratifiers,',design = svy_datum, FUN = svymean,
                                 method = "lo", df = degrees_freedom,keep.var=FALSE)')))
          ##
          eval(parse(text = paste0('colnames(results_',all_stratifiers,') =c("col1","col2")')))
          ##
          eval(parse(text = paste0('comb_rslts = ','rbind(',paste0('results_',all_stratifiers, collapse = ","),')')))
          #
          total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", df = degrees_freedom)

          ##
          comb_rslts = rbind(cbind(ind_level,comb_rslts)%>% as.data.frame() %>%
                               mutate(col1 = as.character(col1),
                                      col2 = as.character(col2)),
                             c(ind_level,"Total",total_est_ciprop[1]))

          ####Computation of median and IQR
          median_compute = unique(type_indicators)=='median'
          if(median_compute==TRUE)
          {
            ###
            eval(parse(text = paste0('results_',all_stratifiers,' = svyby(formula, by = ~',all_stratifiers,
            ',design = svy_datum, FUN = svyquantile,
                                 method = "lo", df = degrees_freedom,keep.var=FALSE, quantiles = .5)[,1:2]')))
            ##
            eval(parse(text = paste0('colnames(results_',all_stratifiers,') =c("col1","col2")')))
            ##
            eval(parse(text = paste0('comb_rslts = ','rbind(',paste0('results_',all_stratifiers, collapse = ","),')')))
            ###
            total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", 
                                           quantiles = 0.5, df = degrees_freedom)[[1]][1]

            ##
            comb_rslts = rbind(cbind(ind_level,comb_rslts)%>% as.data.frame() %>%
                                 mutate(col1 = as.character(col1),
                                        col2 = as.character(col2)),
                               c(ind_level,"Total",total_est_ciprop[1]))
          }

        } else if(type_indicators[grep(ind_level,subset_indicators)] == 'categorical'){
          ##
          eval(parse(text=paste0('formula = ~I(',ind_level, '=="',1,'")')))

          ###
          eval(parse(text = paste0('results_',all_stratifiers,
          ' = svyby(formula, by = ~',all_stratifiers,
          ',design = svy_datum, FUN = svyciprop,
                                 method = "lo", df = degrees_freedom,keep.var=FALSE)')))
          ##
          eval(parse(text = paste0('colnames(results_',all_stratifiers,') =c("col1","col2")')))
          ##
          eval(parse(text = paste0('comb_rslts = ','rbind(',paste0('results_',all_stratifiers, collapse = ","),')')))
          #
          total_est_ciprop = svyciprop(formula, design = svy_datum, method = "lo", df = degrees_freedom)

          ##
          comb_rslts = rbind(cbind(ind_level,comb_rslts)%>% as.data.frame() %>%
                               mutate(col1 = as.character(col1),
                                      col2 = as.character(col2)),
                             c(ind_level,"Total",total_est_ciprop[1]))
        }
        ########Combining estimates
        sub_section_results = rbind(sub_section_results,comb_rslts)
      }else{}
    }

    section_results = rbind(section_results,sub_section_results)

  }
  return(section_results)
}


# ============================================================
# UTILITY: Convert column to specified data type safely
# ============================================================
convert_to_target_class = function(x, target_class) {
  # Get the appropriate 'as.' function (eg as.numeric, as.factor)
  conversion_function = match.fun(paste0("as.", target_class))
  # Apply the conversion
  return(conversion_function(x))
}

# ============================================================
# UTILITY: Normalise weight step variables in dataset
# ============================================================
normalize_wstep_vars = function(datum, prefix = "wstep", postfix = "_norm") {

  # Find all columns starting with the prefix
  wstep_vars = grep(paste0("^", prefix), names(datum), value = TRUE)
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


########### Function to generate numbers for reporting for comparative analysis between survey rounds
# These functions are used in script 6 - 7 to compute indicator estimates
# and produce structured outputs.

# Supporting functions:
# rev_compute_indicator
# rev_compute_wide_tab
# rev_compute_pvalue
# rev_analyse_indicator
# -------------------------------
# Function: compute_indicator
# -------------------------------
# Purpose: Determines the type of an indicator (mean, median, categorical)
#          and returns the corresponding survey function and formula for computation.

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
# Function: compute_wide_tab
# -------------------------------
# Purpose: Compute indicator estimates across survey years and optionally stratified,
#          then reshape results into wide format with change between last two years.

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
# Function: compute_pvalue
# -------------------------------
# Purpose: Compute p-value for difference in indicator between survey years,
#          optionally within a stratified group.

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
      test$p.value
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

  round(p, 4)
}

# -------------------------------
# Function: analyse_indicator
# -------------------------------
# Purpose: Wrapper to compute indicator estimates with stratifiers, totals,
#          p-values, significance, and attach reporting metadata.

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
# Function: comp_numbers
# -------------------------------
# Purpose: Main function to compute all indicators for a given section.
comp_numbers = function(sect) {
  data = combined_dataset
  section_matrix = comparative_reporting_matrix %>% filter(section == sect)
  #########
  section_results = NULL

  for (i in 1:nrow(section_matrix)) {
    sub_matrix = section_matrix[i,]
    subset_indicators = strsplit(sub_matrix$indicator_var, ";")[[1]]
    type_indicators = strsplit(sub_matrix$type, ";")[[1]]
    denom_logic = strsplit(sub_matrix$pop_subset, ";")[[1]]

    grp_tab_title = sub_matrix$table_title
    tab_subtitle1 = strsplit(sub_matrix$subtitle1, ";")[[1]]
    tab_subtitle2 = sub_matrix$subtitle2 %>% 
      strsplit(";") %>% unlist() %>% 
      strsplit(":") %>% unlist()
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
    svy_data = svydesign(id=~psu_year, 
                         weights=~get(wt_step),
                         strata=~strata_year, 
                         data=data,nest = T)
    # Correct for finite sample size if fpc exists in the data
    if(fpc_var_check & fpc_var_check2){
      svy_data = svydesign(id=~psu_year, 
                           weights=~get(wt_step),
                           strata=~strata_year, 
                           fpc = ~fpc,
                           data=data,nest = T)    
      }
    ###

    for (ind_level in subset_indicators) {
      if(!all(is.na(data[[ind_level]])))
      {
        #print(ind_level)
        ind_position = grep(ind_level, subset_indicators)
        #denom_condition = denom_logic[ind_position]
        denom_condition = if (length(denom_logic) == 1) {denom_logic[1]} else {denom_logic[grep(ind_level,subset_indicators)]}
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
        #Denominator computation
        total_n = nrow(svy_datum$variables %>% 
                         dplyr::filter(svy_year == sort(unique(svy_year)[1])))
        
        section_results = bind_rows(section_results, 
                                    comb_rslts %>% mutate(total_n = total_n))
      }else{}
    }
  }

  return(section_results)
}

# -------------------------------
# Function: add_grey_header
# -------------------------------
# Purpose: Create a grey header row for comparative factsheet using flextable
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

# -------------------------------
# Function: chart_function
# -------------------------------
# Purpose: Generate combined trend and change charts for a given indicator group

chart_function = function(indicator_group = unique(sec_report_matrix$grp_tab_title)[1])
{
  ###Indicator type
  type_data = comparative_reporting_matrix %>% 
    dplyr::filter(table_title == indicator_group)%>%
    dplyr::select(type)
  
  ind_type = strsplit(type_data$type, ";")[[1]][1]
  
  #####
  test_data = sec_report_matrix %>%
    dplyr::filter(grp_tab_title == indicator_group) %>%
    group_by(grp_tab_title) %>%
    mutate(flag = n_distinct(ind_subtitle, na.rm = TRUE) <= 1) %>%
    ungroup()%>%
    dplyr::filter((flag == TRUE & (category %in% c('Total','Men','Women'))) |
                    (flag == FALSE & (category == 'Total')))%>%
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
                             labels = ifelse(language == "english", category_label, llm_translate(category_label))
    ),
    category = gsub('NA:|NA :|NA :| NA:| NA :| NA :','',category),
    category = ifelse(flag == TRUE, 
                      gsub('.*:','',category),
                      gsub(':.*','',category))
    ) %>%
    ungroup()%>%
    mutate(category = factor(category,
                             levels = unique(category),
                             labels = unique(category)))
  
  # === Prepare Plot Data ===
  main_plot_data = select(test_data, category, matches("^\\d{4}$"), change)
  
  test_df_long = main_plot_data %>%
    select(-change) %>%
    pivot_longer(-category)
  
  # === Plot 1: Trend Plot ===
  nudge_value=.6
  num_height = length(unique(test_df_long$category))
  
  p1 = ggplot(test_df_long, aes(x = value, y = category)) +
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
  df_gap = test_data %>%
    mutate(
      change_label = ifelse(change <= 0, change, paste0("+", change)),
      indicator_type = ind_type, report_signf = report_signf,
      #
      change_label = ifelse(indicator_type =='categorical',paste0(change_label,'%'),change_label),
      #
      change_label = ifelse(report_signf =='Yes' &
                              significance_of_change == 'Significant',
                            paste0(change_label,'‡'),change_label)
    )
  
  # === Plot 2: Change Labels ===
  #lab_change = ""
  lab_change = "▲"
  
  p_gap = ggplot(df_gap, aes(x = change, y = category)) +
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
  chart_title = ifelse(language == "english",
                       unique(test_data$grp_tab_title),
                       llm_translate(unique(test_data$grp_tab_title)))
  
  ##Chart position
  #chart_position = grep(paste0("^",unique(test_data$grp_tab_title),"$"),indicator_groups)
  chart_position = which(indicator_groups %in% unique(test_data$grp_tab_title))
  mod_title = as.character(paste0(chart_position,': ',chart_title))
  #p1 = p1 + coord_cartesian(clip = "off")
  p_whole = p1 + p_gap +
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
# -------------------------------
# Function: infog_search_vars
# -------------------------------
# Purpose: Search for rows with relevant indicators for infographic generation

infog_search_vars = function(ind_var = 'selected', ind_group) {
  
  # Create a regex pattern by collapsing variable names with OR operator
  pattern = paste(tolower(ind_var), collapse="|")
  
  # Search for pattern within the logical expression string
  found = grepl(pattern, ind_group)
  
  # Return TRUE if any variable is detected, otherwise FALSE
  return(found)
}


############################################################
## FUNCTION TO GENERATE COMPARATIVE FACTSHEET SECTION
############################################################

# This function computes the results used in a comparative
# factsheet section. It:
# - extracts indicators defined in the factsheet matrix
# - calculates weighted survey estimates
# - computes confidence intervals
# - performs significance testing between survey years
# - formats results for inclusion in the factsheet table

comp_factsheet_section_fn = function(sect = unique(comparative_fact_sheet_matrix$section)[1])
{
  
  ##########################################################
  ## INITIALIZE DATA AND SECTION METADATA
  ##########################################################
  
  # Use the combined dataset containing all survey rounds
  data = combined_dataset
  
  # Extract rows corresponding to the requested section
  section_matrix = comparative_fact_sheet_matrix %>%
    dplyr::filter(section == sect) %>%
    arrange(factsheet_desc)
  
  # Extract survey weight variable used for the section
  wt_step = unique(section_matrix$weight_step)[1]
  
  # Extract numeric step number from weight variable name
  numeric_step = as.numeric(str_extract(wt_step, "\\d+"))
  
  # Create formatted section title for output table
  section_title = c(
    paste0('Step ',numeric_step,' ',unique(section_matrix$section)),
    '','',''
  )
  
  # Certain sections do not include step numbers
  if(sect=='Summary of Combined Risk Factors' |
     sect=='Cardiovascular disease risk')
  {
    section_title = c(unique(section_matrix$section),'','','')
  }
  
  
  ##########################################################
  ## INITIALIZE OUTPUT OBJECTS
  ##########################################################
  
  section_results = NULL
  ind = NULL
  
  
  ##########################################################
  ## LOOP THROUGH INDICATORS IN SECTION
  ##########################################################
  
  for (ind in 1:nrow(section_matrix))
  {
    
    # Extract indicator definition for the current row
    sub_matrix = section_matrix[ind,]
    
    # Determine indicator positions when multiple indicators exist
    ind_position = as.numeric(do.call('c',strsplit(sub_matrix$ind_position, "[;]")))
    
    # Extract indicators included in the section
    subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))[ind_position]
    
    # Extract indicator types (mean / median / categorical)
    type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))[ind_position]
    
    # Extract denominator conditions
    #denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))[ind_position]
    denom_logic = if (length(do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))) == 1) {sub_matrix$pop_subset[1]} else {do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))[ind_position]}
    
    # Extract indicator descriptions used in factsheet
    ind_desc = do.call('c',strsplit(sub_matrix$factsheet_desc, "[;]"))
    
    
    ########################################################
    ## DEFINE SURVEY DESIGN
    ########################################################
    
    # Extract weight variable for the indicator
    wt_step = unique(sub_matrix$weight_step)[1]
    
    # Ensure survey weights are numeric
    data[,wt_step] = as.numeric(as.character(data[,wt_step]))
    
    # Replace missing weights with 0 to preserve design structure
    data[,wt_step][is.na(data[,wt_step])] = 0
    
    # Create survey design object
    svy_data = svydesign(
      id=~psu_year,
      weights=~get(wt_step),
      strata=~strata_year,
      data=data,
      nest = TRUE
    )
    # Correct for finite sample size if fpc exists in the data
    if(fpc_var_check & fpc_var_check2){
      svy_data = svydesign(id=~psu_year, 
                           weights=~get(wt_step),
                           strata=~strata_year, 
                           fpc = ~fpc,
                           data=data,nest = T)    
    }
    
    ########################################################
    ## LOOP THROUGH INDICATOR LEVELS
    ########################################################
    
    ind_level = NULL
    sub_section_results = NULL
    
    for(ind_level in subset_indicators)
    {
      # Extract denominator condition for this indicator
      #denom_condition = denom_logic[grep(ind_level,subset_indicators)]
      denom_condition = if (length(denom_logic) == 1) {denom_logic[1]} else {denom_logic[grep(ind_level,subset_indicators)]}
      
      # Extract indicator description
      full_ind_desc = ind_desc[grep(ind_level,subset_indicators)]
      
      ######################################################
      ## APPLY DENOMINATOR CONDITIONS
      ######################################################
      
      if (denom_condition == 'all') {
        
        datum = data %>%
          filter(!is.na(eval(parse(text = ind_level))) &
                   !is.na(agerange))
        
        svy_datum = subset(
          svy_data,
          !is.na(eval(parse(text = ind_level))) &
            !is.na(agerange) &
            get(wt_step)!=0
        )
        
      } else {
        
        datum = data %>%
          dplyr::filter(
            !is.na(eval(parse(text = ind_level))) &
              !is.na(agerange) &
              eval(parse(text = paste0('(',denom_condition,')')))
          )
        
        svy_datum = subset(
          svy_data,
          !is.na(eval(parse(text = ind_level))) &
            !is.na(agerange) &
            eval(parse(text = paste0('(',denom_condition,')'))) &
            get(wt_step)!=0
        )
      }
      
      
      ######################################################
      ## COMPUTE SURVEY ESTIMATES
      ######################################################
      
      degrees_freedom = degf(svy_datum)-1
      
      
      ######################################################
      ## NUMERIC INDICATORS (MEAN / MEDIAN)
      ######################################################
      
      if(type_indicators[grep(ind_level,subset_indicators)] == 'mean' |
         type_indicators[grep(ind_level,subset_indicators)] == 'median')
      {
        
        eval(parse(text = paste0('formula = ~', ind_level)))
        
        # Compute survey means and confidence intervals
        svyr_est_ciprop =
          svyby(
            formula,
            by = ~svy_year,
            design = svy_datum,
            FUN = svymean,
            method = "lo",
            vartype = 'ci'
          ) %>%
          mutate(ci_l = ifelse(ci_l<0,0,ci_l))
        
        
        ####################################################
        ## MEDIAN AND IQR CALCULATION
        ####################################################
        
        median_compute = unique(type_indicators)=='median'
        
        if(median_compute==TRUE)
        {
          svyr_est_ciprop =
            svyby(
              formula,
              by = ~svy_year,
              design = svy_datum,
              FUN = svyquantile,
              quantiles = c(.5,.25,.75),
              method = "lo"
            )[,1:4]
        }
        
        
        ####################################################
        ## T-TEST BETWEEN SURVEY YEARS
        ####################################################
        
        svy_datum$variables[[ind_level]] =
          as.numeric(svy_datum$variables[[ind_level]])
        
        test =
          svyttest(
            as.formula(paste0(ind_level,
                              " ~ as.factor(svy_year)")),
            design = svy_datum
          )
        
        p.value = as.numeric(test$p.value)
        
        delim_char = '\n('
        mult_n = 1
        change_delim = ''
        
        
        ######################################################
        ## CATEGORICAL INDICATORS
        ######################################################
        
      } else if(type_indicators[grep(ind_level,subset_indicators)] == 'categorical'){
        
        eval(parse(text=paste0(
          'formula = ~I(',ind_level, '=="',1,'")')))
        
        svyr_est_ciprop =
          svyby(
            formula,
            by = ~svy_year,
            design = svy_datum,
            FUN = svyciprop,
            method = "lo",
            vartype = 'ci'
          )
        
        delim_char = '%\n('
        mult_n = 100
        change_delim = '%'
        
        
        ####################################################
        ## CHI-SQUARE TEST FOR CHANGE
        ####################################################
        
        svy_datum$variables[[ind_level]] =
          factor(svy_datum$variables[[ind_level]])
        
        tab =
          svytable(
            as.formula(paste0("~", ind_level,
                              " + svy_year")),
            design = svy_datum
          )
        
        if (nrow(tab) < 2 || ncol(tab) < 2) {
          p.value = NA_real_
        } else {
          
          test =
            svychisq(
              as.formula(paste0("~", ind_level,
                                " + svy_year")),
              design = svy_datum,
              statistic = "Chisq",
              simulate.p.value = TRUE
            )
          
          p.value = test$p.value
        }
      }
      
      
      ######################################################
      ## FORMAT RESULTS
      ######################################################
      
      colnames(svyr_est_ciprop)=c("svy_year",ind_level,"ci_l","ci_u")
      
      all_years =
        data.frame(
          svy_year =
            sort(unique(combined_dataset$svy_year))
        )
      
      svyr_est_ciprop =
        all_years %>%
        left_join(svyr_est_ciprop, by = "svy_year") %>%
        mutate(across(-svy_year,
                      ~ tidyr::replace_na(., 0)))
      
      
      ######################################################
      ## CALCULATE CHANGE BETWEEN SURVEYS
      ######################################################
      
      change_est_ciprop =
        round(mult_n*as.numeric(svyr_est_ciprop[2,ind_level]),1) -
        round(mult_n*as.numeric(svyr_est_ciprop[1,ind_level]),1)
      
      significance =
        ifelse(!is.na(p.value) &
                 as.numeric(p.value) < 0.05,
               '*','')
      
      change_est_ciprop =
        paste0(
          formatC(change_est_ciprop,
                  format = "f", digits = 1),
          change_delim,
          significance
        )
      
      
      ######################################################
      ## COMBINE ESTIMATES INTO OUTPUT FORMAT
      ######################################################
      
      svyr_est1 =
        paste0(
          formatC(round(mult_n*as.numeric(svyr_est_ciprop[1,2]),1),
                  format = "f", digits = 1),
          delim_char,
          formatC(round(mult_n*as.numeric(svyr_est_ciprop[1,3]),1),
                  format = "f", digits = 1),
          ' - ',
          formatC(round(mult_n*as.numeric(svyr_est_ciprop[1,4]),1),
                  format = "f", digits = 1),
          ')'
        )
      
      svyr_est2 =
        paste0(
          formatC(round(mult_n*as.numeric(svyr_est_ciprop[2,2]),1),
                  format = "f", digits = 1),
          delim_char,
          formatC(round(mult_n*as.numeric(svyr_est_ciprop[2,3]),1),
                  format = "f", digits = 1),
          ' - ',
          formatC(round(mult_n*as.numeric(svyr_est_ciprop[2,4]),1),
                  format = "f", digits = 1),
          ')'
        )
      
      combined_results =
        c(full_ind_desc,svyr_est1,svyr_est2,change_est_ciprop)
      
      sub_section_results =
        rbind(sub_section_results,combined_results)
    }
    
    section_results =
      rbind(section_results,sub_section_results)
  }
  
  
  ##########################################################
  ## FINAL OUTPUT TABLE
  ##########################################################
  
  section_results =
    rbind(
      section_title,
      section_results[order(section_results[,1]),]
    )
  
  return(section_results)
}

