#####
# Define a function to generate a summary table for numeric demographic data
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
  names_to_select = c("eval(parse(text = strat_variable))",'count_Men','mean_var_Men','count_Women','mean_var_Women','Total_Count', 'Total_mean')
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
    summary_table = summary_table %>% dplyr::filter(!is.na(`eval(parse(text = strat_variable))`)) ###Excluding the totals row
    # Add a title row for the stratification variable
    summary_table = rbind(c(row_strat_variable_titles[strat_position], rep('',ncol(summary_table)-1)),summary_table %>% as.matrix())%>%as.data.frame()
  } else{}
  
  return(summary_table)
}
#####
# Define a function to generate a summary table for categorical demographic data
demog_cat = function (strat_variable = 'agerange')
{
  # Create a count table by filtering and grouping data
  count_table = datum %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(agerange))%>%# Filter out rows with NA values in the specified variable and 'agerange'
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
    count_table = count_table %>% dplyr::filter(!is.na(`eval(parse(text = strat_variable))`)) ###Excluding the any row with NAs
    # Add a title row for the stratification variable and convert to a data frame
    count_table = rbind(c(row_strat_variable_titles[strat_position], rep('',ncol(count_table)-1)),count_table %>% as.matrix())%>%as.data.frame()
  } else{count_table = count_table %>% as.matrix()%>%as.data.frame()}    # If the stratification variable is 'agerange' or empty, just convert the table to a data frame
  
  
  return(count_table)
}
# Define a function to analyze numeric non-demog data with stratification
analysis_numeric_fn = function(row_strat = 'agerange', col_strat = 'sex')
{
  # Filtering and processing data based on condition
if (denom_condition == 'all') {
  # If denominator condition is 'all', include all data with non-NA values
  datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
  datum[,k] = as.numeric(datum[,k])
  
  svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
  eval(parse(text = paste0('svy_datum$variables$',k,' = as.numeric(as.character(svy_datum$variables$',k,'))')))
  svy_datum = subset(svy_datum, !is.na(eval(parse(text = k))))
  # If no rows in the data, set survey data to have zero counts for the variable
  if(nrow(datum)==0)
  {
    svy_datum = svy_data
    svy_datum$variables[,k]=0
  }
  
} else {    # Apply a specific condition to filter the data
  datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
  datum[,k] = as.numeric(datum[,k])
  svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
  ##
  eval(parse(text = paste0('svy_datum$variables$',k,' = as.numeric(as.character(svy_datum$variables$',k,'))')))
  svy_datum = subset(svy_datum, !is.na(eval(parse(text = k))))
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
      group_by(eval(parse(text=col_strat)), .drop = FALSE) %>% reframe(participants=n())%>%
      pivot_wider(names_from = `eval(parse(text = col_strat))`, values_from = participants) %>%
      mutate(`eval(parse(text = row_strat))`='Total',across(contains(col_strat_var_levels), ~coalesce(., 0)), 
             Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", collapse = '+')))))%>% 
  dplyr::filter(!is.na(`eval(parse(text = row_strat))`))
# Rename columns for clarity
colnames(n_participants) = c(paste0('0',row_strat),paste0(colnames(n_participants)[-1],'_a'))
#Calculate weighted participant percentages
est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+ eval(parse(text=row_strat)), design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
            full_join(svyby(formula, by = ~eval(parse(text=row_strat)), design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T) %>%
                        mutate(`eval(parse(text = col_strat))` = 'Total'))%>%mutate(ci_l = ifelse(ci_l<0,0,ci_l))
##Totals
# Calculate total weighted percentages
colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat)), design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
                       mutate(ci_l = ifelse(ci_l<0,0,ci_l))
total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", df = degrees_freedom, na.rm = T) 
conf_interval = confint(total_est_ciprop, df = degrees_freedom, na.rm=T) 
conf_interval[conf_interval<0] = 0

total_est_ciprop = cbind(as.vector(total_est_ciprop),ci_l = as.numeric(conf_interval[1]),ci_u=as.numeric(conf_interval[2]))%>%
                   as.data.frame() %>% add_column(`eval(parse(text = col_strat))` = 'Total')
# Rename column for consistency
colnames(total_est_ciprop)[1] = colnames(colstrat_var_est_ciprop)[2]
# Combine estimates and confidence intervals
combined_est_ciprop = est_ciprop %>% mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>% 
                        full_join(colstrat_var_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) %>% 
                        full_join(total_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) 

#  # Population level estimates if specified
if(any(sub_matrix$pop_level_num_denom!=''& !is.na(sub_matrix$pop_level_num_denom)))
{
  est_ciprop = datum %>%group_by(eval(parse(text=col_strat)),eval(parse(text=row_strat)))%>%
               reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), ci_l = NA, ci_u = NA)%>%
               full_join(datum %>%group_by(eval(parse(text=row_strat)))%>%
                           reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), ci_l = NA, ci_u = NA)%>%
                           mutate(`eval(parse(text = col_strat))` = 'Total'))
  #####
  colstrat_var_est_ciprop = datum %>%group_by(eval(parse(text=col_strat)))%>%
                            reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), ci_l = NA, ci_u = NA)
  
  ####
  total_est_ciprop = datum %>%
                     reframe(ind_avg = eval(parse(text = sub_pop_num_denom)), ci_l = NA, ci_u = NA)%>% 
                     add_column(`eval(parse(text = col_strat))` = 'Total')
  ###
  combined_est_ciprop = est_ciprop %>% mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>% 
                        full_join(colstrat_var_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) %>% 
                        full_join(total_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total'))%>%
                        mutate(ind_avg=ind_avg*100)%>% as.data.frame()
}

# Compute median and interquartile range (IQR) if specified
if(median_compute==TRUE)
{
  est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+ eval(parse(text=row_strat)), design = svy_datum, FUN = svyquantile, quantiles = c(.5,.25,.75), method = "lo", df = degrees_freedom, na.rm.all = T)[,-c(6:8)]%>%
    full_join(svyby(formula, by = ~eval(parse(text=row_strat)), design = svy_datum, FUN = svyquantile, quantiles = c(.5,.25,.75), method = "lo", df = degrees_freedom, na.rm.all = T)[,-c(5:7)]%>%
                mutate(`eval(parse(text = col_strat))` = 'Total'))
  ##Totals
  colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat)), design = svy_datum, FUN = svyquantile, quantiles = c(.5,.25,.75), method = "lo", df = degrees_freedom, na.rm.all = T)[,-c(5:7)]
  total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", quantiles = c(.5,.25,.75), df = degrees_freedom, ci=FALSE) 
  total_est_ciprop = do.call('c',total_est_ciprop)
  #
  total_est_ciprop = cbind(total_est_ciprop[1],total_est_ciprop[2],total_est_ciprop[3])%>%
    as.data.frame() %>% add_column(`eval(parse(text = col_strat))` = 'Total')
  colnames(total_est_ciprop)[1:3] = colnames(colstrat_var_est_ciprop)[2:4]
  ##Combined est_ciprop
  combined_est_ciprop = est_ciprop %>% mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>% 
    full_join(colstrat_var_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) %>% 
    full_join(total_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) 
  
}
#####
combined_est_ciprop[,3:5] = round(combined_est_ciprop[,3:5],1)
eval(parse(text = paste0('combined_est_ciprop[,',3:5,'] = paste0(formatC(combined_est_ciprop[,',3:5,'],format = "f", digits = 1))')))
##Point estimate
point_estimate = combined_est_ciprop[,1:3]
colnames(point_estimate)=c(col_strat,row_strat,'point_est')
ci_estimate = cbind(combined_est_ciprop[,1:2], paste0(combined_est_ciprop[,4],' - ',combined_est_ciprop[,5]))
colnames(ci_estimate)=c(col_strat,row_strat,'interval')
##conversion to wider format
point_estimate=point_estimate%>%pivot_wider(names_from = all_of(col_strat), values_from = point_est)
colnames(point_estimate) = c(paste0('0',row_strat),paste0(colnames(point_estimate)[-1],'_b'))
#
ci_estimate=ci_estimate%>%pivot_wider(names_from = all_of(col_strat), values_from = interval)
colnames(ci_estimate) = c(paste0('0',row_strat),paste0(colnames(ci_estimate)[-1],'_ci'))

# Further formatting and combining results
computed_results = n_participants %>% mutate_all(as.character) %>% 
                   left_join(point_estimate %>% mutate_all(as.character)) %>% 
                   left_join(ci_estimate %>% mutate_all(as.character))
summary_table = computed_results %>% dplyr::select(sort(colnames(computed_results)))
# Handle any missing columns
names_diff = setdiff(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
                       paste0(col_strat_var_levels,'_',c('ci')),'Total_a','Total_b','Total_ci'), names(summary_table))
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
  summary_table = summary_table%>%dplyr::select(all_of(c(paste0('0',row_strat),sort(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
                                                                                       paste0(col_strat_var_levels,'_',c('ci')))),'Total_a','Total_b','Total_ci')))
  } else{
  summary_table = summary_table[-nrow(summary_table),]%>%dplyr::select(all_of(c(paste0('0',row_strat),sort(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
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
# Define a function to analyze categorical non-demog data with stratification
analysis_categorical_fn = function(row_strat = 'agerange', col_strat = 'sex')
{
  # Convert the row stratification variable to a factor, ensuring consistent levels and labels
  data[,row_strat] = factor(data[,row_strat], levels = names(table(data[,row_strat])), labels = names(table(data[,row_strat])))
  # Conditional logic based on specific analysis types
  if(i=="Cardiovascular disease risk")
  {
    # Recode 'agerange' for cardiovascular risk analysis into 40-54 and 55-69 age groups
    data = data %>%mutate(agerange = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
                          agerange = factor(agerange,levels=1:2, labels=c('40-54','55-69')))
    # Define the survey design object using appropriate weights, strata, and nesting
    svy_data = svydesign(id=~psu, weights=~wstep3,strata=~stratum, data=data,nest = T)
    
  } else if(i=="Summary of Combined Risk Factors")
  {
    # Recode 'agerange' for combined risk factor analysis into 18-44 and 45-69 age groups
    data = data %>%mutate(agerange = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
                          agerange = factor(agerange,levels=1:2, labels=c('18-44','45-69')))
    # Define the survey design object with different weights
    svy_data = svydesign(id=~psu, weights=~wstep2,strata=~stratum, data=data,nest = T)
    # Default case: use the existing data without modification
  }else{data = data}
  # Conditional filtering based on the specified denominator condition
  if (denom_condition == 'all') {
    # Filter data where the key variable (k) and the row stratification variable are not missing
    datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
    # Subset the survey design data similarly
    svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
    # Handle cases where no rows match the filter criteria
    if(nrow(datum)==0)
    {
      svy_datum = svy_data
      svy_datum$variables[,k]=0# Set the key variable to 0 if no data matches
    }
    
  } else {
    # Apply custom filtering condition along with the default criteria
    datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
    svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
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
    group_by(eval(parse(text=col_strat)),eval(parse(text=row_strat)), .drop = FALSE) %>% 
    reframe(participants=n())%>%
    pivot_wider(names_from = `eval(parse(text = col_strat))`, values_from = participants) %>%
    mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`),
           across(contains(col_strat_var_levels), ~coalesce(., 0)), 
           Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", collapse = '+'))))%>%
    full_join(
      datum %>%
        group_by(eval(parse(text=col_strat)), .drop = FALSE) %>% reframe(participants=n())%>%
        pivot_wider(names_from = `eval(parse(text = col_strat))`, values_from = participants) %>%
        # Ensure row stratification is treated as a character variable for consistency
        # Replace missing values with 0 for participant counts
        mutate(`eval(parse(text = row_strat))`='Total',across(contains(col_strat_var_levels), ~coalesce(., 0)),       
               # Calculate the total participants across all column stratification levels
               Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", collapse = '+')))))%>% 
    dplyr::filter(!is.na(`eval(parse(text = row_strat))`))# Filter out rows where the row stratification variable is missing
  # Rename the columns to differentiate participant counts
  colnames(n_participants) = c(paste0('0',row_strat),paste0(colnames(n_participants)[-1],'_a'))
  # Calculate weighted participant percentages and confidence intervals
  est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+eval(parse(text=row_strat)), design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
    # Add a row for the total across all column stratification levels
    full_join(svyby(formula, by = ~eval(parse(text=row_strat)), design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
                mutate(`eval(parse(text = col_strat))` = 'Total'))#%>%
    #mutate(sex =ifelse(is.na(sex),'Total',sex))
  # Calculate overall totals for the column stratification variable
  colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text = col_strat)), design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)#%>%
  # Calculate the overall total estimate and confidence intervals
  total_est_ciprop = svyciprop(formula, design = svy_datum, method = "lo", df = degrees_freedom, na.rm=T) 
  
  total_est_ciprop = cbind(as.vector(total_est_ciprop),ci_l = as.numeric(attr(total_est_ciprop, "ci")[1]),ci_u=as.numeric(attr(total_est_ciprop, "ci")[2]))%>%
    as.data.frame() %>% add_column(`eval(parse(text = col_strat))` = 'Total')
  colnames(total_est_ciprop)[1] = colnames(colstrat_var_est_ciprop)[2]
  # Combine all the calculated estimates
  combined_est_ciprop = est_ciprop %>% mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>% 
    full_join(colstrat_var_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) %>% 
    full_join(total_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) #%>%
  # Convert the estimates to percentages and format the output
  combined_est_ciprop[,3:5] = round(100*combined_est_ciprop[,3:5],1)
  eval(parse(text = paste0('combined_est_ciprop[,',3:5,'] = paste0(formatC(combined_est_ciprop[,',3:5,'],format = "f", digits = 1))')))
  # Extract and reshape the point estimates
  point_estimate = combined_est_ciprop[,1:3]
  colnames(point_estimate)=c(col_strat,row_strat,'point_est')
  ci_estimate = cbind(combined_est_ciprop[,1:2], paste0(combined_est_ciprop[,4],' - ',combined_est_ciprop[,5]))
  colnames(ci_estimate)=c(col_strat,row_strat,'interval')
  ##conversion to wider format
  point_estimate=point_estimate%>%pivot_wider(names_from = all_of(col_strat), values_from = point_est)
  colnames(point_estimate) = c(paste0('0',row_strat),paste0(colnames(point_estimate)[-1],'_b'))
  #
  ci_estimate=ci_estimate%>%pivot_wider(names_from = all_of(col_strat), values_from = interval)
  colnames(ci_estimate) = c(paste0('0',row_strat),paste0(colnames(ci_estimate)[-1],'_ci'))
  
  ##Further formatting
  computed_results = n_participants %>%mutate_all(as.character)%>% left_join(point_estimate) %>% left_join(ci_estimate)
  summary_table = computed_results %>% dplyr::select(sort(colnames(computed_results)))
  ###
  names_diff = setdiff(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
                         paste0(col_strat_var_levels,'_',c('ci')),'Total_a','Total_b','Total_ci'), names(summary_table))
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
      summary_table = summary_table %>% dplyr::select(all_of(c(paste0('0',row_strat),sort(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
                                                                                                   paste0(col_strat_var_levels,'_',c('ci')))),'Total_a','Total_b','Total_ci')))
    } else{
      summary_table = summary_table[-nrow(summary_table),] %>% dplyr::select(all_of(c(paste0('0',row_strat),sort(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
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
  return(summary_table)
}


####
# Define a function that splits a given number of columns (num_cols) into multiple parts
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

####
#This function defines flestables for grouped indicator tables
flextab_function = function(index =1, table_label ='Men')
{
  ####Defining list split function first::
  list_split_number <- function(num = number_columns, max_part = 10) {
    if (num <= max_part) {
      # If num is less than or equal to max_part, return num
      return(num)
    } else {
      # If num is greater than max_part, apply the distribution logic

      # If num is greater than max_part, distribute parts evenly
      times <- ceiling(num / max_part)
      quotient <- num %/% times
      remainder <- num %% times

      # Initialize list to store parts
      parts <- numeric(times)

      # Calculate evenly distributed parts
      if (remainder == 0) {
        parts <- rep(quotient, times)
      } else {
        parts[1:remainder] <- quotient + 1
        parts[(remainder + 1):times] <- quotient
      }

      # Ensure preceding parts are multiples of 2 plus 2
      if(length(rev_n_colnames)==1 & i !="Demographic")
      {
        for (i in 1:(times - 1)) {
          excess <- parts[i] %% 2
          if (excess != 2) {
            parts[i] <- parts[i] + (2 - excess)
            parts[times] <- parts[times] - (2 - excess)
          }
        }
      }
      # Ensure preceding parts are multiples of 3 plus 1
      if((length(rev_n_colnames)>1 & i !="Demographic")|(number_columns>=20 & length(rev_n_colnames)>1))#(number_columns>=20 & length(rev_n_colnames)>1)
      {
        for (i in 1:(times - 1)) {
          excess <- parts[i] %% 3
          if (excess != 1) {
            parts[i] <- parts[i] + (1 - excess)
            parts[times] <- parts[times] - (1 - excess)
          }
        }
      }
      # Adjust the last part to ensure the sum equals num
      parts[times] <- parts[times] + num - sum(parts)

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
  # if(i!='Demographic')
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
      if((all(is.na(subtitle2))|all(subtitle2=='')) & i != "Demographic")
      {
        edited_inline_text = edited_inline_text[-excl_colpos]
        
      } else if((all(is.na(subtitle2))|all(subtitle2=='')) & i == "Demographic")
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
    col_range <- if (length(rev_n_colnames) == 1 & number_columns<20) {
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
    split_tab <- extracted_table[, col_range]
    
    # Subset the edited_inline_text based on the condition
    if (all(is.na(subtitle2)) | all(subtitle2 == '')) {
      sub_edited_inline_text <- edited_inline_text[col_range]
    } else {
      sub_edited_inline_text <- edited_inline_text[, col_range]
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

####
custom_sort1 <- function(name) {
  number <- sort(as.numeric(sub("Men_(\\d+)_Percentage", "\\1", name)))
  return(paste0('Men_',number,'_Percentage'))
}
custom_sort2 <- function(name) {
  number <- sort(as.numeric(sub("Women_(\\d+)_Percentage", "\\1", name)))
  return(paste0('Women_',number,'_Percentage'))
}
custom_sort3 <- function(name) {
  number <- sort(as.numeric(sub("sex_Total_(\\d+)_Percentage", "\\1", name)))
  return(paste0('sex_Total_',number,'_Percentage'))
}

####################Extracting reference parameters for calibrating cvd risk: The ref dataset is named risk_ref_data.dta
ref_year = 2017
risk_ref_data = read_dta('scripts/functions/risk_ref_data.dta') %>%dplyr::filter(ccode==country_ISO & year == ref_year) %>%
                dplyr::select(sex, cal2_m1_cons_ep_crbv, cal2_m1_slope_ep_crbv,cal2_m1_cons_ep_chdmi,cal2_m1_slope_ep_chdmi)
####
int_males_crbv = risk_ref_data$cal2_m1_cons_ep_crbv[risk_ref_data$sex==1]
int_females_crbv = risk_ref_data$cal2_m1_cons_ep_crbv[risk_ref_data$sex==2]
slope_males_crbv = risk_ref_data$cal2_m1_slope_ep_crbv[risk_ref_data$sex==1]
slope_females_crbv = risk_ref_data$cal2_m1_slope_ep_crbv[risk_ref_data$sex==2]
##
int_males_chdmi = risk_ref_data$cal2_m1_cons_ep_chdmi[risk_ref_data$sex==1]
int_females_chdmi  = risk_ref_data$cal2_m1_cons_ep_chdmi[risk_ref_data$sex==2]
slope_males_chdmi  = risk_ref_data$cal2_m1_slope_ep_chdmi[risk_ref_data$sex==1]
slope_females_chdmi  = risk_ref_data$cal2_m1_slope_ep_chdmi[risk_ref_data$sex==2]

