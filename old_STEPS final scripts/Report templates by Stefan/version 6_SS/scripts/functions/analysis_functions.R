#####
demog_numeric = function(strat_variable = 'agerange')
{
  summary_table = datum %>%
    group_by(eval(parse(text=strat_variable)), sex,.drop = FALSE) %>%
    summarise(count = n(), mean_var = round(mean(eval(parse(text = k)), na.rm = TRUE),1)) %>%
    pivot_wider(names_from = sex, values_from = c(count, mean_var)) %>%
    mutate(
      across(contains(c("Men", "Women")), ~coalesce(., 0)),
      Total_Count = rowSums(across(contains(c("count_Men", "count_Women")), as.numeric), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(
      datum %>%
        group_by(eval(parse(text=strat_variable))) %>%
        summarise(Total_mean = round(mean(eval(parse(text = k)), na.rm = TRUE),1))
    ) %>% full_join(
      datum %>% 
        group_by(sex) %>%
        summarise(count = n(), mean_var = round(mean(eval(parse(text = k)), na.rm = TRUE),1)) %>%
        pivot_wider(names_from = sex, values_from = c(count, mean_var)) %>%
        mutate(agerange = 'Total') %>%
        add_column(datum %>% summarise(Total_Count = n(), Total_mean=round(mean(eval(parse(text = k)), na.rm=T),1))))
  
  # Selecting columns for the final summary table
  names_to_select = c("eval(parse(text = strat_variable))",'count_Men','mean_var_Men','count_Women','mean_var_Women','Total_Count', 'Total_mean')
  # Selecting columns based on 'names_to_select'
  summary_table = summary_table %>% dplyr::select(all_of(names_to_select))  
  ###Editing summary_table
  summary_table = summary_table %>% dplyr::mutate(
    mean_var_Men = formatC(round(mean_var_Men,1),format = "f", digits = 1),
    mean_var_Women = formatC(round(mean_var_Women,1),format = "f", digits = 1),
    Total_mean = formatC(round(Total_mean,1),format = "f", digits = 1),
    ####
    mean_var_Men = replace(mean_var_Men,count_Men<denom_limit,'-'),
    mean_var_Women = replace(mean_var_Women,count_Women<denom_limit,'-'),
    Total_mean = replace(Total_mean,Total_Count<denom_limit,'-')
  )
  ####
  if(strat_variable != 'agerange')
  {
    strat_position = grep(strat_variable,row_strat_variables)
    ###
    summary_table = summary_table %>% dplyr::filter(!is.na(`eval(parse(text = strat_variable))`)) ###Excluding the totals row
    summary_table = rbind(c(row_strat_variable_titles[strat_position], rep('',ncol(summary_table)-1)),summary_table %>% as.matrix())%>%as.data.frame()
  } else{}
  
  return(summary_table)
}
#####
demog_cat = function (strat_variable = 'agerange')
{
  count_table = datum %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(agerange))%>%
    group_by(eval(parse(text=strat_variable)), eval(parse(text=k)), sex, .drop = FALSE) %>%
    summarise(n = n()) %>%
    rename(variable = `eval(parse(text = k))`) %>%
    pivot_wider(names_from = c(sex, variable), values_from = n) %>%
    mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)))
  ##Adding total columns 
  if(sub_matrix$has_log_exp==TRUE)
  {
    unique_levels = c(0,1)
  } else{unique_levels = sort(unique(data[,k]))}
  
  
  
  eval(parse(text=paste0('count_table$Men_',unique_levels,'[is.null(count_table$Men_',unique_levels,')]=0', sep='\n')))
  eval(parse(text=paste0('count_table$Women_',unique_levels,'[is.null(count_table$Women_',unique_levels,')]=0', sep='\n')))
  #
  eval(parse(text=paste0('count_table$sex_Total_',
                         unique_levels,'= count_table$Men_',
                         unique_levels,'+ count_table$Women_',unique_levels)))
  
  #
  count_table = count_table %>% 
    mutate( 
      #Total
      Total_Count = rowSums(across(contains(c("Men", "Women")), as.numeric), na.rm = TRUE),
      men_Total_Count = rowSums(across(contains(c("Men"),ignore.case = F), as.numeric), na.rm = TRUE),
      women_Total_Count = rowSums(across(contains(c("Women"),ignore.case = F), as.numeric), na.rm = TRUE),
      #Percentages
      across(contains(c("Men"), ignore.case = F), list(Percentage = ~scales::percent(. / men_Total_Count,accuracy = 0.1, suffix = ''))),
      across(contains(c("Women"), ignore.case = F), list(Percentage = ~scales::percent(. / women_Total_Count,accuracy = 0.1, suffix = ''))),
      across(contains(c("sex"), ignore.case = F), list(Percentage = ~scales::percent(. / Total_Count,accuracy = 0.1, suffix = ''))))
  
  #####
  if(sub_matrix$has_log_exp==TRUE)
  {
    names_to_excl = grep('_0', names(count_table), v=T)
    count_table = count_table %>% dplyr::select(-all_of(names_to_excl))
  } else{}
  
  ###########
  ####
  if(strat_variable != 'agerange' & strat_variable!='')
  {
    strat_position = grep(strat_variable,row_strat_variables)
    ###
    count_table = count_table %>% dplyr::filter(!is.na(`eval(parse(text = strat_variable))`)) ###Excluding the any row with NAs
    count_table = rbind(c(row_strat_variable_titles[strat_position], rep('',ncol(count_table)-1)),count_table %>% as.matrix())%>%as.data.frame()
  } else{count_table = count_table %>% as.matrix()%>%as.data.frame()}
  
  
  return(count_table)
}

analysis_numeric_fn = function(row_strat = 'agerange', col_strat = 'sex')
{
  #
if (denom_condition == 'all') {
  datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
  datum[,k] = as.numeric(datum[,k])
  
  svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
  eval(parse(text = paste0('svy_datum$variables$',k,' = as.numeric(as.character(svy_datum$variables$',k,'))')))
  svy_datum = subset(svy_datum, !is.na(eval(parse(text = k))))
  ###
  if(nrow(datum)==0)
  {
    svy_datum = svy_data
    svy_datum$variables[,k]=0
  }
  
} else {
  datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
  datum[,k] = as.numeric(datum[,k])
  svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
  ##
  eval(parse(text = paste0('svy_datum$variables$',k,' = as.numeric(as.character(svy_datum$variables$',k,'))')))
  svy_datum = subset(svy_datum, !is.na(eval(parse(text = k))))
  ###
  if(nrow(datum)==0)
  {
    svy_datum = svy_data
    svy_datum$variables[,k]=0
  }
}
  
# 
degrees_freedom = ifelse(degf(svy_datum)==0,1,degf(svy_datum)-1)
#
col_strat_var_levels = names(table(data[,col_strat]))

##Unweighted participant numbers
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
#
colnames(n_participants) = c(paste0('0',row_strat),paste0(colnames(n_participants)[-1],'_a'))
##Weighted participant %s
est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+ eval(parse(text=row_strat)), design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
            full_join(svyby(formula, by = ~eval(parse(text=row_strat)), design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T) %>%
                        mutate(`eval(parse(text = col_strat))` = 'Total'))%>%mutate(ci_l = ifelse(ci_l<0,0,ci_l))
##Totals
colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat)), design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
                       mutate(ci_l = ifelse(ci_l<0,0,ci_l))
total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", df = degrees_freedom, na.rm = T) 
conf_interval = confint(total_est_ciprop, df = degrees_freedom, na.rm=T) 
conf_interval[conf_interval<0] = 0

total_est_ciprop = cbind(as.vector(total_est_ciprop),ci_l = as.numeric(conf_interval[1]),ci_u=as.numeric(conf_interval[2]))%>%
                   as.data.frame() %>% add_column(`eval(parse(text = col_strat))` = 'Total')
colnames(total_est_ciprop)[1] = colnames(colstrat_var_est_ciprop)[2]
##Combined est_ciprop
combined_est_ciprop = est_ciprop %>% mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>% 
                        full_join(colstrat_var_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) %>% 
                        full_join(total_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) 

###This section handles population level estimates::To recheck after revisions
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

####Computation of median and IQR
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

##Further formatting
computed_results = n_participants %>% mutate_all(as.character) %>% 
                   left_join(point_estimate %>% mutate_all(as.character)) %>% 
                   left_join(ci_estimate %>% mutate_all(as.character))
summary_table = computed_results %>% dplyr::select(sort(colnames(computed_results)))
##
###
names_diff = setdiff(c(paste0(col_strat_var_levels,'_',c('a')),paste0(col_strat_var_levels,'_',c('b')),
                       paste0(col_strat_var_levels,'_',c('ci')),'Total_a','Total_b','Total_ci'), names(summary_table))
if(length(names_diff)>0)
{
  eval(parse(text=paste0('summary_table$`',names_diff,'`=NA')))
}
####
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
analysis_categorical_fn = function(row_strat = 'agerange', col_strat = 'sex')
{
  data[,row_strat] = factor(data[,row_strat], levels = names(table(data[,row_strat])), labels = names(table(data[,row_strat])))
  if(i=="Cardiovascular disease risk")
  {
    data = data %>%mutate(agerange = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
                          agerange = factor(agerange,levels=1:2, labels=c('40-54','55-69')))
    svy_data = svydesign(id=~psu, weights=~wstep3,strata=~stratum, data=data,nest = T)
    
  } else if(i=="Summary of Combined Risk Factors")
  {
    data = data %>%mutate(agerange = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
                          agerange = factor(agerange,levels=1:2, labels=c('18-44','45-69')))
    svy_data = svydesign(id=~psu, weights=~wstep2,strata=~stratum, data=data,nest = T)
    
  }else{data = data}
  #####
  if (denom_condition == 'all') {
    datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
    svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))))
    ###
    if(nrow(datum)==0)
    {
      svy_datum = svy_data
      svy_datum$variables[,k]=0
    }
    
  } else {
    datum = data %>% dplyr::filter(!is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
    svy_datum = subset(svy_data, !is.na(eval(parse(text = k))) & !is.na(eval(parse(text=row_strat))) & eval(parse(text = paste0('(',denom_condition,')'))))
    ###
    if(nrow(datum)==0)
    {
      svy_datum = svy_data
      svy_datum$variables[,k]=0
    }
  }
  
  #degrees_freedom = degf(svy_datum)-1
  degrees_freedom = ifelse(degf(svy_datum)==0,1,degf(svy_datum)-1)
  
  ####
  col_strat_var_levels = names(table(data[,col_strat]))
  ##Unweighted participant numbers
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
        mutate(`eval(parse(text = row_strat))`='Total',across(contains(col_strat_var_levels), ~coalesce(., 0)), 
               Total=eval(parse(text = paste0("`",col_strat_var_levels,"`", collapse = '+')))))%>% 
    dplyr::filter(!is.na(`eval(parse(text = row_strat))`))
  #
  colnames(n_participants) = c(paste0('0',row_strat),paste0(colnames(n_participants)[-1],'_a'))
  ##Weighted participant %s
  est_ciprop = svyby(formula, by = ~eval(parse(text=col_strat))+eval(parse(text=row_strat)), design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
    full_join(svyby(formula, by = ~eval(parse(text=row_strat)), design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)%>%
                mutate(`eval(parse(text = col_strat))` = 'Total'))#%>%
    #mutate(sex =ifelse(is.na(sex),'Total',sex))
  ##Totals
  colstrat_var_est_ciprop = svyby(formula, by = ~eval(parse(text = col_strat)), design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci', na.rm.all = T)#%>%
  #mutate(sex = gsub('\\s','',paste(sex,'_Total')))
  total_est_ciprop = svyciprop(formula, design = svy_datum, method = "lo", df = degrees_freedom, na.rm=T) 
  
  total_est_ciprop = cbind(as.vector(total_est_ciprop),ci_l = as.numeric(attr(total_est_ciprop, "ci")[1]),ci_u=as.numeric(attr(total_est_ciprop, "ci")[2]))%>%
    as.data.frame() %>% add_column(`eval(parse(text = col_strat))` = 'Total')
  colnames(total_est_ciprop)[1] = colnames(colstrat_var_est_ciprop)[2]
  ##Combined est_ciprop
   combined_est_ciprop = est_ciprop %>% mutate(`eval(parse(text = row_strat))` = as.character(`eval(parse(text = row_strat))`)) %>% 
    full_join(colstrat_var_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) %>% 
    full_join(total_est_ciprop %>% mutate(`eval(parse(text = row_strat))`='Total')) #%>%
  #
  combined_est_ciprop[,3:5] = round(100*combined_est_ciprop[,3:5],1)
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
####
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

####
#####
#####
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
risk_ref_data = read_dta('data_input/risk_ref_data.dta') %>%dplyr::filter(ccode==country_ISO & year == ref_year) %>%
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

