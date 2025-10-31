###########Function to generate numbers for reporting
##Generate stratifier
analysis_data = analysis_data %>%
                mutate(sex_age = case_when(sex == 'Men' & (agerange=='18-29'|agerange=='30-44') ~ 1,
                                           sex == 'Men' & (agerange=='45-59'|agerange=='60-69') ~ 2,
                                           sex == 'Women' & (agerange=='18-29'|agerange=='30-44') ~ 3,
                                           sex == 'Women' & (agerange=='45-59'|agerange=='60-69') ~ 4),
                       sex_age = factor(sex_age, levels = 1:4, 
                                                 labels = c('Men 18 - 44','Men 45 - 69','Women 18 - 44','Women 45 - 69')),
                       bin_age = case_when(agerange=='18-29'|agerange=='30-44' ~ 1,
                                           agerange=='45-59'|agerange=='60-69' ~ 2),
                       bin_age = factor(bin_age,levels = 1:2, labels = c('18-44','45-69')))

###
all_stratifiers = c(col_strat_variable,row_strat_variables,'sex_age','bin_age')

#
gen_numbers_fn = function(sect = unique(reporting_matrix$section)[2])
{
  data = analysis_data
  
  section_matrix = reporting_matrix %>% dplyr::filter(section == sect)
  ##
  wt_step = unique(section_matrix$weight_step)[1]
  numeric_step = as.numeric(str_extract(wt_step, "\\d+"))
  data = data %>% dplyr::filter(!is.na(get(wt_step)))
  svy_data = svydesign(id=~psu, weights=~get(wt_step),strata=~stratum, data=data,nest = T)
  
  #### 
  section_results = NULL
  ind = NULL
  
  for (ind in 1:nrow(section_matrix))
  {
    sub_matrix = section_matrix[ind,]
    
    #
    subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))
    type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))
    denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))
    #
    
    ind_level = NULL
    sub_section_results = NULL
    
    for(ind_level in subset_indicators)
    {
      denom_condition = denom_logic[grep(ind_level,subset_indicators)]
      #
      if(sect=="Cardiovascular disease risk")
      {
        data = data %>%mutate(agerange = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
                              agerange = factor(agerange,levels=1:2, labels=c('40-54','55-69')))
        
        svy_data = svydesign(id=~psu, weights=~wstep3,strata=~stratum, data=data,nest = T)
        
      } else if(sect=="Summary of Combined Risk Factors")
      {
        data = data %>%mutate(agerange = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
                              agerange = factor(agerange,levels=1:2, labels=c('18-44','45-69')))
        svy_data = svydesign(id=~psu, weights=~wstep2,strata=~stratum, data=data,nest = T)
        
      }else{data = data}
      #
      if (denom_condition == 'all') {
        datum = data %>% filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange))
        svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange))
      } else {
        datum = data %>% dplyr::filter(!is.na(eval(parse(text = ind_level))) & !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))))
        svy_datum = subset(svy_data, !is.na(eval(parse(text = ind_level))) & !is.na(agerange) & eval(parse(text = paste0('(',denom_condition,')'))))
      }
      
      #####
      degrees_freedom = degf(svy_data)-1
      
      if(type_indicators[grep(ind_level,subset_indicators)] == 'mean'|type_indicators[grep(ind_level,subset_indicators)] == 'median')
      {
        data[,ind_level] = as.numeric(data[,ind_level])
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
          eval(parse(text = paste0('results_',all_stratifiers,' = svyby(formula, by = ~',all_stratifiers,',design = svy_datum, FUN = svyquantile, 
                                 method = "lo", df = degrees_freedom,keep.var=FALSE, quantiles = .5)[,1:2]')))
          ##
          eval(parse(text = paste0('colnames(results_',all_stratifiers,') =c("col1","col2")')))
          ##
          eval(parse(text = paste0('comb_rslts = ','rbind(',paste0('results_',all_stratifiers, collapse = ","),')')))
          ###
          total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", quantiles = 0.5, df = degrees_freedom)[[1]][1]
          
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
        eval(parse(text = paste0('results_',all_stratifiers,' = svyby(formula, by = ~',all_stratifiers,',design = svy_datum, FUN = svyciprop, 
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
    }
    section_results = rbind(section_results,sub_section_results) 
    
  }
  return(section_results)  
}


# Generate a table for all indicators by applying `gen_numbers` to each unique section
indicator_results = do.call('rbind',lapply(unique(reporting_matrix$section),gen_numbers_fn))%>%
                    mutate(col2 = as.numeric(col2))

##
colnames(indicator_results)=c("ind_level","var_strat_level","col2")

#####################Exporting the numbers to Excel template with charts###########################
# Path to workbook
file = 'excel_templates/template_data_fromR.xlsx'
save_file = 'excel_templates/data_fromR.xlsx'

# Load the existing workbook
wb = loadWorkbook(file)
existing = read.xlsx(file, 'data', skipEmptyRows = FALSE)
transl_lang = read.xlsx(file, 'languages', skipEmptyRows = FALSE)[,language]
# Updating only col2 where keys match
updated = existing %>% 
  left_join(indicator_results, by = c("ind_level", "var_strat_level"), suffix = c("", "_new")) %>%
  mutate(col2 = ifelse(!is.na(col2_new), col2_new, col2)) %>% dplyr::select(-c(col2_new))

# New data for col2
new_values = updated$col2

# Write numbers into col2
writeData(wb, sheet = "data", x = new_values, 
          startCol = 4, startRow = 2, colNames = FALSE, rowNames = FALSE)
#Write translated text into the Excel
writeData(wb, sheet = "data", x = transl_lang, 
          startCol = 3, startRow = 2, colNames = FALSE, rowNames = FALSE)
# Save back without disturbing formulas
saveWorkbook(wb, save_file, overwrite = TRUE)
