fact_sheet_matrix = indicator_matrix %>% dplyr::filter(!is.na(factsheet_desc)) 
if(nrow(fact_sheet_matrix)>0)
{
sect = NULL

factsheet_section_fn = function(sect = unique(fact_sheet_matrix$section)[8])
{
  data = analysis_data
  
  section_matrix = fact_sheet_matrix %>% dplyr::filter(section == sect)%>% arrange(factsheet_desc)
  ##
  wt_step = unique(section_matrix$weight_step)[1]
  numeric_step = as.numeric(str_extract(wt_step, "\\d+"))
  data = data %>% dplyr::filter(!is.na(get(wt_step)))
  svy_data = svydesign(id=~psu, weights=~get(wt_step),strata=~stratum, data=data,nest = T)
  section_title = c(paste0('Step ',numeric_step,' ',unique(section_matrix$section)),'','','')
  
  # 
  if(sect=='Summary of Combined Risk Factors'|sect=='Cardiovascular disease risk')
  {
    section_title = c(unique(section_matrix$section),'','','')
  }
  
  section_results = NULL
  ind = NULL
  
  for (ind in 1:nrow(section_matrix))
  {
    sub_matrix = section_matrix[ind,]
    
    ind_position = as.numeric(do.call('c',strsplit(sub_matrix$ind_position, "[;]")))
    subset_indicators = do.call('c',strsplit(sub_matrix$indicator_var, "[;]"))[ind_position]
    type_indicators = do.call('c',strsplit(sub_matrix$type, "[;]"))[ind_position]
    denom_logic = do.call('c',strsplit(sub_matrix$pop_subset, "[;]"))[ind_position]
    ind_desc = do.call('c',strsplit(sub_matrix$fsctsheet_desc, "[;]"))
    
    ind_level = NULL
    sub_section_results = NULL
    
    for(ind_level in subset_indicators)
    {
      denom_condition = denom_logic[grep(ind_level,subset_indicators)]
      full_ind_desc = ind_desc[grep(ind_level,subset_indicators)]
      ##
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
      degrees_freedom = degf(svy_datum)-1
      
      if(type_indicators[grep(ind_level,subset_indicators)] == 'mean'|type_indicators[grep(ind_level,subset_indicators)] == 'median')
      {
        data[,ind_level] = as.numeric(data[,ind_level])
        #
        eval(parse(text = paste0('formula = ~', ind_level)))
        n_participants = datum %>% group_by(sex,.drop=FALSE) %>% reframe(participants=n())%>%
          pivot_wider(names_from = c(sex), values_from = participants) %>%
          mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)), Total=Men+Women)
        ##
        men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svymean, method = "lo", df = degrees_freedom, vartype = 'ci')%>%
          mutate(ci_l = ifelse(ci_l<0,0,ci_l))
        total_est_ciprop = svymean(formula, design = svy_datum, method = "lo", df = degrees_freedom) 
        conf_interval = confint(total_est_ciprop) 
        conf_interval[conf_interval<0] = 0
        ###
        ####Computation of median and IQR
        median_compute = unique(type_indicators)=='median'
        if(median_compute==TRUE)
        {
          men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svyquantile, quantiles = c(.5,.25,.75), method = "lo", df = degrees_freedom)[,1:4]
          median_total_est_ciprop = svyquantile(formula, design = svy_datum, method = "lo", quantiles = c(.5,.25,.75), df = degrees_freedom, ci=FALSE) 
          total_est_ciprop = as.vector(unlist(median_total_est_ciprop)[1])
          conf_interval = as.vector(unlist(median_total_est_ciprop)[2:3])
        }
        ###Combining estimates
        total_est = paste0(formatC(round(as.vector(total_est_ciprop),1),format = "f", digits = 1),'\n(',
                           formatC(round(as.numeric(conf_interval[1]),1),format = "f", digits = 1), ' - ',
                           formatC(round(as.numeric(conf_interval[2]),1),format = "f", digits = 1),')')
        
        #####
        #######
        delim_char = '\n('
        mult_n = 1
        
      } else if(type_indicators[grep(ind_level,subset_indicators)] == 'categorical'){
        ##
        eval(parse(text=paste0('formula = ~I(',ind_level, '=="',1,'")')))
        
        n_participants = datum %>% group_by(sex, .drop = FALSE) %>% reframe(participants=n())%>%
          pivot_wider(names_from = c(sex), values_from = participants) %>%
          mutate(across(contains(c("Men", "Women")), ~coalesce(., 0)), Total=Men+Women)
        
        ###
        men_women_est_ciprop = svyby(formula, by = ~sex, design = svy_datum, FUN = svyciprop, method = "lo", df = degrees_freedom, vartype = 'ci')
        total_est_ciprop = svyciprop(formula, design = svy_datum, method = "lo", df = degrees_freedom) 
        ##
        total_est = paste0(formatC(round(100*as.vector(total_est_ciprop),1),format = "f", digits = 1),'%\n(',
                           formatC(round(100*as.numeric(attr(total_est_ciprop, "ci")[1]),1),format = "f", digits = 1), ' - ',
                           formatC(round(100*as.numeric(attr(total_est_ciprop, "ci")[2]),1),format = "f", digits = 1),')')
        ##
        delim_char = '%\n('
        mult_n = 100
      }
      ########Combining estimates
      males_est = paste0(formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',2]),1),format = "f", digits = 1),delim_char,
                         formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',3]),1),format = "f", digits = 1), ' - ',
                         formatC(round(mult_n*as.numeric(men_women_est_ciprop['Men',4]),1),format = "f", digits = 1),')')
      
      females_est = paste0(formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',2]),1),format = "f", digits = 1),delim_char,
                           formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',3]),1),format = "f", digits = 1), ' - ',
                           formatC(round(mult_n*as.numeric(men_women_est_ciprop['Women',4]),1),format = "f", digits = 1),')')
      
      combined_results = c(full_ind_desc, total_est,males_est,females_est)
      sub_section_results = rbind(sub_section_results,combined_results)    
    }
    section_results = rbind(section_results,sub_section_results) 
    
  }
  section_results = rbind(section_title,section_results[order(section_results[,1]),])
  return(section_results )  
}


###Factsheet table for all the indicators
factsheet_table = do.call('rbind',lapply(unique(fact_sheet_matrix$section),factsheet_section_fn))
colnames(factsheet_table)=c('Results for adults aged 18-69 years (incl. 95% CI)','Both Sexes',
                            'Males','Females')

factsheet_table = as.matrix(factsheet_table)
factsheet_table[,1] = gsub("^\\d+\\.\\s*", "", factsheet_table[,1])


factsheet_table = factsheet_table %>% as.data.frame()%>% mutate(row_numbers = 1:n())
rownames(factsheet_table)=NULL
extract_rows = (factsheet_table %>% dplyr::filter(Females=='') %>% dplyr::select(row_numbers))$row_numbers
###
factsheet_table = factsheet_table %>% dplyr::select(-row_numbers)
###
flex_fact_sheet = factsheet_table%>% flextable() %>% autofit() %>%
                  flextable::style(pr_t=fp_text(font.size=10,font.family='Source Sans Pro'), part = 'all')%>%
                  bold(i = c(1,extract_rows))%>%
                  bg(bg="white",i=1,part="header")%>%  
                  theme_box()%>% 
                  align(align = "center", j = 2:4, part = "all") %>%
                  fontsize(size = 9 ,part = "all")%>%
                  merge_h_range(i=extract_rows, j1=1,j2=4)%>%
                  width(j = 2:4, 4.3, unit = "in")%>% 
                  bg(bg="#339966",i=1,part="header")%>%
                  bg(bg="#CCFFFF",i=extract_rows,part="body")%>%
                  color(color = "white", part = 'header')%>%
                  padding(padding = 0, part = "all") %>%
                  paginate()


####
####
doc = officer::read_docx(paste0(getwd(),'/templates/factsheet_template.docx'))
doc <- doc %>%
  body_replace_all_text(old_value = "survey_year", new_value = as.character(survey_year), only_at_cursor = FALSE) %>%
  body_replace_all_text(old_value = "country_name", new_value = as.character(country), only_at_cursor = FALSE)

doc=doc %>% cursor_bookmark(id  = "bmk1") %>%
  body_add_flextable(width(flex_fact_sheet, width = dim(flex_fact_sheet)$widths*7.25/(flextable_dim(flex_fact_sheet)$widths)), pos = "on", align = 'left')

# print(doc,target=paste0(getwd(),'/outputs/Factsheet.docx')) 
print(doc, target = paste0('outputs/', country_ISO, '-', survey_year, '_Factsheet_', format(Sys.time(), "%d-%b-%y_%H-%M-%S"), '.docx'))

}else{}
