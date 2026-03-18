############################################################
## SCRIPT FOR GENERATING FACTSHEET BASED ON A SURVEY ROUND
############################################################

###-----------------------------------------------------------
### Generate factsheet table for all sections
###-----------------------------------------------------------
# Check that the factsheet configuration matrix contains rows
if(nrow(fact_sheet_matrix)>0)
{
  factsheet_table = do.call('rbind',
                            lapply(unique(fact_sheet_matrix$section),
                                   factsheet_section_fn))

# Set column names
  colnames(factsheet_table)=c(other_language[11,language],other_language[3,language],
                              other_language[1,language],other_language[2,language])

# Convert to matrix
  factsheet_table = as.matrix(factsheet_table)

# Remove numeric prefixes from indicator descriptions
  factsheet_table[,1] = gsub("^\\d+\\.\\s*", "", factsheet_table[,1])

# Convert to dataframe and create row index
  factsheet_table = factsheet_table %>% 
                    as.data.frame()%>% mutate(row_numbers = 1:n())
  rownames(factsheet_table)=NULL

# Identify section header rows
  extract_rows = (factsheet_table %>% 
                    dplyr::filter(get(colnames(factsheet_table)[4])=='') %>% 
                    dplyr::select(row_numbers))$row_numbers

###
  factsheet_table = factsheet_table %>% 
                    dplyr::select(-row_numbers)

###
#-----------------------------------------------------------
# Create formatted flextable for Word output
#-----------------------------------------------------------
flex_fact_sheet = factsheet_table%>% 
                  flextable() %>% autofit() %>%
  flextable::style(pr_t=fp_text(font.size=10,font.family='Source Sans Pro'), part = 'all')%>%
  bold(i = c(1,extract_rows))%>%
  bg(bg="white",i=1,part="header")%>%  
  theme_box()%>% 
  align(align = "center", j = 2:4, part = "all") %>%
  fontsize(size = 9 ,part = "all")%>%
  merge_h_range(i=extract_rows, j1=1,j2=4)%>%
  width(j = 2:4, 4.3, unit = "in")%>% 
  bg(bg="#339966",i=1,part="header")%>%
  bg(bg="#C9DDF3",i=extract_rows,part="body")%>%
  color(color = "white", part = 'header')%>%
  padding(padding = 0, part = "all") %>%
  paginate()

#-----------------------------------------------------------
# Insert the factsheet table into a Word template
#-----------------------------------------------------------
doc = officer::read_docx(paste0(getwd(),'/templates/factsheet_template.docx'))

# Replace template placeholders
doc <- doc %>%
  body_replace_all_text(old_value = "survey_year", new_value = as.character(survey_year), only_at_cursor = FALSE) %>%
  body_replace_all_text(old_value = "country_name", new_value = as.character(country), only_at_cursor = FALSE)

# Insert table at bookmark location
doc=doc %>% cursor_bookmark(id  = "bmk1") %>%
  body_add_flextable(width(flex_fact_sheet, width = dim(flex_fact_sheet)$widths*7.25/(flextable_dim(flex_fact_sheet)$widths)), pos = "on", align = 'left')

# Save Word document
print(doc, target = paste0('outputs/', country_ISO, '-', survey_year, '_Factsheet_', format(Sys.time(), "%d-%b-%y_%H-%M-%S"), '.docx'))

}else{}

