##Reading in data
##files in data input folder
#input_files = list.files('data input')
#data = read.xlsx('data input/matrix.xlsx','data')# Reads data from an Excel file located at 'data input/data.xlsx' into a data frame called 'data'.
#data = read.xlsx(paste0('data_input/',country_ISO,'_data.xlsx'))# Reads data from an Excel file located at 'data_input/data.xlsx' into a data frame called 'data'.

raw_dataset2 = read.xlsx(paste0('data_input/',country_ISO,'_data_for_comparison.xlsx'))

# randomised_dataset <- as.data.frame(
#   lapply(analysis_data, function(x) {
#     # resample each column independently with replacement
#     sample(x, size = length(x), replace = TRUE)
#   })
# )

colnames(raw_dataset2) =tolower(colnames(raw_dataset2))# Converts all column names in the 'data' data frame to lowercase to ensure uniformity in column name handling.
#######Generating minimum and maximum age

# Check if 'valid' column exists in the dataset
if ("valid" %in% names(raw_dataset2)) {
  raw_dataset2 = raw_dataset2 %>% 
                 dplyr::filter(valid == 1) %>% 
                 mutate(minage = min(age, na.rm = T),maxage = max(age, na.rm = T))
} else {
  raw_dataset2 = raw_dataset2 %>% 
                 mutate(valid = ifelse(!is.na(sex) & !is.na(age),1,0),
                        minage = min(age, na.rm = T),maxage = max(age, na.rm = T))%>%
                 dplyr::filter(valid == 1)
}

####Variable groups
var_groups =c(
  'c10a','c10b','c10c','c10d',
  't4a','t4b','t4c',
  't5a','t5b','t5c','t5d','t5e','t5other',
  't5aw','t5bw','t5cw','t5dw','t5ew','t5otherw',
  't14a','t14b','t14c','t14d','t14e','t14other','t14aw','t14bw','t14cw','t14dw','t14ew','t14otherw',
  'a10a','a10b','a10c','a10d','a10e','a10f','a10g',
  't11a','t11b','t11c',
  'a12a','a12b','a12c','a12d','a12e')

# Defines a vector 'var_groups' containing a list of variable names or codes grouped for later reference.

##### Examining the existence of var_groups in data if none added to the dataset to avoid script breakdown
####### Adding b5 and b8 in case not captured for smooth execution of the script
var_intersect = intersect(c(var_groups,'b5','b8'), names(raw_dataset2))
# Finds the intersection between the variables in 'var_groups' and the additional variables 'b5' and 'b8' with the column names of the 'data' data frame. This checks which of the specified variables are actually present in the dataset.
##Extracting none existing variables
none_exist_var = setdiff(c(var_groups,'b5','b8'),var_intersect)
# Identifies which variables from 'var_groups' and 'b5', 'b8' are not present in the dataset by taking the set difference between the total list and the intersected list.

###Adding these to the data with NAs initialised
if(length(none_exist_var)>0){eval(parse(text = paste0('raw_dataset2$',none_exist_var,' = NA', sep = '\n')))}
# Dynamically adds columns for the variables that do not exist in 'data' and initializes them with NA values. 'eval(parse(text = ...))' is used to construct and execute R code as a string.

####Calling functions to be used for analysis
# Sources the R script 'analysis_functions.R' from the 'scripts/functions' directory, making any functions defined in that script available for use.

###Reading xml file for cleaning of out of range values together with checking of skip logic
# xml_file = read_excel('data input/xml_file.xlsx','survey') %>% 
#   dplyr::filter(!(is.na(constraint) & is.na(relevant))) 
xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') %>% 
           dplyr::filter(!(is.na(constraint) & is.na(relevant))) 
# Reads the 'survey' sheet from the 'xml_file.xlsx' Excel file into 'xml_file', and filters out rows where both 'constraint' and 'relevant' columns are NA.

colnames(xml_file)=tolower(colnames(xml_file))
xml_file = xml_file%>%dplyr::select(all_of(c('type','name','constraint','relevant')))
# Selects only the columns 'type', 'name', 'constraint', and 'relevant' from 'xml_file'.


###row number to start from:
step_rows = which(grepl('step', xml_file$name)) 
# Identifies the row indices where the 'name' column contains the word 'step' using regular expression matching.

#
reduced_xml = xml_file[step_rows[1]:nrow(xml_file),]
# Subsets 'xml_file' from the first row that contains 'step' to the end of the data frame, assigning this subset to 'reduced_xml'.

#further cleaning

eval(parse(text = paste0(c('eff_step_rows','type_rows','note_rows','group_rows','one_quin_rows'),"= which(grepl('",
                         c('step','type','note','group','select_one quin'),"',reduced_xml$",c('name','name','type','type','type'),'))', sep='\n')))
# Dynamically creates variables ('eff_step_rows', 'type_rows', etc.) to store row indices based on different patterns found in 'reduced_xml$name' and 'reduced_xml$type'.
##Further filtering of the xml file
reduced_xml = reduced_xml[-sort(unique(c(eff_step_rows,type_rows,note_rows,group_rows,one_quin_rows))),] %>%
  mutate(name = tolower(name), constraint = tolower(constraint), relevant = tolower(relevant))
# Removes rows from 'reduced_xml' that correspond to the indices of various patterns. Converts the 'name', 'constraint', and 'relevant' columns to lowercase.

search_vars = function(nonexist_vars = 'selected', logic_denom) {
  pattern = paste(tolower(nonexist_vars), collapse="|") # Creates a regex pattern from 'nonexist_vars' to match any of the variable names.
  found = grepl(pattern, logic_denom) # Checks if any of the variables in 'nonexist_vars' are present in 'logic_denom' using the regex pattern.
  return(found) # Returns TRUE if a match is found, otherwise FALSE.
}


####Comparing names in reduced_xml versus the dataset
vars_not_indataset = setdiff(reduced_xml$name, names(raw_dataset2))
# Finds variables that are in 'reduced_xml$name' but not present in the columns of 'data' by taking the set difference.

##
reduced_xml = reduced_xml%>%dplyr::filter(eval(parse(text = paste0('name!="',vars_not_indataset,'"', collapse = '& '))))
# Filters out rows in 'reduced_xml' where the 'name' column matches any of the variables in 'vars_not_indataset'.

######Selecting numeric variables from xls file--NOTE on timestamp
select_numeric_vars = reduced_xml %>% rename_with(tolower) %>% 
                      dplyr::filter(type %in% c("calculate", "integer")) %>% dplyr::pull(name)

##Converting the variables to type numeric
raw_dataset2 = raw_dataset2 %>% mutate(across(all_of(select_numeric_vars),~ as.numeric(as.character(.))))

########Cleaning out of range values:
# outofrange_logic = reduced_xml %>% dplyr::filter(!is.na(constraint)) %>%
#   rowwise %>%
#   dplyr::mutate(constraint = gsub('\\}|\\$','',constraint),
#                 constraint = gsub('\\.',paste0('data$',name),constraint),
#                 constraint = gsub('=','==',constraint),
#                 constraint = gsub('<==','<=',constraint),
#                 constraint = gsub('>==','>=',constraint),
#                 constraint = gsub('\\sand\\s','&',constraint),
#                 constraint = gsub('\\sor\\s','|',constraint),
#                 constraint = gsub('\\{','data$',constraint),gsub('}','data$',constraint),
#                 constraint = gsub('!==','!=',constraint),
#                 constraint = gsub('\\n|\\r','',constraint),
#                 const_logic = paste0('!(',constraint,')'))  %>%
#   mutate(select_fn = search_vars(logic_denom=constraint)) %>% dplyr::filter(select_fn==FALSE)
# # Processes 'constraint' in 'reduced_xml' to replace certain patterns with corresponding R syntax, to create logical expressions. 'const_logic' stores these expressions negated. Filters rows where 'search_vars' function returns FALSE (i.e., no match).
# 
# ##Applying outofrange_logic to the dataset
# eval(parse(text = paste0('raw_dataset2$',outofrange_logic$name,'[',outofrange_logic$const_logic,'] = NA', sep = '\n')))
# Applies the cleaned 'outofrange_logic' to the dataset, setting values to NA where the out-of-range condition is true.
#####NOTE --Need to set 66,666,77,777,88,888,99,999 to NAs 

##vars_with_77_or_88_not part of constraints
#level_xml_file_77_88 = read_excel('data input/xml_file.xlsx','choices') %>% dplyr::filter(!is.na(`list name`) & (name==77 | name==88))
level_xml_file_77_88 = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'choices') %>% dplyr::filter(!is.na(`list name`) & (name==77 | name==88))
# Reads the 'choices' sheet from 'xml_file.xlsx' into 'level_xml_file_77_88', and filters rows where 'list name' is not NA and 'name' is either 77 or 88.
#
colnames(level_xml_file_77_88)=tolower(colnames(level_xml_file_77_88))
level_xml_file_77_88 = level_xml_file_77_88 %>% dplyr::select(all_of(c("list name",'name',paste0("label::",language))))
# Selects 'list name', 'name', and 'label' columns from 'level_xml_file_77_88', with 'label' column name constructed using a variable 'language'.
####
extracted_variables_77_88 = unique(level_xml_file_77_88$`list name`)
# Extracts unique values from the 'list name' column of 'level_xml_file_77_88' into 'extracted_variables_77_88'.

#original_xml_file = read_excel('data input/xml_file.xlsx','survey') %>% dplyr::filter(!is.na(name)) %>% mutate(name = tolower(name))
original_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') %>% dplyr::filter(!is.na(name)) %>% mutate(name = tolower(name))
# Reads the 'survey' sheet from 'xml_file.xlsx' into 'original_xml_file', filters out rows where 'name' is NA, and converts 'name' column to lowercase.

i = NULL
matched_variables_77_88 = data.frame()
for (i in extracted_variables_77_88)
{
  # Iterates over each variable in 'extracted_variables_77_88'.
  #guide_var_77_88 = unique(grep(paste0('^select_one\\s',i,'$'), original_xml_file$type, v=T))
  guide_var_77_88 = unique(grep(paste0('^select_one\\s',i,'$|^select_multiple\\s',i,'$'), original_xml_file$type, v=T))
  
  # Searches for 'select_one' variables in 'original_xml_file$type' that match the current variable pattern.
  if (length(guide_var_77_88)>0)
  {
    # If matching 'select_one' variable is found:
    match_var_77_88 = (original_xml_file %>% dplyr::filter(type == guide_var_77_88) %>% dplyr::select(name))$name
    # Filters 'original_xml_file' for the type found, selects 'name' column.
    matched_variables_77_88 = rbind(matched_variables_77_88, cbind(i,match_var_77_88))
    # Appends the matched variables to 'matched_variables_77_88' data frame.
  } else{}
  # If no matches are found, do nothing.
}

pre_vars_with_77_or_88 = setdiff(matched_variables_77_88$match_var_77_88,vars_exempt_77_88)
# Determines which variables from 'matched_variables_77_88' are not in 'vars_exempt_77_88'.
##
vars_with_77_or_88 = intersect(pre_vars_with_77_or_88, names(raw_dataset2))
# Finds the intersection between 'pre_vars_with_77_or_88' and the column names of 'data'.
##
if(length(vars_with_77_or_88)>0)
{
  # Checks if there are variables to process.
  eval(parse(text=paste0('raw_dataset2$',vars_with_77_or_88,'[raw_dataset2$',vars_with_77_or_88,'==77 | raw_dataset2$',vars_with_77_or_88,'==88] = NA', sep='\n')))
  # Sets values to NA in the specified columns where the values are either 77 or 88.
}
##Others in xml 
unique_NA_numbers = c(66,666,6666,77,777,7777,88,888,8888,99,999,9999)
# Defines a vector 'unique_NA_numbers' containing specific numbers to be replaced with NA.
eval(parse(text = paste0('restrict_',unique_NA_numbers,' = grep("',unique_NA_numbers,'",reduced_xml$constraint)', sep='\n')))
# Creates variables (e.g., 'restrict_66', 'restrict_666') to store the indices of constraints containing the unique numbers.
###
i = NULL
for(i in 6:9)
{
  # Iterates over the numbers 6 to 9.
  eval(parse(text =  paste0('restric_',paste0(rep(i,2), collapse = ''),'_vars =  reduced_xml$name[setdiff(restrict_',
                            paste0(rep(i,2), collapse = ''),', c(restrict_',paste0(rep(i,3), collapse = ''),',restrict_',
                            paste0(rep(i,4), collapse = ''),'))]', sep='\n')))
  # Creates variables for constraints with the current number of digits (e.g., 'restric_666_vars') excluding those with more digits.
  eval(parse(text = paste0('restric_',paste0(rep(i,3), collapse = ''),'_vars = reduced_xml$name[setdiff(restrict_',
                           paste0(rep(i,3), collapse = ''),', restrict_',paste0(rep(i,4), collapse = ''),')]', sep='\n')))
  # Similar to above but for constraints with one more digit.
  eval(parse(text = paste0('restric_',paste0(rep(i,4), collapse = ''),'_vars = reduced_xml$name[restrict_',paste0(rep(i,4), collapse = ''),']', sep='\n')))
  # Creates variables for constraints with exactly 'i' digits.
}

###Setting variables with these unique numbers to NA

i = NULL
for(i in paste0('restric_',unique_NA_numbers,'_vars'))
{
  # Iterates over each restriction variable name.
  var_set = get(i)
  # Retrieves the variable set corresponding to the current restriction variable.
  if(length(var_set)>0)
  {
    # If there are variables to process:
    related_no = str_extract_all(i,"\\d+")[[1]][1]
    # Extracts the number from the variable name (e.g., '66' from 'restric_66_vars').
    eval(parse(text = paste0('raw_dataset2$',var_set,'[raw_dataset2$',var_set,' == ',related_no,'] = NA', sep = '\n')))
    # Sets values to NA in the specified columns where the values match the extracted number.
  } else{}
  # If no variables are found, do nothing.
}

#########Generating indicators
####Reading indicator and secondary variable matrix
#from an Excel file located at 'data input/input_matrix.xls' from the sheet named 'indicators'
indicator_matrix_v2 = read_excel(paste0('data_input/',country_ISO,'_input_matrix.xlsx'),'indicators')%>%dplyr::filter(include_in_analysis=='Yes')

# Converting all column names of indicator_matrix to lowercase to ensure consistent naming
colnames(indicator_matrix_v2) = tolower(colnames(indicator_matrix_v2))
# Converting all values in indicator_matrix columns to character type
eval(parse(text = paste0('indicator_matrix_v2$',colnames(indicator_matrix_v2),'= as.character(indicator_matrix_v2$',colnames(indicator_matrix_v2),')', sep='\n')))
# Checking if primary variables specified in the matrix exist in the dataset
indicator_matrix_v2 = indicator_matrix_v2 %>% rowwise %>%# Ensuring operations are applied row by row
  mutate(prim_exist = ifelse(!is.na(tolower(primary_variables)),# Check if primary_variables is not NA after converting to lowercase
                             eval(parse(text = paste0('all(',paste0('all(',paste0('!is.null(raw_dataset2$',do.call('c',strsplit(tolower(primary_variables), "[;]")),')',collapse = ' & '),')'),',',
                                                      paste0('all(',paste0('any(!is.na(raw_dataset2$',do.call('c',strsplit(tolower(primary_variables), "[;]")),'))',collapse = ' | '),')'),')'))), TRUE))%>%
  dplyr::filter(prim_exist==TRUE)# Filtering rows where prim_exist is TRUE

###
# Reading another data matrix from the same Excel file, but from the 'derivedvars' sheet
#dervar_matrix = read_excel('data input/matrix.xlsx',sheet = 'derivedvars')
dervar_matrix_v2 = read_excel(paste0('data_input/',country_ISO,'_input_matrix.xlsx'),sheet = 'derivedvars')
# Converting all column names of dervar_matrix to lowercase for consistency
colnames(dervar_matrix_v2) = tolower(colnames(dervar_matrix_v2))
# Checking if derived variables specified in the matrix exist in the dataset
dervar_matrix_v2 = dervar_matrix_v2 %>% rowwise %>%
  mutate(all_exist = eval(parse(text = paste0('all(',paste0('!is.null(raw_dataset2$',do.call('c',strsplit(as.character(primary_vars), "[;]")),')',collapse = ' & '),')')))) 
# Filtering derived variables where all exist is TRUE
logic_exist_vars_v2 = dervar_matrix_v2 %>% dplyr::filter(all_exist == TRUE)
# Extracting list of derived variables that do not exist
list_nonexist_dervars_v2 = do.call('c',strsplit((dervar_matrix_v2 %>% as.data.frame() %>% 
                                                dplyr::filter(all_exist == FALSE) %>% 
                                                  dplyr::select(derived_vars))$derived_vars, "[;]")) ###Also lines 182 - where these objects are used

####Excluding indicators with no primary variables from the input matrix
# Function to search for non-existent variables in logical expressions
search_vars = function(nonexist_vars = list_nonexist_dervars_v2, logic_denom) {
  pattern = paste(tolower(nonexist_vars), collapse="|") # Creating a regex pattern from list_nonexist_vars
  found = grepl(pattern, logic_denom) # Checking if any of the variables in list_nonexist_vars match either logical expression or logic for denominator
  return(found)
}

###Adding concatinated variable for searching non_existing variables
indicator_matrix_v2 = indicator_matrix_v2 %>% mutate(logic_condition_var = tolower(logic_condition_var), # Convert logic_condition_var to lowercase
                                               pop_subset = tolower(pop_subset),# Convert pop_subset to lowercase
                                               concat_var = paste0(logic_condition_var,' ', pop_subset))# Concatenate logic_condition_var and pop_subset
# 
# # If there are non-existent derived variables, update indicator_matrix to exclude them
if(!is.null(list_nonexist_dervars_v2))
{
  indicator_matrix_v2 = indicator_matrix_v2 %>% mutate(der_varsearch = search_vars(logic_denom = concat_var)) %>%# Search for non-existent derived variables in the concatenated variable
    dplyr::filter(der_varsearch == FALSE)# Filter out rows where der_varsearch is TRUE
}else{indicator_matrix_v2 = indicator_matrix_v2}# No changes if list_nonexist_dervars is NULL


######
# Processing glucose and cholesterol data, converting units if necessary
raw_dataset2 = raw_dataset2 %>% mutate(median_b5 = median(b5, na.rm = T),# Calculate median of b5, ignoring NA values
                       median_b8 = median(b8, na.rm = T)) %>%# Calculate median of b8, ignoring NA values
  rowwise %>% mutate(b5 = ifelse(median_b5>35, b5/18.01, b5), # Convert b5 to mg/dl if median_b5 > 35
                     b8 = ifelse(median_b8>12, b8/38.67, b8)) # Convert b8 to mg/d if median_b8 > 12
# Processing and cleaning indicator_matrix
indicator_matrix_v2 = indicator_matrix_v2 %>%
  separate(indicator, into = c("indicator_var", "indicator_short_desc"), sep = ":", remove = FALSE) %>%  # Separate 'indicator' into 'indicator_var' and 'indicator_short_desc'
  mutate(
    indicator_var = tolower(indicator_var),  # Convert indicator_var to lowercase
    logic_condition_var = tolower(logic_condition_var),  # Convert logic_condition_var to lowercase
    pop_subset = tolower(pop_subset)  # Convert pop_subset to lowercase
  ) %>%
  rowwise() %>%  # Apply transformations row by row
  mutate(
    has_log_exp = grepl('=|>|<', logic_condition_var),  # Check if logic_condition_var contains any logical operators
    # logic_condition_var = gsub('=', '==', logic_condition_var),  # Replace '=' with '==' in logic_condition_var
    # logic_condition_var = gsub('<==', '<=', logic_condition_var),  # Replace '<==' with '<=' in logic_condition_var
    # logic_condition_var = gsub('>==', '>=', logic_condition_var),  # Replace '>==' with '>=' in logic_condition_var
    # logic_condition_var = gsub('!==', '!=', logic_condition_var),  # Replace '!==', '!=' in logic_condition_var
    # pop_subset = gsub('=', '==', pop_subset),  # Replace '=' with '==' in pop_subset
    # pop_subset = gsub('<==', '<=', pop_subset),  # Replace '<==' with '<=' in pop_subset
    # pop_subset = gsub('>==', '>=', pop_subset),  # Replace '>==' with '>=' in pop_subset
    # pop_subset = gsub('!==', '!=', pop_subset),  # Replace '!==', '!=' in pop_subset
    n_semicolons = str_count(logic_condition_var, ";") + 1,  # Count number of semicolons in logic_condition_var and add 1
    indicator_var = paste0(indicator_var, 1:n_semicolons, collapse = ';')  # Append a sequence number to indicator_var
  )

######
####Applying derivation logic based on derived variables linked to existing variables
logic_exist_vars_v2 = logic_exist_vars_v2 %>% rowwise %>%
                      mutate(logic_exp = gsub('data','raw_dataset2', logic_exp))
cleaned_logic2 = gsub('\\r|\\n','',logic_exist_vars_v2$logic_exp) # Remove carriage returns and newlines from logic expressions
eval(parse(text = paste0(cleaned_logic2, sep = '\n')))# Evaluate and execute the cleaned logic expressions

# Assigning data to indicators where no logical expressions are involved 
matrix_without_logexp_v2 = indicator_matrix_v2 %>% dplyr::filter(has_log_exp == FALSE)# Filter rows without logical expressions
indicators_without_logexp_v2 = do.call('c',strsplit(matrix_without_logexp_v2$indicator_var, "[;]"))# Extract indicator variables without logical expressions  
vars_without_logexp_v2 = do.call('c',strsplit(matrix_without_logexp_v2$logic_condition_var, "[;]"))# Extract variables without logical expressions  
#
# ind_var_without_logexp = cbind(vars_without_logexp,indicators_without_logexp) %>% as.data.frame()
# #  
# common_vars_without_logexp = intersect(vars_without_logexp, names(raw_dataset2))
# #
# filt_ind_var_without_logexp = ind_var_without_logexp %>% dplyr::filter(eval(parse(text = paste0(' vars_without_logexp =="',  common_vars_without_logexp,'"', collapse = '|'))))
# 
# common_indicators_without_logexp = unique(filt_ind_var_without_logexp$indicators_without_logexp)

# If there are variables without logical expressions, assign them directly in the data
if(!is.null(indicators_without_logexp_v2))
{
  eval(parse(text = paste0('raw_dataset2$',indicators_without_logexp_v2,'= raw_dataset2$',vars_without_logexp_v2, sep='\n')))
}
# Assigning data to indicators where logical expressions are involved
matrix_with_logexp_v2 = indicator_matrix_v2 %>% dplyr::filter(has_log_exp == TRUE)
indicators_with_logexp_v2 = do.call('c',strsplit(matrix_with_logexp_v2$indicator_var, "[;]"))
vars_with_logexp_v2 = do.call('c',strsplit(matrix_with_logexp_v2$logic_condition_var, "[;]"))
# ind_var_with_logexp = cbind(vars_with_logexp,indicators_with_logexp) %>% as.data.frame()
# #  
# common_vars_with_logexp = intersect(vars_with_logexp, names(raw_dataset2))
# #
# filt_ind_var_with_logexp = ind_var_with_logexp %>% dplyr::filter(eval(parse(text = paste0(' vars_with_logexp =="',  common_vars_with_logexp,'"', collapse = '|'))))
# 
# common_indicators_with_logexp = unique(filt_ind_var_with_logexp$indicators_with_logexp)





# If there are indicators with logical expressions, mutate the data accordingly to derive binary variables with 0/1 assignment
if(!is.null(indicators_with_logexp_v2))
{
  eval(parse(text = paste0('raw_dataset2 = raw_dataset2 %>% mutate(',indicators_with_logexp_v2,'= ifelse(',vars_with_logexp_v2,',1,0))', sep='\n')))
  eval(parse(text=paste0('raw_dataset2$',indicators_with_logexp_v2,'[is.na(raw_dataset2$',indicators_with_logexp_v2,')]=0', sep='\n')))
  
  # Assign NA to variables with all levels being 0
  eval(parse(text=paste0('raw_dataset2$',indicators_with_logexp_v2,'[all(raw_dataset2$',indicators_with_logexp_v2,' == 0)]=NA', sep='\n')))
}
################################################
# Reading an XML file for level information and filtering out unnecessary rows
# level_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'choices')%>% 
#   dplyr::filter(!is.na(`list name`) & `list name`!='yn' & 
#                   `list name`!='yndk' & `list name`!='yndkr' & 
#                   `list name`!='ynr' & `list name`!='yndrr' & 
#                   `list name`!='yndw' & `list name`!='time_dd' &
#                   `list name`!='time_mm' & `list name`!='time_yyyy' &
#                   `list name`!='psu' & `list name`!='ssu' & `list name`!='tsu' &
#                   name!=77 & name!=88)
# # Converting column names of level_xml_file to lowercase
# colnames(level_xml_file)=tolower(colnames(level_xml_file))
# # Selecting relevant columns from level_xml_file
# level_xml_file = level_xml_file %>% dplyr::select(all_of(c("list name",'name',paste0("label::",language))))
# 
# ###mapping of variables
# # Extracting unique variable names from the level XML file
# extracted_variables = unique(level_xml_file$`list name`)
# # Reading another sheet from the XML file for survey details
# original_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') %>% dplyr::filter(!is.na(name)) %>% mutate(name = tolower(name))
# 
# i = NULL
# matched_variables = data.frame()
# # Loop through extracted variables to find matching survey variables
# for (i in extracted_variables)
# {
#   #unique_var = i
#   #guide_var = unique(grep(paste0('^select_one\\s',i,'$'), original_xml_file$type, v=T))# Find matching guide variables
#   guide_var = unique(grep(paste0('^select_one\\s',i,'$|^select_multiple\\s',i,'$'), original_xml_file$type, v=T))
#   
#   if (length(guide_var)>0)
#   {
#     match_var = (original_xml_file %>% dplyr::filter(type == guide_var) %>% dplyr::select(name))$name # Get matched variables
#     matched_variables = rbind(matched_variables, cbind(i,match_var))# Combine extracted and matched variables
#   } else{}
# }
# 
# # Nov 4 - unclear why these remain in dataset and cause issues later, thus removing
# ###Investigating this further
# #matched_variables <- matched_variables[!(matched_variables$match_var %in% c("sex", "agegrp")), ]
# 
# # Renaming columns of matched_variables data frame
# colnames(matched_variables) = c('level_var','matched_var')
# # Removing '_3' suffix from matched variables
# matched_variables$matched_var = gsub('_3','', matched_variables$matched_var)
# Checking if matched demographic variables exist in the data
existing_vars_dataset_v2 = unique(intersect(matched_variables$matched_var, names(raw_dataset2)))
# Retaining original demographic variables in the dataset
eval(parse(text=paste0('raw_dataset2$demog_',existing_vars_dataset_v2,'=raw_dataset2$',existing_vars_dataset_v2, sep='\n')))
##

i = NULL
# Loop through existing variables dataset to adjust factor levels based on XML file
for(i in  existing_vars_dataset_v2) ###unique(matched_variables$matched_var)
{
  sel_level_var = (matched_variables %>% dplyr::filter(matched_var == i) %>% dplyr::select(level_var))$level_var# Get level variable
  var_attrs = level_xml_file %>%dplyr::filter(`list name` == sel_level_var)# Get variable attributes
  
  eval(parse(text = paste0('raw_dataset2$demog_',i,' = factor(raw_dataset2$demog_',i,', levels = c(',
                           paste0('"',var_attrs$name,'"', collapse = ','),'), labels = c(',
                           paste0('"',eval(parse(text = paste0('var_attrs$`label::',language,'`'))),'"', collapse = ',') ,'))')))
}

# Convert the data to a data frame and adjust factor levels
raw_dataset2 = raw_dataset2 %>% as.data.frame() %>% mutate(sex = factor(c1, levels=1:2, labels=c('Men','Women')),
                                           demog_sex = sex,
                                           agerange = factor(agerange, levels=names(table(raw_dataset2[,'agerange'])), labels=names(table(raw_dataset2[,'agerange']))))

###########Generating agecat2 variable for testing agevar in the matrix
raw_dataset2 = raw_dataset2 %>%mutate(agerange1 = case_when(age>=30 & age <50 ~1),
                      agerange1 = factor(agerange1,levels=1, labels=c('30-49')),
                      agerange2 = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
                      agerange2 = factor(agerange2,levels=1:2, labels=c('40-54','55-69')),
                      agerange3 = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
                      agerange3 = factor(agerange3,levels=1:2, labels=c('18-44','45-69')))


# Copying the cleaned data to analysis_data for further analysis
analysis_dataset2 = raw_dataset2
