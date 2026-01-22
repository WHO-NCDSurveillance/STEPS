##Reading in data
##files in data input folder
#input_files = list.files('data input')
#data = read.xlsx('data input/matrix.xlsx','data')# Reads data from an Excel file located at 'data input/data.xlsx' into a data frame called 'data'.
data = read.xlsx(paste0('data_input/',country_ISO,'_data.xlsx'))# Reads data from an Excel file located at 'data_input/data.xlsx' into a data frame called 'data'.
colnames(data) =tolower(colnames(data))# Converts all column names in the 'data' data frame to lowercase to ensure uniformity in column name handling.
#######Generating minimum and maximum age
data = data %>% dplyr::filter(valid ==1) %>% mutate(minage = min(age, na.rm = T),maxage = max(age, na.rm = T))

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
var_intersect = intersect(c(var_groups,'b5','b8'), names(data))
# Finds the intersection between the variables in 'var_groups' and the additional variables 'b5' and 'b8' with the column names of the 'data' data frame. This checks which of the specified variables are actually present in the dataset.
##Extracting none existing variables
none_exist_var = setdiff(c(var_groups,'b5','b8'),var_intersect)
# Identifies which variables from 'var_groups' and 'b5', 'b8' are not present in the dataset by taking the set difference between the total list and the intersected list.

###Adding these to the data with NAs initialised
if(length(none_exist_var)>0) {eval(parse(text = paste0('data$',none_exist_var,' = NA', sep = '\n')))}
# Dynamically adds columns for the variables that do not exist in 'data' and initializes them with NA values. 'eval(parse(text = ...))' is used to construct and execute R code as a string.

####Calling functions to be used for analysis
source('scripts/functions/analysis_functions.R', local = T)
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
vars_not_indataset = setdiff(reduced_xml$name, names(data))
# Finds variables that are in 'reduced_xml$name' but not present in the columns of 'data' by taking the set difference.

##
reduced_xml = reduced_xml%>%dplyr::filter(eval(parse(text = paste0('name!="',vars_not_indataset,'"', collapse = '& '))))
# Filters out rows in 'reduced_xml' where the 'name' column matches any of the variables in 'vars_not_indataset'.

###Enforcing variables to be of type numeric
#Selecting numeric variables from xls file--NOTE on timestamp
full_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') 
#
full_xml_file = full_xml_file[-sort(unique(c(eff_step_rows,type_rows,note_rows,group_rows,one_quin_rows))),] 
#
select_numeric_vars = full_xml_file %>% rename_with(tolower) %>% mutate(name = tolower(name)) %>%
                      dplyr::filter(type %in% c("calculate", "integer") | str_detect(type, "select_one")) %>% #
                      dplyr::pull(name) %>% intersect(setdiff(names(data),c('agerange','sex')))

##Converting the variables to type numeric
data = data %>% mutate(across(all_of(select_numeric_vars),~ as.numeric(as.character(.))))

########Cleaning out of range values:
outofrange_logic = reduced_xml %>% dplyr::filter(!is.na(constraint)) %>%
  rowwise %>%
  dplyr::mutate(constraint = gsub('\\}|\\$','',constraint),
                constraint = gsub('\\.',paste0('data$',name),constraint),
                constraint = gsub('=','==',constraint),
                constraint = gsub('<==','<=',constraint),
                constraint = gsub('>==','>=',constraint),
                constraint = gsub('\\sand\\s','&',constraint),
                constraint = gsub('\\sor\\s','|',constraint),
                constraint = gsub('\\{','data$',constraint),gsub('}','data$',constraint),
                constraint = gsub('!==','!=',constraint),
                constraint = gsub('\\n|\\r','',constraint),
                const_logic = paste0('!(',constraint,')'))  %>%
  mutate(select_fn = search_vars(logic_denom=constraint)) %>% dplyr::filter(select_fn==FALSE)
# Processes 'constraint' in 'reduced_xml' to replace certain patterns with corresponding R syntax, to create logical expressions. 'const_logic' stores these expressions negated. Filters rows where 'search_vars' function returns FALSE (i.e., no match).

##Applying outofrange_logic to the dataset
eval(parse(text = paste0('data$',outofrange_logic$name,'[',outofrange_logic$const_logic,'] = NA', sep = '\n')))
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
vars_with_77_or_88 = intersect(pre_vars_with_77_or_88, names(data))
# Finds the intersection between 'pre_vars_with_77_or_88' and the column names of 'data'.
##
if(length(vars_with_77_or_88)>0)
{
  # Checks if there are variables to process.
  eval(parse(text=paste0('data$',vars_with_77_or_88,'[data$',vars_with_77_or_88,'==77 | data$',vars_with_77_or_88,'==88] = NA', sep='\n')))
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
    eval(parse(text = paste0('data$',var_set,'[data$',var_set,' == ',related_no,'] = NA', sep = '\n')))
    # Sets values to NA in the specified columns where the values match the extracted number.
  } else{}
  # If no variables are found, do nothing.
}

#########Generating indicators
####Reading indicator and secondary variable matrix
#from an Excel file located at 'data input/input_matrix.xls' from the sheet named 'indicators'
#indicator_matrix = read_excel('data input/matrix.xlsx','indicators')%>%dplyr::filter(include_in_analysis=='Yes')
indicator_matrix = read_excel(paste0('data_input/',country_ISO,'_input_matrix.xlsx'),'indicators')%>%dplyr::filter(include_in_analysis=='Yes')

# Converting all column names of indicator_matrix to lowercase to ensure consistent naming
colnames(indicator_matrix) = tolower(colnames(indicator_matrix))
# Converting all values in indicator_matrix columns to character type
eval(parse(text = paste0('indicator_matrix$',colnames(indicator_matrix),'= as.character(indicator_matrix$',colnames(indicator_matrix),')', sep='\n')))
# Checking if primary variables specified in the matrix exist in the dataset
indicator_matrix = indicator_matrix %>% rowwise %>%# Ensuring operations are applied row by row
  mutate(prim_exist = ifelse(!is.na(tolower(primary_variables)),# Check if primary_variables is not NA after converting to lowercase
                             eval(parse(text = paste0('all(',paste0('all(',paste0('!is.null(data$',do.call('c',strsplit(tolower(primary_variables), "[;]")),')',collapse = ' & '),')'),',',
                                                      paste0('all(',paste0('any(!is.na(data$',do.call('c',strsplit(tolower(primary_variables), "[;]")),'))',collapse = ' | '),')'),')'))), TRUE))%>%
  dplyr::filter(prim_exist==TRUE)# Filtering rows where prim_exist is TRUE

###
# Reading another data matrix from the same Excel file, but from the 'derivedvars' sheet
#dervar_matrix = read_excel('data input/matrix.xlsx',sheet = 'derivedvars')
dervar_matrix = read_excel(paste0('data_input/',country_ISO,'_input_matrix.xlsx'),sheet = 'derivedvars')
# Converting all column names of dervar_matrix to lowercase for consistency
colnames(dervar_matrix) = tolower(colnames(dervar_matrix))
# Checking if derived variables specified in the matrix exist in the dataset
dervar_matrix = dervar_matrix %>% rowwise %>%
  mutate(all_exist = eval(parse(text = paste0('all(',paste0('!is.null(data$',do.call('c',strsplit(as.character(primary_vars), "[;]")),')',collapse = ' & '),')')))) 
# Filtering derived variables where all exist is TRUE
logic_exist_vars = dervar_matrix %>% dplyr::filter(all_exist == TRUE)
# Extracting list of derived variables that do not exist
list_nonexist_dervars = do.call('c',strsplit((dervar_matrix %>% as.data.frame() %>% 
                                                dplyr::filter(all_exist == FALSE) %>% dplyr::select(derived_vars))$derived_vars, "[;]")) ###Also lines 182 - where these objects are used

####Excluding indicators with no primary variables from the input matrix
# Function to search for non-existent variables in logical expressions
search_vars = function(nonexist_vars = list_nonexist_dervars, logic_denom) {
  pattern = paste(tolower(nonexist_vars), collapse="|") # Creating a regex pattern from list_nonexist_vars
  found = grepl(pattern, logic_denom) # Checking if any of the variables in list_nonexist_vars match either logical expression or logic for denominator
  return(found)
}

###Adding concatinated variable for searching non_existing variables
indicator_matrix = indicator_matrix %>% mutate(logic_condition_var = tolower(logic_condition_var), # Convert logic_condition_var to lowercase
                                               pop_subset = tolower(pop_subset),# Convert pop_subset to lowercase
                                               concat_var = paste0(logic_condition_var,' ', pop_subset))# Concatenate logic_condition_var and pop_subset

# If there are non-existent derived variables, update indicator_matrix to exclude them
if(!is.null(list_nonexist_dervars))
{
  indicator_matrix = indicator_matrix %>% mutate(der_varsearch = search_vars(logic_denom = concat_var)) %>%# Search for non-existent derived variables in the concatenated variable
    dplyr::filter(der_varsearch == FALSE)# Filter out rows where der_varsearch is TRUE
}else{indicator_matrix = indicator_matrix}# No changes if list_nonexist_dervars is NULL


######
# Processing glucose and cholesterol data, converting units if necessary
data = data %>% mutate(median_b5 = median(b5, na.rm = T),# Calculate median of b5, ignoring NA values
                       median_b8 = median(b8, na.rm = T)) %>%# Calculate median of b8, ignoring NA values
  rowwise %>% mutate(b5 = ifelse(median_b5>35, b5/18.01, b5), # Convert b5 to mg/dl if median_b5 > 35
                     b8 = ifelse(median_b8>12, b8/38.67, b8)) # Convert b8 to mg/d if median_b8 > 12
# Processing and cleaning indicator_matrix
indicator_matrix = indicator_matrix %>%
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
cleaned_logic = gsub('\\r|\\n','',logic_exist_vars$logic_exp) # Remove carriage returns and newlines from logic expressions
eval(parse(text = paste0(cleaned_logic, sep = '\n')))# Evaluate and execute the cleaned logic expressions

# Assigning data to indicators where no logical expressions are involved 
matrix_without_logexp = indicator_matrix %>% dplyr::filter(has_log_exp == FALSE)# Filter rows without logical expressions
indicators_without_logexp = do.call('c',strsplit(matrix_without_logexp$indicator_var, "[;]"))# Extract indicator variables without logical expressions  
vars_without_logexp = do.call('c',strsplit(matrix_without_logexp$logic_condition_var, "[;]"))# Extract variables for logic conditions without logical expressions  
# If there are variables without logical expressions, assign them directly in the data
if(!is.null(vars_without_logexp))
{
  eval(parse(text = paste0('data$',indicators_without_logexp,'= data$',vars_without_logexp, sep='\n')))
}
# Assigning data to indicators where logical expressions are involved
matrix_with_logexp = indicator_matrix %>% dplyr::filter(has_log_exp == TRUE)
indicators_with_logexp = do.call('c',strsplit(matrix_with_logexp$indicator_var, "[;]"))  
vars_with_logexp = do.call('c',strsplit(matrix_with_logexp$logic_condition_var, "[;]"))  

# If there are indicators with logical expressions, mutate the data accordingly to derive binary variables with 0/1 assignment
if(!is.null(indicators_with_logexp))
{
  eval(parse(text = paste0('data = data %>% mutate(',indicators_with_logexp,'= ifelse(',vars_with_logexp,',1,0))', sep='\n')))
  eval(parse(text=paste0('data$',indicators_with_logexp,'[is.na(data$',indicators_with_logexp,')]=0', sep='\n')))
  
  # Assign NA to variables with all levels being 0
  eval(parse(text=paste0('data$',indicators_with_logexp,'[all(data$',indicators_with_logexp,' == 0)]=NA', sep='\n')))
}
################################################
# Reading an XML file for level information and filtering out unnecessary rows
level_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'choices')%>% 
  dplyr::filter(!is.na(`list name`) & `list name`!='yn' & 
                  `list name`!='yndk' & `list name`!='yndkr' & 
                  `list name`!='ynr' & `list name`!='yndrr' & 
                  `list name`!='yndw' & `list name`!='time_dd' &
                  `list name`!='time_mm' & `list name`!='time_yyyy' &
                  `list name`!='psu' & `list name`!='ssu' & `list name`!='tsu' &
                  name!=77 & name!=88)
# Converting column names of level_xml_file to lowercase
colnames(level_xml_file)=tolower(colnames(level_xml_file))
# Selecting relevant columns from level_xml_file
level_xml_file = level_xml_file %>% dplyr::select(all_of(c("list name",'name',paste0("label::",language))))

###mapping of variables
# Extracting unique variable names from the level XML file
extracted_variables = unique(level_xml_file$`list name`)
# Reading another sheet from the XML file for survey details
original_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') %>% dplyr::filter(!is.na(name)) %>% mutate(name = tolower(name))

i = NULL
matched_variables = data.frame()
# Loop through extracted variables to find matching survey variables
for (i in extracted_variables)
{
  #unique_var = i
  #guide_var = unique(grep(paste0('^select_one\\s',i,'$'), original_xml_file$type, v=T))# Find matching guide variables
  guide_var = unique(grep(paste0('^select_one\\s',i,'$|^select_multiple\\s',i,'$'), original_xml_file$type, v=T))
  
  if (length(guide_var)>0)
  {
    match_var = (original_xml_file %>% dplyr::filter(type == guide_var) %>% dplyr::select(name))$name # Get matched variables
    matched_variables = rbind(matched_variables, cbind(i,match_var))# Combine extracted and matched variables
  } else{}
}

# Nov 4 - unclear why these remain in dataset and cause issues later, thus removing
###Investigating this further
#matched_variables <- matched_variables[!(matched_variables$match_var %in% c("sex", "agegrp")), ]

# Renaming columns of matched_variables data frame
colnames(matched_variables) = c('level_var','matched_var')
# Removing '_3' suffix from matched variables
matched_variables$matched_var = gsub('_3','', matched_variables$matched_var)
# Checking if matched variables exist in the data
existing_vars_dataset = unique(intersect(matched_variables$matched_var, names(data)))
# Retaining original demographic variables in the dataset
eval(parse(text=paste0('data$demog_',existing_vars_dataset,'=data$',existing_vars_dataset, sep='\n')))
##

i = NULL
# Loop through existing variables dataset to adjust factor levels based on XML file
for(i in  existing_vars_dataset) ###unique(matched_variables$matched_var)
{
  sel_level_var = (matched_variables %>% dplyr::filter(matched_var == i) %>% dplyr::select(level_var))$level_var# Get level variable
  var_attrs = level_xml_file %>%dplyr::filter(`list name` == sel_level_var)# Get variable attributes
  
  eval(parse(text = paste0('data$demog_',i,' = factor(data$demog_',i,', levels = c(',
                           paste0('"',var_attrs$name,'"', collapse = ','),'), labels = c(',
                           paste0('"',eval(parse(text = paste0('var_attrs$`label::',language,'`'))),'"', collapse = ',') ,'))')))
}

# Convert the data frame to a data frame and adjust factor levels
data = data %>% as.data.frame() %>% mutate(sex = factor(c1, levels=1:2, labels=c('Men','Women')),
                                           demog_sex = sex,
                                           agerange = factor(agerange, levels=names(table(data[,'agerange'])), labels=names(table(data[,'agerange']))))

###########Generating agecat2 variable for testing agevar in the matrix
# data = data %>%mutate(agerange1 = case_when(age>=30 & age <50 ~1),
#                       agerange1 = factor(agerange1,levels=1, labels=c('30-49')),
#                       agerange2 = case_when(age>=40 & age <55 ~1,age>=55 & age <70 ~2),
#                       agerange2 = factor(agerange2,levels=1:2, labels=c('40-54','55-69')),
#                       agerange3 = case_when(age>=18 & age <45 ~1,age>=45 & age <70 ~2),
#                       agerange3 = factor(agerange3,levels=1:2, labels=c('18-44','45-69')))

# Copying the cleaned data to analysis_data for further analysis
analysis_data = data
####Adjusting col_strat_variable and row_strat_variables objects
i= NULL
adj_col_strat_variable=NULL
# Loop through column stratification variables and adjust based on matched variables
for(i in col_strat_variable)
{
  var_match = grep(i, matched_variables$matched_var, v=T)# Find matched variables
  if(length(var_match)>0)
  {
    adj_col_strat_variable = c(adj_col_strat_variable, paste0('demog_',var_match))# Adjust column stratification variables
  } else {adj_col_strat_variable = c(adj_col_strat_variable, i)}# Keep original if no match
}

# Adjusting row stratification variables
i= NULL
adj_row_strat_variable=NULL
# Loop through row stratification variables and adjust based on matched variables
for(i in row_strat_variables)
{
  var_match = grep(i, matched_variables$matched_var, v=T)# Find matched variables
  if(length(var_match)>0)
  {
    adj_row_strat_variable = c(adj_row_strat_variable, paste0('demog_',var_match))# Adjust row stratification variables
  } else {adj_row_strat_variable = c(adj_row_strat_variable, i)}# Keep original if no match
}

# Updating column and row stratification variables with adjusted ones
col_strat_variable = adj_col_strat_variable
row_strat_variables = adj_row_strat_variable

# Reading and processing another sheet from the Excel file for language translations
other_language = read_excel(paste0('data_input/',country_ISO,'_input_matrix.xlsx'),sheet = 'other')%>%as.data.frame()
colnames(other_language)=tolower(colnames(other_language))
other_language = other_language[,c('item',language)]

###Original file for other_language
language_translation = read_excel(paste0('data_input/',country_ISO,'_input_matrix.xlsx'),sheet = 'other')%>%as.data.frame()
colnames(language_translation)=tolower(colnames(language_translation))

# Rewriting translations for 'sex'
# other_language[1,2] = levels(data$demog_c1)[1]
# other_language[2,2] = levels(data$demog_c1)[2]
####################################Editing the indicator matrix to drop indicators with NA##########################################
indicator_matrix$excl_missing_ind = 'no'
###
indicator_matrix = indicator_matrix %>% rowwise %>% mutate(ind_reduce = search_vars(logic_denom = concat_var, nonexist_vars = none_exist_var),
                                                           excl_missing_ind = ifelse(ind_reduce == TRUE,'yes',excl_missing_ind),
                                                           type_length = length(do.call('c',strsplit(type, "[;]"))),
                                                           type = ifelse(type_length==1,paste0(rep(type,n_semicolons),collapse = ';'),type),
                                                           pop_subset = ifelse(type_length==1,paste0(rep(pop_subset,n_semicolons),collapse = ';'),pop_subset))

###
indicator_matrix = indicator_matrix %>% rowwise %>%
  mutate(
    # Create a column 'var_missing' where each row contains a list of logical values indicating if each variable in 'indicator_var' is missing in 'data'
    var_missing = list(eval(parse(text=paste0('c(',paste0('all(is.na(data$',do.call('c',strsplit(indicator_var, "[;]")),'))', collapse = ','),')')))),
    ##
    rev_popsubset = gsub('^all$','TRUE',pop_subset),
    rev_popsubset = gsub(';all',';TRUE',rev_popsubset),
    rev_popsubset = gsub('all;','TRUE;',rev_popsubset),
    pop_missing = list(eval(parse(text=paste0('c(',paste0('with(data, !any(',do.call('c',strsplit(rev_popsubset, "[;]")),',na.rm=T))', collapse = ','),')')))),
    #
    rev_var_missing = list(eval(parse(text=paste0('c(',paste0(unlist(var_missing),'|',unlist(pop_missing), collapse = ','),')')))),
    ###
    pos_true = paste0(as.character(list(grep('TRUE',rev_var_missing))), collapse = ','),
    pos_true = ifelse(pos_true=='integer(0)'|excl_missing_ind == 'no','999',pos_true),
    ###updated n_semicolons
    leng_remove = eval(parse(text=paste0('length(',pos_true,')'))),
    rev_n_semicolons = ifelse(pos_true>1 & pos_true!=999, n_semicolons - leng_remove,n_semicolons),
    
    ####indicator_var
    indicator_var = as.character(list(if(n_semicolons==1 ) {
      eval(parse(text = paste0('"',strsplit(indicator_var, "[;]"),'"[-c(',pos_true,')]')))
    }else{eval(parse(text = paste0(strsplit(indicator_var, "[;]"),'[-c(',pos_true,')]')))})),
    len_ind_var = ifelse(((n_semicolons==1|rev_n_semicolons==1) &  pos_true!='999') |((n_semicolons==1||rev_n_semicolons==1) &  pos_true=='999'),1,length(eval(parse(text = indicator_var)))),
    #var_missing = as.character(unlist(var_missing)),
    len_ind_var = ifelse(n_semicolons==1 & rev_n_semicolons==1 & pos_true!=999,0,len_ind_var)#,pos_true = as.numeric(pos_true)
  )%>%dplyr::filter(len_ind_var>0)

#########
indicator_matrix = indicator_matrix %>% rowwise %>% mutate(
  indicator_var = list(if(len_ind_var==1 ){as.character(indicator_var)}else{paste0(eval(parse(text = as.character(indicator_var))), collapse = ';')}),
  ####logic_condition_var
  logic_condition_var = as.character(list(if(n_semicolons==1) {
    eval(parse(text = paste0('"',strsplit(logic_condition_var, "[;]"),'"[-c(',pos_true,')]')))
  }else{eval(parse(text = paste0(strsplit(logic_condition_var, "[;]"),'[-c(',pos_true,')]')))})),
  logic_condition_var = list(if(len_ind_var==1 ){as.character(logic_condition_var)}else{paste0(eval(parse(text = as.character(logic_condition_var))), collapse = ';')}),
  ####pop_subset
  pop_subset = as.character(list(if(n_semicolons==1) {
    eval(parse(text = paste0('"',strsplit(pop_subset, "[;]"),'"[-c(',pos_true,')]')))
  }else{eval(parse(text = paste0(strsplit(pop_subset, "[;]"),'[-c(',pos_true,')]')))})),
  pop_subset = list(if(len_ind_var==1 ){as.character(pop_subset)}else{paste0(eval(parse(text = as.character(pop_subset))), collapse = ';')}),
  #####subtitle1
  subtitle1 = as.character(list(if(n_semicolons==1) {
    as.character(subtitle1)
  }else if (!is.na(subtitle2)){
    subtitle1
  }else{eval(parse(text = as.character(strsplit(subtitle1, "[;]"))))[eval(parse(text = paste0('-c(',pos_true,')')))]})),
  subtitle1 = list(if(len_ind_var==1 & is.na(subtitle2)){as.character(subtitle1)}
                   else if (!is.na(subtitle2)){
                     subtitle1
                   }else{paste0(eval(parse(text = as.character(subtitle1))), collapse = ';')}),
  # ###subtitle2
  subtitle2 = as.character(list(if(n_semicolons==1) {
    as.character(subtitle2)
  }else{eval(parse(text = paste0(strsplit(as.character(subtitle2), "[;]"),'[-c(',pos_true,')]')))})),
  subtitle2 = list(if(len_ind_var==1 ){as.character(subtitle2)}else{paste0(eval(parse(text = as.character(subtitle2))), collapse = ';')}),
  # #####type
  type = as.character(list(if(n_semicolons==1) {
    eval(parse(text = paste0('"',strsplit(as.character(type), "[;]"),'"[-c(',pos_true,')]')))
  }else{eval(parse(text = paste0(strsplit(as.character(type), "[;]"),'[-c(',pos_true,')]')))})),
  type = list(if(len_ind_var==1 ){as.character(type)}else{paste0(eval(parse(text = as.character(type))), collapse = ';')}),
  ####Variable types
  indicator_var = as.character(indicator_var),
  logic_condition_var = as.character(logic_condition_var),
  pop_subset = as.character(pop_subset),
  subtitle1 = ifelse(!is.null(subtitle1[[1]]),as.character(subtitle1),NA),
  subtitle1 = ifelse(subtitle1=="logical(0)"|subtitle1=="NA"|is.na(subtitle1), NA,subtitle1),
  subtitle2 = as.character(subtitle2),
  subtitle2 = ifelse(subtitle2=="logical(0)"|subtitle2=="NA"|is.na(subtitle1), NA,subtitle2),
  subtitle1 = as.character(subtitle1),subtitle2 = as.character(subtitle2),
  type = as.character(type) )

#####
indicator_matrix = indicator_matrix %>% mutate(column_strat = tolower(column_strat))

##################
#################Generating data processing report
existence_ISO_rep = ifelse(ISO_existence,paste0('ISO code for ',country,' exists'),paste0('ISO code for ',country,' does not exist hence cvd risk has not been adjusted'))
non_exisiting_group_vars = ifelse(length(none_exist_var)>0, 
                                  paste0('The following variables are not in the dataset and have been generated and set to missing as they are required as part of the grouped variables required for derivation of secondary variables: ',
                                         paste0(none_exist_var,collapse = ',')),'All required grouped variables are available')
vars_in_xml_not_dataset = ifelse(length(vars_not_indataset)>0, 
                                 paste0('The following variables are in the xls form and not in the dataset: ',
                                        paste0(vars_not_indataset,collapse = ',')),'Variables match between the xls form and the dataset')
resp_77_88  = ifelse(length(vars_with_77_or_88)>0, 
                     paste0('The following variables were assigned either 77 or 88 for dont knows or refusals and these have been set to missing: ',
                            paste0(vars_with_77_or_88,collapse = ',')),'No variable has been assigned either 77 or 88 for dont knows or refusals in the dataset')  

sec_derivarion  = ifelse(length(list_nonexist_dervars)>0, 
                         paste0('The following secondary variables were part of the secondary variables to be derived although the primary variables required for their derivation are partially available: ',
                                paste0(list_nonexist_dervars,collapse = ',')),'Primary variables are available to derive the secondary variables') 

status_execution = ifelse(ISO_existence,'Script execution progressed','The supplied ISO code does not exist in the reference dataset for cvd calculation')

data_processing_report = cbind(Item = c('Existence of ISO code','Completeness of grouped variables','Variables in xls vs dataset','Variables with values set to NA if has 77 or 88 as values',
                                        'Secondary variables without primary variables for derivation','Excecution status'),
                               Description = c(existence_ISO_rep,non_exisiting_group_vars,vars_in_xml_not_dataset,resp_77_88,sec_derivarion,status_execution))

#########################
# flex_processing_report = data_processing_report %>% as.data.frame() %>% flextable()%>%
#   flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
#   bold(part = 'header')%>%
#   fontsize(size = 9 ,part = "all")%>%autofit()%>% 
#   align(j = 2, align = 'center', part = 'header') %>% theme_vanilla()
# 
# ## Printing of Codebook::::
# doc = read_docx(paste0(getwd(),'/templates/data_processing_template.docx'))
# #
# doc = headers_replace_text_at_bkm(doc,"country",paste0(toupper(country),' ',survey_year))


#
# doc=doc %>% cursor_bookmark(id  = "table1") %>%
#   body_add_flextable(width(flex_processing_report, width = dim(flex_processing_report)$widths*6.5/(flextable_dim(flex_processing_report)$widths)), pos = "on", align = 'left')
# 
# print(doc,target=paste0(getwd(),'/outputs/Data processing report.docx')) 

#####Stopping script execution if ISO code is missing
if(!ISO_existence)stop('The supplied ISO code does not exist in the reference dataset for cvd calculation')

####clearing temp folder
#unlink(paste0(getwd(),'/temp/*'))
# clearing Tables folder 
files_to_remove = list.files(paste0(getwd(),'/temp'))#setdiff(list.files(paste0(getwd(),'/temp')),'Part1.docx')
eval(parse(text = paste0('file.remove("',getwd(),'/temp/',files_to_remove,'")', sep='\n')))
##Clearing other folders
unlink(paste0(getwd(),'/outputs/*'))
# unlink(paste0(getwd(),'/comparative/*'))
# unlink(paste0(getwd(),'/report sections/*'))
file.remove(paste0(getwd(),'/report outputs/combined_report.docx'))
###Reducing the matrix to required fields for written report
##To deactivate when generating databook and accompanying facheet
#indicator_matrix = indicator_matrix %>% dplyr::filter(!is.na(section_title))
#original_matrix = indicator_matrix


