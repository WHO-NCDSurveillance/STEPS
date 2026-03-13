############################################################
## READ INPUT DATA
############################################################

# Read the country-specific dataset used for comparison
raw_dataset2 = read.xlsx(paste0('data_input/',country_ISO,'_data_for_comparison.xlsx'))

# Convert all column names to lowercase to ensure consistency
colnames(raw_dataset2) = tolower(colnames(raw_dataset2))


############################################################
## GENERATE MINIMUM AND MAXIMUM AGE VARIABLES
############################################################

# Check if a 'valid' column already exists
if ("valid" %in% names(raw_dataset2)) {
  
  # If it exists, keep only valid observations and compute age range
  raw_dataset2 = raw_dataset2 %>% 
    dplyr::filter(valid == 1) %>% 
    mutate(
      minage = min(age, na.rm = TRUE),   # minimum age in dataset
      maxage = max(age, na.rm = TRUE)    # maximum age in dataset
    )
  
} else {
  
  # If 'valid' column does not exist, create it
  # A record is valid if both sex and age are available
  raw_dataset2 = raw_dataset2 %>% 
    mutate(
      valid = ifelse(!is.na(sex) & !is.na(age),1,0),
      minage = min(age, na.rm = TRUE),
      maxage = max(age, na.rm = TRUE)
    ) %>%
    dplyr::filter(valid == 1)
}


############################################################
## DEFINE GROUPS OF VARIABLES EXPECTED IN THE DATASET
############################################################

# List of expected variables grouped for later processing
var_groups = c(
  'c10a','c10b','c10c','c10d',
  't4a','t4b','t4c',
  't5a','t5b','t5c','t5d','t5e','t5other',
  't5aw','t5bw','t5cw','t5dw','t5ew','t5otherw',
  't14a','t14b','t14c','t14d','t14e','t14other','t14aw','t14bw','t14cw','t14dw','t14ew','t14otherw',
  'a10a','a10b','a10c','a10d','a10e','a10f','a10g',
  't11a','t11b','t11c',
  'a12a','a12b','a12c','a12d','a12e'
)


############################################################
## CHECK WHICH VARIABLES EXIST IN THE DATASET
############################################################

# Identify variables present in the dataset
var_intersect = intersect(c(var_groups,'b5','b8'), names(raw_dataset2))

# Identify variables missing from the dataset
none_exist_var = setdiff(c(var_groups,'b5','b8'), var_intersect)

# Add missing variables to dataset with NA values
if(length(none_exist_var)>0){
  eval(parse(text = paste0('raw_dataset2$',none_exist_var,' = NA', sep = '\n')))
}


############################################################
## READ XLSFORM SURVEY STRUCTURE
############################################################

# Read survey sheet from XLSForm used to build the questionnaire
xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') %>% 
  dplyr::filter(!(is.na(constraint) & is.na(relevant)))

# Standardize column names
colnames(xml_file)=tolower(colnames(xml_file))

# Keep only relevant metadata fields
xml_file = xml_file %>% dplyr::select(all_of(c('type','name','constraint','relevant')))


############################################################
## IDENTIFY STEP SECTION IN SURVEY STRUCTURE
############################################################

# Identify rows where variable names include "step"
step_rows = which(grepl('step', xml_file$name))

# Keep only rows starting from first "step"
reduced_xml = xml_file[step_rows[1]:nrow(xml_file),]


############################################################
## CLEAN SURVEY METADATA
############################################################

# Identify rows corresponding to specific metadata types
eval(parse(text = paste0(
  c('eff_step_rows','type_rows','note_rows','group_rows','one_quin_rows'),
  "= which(grepl('",
  c('step','type','note','group','select_one quin'),
  "',reduced_xml$",
  c('name','name','type','type','type'),
  '))',
  sep='\n'
)))

# Remove metadata rows and standardize text fields
reduced_xml = reduced_xml[
  -sort(unique(c(eff_step_rows,type_rows,note_rows,group_rows,one_quin_rows))),] %>%
  mutate(
    name = tolower(name),
    constraint = tolower(constraint),
    relevant = tolower(relevant)
  )


############################################################
## REMOVE VARIABLES NOT PRESENT IN DATASET
############################################################

# Identify variables referenced in XML but absent in dataset
vars_not_indataset = setdiff(reduced_xml$name, names(raw_dataset2))

# Remove those rows from metadata
reduced_xml = reduced_xml %>%
  dplyr::filter(eval(parse(text = paste0(
    'name!="',vars_not_indataset,'"', collapse = '& '
  ))))


############################################################
## IDENTIFY NUMERIC VARIABLES FROM XLSFORM
############################################################

select_numeric_vars = full_xml_file %>%
  rename_with(tolower) %>%
  mutate(name = tolower(name)) %>%
  dplyr::filter(type %in% c("calculate", "integer") | str_detect(type, "select_one")) %>%
  dplyr::pull(name) %>%
  intersect(setdiff(names(data), c("agerange", "sex"))) %>%
  .[sapply(data[.], function(x) {
    
    # Ensure values are numeric-like
    x = as.character(x)
    x = x[!is.na(x)]
    
    if (length(x) == 0) return(TRUE)
    
    all(grepl("[0-9]", x) & !grepl("[A-Za-z]", x))
  })]

# Convert selected variables to numeric
raw_dataset2 = raw_dataset2 %>%
  mutate(across(all_of(intersect(names(raw_dataset2),select_numeric_vars)),
                ~ as.numeric(as.character(.))))


############################################################
## HANDLE SPECIAL MISSING VALUE CODES
############################################################

# Codes often used to represent missing values in surveys
unique_NA_numbers = c(
  66,666,6666,
  77,777,7777,
  88,888,8888,
  99,999,9999
)


############################################################
## CONVERT SPECIAL CODES TO NA
############################################################

# Identify constraints in the XML referencing these codes
eval(parse(text = paste0(
  'restrict_',unique_NA_numbers,
  ' = grep("',unique_NA_numbers,'",reduced_xml$constraint)',
  sep='\n'
)))


############################################################
## APPLY CONSTRAINT RULES TO DATASET
############################################################

i = NULL
for(i in 6:9)
{
  # Iterates over the numbers 6 to 9.
  eval(parse(text =  paste0('restric_',paste0(rep(i,2), collapse = ''),
                            '_vars =  reduced_xml$name[setdiff(restrict_',
                            paste0(rep(i,2), collapse = ''),', c(restrict_',
                            paste0(rep(i,3), collapse = ''),',restrict_',
                            paste0(rep(i,4), collapse = ''),'))]', sep='\n')))
  # Creates variables for constraints with the current number of digits (e.g., 'restric_666_vars') excluding those with more digits.
  eval(parse(text = paste0('restric_',paste0(rep(i,3), collapse = ''),
                           '_vars = reduced_xml$name[setdiff(restrict_',
                           paste0(rep(i,3), collapse = ''),
                           ', restrict_',paste0(rep(i,4), 
                                                collapse = ''),')]', sep='\n')))
  # Similar to above but for constraints with one more digit.
  eval(parse(text = paste0('restric_',paste0(rep(i,4), collapse = ''),
                           '_vars = reduced_xml$name[restrict_',
                           paste0(rep(i,4), collapse = ''),']', sep='\n')))
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

############################################################
## READ INDICATOR MATRIX
############################################################

indicator_matrix_v2 = read_excel(
  paste0('data_input/',country_ISO,'_input_matrix.xlsx'),
  'indicators'
) %>%
  dplyr::filter(include_in_analysis=='Yes')

# Standardize column names
colnames(indicator_matrix_v2) = tolower(colnames(indicator_matrix_v2))


############################################################
## CHECK IF PRIMARY VARIABLES EXIST IN DATA
############################################################

indicator_matrix_v2 = indicator_matrix_v2 %>%
  rowwise() %>%
  mutate(
    prim_exist = ifelse(
      !is.na(tolower(primary_variables)),
      eval(parse(text = paste0(
        'all(',
        paste0(
          '!is.null(raw_dataset2$',
          do.call('c',strsplit(tolower(primary_variables), "[;]")),
          ')',
          collapse = ' & '
        ),
        ')'
      ))),
      TRUE
    )
  ) %>%
  dplyr::filter(prim_exist==TRUE)
############################################################
## READ DERIVED VARIABLE MATRIX
############################################################

# Read the sheet 'derivedvars' from the country-specific input matrix file.
# This sheet contains definitions of derived variables and their required
# primary variables.
#dervar_matrix = read_excel('data input/matrix.xlsx',sheet = 'derivedvars')
dervar_matrix_v2 = read_excel(
  paste0('data_input/',country_ISO,'_input_matrix.xlsx'),
  sheet = 'derivedvars'
)

# Convert all column names to lowercase to ensure consistent referencing
colnames(dervar_matrix_v2) = tolower(colnames(dervar_matrix_v2))


############################################################
## CHECK WHETHER REQUIRED PRIMARY VARIABLES EXIST
############################################################

# For each row in the derived variable matrix:
# - 'primary_vars' contains one or more variables (semicolon separated)
# - The code checks whether all these variables exist in raw_dataset2
# - If all exist, 'all_exist' will be TRUE; otherwise FALSE
dervar_matrix_v2 = dervar_matrix_v2 %>%
  rowwise() %>%
  mutate(
    all_exist = eval(parse(
      text = paste0(
        'all(',
        paste0(
          '!is.null(raw_dataset2$',
          do.call('c',strsplit(as.character(primary_vars), "[;]")),
          ')',
          collapse = ' & '
        ),
        ')'
      )
    ))
  )


############################################################
## SPLIT EXISTING AND NON-EXISTING DERIVED VARIABLES
############################################################

# Keep only rows where all required primary variables exist.
# These derived variables can safely be calculated later.
logic_exist_vars_v2 = dervar_matrix_v2 %>%
  dplyr::filter(all_exist == TRUE)

# Extract derived variables whose required primary variables
# are missing in the dataset.
# These will later be excluded from indicator calculations.
list_nonexist_dervars_v2 =
  do.call(
    'c',
    strsplit(
      (dervar_matrix_v2 %>%
         as.data.frame() %>%
         dplyr::filter(all_exist == FALSE) %>%
         dplyr::select(derived_vars)
      )$derived_vars,
      "[;]"
    )
  )  ### Also referenced later in the script


############################################################
## REMOVE INDICATORS THAT DEPEND ON NON-EXISTENT VARIABLES
############################################################

# Prepare variables used to search indicator logic
indicator_matrix_v2 = indicator_matrix_v2 %>%
  mutate(
    logic_condition_var = tolower(logic_condition_var),  # standardize case
    pop_subset = tolower(pop_subset),                    # standardize case
    concat_var = paste0(logic_condition_var,' ', pop_subset) 
    # combine logic condition and population subset to search dependencies
  )

# If there are derived variables that do not exist, remove indicators
# whose logic refers to them
if(!is.null(list_nonexist_dervars_v2))
{
  indicator_matrix_v2 =
    indicator_matrix_v2 %>%
    mutate(
      der_varsearch = search_vars(logic_denom = concat_var)
      # search for non-existing derived variables inside indicator logic
    ) %>%
    dplyr::filter(der_varsearch == FALSE)
  # keep only indicators whose logic does not reference missing variables
}else{
  indicator_matrix_v2 = indicator_matrix_v2
  # If no missing derived variables were detected, keep matrix unchanged
}


############################################################
## UNIT STANDARDIZATION FOR BIOMARKERS
############################################################

# Calculate dataset medians for biomarkers b5 and b8
# These medians are used to detect the unit used in the dataset.
raw_dataset2 = raw_dataset2 %>%
  mutate(
    median_b5 = median(b5, na.rm = TRUE),  # median glucose value
    median_b8 = median(b8, na.rm = TRUE)   # median cholesterol value
  ) %>%
  rowwise() %>%
  mutate(
    # If median_b5 > 35, glucose is likely in mg/dL and must be converted
    b5 = ifelse(median_b5 > 35, b5 / 18.01, b5),
    
    # If median_b8 > 12, cholesterol is likely in mg/dL and must be converted
    b8 = ifelse(median_b8 > 12, b8 / 38.67, b8)
  )


############################################################
## CLEAN AND PREPARE INDICATOR MATRIX
############################################################

indicator_matrix_v2 = indicator_matrix_v2 %>%
  
  # Split the indicator column into variable name and description
  separate(
    indicator,
    into = c("indicator_var", "indicator_short_desc"),
    sep = ":",
    remove = FALSE
  ) %>%
  
  # Standardize text fields to lowercase
  mutate(
    indicator_var = tolower(indicator_var),
    logic_condition_var = tolower(logic_condition_var),
    pop_subset = tolower(pop_subset)
  ) %>%
  
  # Process each row individually
  rowwise() %>%
  mutate(
    
    # Detect whether the logic condition contains a logical expression
    # such as '=', '>', or '<'
    has_log_exp = grepl('=|>|<', logic_condition_var),
    
    # Count number of semicolon-separated conditions
    # (used when multiple indicators are derived from one rule)
    n_semicolons = str_count(logic_condition_var, ";") + 1,
    
    # Expand indicator variable names by appending sequence numbers
    # Example: indicator -> indicator1;indicator2;indicator3
    indicator_var = paste0(indicator_var, 1:n_semicolons, collapse = ';')
  )

############################################################
## APPLY DERIVED VARIABLE LOGIC
############################################################

logic_exist_vars_v2 = logic_exist_vars_v2 %>%
  rowwise() %>%
  mutate(logic_exp = gsub('data','raw_dataset2', logic_exp))

# Remove newline characters
cleaned_logic2 = gsub('\\r|\\n','',logic_exist_vars_v2$logic_exp)

# Execute derivation expressions
eval(parse(text = paste0(cleaned_logic2, sep = '\n')))


############################################################
## CREATE INDICATOR VARIABLES
############################################################

# Indicators without logical expressions
matrix_without_logexp_v2 = indicator_matrix_v2 %>%
  dplyr::filter(has_log_exp == FALSE)

indicators_without_logexp_v2 =
  do.call('c',strsplit(matrix_without_logexp_v2$indicator_var, "[;]"))

vars_without_logexp_v2 =
  do.call('c',strsplit(matrix_without_logexp_v2$logic_condition_var, "[;]"))

# Directly assign variables
if(!is.null(indicators_without_logexp_v2)){
  eval(parse(text = paste0(
    'raw_dataset2$',
    indicators_without_logexp_v2,
    '= raw_dataset2$',
    vars_without_logexp_v2,
    sep='\n'
  )))
}


############################################################
## CREATE INDICATORS BASED ON LOGICAL CONDITIONS
############################################################

matrix_with_logexp_v2 = indicator_matrix_v2 %>%
  dplyr::filter(has_log_exp == TRUE)

indicators_with_logexp_v2 =
  do.call('c',strsplit(matrix_with_logexp_v2$indicator_var, "[;]"))

vars_with_logexp_v2 =
  do.call('c',strsplit(matrix_with_logexp_v2$logic_condition_var, "[;]"))

if(!is.null(indicators_with_logexp_v2)){
  
  # Create binary indicators
  eval(parse(text = paste0(
    'raw_dataset2 = raw_dataset2 %>% mutate(',
    indicators_with_logexp_v2,
    '= ifelse(',
    vars_with_logexp_v2,
    ',1,0))',
    sep='\n'
  )))
  
  # Replace missing values with 0
  eval(parse(text=paste0(
    'raw_dataset2$',
    indicators_with_logexp_v2,
    '[is.na(raw_dataset2$',
    indicators_with_logexp_v2,
    ')]=0',
    sep='\n'
  )))
  
}


############################################################
## CREATE DEMOGRAPHIC FACTORS
############################################################

raw_dataset2 = raw_dataset2 %>%
  as.data.frame() %>%
  mutate(
    sex = factor(c1, levels=1:2, labels=c('Men','Women')),
    demog_sex = sex,
    agerange = factor(
      agerange,
      levels = names(table(raw_dataset2[,'agerange'])),
      labels = names(table(raw_dataset2[,'agerange']))
    )
  )


############################################################
## FINAL DATASET FOR ANALYSIS
############################################################

analysis_dataset2 = raw_dataset2

## Checking if dataset names have fpc
fpc_var_check2 = grep('fpc', names(analysis_dataset2), v = T) == 'fpc'
fpc_var_check2 = ifelse(length(fpc_var_check2)==0, FALSE, fpc_var_check2)

