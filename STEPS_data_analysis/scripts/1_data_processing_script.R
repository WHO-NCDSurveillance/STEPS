############################################################
## SCRIPT FOR DATA PROCESSING 
############################################################

########################################
## Reading in data
########################################

## Files in data input folder
data = read.xlsx(paste0('data_input/',country_ISO,'_data.xlsx')) # Reads data from an Excel file located at 'data_input/data.xlsx' into a data frame called 'data'.
colnames(data) = tolower(colnames(data)) # Converts all column names in the 'data' data frame to lowercase to ensure uniformity in column name handling.

## Checking if dataset names have fpc
fpc_var_check = grep('fpc', names(data), v = T) == 'fpc'
fpc_var_check = ifelse(length(fpc_var_check)==0, FALSE, fpc_var_check)

####### Generating minimum and maximum age
data = data %>% dplyr::filter(valid == 1) %>% 
       mutate(minage = min(age, na.rm = T), # Minimum age in valid rows
         maxage = max(age, na.rm = T), # Maximum age in valid rows
         sex = factor(c1, levels = 1:2, labels = c('Men', 'Women'))) 


########################################
#### Variable groups
########################################

var_groups = c(
  't5a','t5b','t5c','t5d','t5e','t5other',
  't5aw','t5bw','t5cw','t5dw','t5ew','t5otherw',
  't14a','t14b','t14c','t14d','t14e','t14other',
  't14aw','t14bw','t14cw','t14dw','t14ew','t14otherw',
  'a12a','a12b','a12c','a12d','a12e'
) # Defines a vector 'var_groups' containing a list of variable names or codes grouped for later reference.

##### Examining the existence of var_groups in data if none added to the dataset to avoid script breakdown
####### Adding b5 and b8 in case not captured for smooth execution of the script
var_intersect = intersect(c(var_groups,'b5','b8'), names(data)) # Finds the intersection between the variables in 'var_groups' and the additional variables 'b5' and 'b8' with the column names of the 'data' data frame. This checks which of the specified variables are actually present in the dataset.
none_exist_var = setdiff(c(var_groups,'b5','b8'), var_intersect) # Identifies which variables from 'var_groups' and 'b5', 'b8' are not present in the dataset by taking the set difference between the total list and the intersected list.

### Adding these to the data with NAs initialised
if(length(none_exist_var) > 0) {eval(parse(text = paste0('data$',none_exist_var,' = NA', sep = '\n'))) } # Dynamically adds columns for the variables that do not exist in 'data' and initializes them with NA values.


########################################
#### Calling functions to be used for analysis
########################################

source('scripts/functions/analysis_functions.R', local = T) # Sources the R script 'analysis_functions.R' from the 'scripts/functions' directory, making any functions defined in that script available for use.

########################################
### Reading XML file for cleaning out of range values together with checking of skip logic
########################################

xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') %>% 
  dplyr::filter(!(is.na(constraint) & is.na(relevant))) # Reads the 'survey' sheet and filters out rows where both 'constraint' and 'relevant' columns are NA

colnames(xml_file) = tolower(colnames(xml_file))
xml_file = xml_file %>% dplyr::select(all_of(c('type','name','constraint','relevant'))) # Selects only the relevant columns

### Row number to start from:
step_rows = which(grepl('step', xml_file$name)) # Identifies the row indices where the 'name' column contains the word 'step' using regular expression matching
reduced_xml = xml_file[step_rows[1]:nrow(xml_file),] # Subsets 'xml_file' from the first row that contains 'step' to the end of the data frame

# Further cleaning
eval(parse(text = paste0(c('eff_step_rows','type_rows','note_rows','group_rows','one_quin_rows'),
                         "= which(grepl('", c('step','type','note','group','select_one quin'),
                         "',reduced_xml$",c('name','name','type','type','type'),'))', sep='\n')))
# Dynamically creates variables ('eff_step_rows', 'type_rows', etc.) to store row indices based on different patterns found in 'reduced_xml$name' and 'reduced_xml$type'.

## Further filtering of the xml file
reduced_xml = reduced_xml[-sort(unique(c(eff_step_rows,type_rows,note_rows,group_rows,one_quin_rows))), ] %>%
  mutate(name = tolower(name), constraint = tolower(constraint), relevant = tolower(relevant)) # Removes rows for these patterns and converts columns to lowercase

########################################
### Comparing names in reduced_xml versus the dataset
########################################

vars_not_indataset = setdiff(reduced_xml$name, names(data)) # Finds variables in 'reduced_xml$name' not present in 'data'
reduced_xml = reduced_xml %>% 
  dplyr::filter(eval(parse(text = paste0('name!="', vars_not_indataset,'"', collapse = '& ')))) # Filters out rows not in dataset


########################################
### Enforcing variables to be of type numeric
########################################

full_xml_file = read_excel(paste0('data_input/',country_ISO,'_xls_form.xlsx'),'survey') # Reading full xml
full_xml_file = full_xml_file[-sort(unique(c(eff_step_rows,type_rows,note_rows,group_rows,one_quin_rows))),] # Remove unwanted rows

select_numeric_vars = full_xml_file %>%
  rename_with(tolower) %>%
  mutate(name = tolower(name)) %>%
  dplyr::filter(type %in% c("calculate", "integer") | str_detect(type, "select_one")) %>%
  dplyr::pull(name) %>%
  intersect(setdiff(names(data), c("agerange", "sex"))) %>%
  .[sapply(data[.], function(x) { # Ensure only numeric-looking variables
    x = as.character(x)
    x = x[!is.na(x)]
    if (length(x) == 0) return(TRUE)
    all(grepl("[0-9]", x) & !grepl("[A-Za-z]", x))
  })]

data = data %>% mutate(across(all_of(select_numeric_vars), ~ as.numeric(as.character(.)))) # Converting variables to numeric


########################################
### Cleaning out of range values
########################################

outofrange_logic = reduced_xml %>%
  dplyr::filter(!is.na(constraint)) %>%
  rowwise %>%
  dplyr::mutate(constraint = gsub('\\}|\\$','',constraint),
                #constraint = gsub('\\.',paste0('data$',name),constraint),
                constraint = gsub("(?<!\\d)\\.(?!\\d)", paste0("data$", name), constraint, perl = TRUE),
                constraint = gsub('=','==',constraint),
                constraint = gsub('<==','<=',constraint),
                constraint = gsub('>==','>=',constraint),
                constraint = gsub('\\sand\\s','&',constraint),
                constraint = gsub('\\sor\\s','|',constraint),
                constraint = gsub('\\{','data$',constraint), gsub('}','data$',constraint),
                constraint = gsub('!==','!=',constraint),
                constraint = gsub('\\n|\\r','',constraint),
                const_logic = paste0('!(',constraint,')')) %>%
  mutate(select_fn = search_vars(logic_denom=constraint)) %>% 
  dplyr::filter(select_fn == FALSE) # Process constraint expressions

eval(parse(text = paste0('data$',outofrange_logic$name,'[',outofrange_logic$const_logic,'] = NA', sep='\n'))) # Apply constraints

########################################
### Handling special codes (77, 88, etc.)
########################################

# Read 'choices' sheet and filter only special codes
level_xml_file_77_88 = read_excel(paste0('data_input/', country_ISO, '_xls_form.xlsx'), 'choices') %>%
  dplyr::filter(!is.na(`list name`) & name %in% c(77, 88)) %>%
  rename_with(tolower) %>%
  dplyr::select(all_of(c("list name", "name", paste0("label::", language)))) # Keep relevant columns

# Extract unique variable groups that contain special codes
extracted_variables_77_88 = unique(level_xml_file_77_88$`list name`)

# Read survey xml and standardize names
original_xml_file = read_excel(paste0('data_input/', country_ISO, '_xls_form.xlsx'), 'survey') %>%
  dplyr::filter(!is.na(name)) %>%
  mutate(name = tolower(name))

# Map each variable group to actual data columns
matched_variables_77_88 = map_dfr(extracted_variables_77_88, function(var_group) {
  # Find matching select_one/select_multiple types
  guide_var = grep(paste0('^select_one\\s', var_group, '$|^select_multiple\\s', var_group, '$'),
                   original_xml_file$type, value = TRUE)
  
  if(length(guide_var) > 0) {
    tibble(
      group = var_group,
      variable = original_xml_file %>%
        dplyr::filter(type %in% guide_var) %>%
        pull(name)
    )
  } else {
    NULL
  }
})

# Remove exempt variables and keep only those existing in data
vars_with_77_or_88 = intersect(setdiff(matched_variables_77_88$variable, vars_exempt_77_88), names(data))

# Sets values to NA in the specified columns where the values are either 77 or 88.
if(length(vars_with_77_or_88)>0)
{eval(parse(text=paste0('data$',vars_with_77_or_88,'[data$',vars_with_77_or_88,'==77 | data$',vars_with_77_or_88,'==88] = NA', sep='\n')))}

########################################
### Other unique numbers to NA
########################################
# Define numbers to replace with NA
unique_NA_numbers = c(66, 666, 6666, 77, 777, 7777, 88, 888, 8888, 99, 999, 9999)

# Identify variables in reduced_xml (xls form) that contain these numbers in their constraints
na_vars_list = map(unique_NA_numbers, function(num) {
  pattern <- paste0("(^|[^0-9])", num, "([^0-9]|$)")
  vars <- reduced_xml$name[str_detect(reduced_xml$constraint, pattern)]
  # Keep only variables that exist in data
  vars <- intersect(vars, names(data))
  if(length(vars) > 0) {
    tibble(variable = vars, value_to_na = num)
  } else {
    NULL
  }
}) %>% bind_rows()

# Replace 66, 77, 88, etc. values in data with NA for each variable
i=NULL
for (i in seq_len(nrow(na_vars_list))) {
  var <- na_vars_list$variable[i]
  val <- na_vars_list$value_to_na[i]
  data[[var]][data[[var]] == val] <- NA
}

######### Generating indicators #########

#### Reading indicator and secondary variable matrix ####
# Load indicator matrix from Excel and filter for inclusion
indicator_matrix = read_excel(
  paste0('data_input/', country_ISO, '_input_matrix.xlsx'),
  'indicators'
) %>% dplyr::filter(include_in_analysis == 'Yes')

# Standardize column names and convert all values to character
colnames(indicator_matrix) = tolower(colnames(indicator_matrix))
eval(parse(text = paste0(
  'indicator_matrix$', colnames(indicator_matrix),
  '= as.character(indicator_matrix$', colnames(indicator_matrix), ')', sep = '\n'
)))

#### Check if primary variables exist in the dataset ####
indicator_matrix = indicator_matrix %>%
  rowwise() %>%
  mutate(
    prim_exist = ifelse(
      !is.na(tolower(primary_variables)),
      eval(parse(text = paste0(
        'all(', paste0(
          'all(', paste0('!is.null(data$', do.call('c', strsplit(tolower(primary_variables), "[;]")), ')', collapse = ' & '), ')',
          ',',
          paste0('all(', paste0('any(!is.na(data$', do.call('c', strsplit(tolower(primary_variables), "[;]")), '))', collapse = ' | '), ')')
        ), ')'
      ))),
      TRUE
    )
  ) %>%
  dplyr::filter(prim_exist == TRUE)

#### Reading derived variables matrix ####
dervar_matrix = read_excel(
  paste0('data_input/', country_ISO, '_input_matrix.xlsx'),
  sheet = 'derivedvars'
)

colnames(dervar_matrix) = tolower(colnames(dervar_matrix))
dervar_matrix = dervar_matrix %>%
                rowwise() %>%
                mutate(
                  all_exist = eval(parse(text = paste0(
                    'all(', paste0('!is.null(data$', do.call('c', strsplit(as.character(primary_vars), "[;]")), ')', collapse = ' & '), ')'
                  )))
                )

# Filter derived variables that exist
logic_exist_vars = dervar_matrix %>% dplyr::filter(all_exist == TRUE)

# Extract non-existent derived variables
list_nonexist_dervars = do.call('c', strsplit(
  (dervar_matrix %>%
     as.data.frame() %>%
     dplyr::filter(all_exist == FALSE) %>%
     dplyr::select(derived_vars))$derived_vars,
  "[;]"
))

# Extract list of generated derived variables (for processing report)

list_gen_dervars = na.omit(
  do.call('c', strsplit(
    (dervar_matrix %>%
       as.data.frame() %>%
       dplyr::filter(all_exist == TRUE) %>%
       dplyr::select(derived_vars))$derived_vars,
    "[;]"
)))

#### Exclude indicators with non-existent primary/derived variables ####
indicator_matrix = indicator_matrix %>%
  mutate(
    logic_condition_var = tolower(logic_condition_var),
    pop_subset = tolower(pop_subset),
    concat_var = paste0(logic_condition_var, ' ', pop_subset)
  )

if (!is.null(list_nonexist_dervars)) {
  indicator_matrix = indicator_matrix %>% rowwise %>%
    mutate(der_varsearch = search_vars(nonexist_vars = list_nonexist_dervars, logic_denom = concat_var)) %>%
    dplyr::filter(der_varsearch == FALSE)
} else {
  indicator_matrix = indicator_matrix
}

#### Processing glucose and cholesterol data ####
data = data %>%
  mutate(
    median_b5 = median(b5, na.rm = TRUE),
    median_b8 = median(b8, na.rm = TRUE)
  ) %>%
  rowwise() %>%
  mutate(
    b5 = ifelse(median_b5 > 35, b5 / 18.01, b5),
    b8 = ifelse(median_b8 > 12, b8 / 38.67, b8)
  )

#### Cleaning indicator matrix ####
indicator_matrix = indicator_matrix %>%
  separate(indicator, into = c("indicator_var", "indicator_short_desc"), sep = ":", remove = FALSE) %>%
  mutate(
    indicator_var = tolower(indicator_var),
    logic_condition_var = tolower(logic_condition_var),
    pop_subset = tolower(pop_subset)
  ) %>%
  rowwise() %>%
  mutate(
    has_log_exp = grepl('=|>|<', logic_condition_var),
    n_semicolons = str_count(logic_condition_var, ";") + 1,
    indicator_var = paste0(indicator_var, 1:n_semicolons, collapse = ';')
  )

#### Applying derivation logic for derived variables ####
cleaned_logic = gsub('\\r|\\n', '', logic_exist_vars$logic_exp)
eval(parse(text = paste0(cleaned_logic, sep = '\n')))

#### Assigning data for indicators ####
# Indicators without logical expressions
matrix_without_logexp = indicator_matrix %>% 
                        dplyr::filter(has_log_exp == FALSE)
indicators_without_logexp = do.call('c', strsplit(matrix_without_logexp$indicator_var, "[;]"))
vars_without_logexp = do.call('c', strsplit(matrix_without_logexp$logic_condition_var, "[;]"))

if (!is.null(vars_without_logexp)) {
  eval(parse(text = paste0('data$', indicators_without_logexp, '= data$', vars_without_logexp, sep = '\n')))
}

# Indicators with logical expressions
matrix_with_logexp = indicator_matrix %>% dplyr::filter(has_log_exp == TRUE)
indicators_with_logexp = do.call('c', strsplit(matrix_with_logexp$indicator_var, "[;]"))
vars_with_logexp = do.call('c', strsplit(matrix_with_logexp$logic_condition_var, "[;]"))

if (!is.null(indicators_with_logexp)) {
  eval(parse(text = paste0(
    'data = data %>% mutate(', indicators_with_logexp, '= ifelse(', vars_with_logexp, ',1,0))', sep = '\n'
  )))
  eval(parse(text = paste0('data$', indicators_with_logexp, '[is.na(data$', indicators_with_logexp, ')] = 0', sep = '\n')))
  eval(parse(text = paste0('data$', indicators_with_logexp, '[all(data$', indicators_with_logexp, ' == 0)] = NA', sep = '\n')))
}

################################################
#### Reading XML file for level information ####
level_xml_file = read_excel(
  paste0('data_input/', country_ISO, '_xls_form.xlsx'),
  'choices'
) %>%
  dplyr::filter(
    !is.na(`list name`) & !`list name` %in% c(
      'yn', 'yndk', 'yndkr', 'ynr', 'yndrr', 'yndw', 'time_dd', 'time_mm',
      'time_yyyy', 'psu', 'ssu', 'tsu'
    ) & name != 77 & name != 88
  )

colnames(level_xml_file) = tolower(colnames(level_xml_file))
level_xml_file = level_xml_file %>% dplyr::select(all_of(c("list name", 'name', paste0("label::", language))))

#### Mapping variables ####
extracted_variables = unique(level_xml_file$`list name`)
original_xml_file = read_excel(
  paste0('data_input/', country_ISO, '_xls_form.xlsx'),
  'survey'
) %>%
  dplyr::filter(!is.na(name)) %>%
  mutate(name = tolower(name))

matched_variables = original_xml_file %>%
  dplyr::filter(grepl("^select_one\\s|^select_multiple\\s", type)) %>%
  dplyr::mutate(extracted_var = sub("^select_(one|multiple)\\s", "", type)) %>%
  dplyr::filter(extracted_var %in% extracted_variables) %>%
  dplyr::select(extracted_var, matched_var = name)

colnames(matched_variables) = c('level_var', 'matched_var')
matched_variables$matched_var = gsub('_3', '', matched_variables$matched_var)

existing_vars_dataset = unique(intersect(matched_variables$matched_var, names(data)))
eval(parse(text = paste0('data$demog_', existing_vars_dataset, '= data$', existing_vars_dataset, sep = '\n')))

# Adjust factor levels based on XML
for (i in existing_vars_dataset) {
  sel_level_var = (matched_variables %>% 
                     dplyr::filter(matched_var == i) %>% 
                     dplyr::pull(level_var))
  var_attrs = level_xml_file %>% 
    dplyr::filter(`list name` == sel_level_var)
  
  eval(parse(text = paste0(
    'data$demog_', i, ' = factor(data$demog_', i, ', levels = c(',
    paste0('"', var_attrs$name, '"', collapse = ','), '), labels = c(',
    paste0('"', eval(parse(text = paste0('var_attrs$`label::', language, '`'))), '"', collapse = ','), '))'
  )))
}

#### Final adjustments and copying to analysis_data ####
data = data %>%
  as.data.frame() %>%
  mutate(
    #sex = factor(c1, levels = 1:2, labels = c('Men', 'Women')),
    demog_sex = sex,
    agerange = factor(agerange, levels = names(table(data[,'agerange'])), labels = names(table(data[,'agerange'])))
  )

analysis_data = data

######### Adjusting Stratification Variables #########
# Apply adjustments to both column and row stratification variables
col_strat_variable  = adjust_strat_vars(col_strat_variable, matched_variables)
row_strat_variables = adjust_strat_vars(row_strat_variables, matched_variables)

######### Reading Language Translations #########
# Load translations from Excel for a specific language
other_language = read_excel(
  paste0('data_input/', country_ISO, '_input_matrix.xlsx'),
  sheet = 'translations'
) %>% as.data.frame()
colnames(other_language) = tolower(colnames(other_language))
other_language = other_language[, c('item', language)]

#################################### 
#### Editing the Indicator Matrix to Drop Indicators with NA ####
####################################
# Initialize flag for excluding indicators with missing variables
indicator_matrix$excl_missing_ind = "no"

# Process the indicator matrix row-wise
indicator_matrix = indicator_matrix %>%
  rowwise() %>%
  mutate(
    # Identify indicators to reduce based on non-existent variables
    ind_reduce = search_vars(logic_denom = concat_var, nonexist_vars = none_exist_var),
    excl_missing_ind = ifelse(ind_reduce, "yes", excl_missing_ind),
    
    # Expand single-value fields to match number of semicolons
    type_length = length(strsplit(type, ";")[[1]]),
    type = if (type_length == 1) paste(rep(type, n_semicolons), collapse = ";") else type,
    pop_subset = if (type_length == 1) paste(rep(pop_subset, n_semicolons), collapse = ";") else pop_subset,
    
    # Determine positions of variables to remove
    remove_idx = {
      ind_split = strsplit(indicator_var, ";")[[1]]
      pop_split = strsplit(pop_subset, ";")[[1]]
      pop_split = ifelse(pop_split == "all", "TRUE", pop_split)
      
      var_missing = sapply(ind_split, function(v) all(is.na(data[[v]])))
      pop_missing = sapply(pop_split, function(expr) !with(data, any(eval(parse(text = expr)), na.rm = TRUE)))
      
      idx = which(var_missing | pop_missing)
      if (length(idx) == 0 || excl_missing_ind == "no") 999 else idx
    },
    
    # Apply cleaning using the helper function
    indicator_var       = clean_field(indicator_var, remove_idx),
    logic_condition_var = clean_field(logic_condition_var, remove_idx),
    pop_subset          = clean_field(pop_subset, remove_idx),
    subtitle1           = clean_field(subtitle1, remove_idx),
    subtitle2           = clean_field(subtitle2, remove_idx),
    type                = clean_field(type, remove_idx),
    
    # Calculate length of cleaned indicator_var
    len_ind_var = ifelse(is.na(indicator_var), 0, length(strsplit(indicator_var, ";")[[1]])),
    
    # Final cleaning of subtitle fields
    subtitle1 = ifelse(subtitle1 %in% c("logical(0)", "NA", ""), NA, subtitle1),
    subtitle2 = ifelse(subtitle2 %in% c("logical(0)", "NA", ""), NA, subtitle2)
  ) %>%
  dplyr::filter(len_ind_var > 0) %>%
  ungroup()

# Standardize column_strat to lowercase
indicator_matrix = indicator_matrix %>% mutate(column_strat = tolower(column_strat))

######### Preparing Section Parts for Narrative #########
section_part_edit = cbind(section_title = unique(indicator_matrix$section_title)) %>%
  as.data.frame() %>%
  mutate(Part = paste0('Part', 1:n()))

indicator_matrix = indicator_matrix %>%
  left_join(section_part_edit) %>%
  group_by(Part) %>%
  mutate(
    indicator_der_num = paste0('Indicator', 1:n()),
    part_ind_no = paste0(Part, '_', indicator_der_num)
  )

######### Reducing Matrix for Written Report #########
reporting_matrix = indicator_matrix %>%
  dplyr::filter(!is.na(section_title)) %>%
  mutate(
    order_ind = as.numeric(sub(".*_", "", sub_section_title)),
    arrange_num = paste0(order_ind, '_', part_ind_no),
    sub_section_text = sub("_.*$", "", sub_section_title)
  )

######### Generating Factsheet Matrix #########
fact_sheet_matrix = indicator_matrix %>%
  dplyr::filter(!is.na(factsheet_desc))

######### Clearing Temporary Folders #########
# Remove old temp folder and recreate it
if (dir.exists('temp')) {
  unlink('temp', recursive = TRUE, force = TRUE)
}
dir.create('temp', recursive = TRUE)

# Clear outputs and report files
#unlink(paste0(getwd(), '/outputs/*'))
#file.remove(paste0(getwd(), '/report outputs/combined_report.docx'))

##################
#################Generating data processing report

all_vars_with_err_codes <- unique(na_vars_list$variable)
all_vars_with_err_codes <- c(vars_with_77_or_88, all_vars_with_err_codes)
                        
non_exisiting_group_vars = ifelse(length(none_exist_var)>0, 
                                  paste0('The following variables are not in the dataset and have been generated and set to missing as they are required as part of a group of variables for analysis: ',
                                         paste0(none_exist_var,collapse = ',')),'All required grouped variables are available.')
vars_in_xml_not_dataset = ifelse(length(vars_not_indataset)>0, 
                                 paste0('The following variables are in the xls form and not in the dataset: ',
                                        paste0(vars_not_indataset,collapse = ',')),'Variables match between the xls form and the dataset')
resp_77_88  = ifelse(length(all_vars_with_err_codes)>0, 
                     paste0("The following variables have had don't know or refused codes (e.g. 666, 77, 777, 88, 99) set to missing: ",
                            paste0(all_vars_with_err_codes,collapse = ',')),"No variables have had don't know or refusal values set to missing in the dataset.")  

sec_derivarion  = ifelse(length(list_gen_dervars)>0, 
                         paste0('The following secondary variables were derived from primary variables in the dataset: ',
                                paste0(list_gen_dervars,collapse = ',')),'No additional variables were derived from primary variables in the dataset.') 

data_processing_report = cbind(Item = c('Completeness of grouped variables','Variables in xls form vs dataset','Variables with values set to NA',
                                        'Derived variables'),
                               Description = c(non_exisiting_group_vars,vars_in_xml_not_dataset,resp_77_88,sec_derivarion))


flex_processing_report = data_processing_report %>% as.data.frame() %>% flextable()%>%
  flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
  bold(part = 'header')%>%
  fontsize(size = 9 ,part = "all")%>%
  align(j = 2, align = 'center', part = 'header') %>% theme_vanilla() 

flex_processing_report <- set_table_properties(flex_processing_report, layout="fixed")
flex_processing_report <- width(flex_processing_report, 1:2, c(6.5*0.3, 6.5*0.7))


## Printing of data processing report
doc = officer::read_docx(paste0(getwd(),'/templates/data_processing_template.docx'))
#
doc = headers_replace_text_at_bkm(doc,"country",paste0(toupper(country),' ',survey_year))
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(width(flex_processing_report, width = dim(flex_processing_report)$widths*6.5/(flextable_dim(flex_processing_report)$widths)), pos = "on", align = 'left')

print(doc,target=paste0(getwd(),'/outputs/Data processing report.docx'))


