############################################################
## SCRIPT FOR PREPARING DATASETS AND MATRIX FOR COMPARATIVE 
## ANALYSIS BETWEEN SURVEY ROUNDS
############################################################

# Dataset from the current survey round
dataset1 = analysis_data

# Load processing script for the second survey dataset
# This script creates `analysis_dataset2`
source('scripts/functions/processing_dataset2.R', local = TRUE)

# Dataset from the previous survey round
dataset2 = analysis_dataset2

# -----------------------------------------------------------
# Identify variables that exist in both datasets
# -----------------------------------------------------------

# Extract the intersection of variable names across both surveys
# This ensures comparability when merging datasets
common_variables = intersect(names(dataset1), names(dataset2))

### ----------------------------------------------------------
### Convert dataset2 variable types to match dataset1
### ----------------------------------------------------------

# Extract variable classes from dataset1 for all common variables
# These will serve as the target classes
target_classes = sapply(dataset1[common_variables], class)

# Convert dataset2 variables to match the class types in dataset1
# This avoids issues during merging or analysis
dataset2 = dataset2 %>%
  mutate(across(
    all_of(common_variables),
    ~ convert_to_target_class(., target_classes[cur_column()])
  ))

############# Normalisation #########################

# -----------------------------------------------------------
# Prepare dataset1 for combined comparative analysis
# -----------------------------------------------------------

dataset1 = dataset1 %>%
  # Keep only variables shared with dataset2
  dplyr::select(all_of(common_variables)) %>%
  
  # Add survey year variable to identify the survey round
  dplyr::mutate(svy_year = survey_year) %>%
  
  # Normalize survey weight variables
  normalize_wstep_vars(postfix = "_norm")

##
# -----------------------------------------------------------
# Prepare dataset2 similarly
# -----------------------------------------------------------

dataset2 = dataset2 %>%
  # Keep only common variables
  dplyr::select(all_of(common_variables)) %>%
  
  # Assign survey year for previous survey
  dplyr::mutate(svy_year = previous_survey_year) %>%
  
  # Normalize survey weight variables
  normalize_wstep_vars(postfix = "_norm")

# -----------------------------------------------------------
# Combine the two survey datasets
# -----------------------------------------------------------

combined_dataset = full_join(dataset1, dataset2) %>%
  
  # Create survey-year-specific strata identifiers
  mutate(strata_year = interaction(svy_year, stratum),
         
         # Create survey-year-specific PSU identifiers
         psu_year = interaction(svy_year, psu))

### ----------------------------------------------------------
### Harmonizing age ranges across survey rounds
### ----------------------------------------------------------

# NOTE:
# Age range definitions may differ between survey rounds.
# This script adjusts age ranges to ensure comparability.
source('scripts/functions/set_comparison_agerange.R', local = TRUE)


### ----------------------------------------------------------
### Deriving comparative reporting matrix for narrative report
### ----------------------------------------------------------

# Extract reporting matrix from the second survey configuration
reporting_matrix_v2 = indicator_matrix_v2 %>% dplyr::filter(!is.na(section_title))

# Identify indicators present in both survey matrices
common_ind_desc = intersect(reporting_matrix$indicator,reporting_matrix_v2$indicator)

# Filter the current reporting matrix to retain only common indicators
comparative_reporting_matrix = reporting_matrix %>%
  dplyr::filter(eval(parse(text = paste0('indicator == "',common_ind_desc,'"', collapse = '|'))))

### ----------------------------------------------------------
### Deriving comparative fact sheet matrix
### ----------------------------------------------------------

# Extract factsheet configuration from the second survey matrix
fact_sheet_matrix_v2 = indicator_matrix_v2 %>% dplyr::filter(!is.na(factsheet_desc)) 

# Identify factsheet indicators common across both surveys
common_factsheet_ind_desc = intersect(fact_sheet_matrix$indicator,fact_sheet_matrix_v2$indicator)

# Filter factsheet matrix to keep only indicators available in both surveys
comparative_fact_sheet_matrix = fact_sheet_matrix %>%
  dplyr::filter(eval(parse(text = paste0('indicator == "',common_factsheet_ind_desc,'"', collapse = '|'))))
