###############################
# Get the source directory of the current executing script
srcdir <- getSrcDirectory(function(){})[1]

# Check if srcdir is empty, then use the current active document's directory
if (srcdir == "") {
  srcdir <- dirname(rstudioapi::getActiveDocumentContext()$path)
}

# Move one level up from the current srcdir
parent_dir <- dirname(srcdir)

# Set the working directory to the parent directory
setwd(parent_dir)
rm(srcdir, parent_dir)
###############################

# Remove all objects from the current R environment
rm(list = ls())

# Set the working directory to the specified path
# setwd("~/Documents/Transferred files/Data/data part 4/WHO/Task order 2/Scripts v5")

# Set survey options to handle 'lonely' primary sampling units (PSUs)
# 'certainty' ensures lonely PSUs are treated with certainty weights
options(survey.lonely.psu="certainty")
# Adjust for lonely PSUs in domain analyses
options(survey.adjust.domain.lonely=TRUE)

### Loading required packages
# List of packages needed for the analysis
list_packages = c('openxlsx','readxl','tidyverse','survey','stringr',
                  'officer','flextable','readstata13','dplyr','haven','polyglotr')

# Load each package in the list
eval(parse(text = paste0('library(',list_packages,')', sep='\n')))

######################## Specifying analysis inputs ########################################

### Define the country and year of the survey
country = 'Eswatini'      # Country for the survey
survey_year = 2024       # Year of the survey

### Define stratification variables for the analysis
col_strat_variable = c('sex')              # Column stratifier (e.g., sex)
row_strat_variables = c('agerange','ruralurban')  # Row stratifiers (e.g., age range, nationality)
row_strat_variable_titles =c("Age Categories (Years)",'Location')  # Titles for row stratifiers
vars_exempt_77_88 = c('')                  # Variables exempt from specific conditions (e.g., missing codes)

### Denominator limit: Minimum sample size required for estimating point estimates and confidence intervals
denom_limit = 1

### Language setting for the analysis
language =c('ENGLISH','FRENCH','ARABIC', 'SPANISH','RUSSIAN','OTHER')[1]  # Select language
if (language =='OTHER'){language = 'SPECIFY'} else{}  # Handle 'OTHER' language option
language = tolower(language)  # Convert language to lowercase for consistency

### Inputs for cardiovascular disease (CVD) risk computation
country_ISO = 'SWZ'  # ISO code for the country

### Check if the specified ISO code exists in the reference dataset
ISO_existence = country_ISO %in% unique(read_dta('data_input/risk_ref_data.dta')$ccode)
# If the ISO code does not exist, stop the script and display an error message
if(!ISO_existence)stop('The supplied ISO code does not exist in the reference dataset for cvd risk calculation')

################# Running scripts #################

# Source external R scripts to process data and generate outputs
source('scripts/1_data_processing_script.R')  # Data processing script
source('scripts/2_databook_generation.R')     # Databook generation script
source('scripts/3_factsheet_generation.R')    # Factsheet generation script