rm(list = ls())
###
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

####
package.list = c('ggplot2','ggtext','dplyr','tidyr','ggimage','ggpubr','grid','cowplot','officer',
                 'flextable','readxl','writexl','purrr','stringr','openai','httr','jsonlite',
                 'openxlsx','tidyverse','survey','haven','polyglotr','magrittr','knitr','docxtractr',
                 'future.apply','future','patchwork','cowplot','magrittr')

# Load each package in the list
eval(parse(text = paste0('library("',package.list,'")', sep ='\n')))
###
# 'certainty' ensures lonely PSUs are treated with certainty weights
options(survey.lonely.psu="adjust")
# Adjust for lonely PSUs in domain analyses
options(survey.adjust.domain.lonely=TRUE)

################################Setup########################################
#GROQ - LLAMA key
Sys.setenv(GROQ_API_KEY = "INDICATE API KEY HERE")
#year, country, and language
survey_year = 2024
country = "ISO"
previous_survey_year = 2017
report_signf = 'ISO'
###
### Language setting for the analysis
language =c('ENGLISH','FRENCH','ARABIC', 'SPANISH','RUSSIAN','OTHER')[1]  # Select language
if (language =='OTHER'){language = 'SPECIFY'} else{}  # Handle 'OTHER' language option -- ensure the language specified matches that in the xls file - choices
language = tolower(language)  # Convert language to lowercase for consistency

### Inputs for cardiovascular disease (CVD) risk computation
country_ISO = 'ISO'  # ISO code for the country

### Check if the specified ISO code exists in the reference dataset
##default is CHE when country_ISO = 'ISO'
if(country_ISO == 'ISO'){check_ISO = 'CHE'} else{check_ISO = country_ISO}
ISO_existence = check_ISO %in% unique(read_dta('scripts/functions/risk_ref_data.dta')$ccode)
# If the ISO code does not exist, stop the script and display an error message
if(!ISO_existence)stop('The supplied ISO code does not exist in the reference dataset for cvd risk calculation')

### Define stratification variables for the analysis
col_strat_variable = c('sex')              # Column stratifier (e.g., sex)
row_strat_variables = c('agerange')  # Row stratifiers (e.g., age range, nationality)
row_strat_variable_titles =c("Age Categories (Years)")  # Titles for row stratifiers
vars_exempt_77_88 = c('')                  # Variables exempt from specific conditions (e.g., missing codes)

###Setting for paralle computation
cores_detected <- parallel::detectCores()
analysis_cores <- ifelse(cores_detected == 1, 1, cores_detected - round(cores_detected/2)) # leave cores free
plan(multisession, workers = analysis_cores)  
####
####
# Check if temp folder exists in the current working directory
if (!dir.exists('temp')) {dir.create('temp') }
### Denominator limit: Minimum sample size required for estimating point estimates and confidence intervals
denom_limit = 1
###Generating tables and exporting to txt files
source('scripts/1_data_processing_script.R', local = T)
source('scripts/2_databook_generation.R', local = T)
source('scripts/3_factsheet_generation.R', local = T)
###
source('scripts/4_generating_numbers_for_narrative.R', local = T)
source('scripts/5_revised_report_narrative.R', local = T)
source('scripts/6_generating data for infographics.R', local = T)
source('scripts/7_comparative_analysis.R', local = T)
source('scripts/8_comparative_factsheet.R', local = T)
