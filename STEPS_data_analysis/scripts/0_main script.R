# Clear workspace
rm(list = ls())

########################################
# Set working directory
########################################

# Get the directory of the currently executing script
srcdir = getSrcDirectory(function(){})[1]

# If srcdir is empty, use the active RStudio document's directory
if (srcdir == "") {
  srcdir = dirname(rstudioapi::getActiveDocumentContext()$path)
}

# Move one level up from the current srcdir
parent_dir = dirname(srcdir)

# Set working directory to the parent directory
setwd(parent_dir)

# Clean up temporary variables
rm(srcdir, parent_dir)

# generate outputs directory if it doesn't exist
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Create "outputs/report sections" directory if it doesn't exist
if (!dir.exists("outputs/report sections")) {
  dir.create("outputs/report sections", recursive = TRUE)
}

########################################
# Load required packages
########################################

package.list = c(
  'ggplot2','ggtext','dplyr','tidyr','ggimage','ggpubr','grid','cowplot','officer',
  'flextable','readxl','writexl','purrr','stringr','openai','httr','jsonlite',
  'openxlsx','tidyverse','survey','haven','polyglotr','magrittr','knitr','docxtractr',
  'future.apply','future','patchwork'
)

# Load each package
invisible(lapply(package.list, function(pkg) library(pkg, character.only = TRUE)))

########################################
# Survey options
########################################

options(survey.lonely.psu = "adjust") # Handle lone PSUs
options(survey.adjust.domain.lonely = TRUE) # Adjust for lonely PSUs in domain analyses

########################################
# API keys and general settings
########################################
# Insert your API key below if you wish to generate a draft report or draft 
# comparison analysis report (4th and 7th scripts). 
Sys.setenv(API_KEY = "INSERT API HERE")

# Survey and report settings
survey_year = 2024
previous_survey_year = 2017
country = "country name"
report_signf = 'Yes'

# Language selection
language = c('ENGLISH','FRENCH','ARABIC','SPANISH','RUSSIAN','OTHER')[1]  # Default: English
if (language == 'OTHER') language = 'SPECIFY'
language = tolower(language)  # Standardize to lowercase

########################################
# Inputs for cardiovascular disease (CVD) risk computation
########################################

country_ISO = 'ISO'  # ISO code for the country

# Check if the specified ISO code exists in the reference dataset
if (country_ISO=="ISO") stop('Please set country_ISO to the 3-letter ISO code of your country.')
CVD_ISO = country_ISO # in rare cases, CVD_ISO may be set to a similar country's ISO code if there is no CVD risk chart for the country
ISO_existence = CVD_ISO %in% unique(read_dta('scripts/functions/risk_ref_data.dta')$ccode)
if (!ISO_existence) stop('The supplied ISO code does not exist in the reference dataset for CVD risk calculation')

########################################
# Stratification and variable settings
########################################

col_strat_variable = c('sex')  # Column stratifier
row_strat_variables = c('agerange')  # Row stratifiers
row_strat_variable_titles = c("Age Categories (Years)")
# row_strat_variables = c('agerange','nationality')  # Row stratifiers (e.g., age range, nationality)
# row_strat_variable_titles =c("Age Categories (Years)","Nationality")  # Titles for row stratifiers
# row_strat_variables = c('governorate')  # Row stratifiers (e.g., age range, nationality)
# row_strat_variable_titles =c("Governorate")  # Titles for row stratifiers
vars_exempt_77_88 = c('')  # Variables exempt from missing code handling

########################################
# Parallel computation setup
########################################

cores_detected = parallel::detectCores()
analysis_cores = max(1, floor(cores_detected / 2))
plan(multisession, workers = analysis_cores)

# Ensure write permissions for temp folder (Windows exception)
if (.Platform$OS.type != "windows") Sys.chmod("temp", mode = "0777")

########################################
# Denominator limit
########################################

denom_limit = 50  # Minimum sample size for estimates

########################################
# Analysis Scripts: Usage Instructions
########################################

# This project contains a set of scripts used to process survey data and
# generate outputs such as data books, fact sheets, narratives, infographics,
# and comparative analyses.
#
# IMPORTANT:
# The analysis setup must be completed first, and the data for the current
# survey must be processed before most outputs can be generated.

########################################
# Step 1: Process data for the current survey (REQUIRED)
########################################
# This script prepares the analytical dataset for the current survey.
# It must be run before generating any single-survey outputs.
source("scripts/1_data_processing_script.R", local = TRUE)

########################################
# Step 2: Generate single-survey outputs
########################################
# After running Script 1, the following scripts can be run independently.
# Each script produces a different output and does not require the others.

# Data book for the current survey
source("scripts/2_databook_generation.R", local = TRUE)

# Fact sheet for the current survey
source("scripts/3_factsheet_generation.R", local = TRUE)

# Report numbers and narrative text
source("scripts/4_generating_numbers_plus_narrative.R", local = TRUE)

# Data used to produce infographics
source("scripts/5_generating_data_for_infographics.R", local = TRUE)

########################################
# Step 3: Prepare comparative analysis dataset (REQUIRED)
########################################
# This script processes datasets needed for comparative analysis
# across surveys and builds the required comparison matrices.

source("scripts/6_comparative_data_and_matrix_processing.R", local = TRUE)

########################################
# Step 4: Generate comparative outputs
########################################
# After running Script 6, the following scripts can be run independently.

# Comparative analysis for report sections
source("scripts/7_comparative_analysis.R", local = TRUE)

# Comparative fact sheet
source("scripts/8_comparative_factsheet.R", local = TRUE)
