############################################################
## SCRIPT FOR GENERATING ANALYSIS FOR INFOGRAPHICS 
############################################################

# -----------------------------------------------------------
# Define paths for Excel template and output workbook
# -----------------------------------------------------------

# Path to the original Excel template containing chart structures
file = 'templates/template_data_fromR.xlsx'

# Path where the updated Excel file will be saved
save_file = 'templates/data_fromR.xlsx'

# -----------------------------------------------------------
# Load the existing Excel workbook and required sheets
# -----------------------------------------------------------

# Load the workbook while preserving formulas and formatting
wb = loadWorkbook(file)

# Read the sheet containing infographic data structure
# skipEmptyRows = FALSE ensures row alignment is preserved
existing = read.xlsx(file, 'data', skipEmptyRows = FALSE)

# Read language translations for infographic labels
# Only extract the column corresponding to the selected language
transl_lang = read.xlsx(file, 'languages', skipEmptyRows = FALSE)[,language]

## ----------------------------------------------------------
## Identify indicators used in the infographic template
## ----------------------------------------------------------

# Extract all unique indicator variables used in the template
infographic_vars = na.omit(unique(existing$ind_level))

# -----------------------------------------------------------
# Filter indicator matrix to keep only relevant infographic indicators
# -----------------------------------------------------------

infographic_matrix = indicator_matrix %>% rowwise() %>%
  mutate(
    # Search function identifies indicator groups matching infographic variables
    ind_info = infog_search_vars(ind_group = indicator_var, ind_var = infographic_vars)) %>%
               dplyr::filter(ind_info == TRUE)

### ----------------------------------------------------------
### Define stratifiers used in infographic calculations
### ----------------------------------------------------------

# Combine column and row stratifiers with additional demographic stratifiers
all_stratifiers = c(col_strat_variable,row_strat_variables,'sex_age','bin_age')

# -----------------------------------------------------------
# Compute indicator values for infographic indicators
# -----------------------------------------------------------

# Generate indicator values by applying gen_numbers_fn across sections

indicator_results <- do.call(
  rbind,
  lapply(unique(infographic_matrix$section), gen_numbers_fn)) %>%
  mutate(col2 = as.numeric(col2))
indicator_results <- unique(indicator_results)

# indicator_results = do.call('rbind',
#                             future_lapply(unique(infographic_matrix$section),
#                                           gen_numbers_fn))%>%
#                     mutate(col2 = as.numeric(col2))

##
# Rename columns for clarity and compatibility with template keys
colnames(indicator_results)=c("ind_level","var_strat_level","value")

##################### Exporting the numbers to Excel template with charts ###########################

# -----------------------------------------------------------
# Update Excel template values based on matching indicator keys
# -----------------------------------------------------------

# Match computed results with existing template rows
updated = existing %>% 
  left_join(indicator_results, by = c("ind_level", "var_strat_level"), 
            suffix = c("", "_new")) %>%
  
  # Replace existing values with newly computed values when available
  mutate(value = ifelse(!is.na(value_new), value_new, value)) %>% 
  
  # Remove temporary column
  dplyr::select(-c(value_new))

# Extract updated values column
new_values = updated$value

# -----------------------------------------------------------
# Write updated values into the Excel template
# -----------------------------------------------------------

# Write updated numeric values into column 4 (value column)
# startRow = 2 preserves header row
writeData(wb, sheet = "data", x = new_values, 
          startCol = 4, startRow = 2, colNames = FALSE, rowNames = FALSE)

# -----------------------------------------------------------
# Write translated indicator labels into the Excel template
# -----------------------------------------------------------

# Insert translated labels into column 3
writeData(wb, sheet = "data", x = transl_lang, 
          startCol = 3, startRow = 2, colNames = FALSE, rowNames = FALSE)

# -----------------------------------------------------------
# Save updated workbook without modifying existing formulas
# -----------------------------------------------------------

saveWorkbook(wb, save_file, overwrite = TRUE)

file.copy(
  from = "templates/infographic charts v2.xlsx",
  to   = paste0('outputs/', country_ISO, '-', survey_year, '-infographic charts v2.xlsx'),
  overwrite = TRUE
)

file.copy(
  from = "templates/data_fromR.xlsx",
  to   = "outputs/data_fromR.xlsx",
  overwrite = TRUE
)




