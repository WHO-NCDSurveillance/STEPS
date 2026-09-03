### Adjust code as needed to match the OVERLAPPING age ranges of the older and newer survey

combined_dataset = combined_dataset %>%
  mutate(
    
    ## -------------------------------------------------------
    ## Recreate the age group variable to ensure consistency
    ## across both survey rounds (overlapping age ranges)
    ## -------------------------------------------------------
    
    ## Overwriting agerange variable using age categories
    ## These categories ensure comparability between surveys
    agerange = case_when(age>=18 & age <30 ~1,
                         age>=30 & age <45 ~2,
                         age>=45 & age <60 ~3,
                         age>=60 ~4),
    
    # Convert numeric codes to labelled factor levels
    agerange = factor(agerange,
                      levels=1:4,
                      labels=c('18-29','30-44','45-59','60+')),
    
    ######
    ## -------------------------------------------------------
    ## Create combined sex and age group variable
    ## Used for stratified analysis in reporting tables
    ## -------------------------------------------------------
    
    sex_age = case_when(
      sex == 'Men' & (agerange=='18-29'|agerange=='30-44') ~ 1,
      sex == 'Men' & (agerange=='45-59'|agerange=='60+') ~ 2,
      sex == 'Women' & (agerange=='18-29'|agerange=='30-44') ~ 3,
      sex == 'Women' & (agerange=='45-59'|agerange=='60+') ~ 4),
    
    # Convert to factor with descriptive labels
    sex_age = factor(sex_age, levels = 1:4, 
                     labels = c('Men 18 - 44',
                                'Men 45 - 69',
                                'Women 18 - 44',
                                'Women 45 - 69')),
    
    ## -------------------------------------------------------
    ## Create binary age category for simplified comparisons
    ## Often used for reporting summary tables or stratifiers
    ## -------------------------------------------------------
    
    bin_age = case_when(
      agerange=='18-29'|agerange=='30-44' ~ 1,
      agerange=='45-59'|agerange=='60+' ~ 2),
    
    # Convert binary age group to factor
    bin_age = factor(bin_age,
                     levels = 1:2,
                     labels = c('18-44','45-69'))
  ) %>%
  
  ## -------------------------------------------------------
## Remove observations with missing age category
## -------------------------------------------------------
dplyr::filter(!is.na(agerange))
