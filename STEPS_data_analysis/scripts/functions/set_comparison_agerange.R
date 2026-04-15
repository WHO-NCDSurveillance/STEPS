## 18-69 - 2 age groups ###
## Adjust code as needed to match the OVERLAPPING age ranges of the older and newer survey

combined_dataset = combined_dataset %>%
  mutate(
    
    ## -------------------------------------------------------
    ## Recreate the age group variable to ensure consistency
    ## across both survey rounds (overlapping age ranges)
    ## -------------------------------------------------------
    
    ## Overwriting agerange variable using age categories
    ## These categories ensure comparability between surveys
    agerange = case_when(age>=18 & age <45 ~1,
                         age>=45 & age <70 ~2),
    
    # Convert numeric codes to labelled factor levels
    agerange = factor(agerange,
                      levels=1:2,
                      labels=c('18-44','45-69')),
    
    ######
    ## -------------------------------------------------------
    ## Create combined sex and age group variable
    ## Used for stratified analysis in reporting tables
    ## -------------------------------------------------------
    
    sex_age = case_when(
      c1==1 & (agerange=='18-44') ~ 1,
      c1==1 & (agerange=='45-69') ~ 2,
      c1==2 & (agerange=='18-44') ~ 3,
      c1==2 & (agerange=='45-69') ~ 4),
    
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
      agerange=='45-59'|agerange=='60-69' ~ 2),
    
    # Convert binary age group to factor
    bin_age = factor(bin_age,
                     levels = 1:2,
                     labels = c('18-44','45-69'))
  ) %>%
  
## -------------------------------------------------------
## Remove observations with missing age category
## -------------------------------------------------------
dplyr::filter(!is.na(agerange))

# ## 18-69 - 4 age groups ###
# ## Adjust code as needed to match the OVERLAPPING age ranges of the older and newer survey
# 
# combined_dataset = combined_dataset %>%
#   mutate(
# 
#     ## -------------------------------------------------------
#     ## Recreate the age group variable to ensure consistency
#     ## across both survey rounds (overlapping age ranges)
#     ## -------------------------------------------------------
# 
#     ## Overwriting agerange variable using age categories
#     ## These categories ensure comparability between surveys
#     agerange = case_when(age>=18 & age <30 ~1,
#                          age>=30 & age <45 ~2,
#                          age>=45 & age <60 ~3,
#                          age>=60 & age <70 ~4),
# 
#     # Convert numeric codes to labelled factor levels
#     agerange = factor(agerange,
#                       levels=1:4,
#                       labels=c('18-29','30-44','45-59','60-69')),
# 
#     ######
#     ## -------------------------------------------------------
#     ## Create combined sex and age group variable
#     ## Used for stratified analysis in reporting tables
#     ## -------------------------------------------------------
# 
#     sex_age = case_when(
#       c1==1 & (agerange=='18-29'|agerange=='30-44') ~ 1,
#       c1==1 & (agerange=='45-59'|agerange=='60-69') ~ 2,
#       c1==2 & (agerange=='18-29'|agerange=='30-44') ~ 3,
#       c1==2 & (agerange=='45-59'|agerange=='60-69') ~ 4),
# 
#     # Convert to factor with descriptive labels
#     sex_age = factor(sex_age, levels = 1:4,
#                      labels = c('Men 18 - 44',
#                                 'Men 45 - 69',
#                                 'Women 18 - 44',
#                                 'Women 45 - 69')),
# 
#     ## -------------------------------------------------------
#     ## Create binary age category for simplified comparisons
#     ## Often used for reporting summary tables or stratifiers
#     ## -------------------------------------------------------
# 
#     bin_age = case_when(
#       agerange=='18-29'|agerange=='30-44' ~ 1,
#       agerange=='45-59'|agerange=='60-69' ~ 2),
# 
#     # Convert binary age group to factor
#     bin_age = factor(bin_age,
#                      levels = 1:2,
#                      labels = c('18-44','45-69'))
#   ) %>%
# 
#   ## -------------------------------------------------------
# ## Remove observations with missing age category
# ## -------------------------------------------------------
# dplyr::filter(!is.na(agerange))

## 18-64 ###

### Adjust code as needed to match the OVERLAPPING age ranges of the older and newer survey

# combined_dataset = combined_dataset %>%
#   mutate(
#     
#     ## -------------------------------------------------------
#     ## Recreate the age group variable to ensure consistency
#     ## across both survey rounds (overlapping age ranges)
#     ## -------------------------------------------------------
#     
#     ## Overwriting agerange variable using age categories
#     ## These categories ensure comparability between surveys
#     agerange = case_when(age>=18 & age <30 ~1,
#                          age>=30 & age <45 ~2,
#                          age>=45 & age <65 ~3),
# 
#     # Convert numeric codes to labelled factor levels
#     agerange = factor(agerange,
#                       levels=1:3,
#                       labels=c('18-29','30-44','45-64')),
#     
#     ######
#     ## -------------------------------------------------------
#     ## Create combined sex and age group variable
#     ## Used for stratified analysis in reporting tables
#     ## -------------------------------------------------------
#     
#     sex_age = case_when(
#       c1==1 & (agerange=='18-29'|agerange=='30-44') ~ 1,
#       c1==1 & (agerange=='45-64') ~ 2,
#       c1==2 & (agerange=='18-29'|agerange=='30-44') ~ 3,
#       c1==2 & (agerange=='45-64') ~ 4),
#     
#     # Convert to factor with descriptive labels
#     sex_age = factor(sex_age, levels = 1:4,
#                      labels = c('Men 18 - 44',
#                                 'Men 45 - 64',
#                                 'Women 18 - 44',
#                                 'Women 45 - 64')),
#     
#     ## -------------------------------------------------------
#     ## Create binary age category for simplified comparisons
#     ## Often used for reporting summary tables or stratifiers
#     ## -------------------------------------------------------
#     
#     bin_age = case_when(
#       agerange=='18-29'|agerange=='30-44' ~ 1,
#       agerange=='45-64' ~ 2),
#     
#     # Convert binary age group to factor
#     bin_age = factor(bin_age,
#                      levels = 1:2,
#                      labels = c('18-44','45-64'))
#   ) %>%
#   
#   ## -------------------------------------------------------
# ## Remove observations with missing age category
# ## -------------------------------------------------------
# dplyr::filter(!is.na(agerange))




#### 15-69 ###

# combined_dataset = combined_dataset %>%
#   mutate(
#     ##Overwriting agerange variable
#     agerange = case_when(age>=15 & age <25 ~1,age>=25 & age <35 ~2,age>=35 & age <45 ~3,
#                          age>=45 & age <55 ~4,age>=55 & age <65 ~5,age>=65 & age <70 ~6),
#     agerange = factor(agerange,levels=1:6, labels=c('15-24','25-34','35-44','45-54','55-64','65-69')),
#     ######
#     sex_age = case_when(c1==1 & (agerange=='15-24'|agerange=='24-34'|agerange=='35-44') ~ 1,
#                         c1==1 & (agerange=='45-54'|agerange=='55-64'|agerange=='65-69') ~ 2,
#                         c1==2 & (agerange=='15-24'|agerange=='24-34'|agerange=='35-44') ~ 3,
#                         c1==2 & (agerange=='45-54'|agerange=='55-64'|agerange=='65-69') ~ 4),
#     sex_age = factor(sex_age, levels = 1:4, 
#                      labels = c('Men 15 - 44','Men 45 - 69','Women 15 - 44','Women 45 - 69')),
#     bin_age = case_when(agerange=='15-24'|agerange=='24-34'|agerange=='35-44' ~ 1,
#                         agerange=='45-54'|agerange=='55-64'|agerange=='65-69' ~ 2),
#     bin_age = factor(bin_age,levels = 1:2, labels = c('15-44','45-69')))%>%
#   
#   dplyr::filter(!is.na(agerange))



#### 18+ ###

# combined_dataset = combined_dataset %>%
#   mutate(
#     
#     ## -------------------------------------------------------
#     ## Recreate the age group variable to ensure consistency
#     ## across both survey rounds (overlapping age ranges)
#     ## -------------------------------------------------------
#     
#     ## Overwriting agerange variable using age categories
#     ## These categories ensure comparability between surveys
#     agerange = case_when(age>=18 & age <30 ~1,
#                          age>=30 & age <45 ~2,
#                          age>=45 & age <60 ~3,
#                          age>=60 ~4),
#     
#     # Convert numeric codes to labelled factor levels
#     agerange = factor(agerange,
#                       levels=1:4,
#                       labels=c('18-29','30-44','45-59','60+')),
#     
#     ######
#     ## -------------------------------------------------------
#     ## Create combined sex and age group variable
#     ## Used for stratified analysis in reporting tables
#     ## -------------------------------------------------------
#     
#     sex_age = case_when(
#       c1==1 & (agerange=='18-29'|agerange=='30-44') ~ 1,
#       c1==1 & (agerange=='45-59'|agerange=='60+') ~ 2,
#       c1==2 & (agerange=='18-29'|agerange=='30-44') ~ 3,
#       c1==2 & (agerange=='45-59'|agerange=='60+') ~ 4),
#     
#     # Convert to factor with descriptive labels
#     sex_age = factor(sex_age, levels = 1:4,
#                      labels = c('Men 18 - 44',
#                                 'Men 45 - 69',
#                                 'Women 18 - 44',
#                                 'Women 45 - 69')),
#     
#     ## -------------------------------------------------------
#     ## Create binary age category for simplified comparisons
#     ## Often used for reporting summary tables or stratifiers
#     ## -------------------------------------------------------
#     
#     bin_age = case_when(
#       agerange=='18-29'|agerange=='30-44' ~ 1,
#       agerange=='45-59'|agerange=='60+' ~ 2),
#     
#     # Convert binary age group to factor
#     bin_age = factor(bin_age,
#                      levels = 1:2,
#                      labels = c('18-44','45-69'))
#   ) %>%
#   
#   ## -------------------------------------------------------
# ## Remove observations with missing age category
# ## -------------------------------------------------------
# dplyr::filter(!is.na(agerange))
