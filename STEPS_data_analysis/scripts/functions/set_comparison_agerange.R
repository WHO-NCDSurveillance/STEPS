###adjust code as needed to match the OVERLAPPING age ranges of the older and newer survey
combined_dataset = combined_dataset %>%
  mutate(
    ##Overwriting agerange variable
    agerange = case_when(age>=18 & age <29 ~1,age>=30 & age <44 ~2,
                         age>=45 & age <59 ~3,age>=60 & age <69 ~4),
    agerange = factor(agerange,levels=1:4, labels=c('18-29','30-44','45-59','60-69')),
    ######
    sex_age = case_when(sex == 'Men' & (agerange=='18-29'|agerange=='30-44') ~ 1,
                        sex == 'Men' & (agerange=='45-59'|agerange=='60-69') ~ 2,
                        sex == 'Women' & (agerange=='18-29'|agerange=='30-44') ~ 3,
                        sex == 'Women' & (agerange=='45-59'|agerange=='60-69') ~ 4),
    sex_age = factor(sex_age, levels = 1:4, 
                     labels = c('Men 18 - 44','Men 45 - 69','Women 18 - 44','Women 45 - 69')),
    bin_age = case_when(agerange=='18-29'|agerange=='30-44' ~ 1,
                        agerange=='45-59'|agerange=='60-69' ~ 2),
    bin_age = factor(bin_age,levels = 1:2, labels = c('18-44','45-69')))%>%
  dplyr::filter(!is.na(agerange))