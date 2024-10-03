library(readxl)
library(writexl)

setwd("D:/temp")

# Replace 'data.xlsx' with the actual file path
data <- read_excel("data.xlsx")
############# sbp;raisedsbp
data = data %>% rowwise %>%mutate(sbp = case_when((is.na(m4a)|!is.na(m4a)) & !is.na(m5a) & !is.na(m6a) ~ (m5a + m6a) / 2,
                         !is.na(m4a) & is.na(m5a) & !is.na(m6a) ~ (m4a + m6a) / 2,
                         !is.na(m4a) & !is.na(m5a) & is.na(m6a) ~ (m4a + m5a) / 2,TRUE ~ NA_real_),raisedsbp = case_when(sbp < 140 ~ 1,sbp >= 140 & sbp < 160 ~ 2,sbp >= 160 ~ 3,TRUE ~ NA_integer_))

############## dbp;raiseddbp
data = data %>%rowwise %>%mutate(dbp = case_when((is.na(m4b)|!is.na(m4b)) & !is.na(m5b) & !is.na(m6b) ~ (m5b + m6b) / 2,
                                                 !is.na(m4b) & is.na(m5b) & !is.na(m6b) ~ (m4b + m6b) / 2,
                                                 !is.na(m4b) & !is.na(m5b) & is.na(m6b) ~ (m4b + m5b) / 2,TRUE ~ NA_real_),raiseddbp = case_when(dbp < 90 ~ 1,dbp >= 90 & dbp < 100 ~ 2,dbp >= 100 ~ 3,TRUE ~ NA_integer_))

############# cln_bp
data = data %>% rowwise %>%mutate(cln_bp = ifelse((!is.na(sbp) | !is.na(dbp)),1,2),
                                  cln_bp = replace(cln_bp,cln_bp==1 & h3==1 & (h2a==2 | is.na(h2a)),2))
############# treatment raisedbp_140_90_meds
data = data %>% rowwise %>%
  mutate(treatment = case_when(h3 == 1 ~ 1,
  h3 == 2 ~ 2),
  treatment = replace(treatment, treatment==1 & cln_bp==2,2),
  treatment = replace(treatment, is.na(treatment) | cln_bp==2,3),
  raisedbp_140_90 = if_else(raisedsbp >= 2 | raiseddbp >= 2, 1,2),
  raisedbp_140_90_meds = if_else(raisedbp_140_90 == 1 | treatment == 1, 1, 2))

############# raisedbp_160_100_meds
data = data %>% rowwise %>%mutate(raisedbp_160_100 = if_else(raisedsbp == 3 | raiseddbp == 3,1,2),raisedbp_160_100_meds = if_else(raisedbp_160_100==1 | treatment == 1,1,2))
#############




# Replace 'new_data.xlsx' with your desired new file name
write_xlsx(data, "temp_data.xlsx")
