library(readxl)
library(writexl)

setwd("D:/temp")
# Replace 'data.xlsx' with the actual file path
data <- read_excel("data.xlsx")

data = data %>% rowwise %>%mutate(b5 = b5 / 18.01)

############ cln_gluc
data = data %>% rowwise %>%mutate(fasted = if_else(b1 == 2, 1, 2),
         B5CLN = if_else(b5 >= 1 & b5 <= 35, 1, 2),
         cln_gluc = if_else(fasted == 1 & B5CLN == 1 & valid == 1, 1, 2))

############ blood_gluclass
data = data %>% rowwise %>%mutate(blood_gluclass = case_when(
  b5 < 6.1 ~ 1,
  b5 >= 6.1 & b5 < 7.0 ~ 2,
  b5 >= 7.0  ~ 3
))
         
########### diabtr
data = data %>% rowwise %>%mutate(diabtr = if_else(h8 == 1 | h9 == 1, 1, 2))


####### raisedbg_or_meds
data = data %>% rowwise %>%mutate(raisedbg_or_meds = if_else((blood_gluclass == 3  | diabtr == 1) & cln_gluc == 1, 1, 2))

####### clnall_gluc!!!!!!!!!!!!!!!!!!!!!!!!!!!
data = data %>% rowwise %>%mutate(clnall_gluc = if_else(fasted == 1  & valid == 1, 1, 2))

############# diagn
data = data %>% rowwise %>%mutate(diagn = case_when(
  b5 >= 7.0 & b5 <= 35 & (h7a == 2 | is.na(h7a)) ~ 1,
  h7a == 1 & diabtr == 2 ~ 2,
  h7a == 1 & diabtr == 1 ~ 3,
  (h7a == 2 | is.na(h7a)) & b5 < 7.0 & b5 >= 1 ~ 4))



############# b8mg;cln_b8
data = data %>% rowwise %>% mutate(cln_b8 = ifelse(b8>=2 & b8<=12 & valid==1,1,2),
   b8 = ifelse(b8>=2 & b8<=12,b8,NA),
   b8mg=(b8*38.67))

# Replace 'new_data.xlsx' with your desired new file name
write_xlsx(data, "temp_data.xlsx")
