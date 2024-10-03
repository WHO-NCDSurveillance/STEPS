library(readxl)
library(writexl)

setwd("D:/temp")
# Replace 'data.xlsx' with the actual file path
data <- read_excel("data.xlsx")

# Define constants
GDP <- 3824
dollarconversion <- 15000

data$priceper100packs <- (data$tp7 / data$tp6) * 2000

# Calculate percent GDP for each row in the dataframe
data$percentGDP <- (data$priceper100packs / (GDP * dollarconversion)) * 100

# Display the updated dataframe
print(data)


# Replace 'new_data.xlsx' with your desired new file name
write_xlsx(data, "temp_data.xlsx")
