getwd()
data <- read.csv(file = "NGA_RNA.csv")
cleaninginspectoR::find_outliers(data) %>% write.csv("outliers_fsp_irQ.csv")

library(cleaninginspectoR)

find_other_responses(data)

find_duplicates(data, "X_uuid")
colnames(data)
sensitive_columns(data)
