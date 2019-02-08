setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
getwd()
source("./scripts/data_check.R")
source("./scripts/dependencies.R")
library(dplyr)
install.packages("qdap", dependencies = T)
library(qdap)
install.packages("XML")
library(chron)
.install_reachR(T, branch = 'develop')

data <- reachR:::read.csv.auto.sep("./data/reach_som_protection_assessment_hh_cleaneddata_feb_2018.csv") 
questionnaire <- load_questionnaire(data = "./data/reach_som_protection_assessment_hh_cleaneddata_feb_2018.csv", 
                                questions.file = "./data/QUESTIONS.csv", 
                                choices.file = "./data/choices1.csv",
                                choices.label.column.to.use = "english")

summary(questionnaire$choices)
reachR:::load_questionnaire


#hack to  fix the csv file that wouldn't read. to be fixed later
hey <- reachR:::read.csv.auto.sep("./data/QUESTIONS.csv", stringsAsFactors = F, header = T); summary(hey)
questionnaire[["questions"]] <- hey

#Getting indices, question names and choices for select_multiple
multiple_questions <- grep("select.multiple", questionnaire[["questions"]]$type, ignore.case = T)
multiple_names <- questionnaire[["questions"]]$name[multiple_questions]
multiple_list <- gsub("select.multiple ", "", questionnaire[["questions"]]$type[multiple_questions])

#Harmonizing names
multiple_names <- reachR:::to_alphanumeric_lowercase(multiple_names)
multiple_names <- gsub("comma", "", multiple_names)
questions.names <- reachR:::to_alphanumeric_lowercase(names(data))
questions.names <- sub("intro.", "", questions.names, fixed = T)

#removing the beginning 
#Make a loop probably 
groups <- grep("begin_group", questionnaire[["questions"]]$type, ignore.case = T)
group_names <- questionnaire[["questions"]]$name[groups]
questions.names <- qdap::mgsub(group_names, "", questions.names, trim = T, order.pattern = T)

for (x in group_names){
  
}


#Indices for the select multiple questions in the data 
questions.names
index_multiple_questions_data <- which(questions.names %in% multiple_names)

#Getting associated choices columns in the data
dependents <- function(x){
  for(x in multiple_list){
    Select_multiple_answers_indices <- list()
    Number <- sum(questionnaire[["choices"]]$list.name == x)
    Index_multiple_answers_data <- c(as.numeric(Index_multiple_questions_data[1]+1):as.numeric(Index_multiple_questions_data[1]+Number))
    names(Index_multiple_answers_data) <- Multiple_names[1]
  }
  return(list)
}

