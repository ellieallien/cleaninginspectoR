setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
getwd()

#.install_reachR(T, branch = "master")
source("./scripts/data_check.R")

# .install_reachR(T,branch="develop")
require("reachR")
require("data.table")
library("reachR")

########################
########################
# PARAMETERS
########################
########################

      parameters<-list()

########################
# FILES AND COLUMN NAMES
########################


      # meta

            parameters$RCID <- "UGA1999" # the reserach cycle ID
            parameters$name<-"World Bank PArent" # name of the project



      # input files
      ########################


            parameters$cleaning_log_available = FALSE
            parameters$stratified<-FALSE
            parameters$questionnaire_available = FALSE


            # WHERE IS THE DATA, AND WHAT ARE THE NAMES OF THE RELEVANT COLUMNS?
            parameters$data_file<- "./UGA_WB_parent.csv"
            parameters$data.uuid.column.name<-"_id"

      # cleaninglog
      ########################

            # IS THERE A CLEANING LOG, WHERE IS IT AND WHAT ARE THE NAMES OF THE RELEVANT COLUMNS?

              parameters$cleaning_log_file = "./data/cleaning_log.csv"
              parameters$cleaning.log.uuid.column = "UUID"
              parameters$cleaning.log.new.value.column = "New Value"
              parameters$cleaning.log.variable.column = "Question Name"


      # samplingframe
      ########################


            # IS THIS STRATIFIED? IF YES, WHERE'S THE SAMPLING FRAME AND WHAT ARE THE NAMES OF THE RELEVANT COLUMNS?
              # Only needed if stratified = TRUE:

              parameters$sampling.frame.file = "./settlement_populations.csv"
              parameters$sampling.frame.population.column = "households"
              parameters$sampling.frame.stratum.column = "settlement"
              parameters$data.stratum.column = "strata"


      # questionnaire
      ########################


            # IS THE QuESTIONNAIRE AVAIALBLE? IF YES, WHERE'S THE QUESTIONS FILE AND WHAT ARE THE NAMES OF THE RELEVANT COLUMNS?
              # Only needed if questionnaire = TRUE:
              parameters$questions.file  = "./QUESTIONNAIRE.csv"
              parameters$choices.file = "./choices.csv"
              parameters$choices.label.column.to.use = "english"



           # git test

              parameters$testpar<-"second_test"

########################
########################
# CUSTOM ANALYSIS
########################
########################

parameters$custom_analysis<-function(data){
print("hi")
}

########################
########################
# let's go!
########################
#######################

# undebug(write.aggregation.results)


validate(parameters)

