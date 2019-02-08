rm(list=ls());if(!("rstudioapi" %in% installed.packages()[,"Package"])){install.packages("rstudioapi")};require("rstudioapi");
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("./")
getwd()
rstudioapi::isAvailable("0.99.149")


devtools::install_github("r-lib/devtools")

usethis::create_package("./cleaninginspectoR")

require(c("devtools", "testthat", "knitr", "reachR"))

library(roxygen2)
library(devtools)
library(survey)
library(dplyr)

has_devel()
print(has_devel())

.onLoad <- function(libname, pkgname){
  packageStartupMessage("Welcome to a package that checks your data cleaning")
}

usethis::use_test()

data <- read.csv("data.csv")
data <- data[data$survey_consent != "no_eligible",]
save(data, file = "data.RDA")

print(rtools_path() )
has_rtools()
build()
devtools::load_all()
devtools::document()
devtools::test()
devtools::build_vignettes()


