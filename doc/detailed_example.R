vignette: >
  %\VignetteIndexEntry{Integrating TCGA Data}
%\VignetteEngine{knitr::rmarkdown}

## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- results='hide', message=FALSE,warning=FALSE,error=FALSE------------
library("cleaninginspectoR")

## ------------------------------------------------------------------------
testdf <- data.frame(a= c(runif(98),7287,-100),
                   b=sample(letters,100,T),
                   uuid=c(1:98, 4,20),
                   water.source.other = c(rep(NA,98),"neighbour's well","neighbour's well"),
                   GPS.lat = runif(100)
                   )

## ----eval=F--------------------------------------------------------------
#  find_duplicates(testdf, duplicate.column.name = "uuid")

## ----echo=F--------------------------------------------------------------
knitr::kable(find_duplicates(testdf, duplicate.column.name = "uuid"))

## ----eval=F--------------------------------------------------------------
#  find_duplicates_uuid(testdf)

## ----echo=F--------------------------------------------------------------
knitr::kable(find_duplicates_uuid(testdf))

## ----eval=F--------------------------------------------------------------
#  find_outliers(testdf)

## ----echo=F--------------------------------------------------------------
knitr::kable(find_outliers(testdf))

## ----eval=F--------------------------------------------------------------
#  find_other_responses(testdf)

## ----echo=F--------------------------------------------------------------
knitr::kable(find_other_responses(testdf))


