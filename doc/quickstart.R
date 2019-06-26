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
#  inspect_all(testdf, uuid.column.name = "uuid")

## ----eval=T,echo=F-------------------------------------------------------
kable(inspect_all(testdf, uuid.column.name = "uuid"))

## ----eval=F--------------------------------------------------------------
#  inspect_all(df = testdf, uuid.column.name = "b")

## ----eval=T--------------------------------------------------------------
kable(inspect_all(df = testdf, uuid.column.name = "b"))

