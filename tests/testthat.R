library(testthat)
library(cleaninginspectoR)

test_check("cleaninginspectoR")


test_that("outliers are found in the data", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})
