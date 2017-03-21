source("../r/datastructure.R")

context("Logrify datastructure")

test_that("datastructure reads a yaml file without blowing up.", {
  expect_that(datastructure("data/settings1.yml"), equals(c("hey", "hi", "hello")))
  expect_that(datastructure("data/settings2.yml"), equals(list(foo=123, bar=456)))
})
