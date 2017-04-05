library(testthat)
source("../r/decomposition.R")

context("Logrify decomposition")

test_that("datastructure reads a yaml file without blowing up.", {
  expect_equal(datastructure("data/settings1.yml"), c("hey", "hi", "hello"))
  expect_equal(datastructure("data/settings2.yml"), list(foo=123, bar=456))
})

test_that("extract_info throws an error when regexps and values are not of the same length.", {
  expect_error(extract_info("w", c("w", "e"), c("west", "east", "south")))
})

test_that("extract_info maps a character vector onto values using the provided regexps.", {
  expect_equal(extract_info("w", "w", "west"), "west")
  expect_equal(extract_info(c("n", "w", "o", "s"), c("w", "o"), c("west", "oost")), c(NA, "west", "oost", NA))
  expect_equal(extract_info(c("Pcrd on B1",
                           "LFrge at W2",
                           "Dt near C3",
                           "Rkr near B3",
                           "Wrf at P1"),
                         
                         c("[Bb]\\d",
                           "[Ww]\\d",
                           "[Cc]\\d",
                           "[Pp]\\d"),
                         
                         c("Bridge",
                           "Warp Engine",
                           "Computer System",
                           "Phaser systems")),
               c("Bridge",
                 "Warp Engine",
                 "Computer System",
                 "Bridge",
                 "Phaser systems"))
})

test_that("map_regexp_value maps a string onto a value using the provided regexp, returns NA when no match is found.", {
  expect_equal(map_regexp_value("w", "w", "west"), "west")
  expect_equal(map_regexp_value("w", "o", "west"), NA)
})

test_that("map_regexp_value maps a vector of strings onto a value using the provided regexp, returns NA when no match is found.", {
  expect_equal(map_regexp_value(c("w", "west wing", "eastenders", "south side", "west_2"), "w", "west"), c("west", "west", NA, NA, "west"))
})

test_that("combine_values creates a vector out of a matrix, by combining all rows into one value", {
  expect_equal(combine_values(matrix(c(1, NA, NA, NA, 2, NA), nrow = 3)), c(1, 2, NA))
})
