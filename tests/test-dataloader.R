source("../r/dataloader.R")

# Run auto_test('r', 'tests') for automated testing during development

context("Logrify Dataloader")

string <- "# Comment 1\n2013-09-23, 23:23, sadf, sdf, 234\n# Comment 2"

test_that("read_regexp reads lines that match the regular expression.", {
  expect_that(read_regexp(string, "^[^#]"), equals("2013-09-23, 23:23, sadf, sdf, 234"))
  expect_that(read_regexp(string, "^[#]"), equals(c("# Comment 1", "# Comment 2")))
})

test_that("read_regexp returns a character vector, of class 'character'.", {
  expect_that(class(read_regexp(string, "^[^#]")), equals("character"))
})

test_that("read_regexp of missing is missing.", {
  expect_that(read_regexp(NA, "^[^#]"), equals(NA))
})

test_that("read_regexp for empty string regexp return everything as a character vector.", {
  expect_that(read_regexp(string, ""), equals(c("# Comment 1", "2013-09-23, 23:23, sadf, sdf, 234", "# Comment 2")))
  expect_that(class(read_regexp(string, "")), equals("character"))
})

test_that("read_regexp passes extra parameters to read_lines function", {
  expect_that(read_regexp(string, "", n_max = 2), equals(c("# Comment 1", "2013-09-23, 23:23, sadf, sdf, 234")))
  expect_that(read_regexp(string, "^[#]", skip = 1), equals("# Comment 2"))
})
