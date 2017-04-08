context("Logrify read_regexp")

string <- "# Comment 1\n2013-09-23, 23:23, sadf, sdf, 234\n# Comment 2"

test_that("read_regexp reads lines that match the regular expression from a string.", {
  expect_equal(read_regexp(string, "^[^#]"), "2013-09-23, 23:23, sadf, sdf, 234")
  expect_equal(read_regexp(string, "^[#]"), c("# Comment 1", "# Comment 2"))
})

test_that("read_regexp reads lines that match the regular expression from a file.", {
  test_data <- "data/test_logs"

  expect_equal(read_regexp(test_data, "^[^#]"), "2013-09-23, 23:23, sadf, sdf, 234")
  expect_equal(read_regexp(test_data, "^[#]"), c("# Comment 1", "# Comment 2"))
})

test_that("read_regexp returns a character vector, of class 'character'.", {
  expect_is(read_regexp(string, "^[^#]"), "character")
})

test_that("read_regexp of missing is missing.", {
  expect_equal(read_regexp(NA, "^[^#]"), NA)
})

test_that("read_regexp for empty string regexp returns everything as a character vector.", {
  expect_equal(read_regexp(string, ""), c("# Comment 1", "2013-09-23, 23:23, sadf, sdf, 234", "# Comment 2"))
  expect_is(read_regexp(string, ""), "character")
})

test_that("read_regexp passes extra parameters to read_lines function.", {
  expect_equal(read_regexp(string, "", n_max = 2), c("# Comment 1", "2013-09-23, 23:23, sadf, sdf, 234"))
  expect_equal(read_regexp(string, "^[#]", skip = 1), "# Comment 2")
})
