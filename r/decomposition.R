library(yaml)
library(stringr)

datastructure <- function (str) {
  yaml.load_file(str)
}

extract_info <- function (str, regexps, values) {
  stopifnot(length(regexps) == length(values))
  
  matrix <- sapply(seq_len(length(regexps)), function(i) {
    map_regexp_value(str, regexps[i], values[i])
  })
  
  combine_values(matrix)
}

map_regexp_value <- function (str, regexp, value) {
  ifelse(str_detect(str, regexp), value, NA)
}

combine_values <- function (matrix) {
  if (class(matrix) != "matrix") {
    matrix <- as.matrix(matrix)
  }
  
  matrix_as_vectors_of_rows <-
    split(matrix, rep(1:nrow(matrix), times = ncol(matrix)))
  unlist(lapply(Map(function(row) {
    row[!is.na(row)]
  }, matrix_as_vectors_of_rows), function(x) {
    ifelse(is.null(x), NA, x)
  }), use.names = FALSE)
}