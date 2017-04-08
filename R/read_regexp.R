#' Read a file line by line, only return lines that match the provided regular
#' expression.
#'
#' @param file Path to the file
#' @param regexp The regular expression
#' @param ... Remaining arguments will be passed to the readr::read_lines method
#' @return Character vector of lines
read_regexp <- function (file, regexp, ...) {
  if (is.na(file)) return (NA)

  lines <- readr::read_lines(file, ...)
  subset(lines, grepl(regexp, lines))
}

read_regexp_strdetect <- function (file, regexp, ...) {
  if (is.na(file)) return (NA)

  lines <- readr::read_lines(file, ...)
  subset(lines, stringr::str_detect(lines, regexp))
}

benchmark_read_regexp <- function () {
  if (requireNamespace("rbenchmark", quietly = TRUE)) {
    rbenchmark::benchmark(
      lapply(
        list.files("C:/Users/datalab1/Documents/Datalab/Schipholtunnel Casus/Data/Logfiles Schipholtunnel/2015 txt/",
                   "Rapp.*.txt",
                   full.names = T),
        FUN=function(file){
          read_regexp(file, "^\\d{2}-\\d{2}-\\d{4}")
          }),
      lapply(
        list.files("C:/Users/datalab1/Documents/Datalab/Schipholtunnel Casus/Data/Logfiles Schipholtunnel/2015 txt/",
                   "Rapp.*.txt",
                   full.names = T),
        FUN=function(file){
          read_regexp_strdetect(file, "^\\d{2}-\\d{2}-\\d{4}")
          })
    )
  } else {
    stop("Package \"rbenchmark\" is needed for this function to work.")
  }
}
