#' Read a file using regexp.
#'
#' Read a file or string line by line, only return lines that match the provided regular
#' expression.
#'
#' Paragraph shown after arguments with more details
#'
#' Another paragraph laying down the details
#'
#' @param file Path to the file or string
#' @param regexp The regular expression
#' @param ... Remaining arguments will be passed to the \code{readr::read_lines} method
#' @inheritParams readr::read_lines
#' @return Character vector of lines
#' @examples
#' read_regexp(c("# comment", "logline"), "^[#]")
read_regexp <- function(file, regexp, ...) {
  if (is.na(file)) return(NA)

  lines <- readr::read_lines(file, ...)
  subset(lines, grepl(regexp, lines))
}

read_regexp_strdetect <- function(file, regexp, ...) {
  if (is.na(file)) return(NA)

  lines <- readr::read_lines(file, ...)
  subset(lines, stringr::str_detect(lines, regexp))
}

benchmark_read_regexp <- function() {
  if (requireNamespace("rbenchmark", quietly = TRUE)) {
    rbenchmark::benchmark(
      lapply(
        list.files(paste0("C:/Users/datalab1/Documents/Datalab/",
                          "Schipholtunnel Casus/Data/Logfiles Schipholtunnel/",
                          "2015 txt/Rapp.*.txt"),
                   full.names = T),
        FUN = function(file) {
          read_regexp(file, "^\\d{2}-\\d{2}-\\d{4}")
          }),
      lapply(
        list.files(paste0("C:/Users/datalab1/Documents/Datalab/",
                          "Schipholtunnel Casus/Data/Logfiles Schipholtunnel/",
                          "2015 txt/Rapp.*.txt"),
                   full.names = T),
        FUN = function(file) {
          read_regexp_strdetect(file, "^\\d{2}-\\d{2}-\\d{4}")
          })
    )
  } else {
    stop("Package \"rbenchmark\" is needed for this function to work.")
  }
}
