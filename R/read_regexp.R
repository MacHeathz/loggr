library(stringr)
library(readr)

read_regexp <- function (file, regexp, ...) {
  if (is.na(file)) return (NA)

  lines <- read_lines(file, ...)
  subset(lines, grepl(regexp, lines))
}

read_regexp_strdetect <- function (file, regexp, ...) {
  if (is.na(file)) return (NA)
  
  lines <- read_lines(file, ...)
  subset(lines, str_detect(lines, regexp))
}

benchmark_read_regexp <- function () {
  benchmark(
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
}