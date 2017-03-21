library(readr)

read_regexp <- function (file, regexp, ...) {
  if (is.na(file)) return (NA)

  lines <- read_lines(file, ...)
  subset(lines, grepl(regexp, lines))
}
