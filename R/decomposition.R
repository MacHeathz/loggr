
datastructure <- function(str) {
  yaml::yaml.load_file(str)
}

extract_info <- function(str, regexps, values) {
  stopifnot(length(regexps) == length(values))

  matrix <- sapply(seq_len(length(regexps)), function(i) {
    map_regexp_value(str, regexps[i], values[i])
  })

  combine_values(matrix)
}

map_regexp_value <- function(str, regexp, value) {
  ifelse(stringr::str_detect(str, regexp), value, NA)
}

combine_values <- function(matrix) {
  if (class(matrix) != "matrix") {
    matrix <- as.matrix(matrix)
  }

  combine_values_reduce(matrix)
}

combine_values_reduce <- function(matrix) {
  Reduce(
    fold_na,
    m_to_v(t(matrix))
  )
}

combine_values_map <- function(matrix) {
  unlist(
    lapply(
      Map(roll_na, m_to_v(matrix)),
      null_to_na),
    use.names = FALSE)
}

combine_values_apply <- function(matrix) {
  unlist(
    lapply(
      apply(matrix, 1, roll_na),
      null_to_na),
    use.names = FALSE)
}

fold_na <- function(a, b){
  ifelse(is.na(b), a, b)
}
m_to_v <- function(m) {
  split(m, rep(1:nrow(m), times = ncol(m)))
}
roll_na <- function(x) {
  x[!is.na(x)]
}
null_to_na <- function(arg) {
  ifelse(is.null(arg), NA, arg)
}

random.matrix <- function(arity) {
  M <- Matrix::Diagonal(arity)
  Ms <- as.character(M)
  Ms[Ms == "0"] <- NA
  matrix(Ms, nrow = arity)
}

random.longmatrix <- function(m,n) {
  R <- random.matrix(n)
  R <- matrix(rep(R, as.integer((m / n) + 1)), ncol = n, byrow = TRUE)
  utils::head(R, n = m)
}

benchmark_combine_values <- function(m, n) {
  if (requireNamespace("rbenchmark", quietly = TRUE)) {
    R <- random.longmatrix(m, n)
    rbenchmark::benchmark(combine_values_reduce(R),
                          combine_values_map(R),
                          combine_values_apply(R))
  } else {
    stop("Package \"rbenchmark\" is needed for this function to work.")
  }
}
