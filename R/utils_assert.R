assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    throw_validate(msg %||% "x must be a character.")
  }
}

assert_package <- function(package, msg = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    throw_validate(msg %||% paste("package ", package, " not installed"))
  }
}

assert_path <- function(path, msg = NULL) {
  missing <- !file.exists(path)
  if (any(missing)) {
    throw_validate(
      msg %||% paste(
        "missing files: ",
        paste(path[missing], collapse = ", ")
      )
    )
  }
}

assert_scalar <- function(x, msg = NULL) {
  if (length(x) != 1) {
    throw_validate(msg %||% "x must have length 1.")
  }
}

assert_stan_file <- function(stan_file) {
  assert_scalar(stan_file, "only one Stan model file allowed at a time.")
  assert_chr(stan_file, "stan_file must be a character.")
  assert_path(stan_file, "stan_file is not an existing file.")
}

assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    throw_validate(paste(msg %||% "duplicated entries:", dups))
  }
}
