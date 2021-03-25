assert_chr <- function(x, msg = NULL) {
  if (!is.character(x)) {
    throw_validate(msg %|||% "x must be a character.")
  }
}

assert_nonempty <- function(x, msg = NULL) {
  if (!length(x)) {
    throw_validate(msg %|||% "x must not be empty")
  }
}

assert_not_in <- function(x, choices, msg = NULL) {
  if (any(x %in% choices)) {
    throw_validate(msg %|||% paste(deparse(x), "is in", deparse(choices)))
  }
}

assert_nzchar <- function(x, msg = NULL) {
  if (any(!nzchar(x))) {
    throw_validate(msg %|||% "x has empty character strings")
  }
}

assert_package <- function(package, msg = NULL) {
  tryCatch(rlang::check_installed(package), error = function(e) {
    throw_validate(conditionMessage(e))
  })
}

assert_path <- function(path, msg = NULL) {
  assert_nonempty(path, "path must not be empty")
  missing <- !file.exists(path)
  if (any(missing)) {
    throw_validate(
      msg %|||% paste(
        "missing files or directories: ",
        paste(path[missing], collapse = ", ")
      )
    )
  }
}

assert_scalar <- function(x, msg = NULL) {
  if (length(x) != 1) {
    throw_validate(msg %|||% "x must have length 1.")
  }
}

assert_stan_file <- function(stan_file) {
  assert_scalar(stan_file, "only one Stan model file allowed at a time.")
  assert_chr(stan_file, "stan_file must be a character.")
  assert_path(stan_file, "stan_file is not an existing file.")
  if (dir.exists(stan_file)) {
    throw_validate("stan_file must not be a directory.")
  }
}

assert_unique <- function(x, msg = NULL) {
  if (anyDuplicated(x)) {
    dups <- paste(unique(x[duplicated(x)]), collapse = ", ")
    throw_validate(paste(msg %|||% "duplicated entries:", dups))
  }
}
