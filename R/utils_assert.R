assert_stan_file <- function(stan_file) {
  targets::tar_assert_chr(stan_file)
  targets::tar_assert_path(stan_file)
  targets::tar_assert_not_dir(stan_file)
}

tar_stan_deprecate <- function(x, alternative) {
  if (!is.null(x)) {
    targets::tar_warn_deprecate(
      "Argument ",
      deparse(substitute(x)),
      " is deprecated. Use ",
      alternative,
      " instead."
    )
  }
}
