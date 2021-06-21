assert_stan_file <- function(stan_file) {
  targets::tar_assert_scalar(
    stan_file,
    paste(
      "stan_file must have length 1.",
      "Some stantargets functions do allow multiple Stan files though."
    )
  )
  targets::tar_assert_chr(stan_file)
  targets::tar_assert_path(stan_file)
  if (dir.exists(stan_file)) {
    targets::tar_throw_validate("Stan model file must not be a directory.")
  }
}
