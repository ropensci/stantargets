assert_stan_file <- function(stan_file) {
  targets::tar_assert_chr(stan_file)
  targets::tar_assert_path(stan_file)
  targets::tar_assert_not_dir(stan_file)
}
