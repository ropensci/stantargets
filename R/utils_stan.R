produce_stan_names <- function(stan_files) {
  out <- trn(
    is.null(names(stan_files)),
    tools::file_path_sans_ext(basename(stan_files)),
    names(stan_files)
  )
  assert_unique(out, "target suffixes from stan_files are not unique.")
  assert_nzchar(out, "target suffixes from stan_files must be nonempty.")
  make.names(out)
}
