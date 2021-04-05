produce_stan_names <- function(stan_files) {
  out <- if_any(
    is.null(names(stan_files)),
    fs::path_ext_remove(basename(stan_files)),
    names(stan_files)
  )
  assert_unique(out, "target suffixes from stan_files are not unique.")
  assert_nzchar(out, "target suffixes from stan_files must be nonempty.")
  make.names(out)
}

command_lines <- function(sym_file) {
  substitute(
    readLines(grep("*.stan$", file, value = TRUE)),
    env = list(file = sym_file)
  )
}
