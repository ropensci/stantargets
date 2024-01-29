produce_stan_names <- function(stan_files) {
  out <- if_any(
    is.null(names(stan_files)),
    fs::path_ext_remove(basename(stan_files)),
    names(stan_files)
  )
  targets::tar_assert_unique(
    out,
    "target suffixes from stan_files are not unique."
  )
  targets::tar_assert_nzchar(
    out,
    "target suffixes from stan_files must be nonempty."
  )
  make.names(out)
}

command_lines <- function(sym_file) {
  substitute(
    readLines(grep("*.stan$", file, value = TRUE)),
    env = list(file = sym_file)
  )
}

remove_temp_files <- function(fit) {
  try(unlink(fit$data_file(), recursive = TRUE), silent = TRUE)
  try(unlink(fit$output_files(), recursive = TRUE), silent = TRUE)
  try(unlink(fit$profile_files(), recursive = TRUE), silent = TRUE)
}
