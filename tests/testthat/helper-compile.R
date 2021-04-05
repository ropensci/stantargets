compiled_dir <- function() {
  parent <- if_any(
    getRversion() >= "4.0.0",
    tools::R_user_dir("stantargets", "cache"),
    tempdir()
  )
  file.path(parent, "models")
}

compiled_path <- function(file) {
  file.path(compiled_dir(), file)
}

compile_models <- function() {
  if (!file.exists(compiled_dir())) {
    dir.create(compiled_dir(), recursive = TRUE)
  }
  withr::local_dir(compiled_dir())
  old <- file.mtime("a.stan")
  tar_stan_example_file("a.stan")
  new <- file.mtime("a.stan")
  if (!identical(old, new) || !file.exists("a")) {
    cmdstan_model("a.stan")
  }
  file.copy("a.stan", "b.stan", copy.date = TRUE)
  file.copy("a", "b", copy.date = TRUE)
}

restore_compiled_models <- function(to = getwd()) {
  for (file in c("a.stan", "b.stan", "a", "b")) {
    file.copy(compiled_path(file), to, copy.date = TRUE)
  }
}

skip_compile_copy <- function() {
  if (identical(Sys.getenv("TAR_STAN_SKIP_TEST_COMPILE_COPY"), "true")) {
    skip("compile = \"copy\"")
  }
}
