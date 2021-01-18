#' @title Target to compile a Stan model
#' @export
#' @description Target to compile a Stan model and return the
#'   original Stan model file. Does not compile the model
#'   if the compilation is already up to date.
#' @details Most of the arguments are passed to the
#'   `$compile()` method of the `CmdStanModel` class.
#'   For details, visit <https://mc-stan.org/cmdstanr/reference/>.
#' @return Character of length 1, the original Stan model file.
#' @inheritParams targets::tar_target
#' @inheritParams tar_stan_compile_run
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({
#' tar_stan_example_file()
#' targets::tar_script({
#' library(stantargets)
#' list(tar_stan_compile(compiled_model, "stantargets_example.stan"))
#' })
#' targets::tar_make()
#' })
#' }
tar_stan_compile <- function(
  name,
  stan_file,
  quiet = TRUE,
  dir = NULL,
  include_paths = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = FALSE,
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name <- deparse_language(substitute(name))
  tar_stan_compile_raw(
    name = name,
    stan_file = stan_file,
    quiet = quiet,
    dir = dir,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

tar_stan_compile_raw <- function(
  name,
  stan_file,
  quiet,
  dir,
  include_paths,
  cpp_options,
  stanc_options,
  force_recompile,
  error,
  memory,
  garbage_collection,
  deployment,
  priority,
  resources,
  storage,
  retrieval,
  cue
) {
  command <- tar_stan_compile_command(
    stan_file = stan_file,
    quiet = quiet,
    dir = dir,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
  targets::tar_target_raw(
    name = name,
    command = command,
    format = "file",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}

tar_stan_compile_command <- function(
  stan_file,
  quiet,
  dir,
  include_paths,
  cpp_options,
  stanc_options,
  force_recompile
) {
  args <- list(
    call_ns("stantargets", "tar_stan_compile_run"),
    stan_file = stan_file,
    quiet = quiet,
    dir = dir,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
  as.expression(as.call(args))
}

#' @title Compile a Stan model and return the model file.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return Character of length 1, the value of `stan_file`
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams cmdstanr::`model-method-compile`
tar_stan_compile_run <- function(
  stan_file,
  quiet = TRUE,
  dir = NULL,
  include_paths = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = FALSE
) {
  assert_stan_file(stan_file)
  cmdstanr::cmdstan_model(
    stan_file = stan_file,
    compile = TRUE,
    quiet = quiet,
    dir = dir,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
  stan_file
}
