#' @title Stan model compilation
#' @export
#' @description Target to compile a Stan model and return the
#'   original Stan model file. Does not compile the model
#'   if the compilation is already up to date.
#' @details Most of the arguments are passed to the
#'   `$compile()` method of the `CmdStanModel` class.
#'   For details, visit <https://mc-stan.org/cmdstanr/reference/>.
#' @return A target object to compile a Stan file.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams targets::tar_target
#' @inheritParams tar_stan_compile_run
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#' library(stantargets)
#' # Do not use temporary storage for stan files in real projects
#' # or else your targets will always rerun.
#' path <- tempfile(pattern = "", fileext = ".stan")
#' tar_stan_example_file(path = path)
#' list(tar_stan_compile(compiled_model, path))
#' })
#' targets::tar_make()
#' })
#' }
tar_stan_compile <- function(
  name,
  stan_file,
  quiet = TRUE,
  stdout = NULL,
  stderr = NULL,
  dir = NULL,
  pedantic = FALSE,
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
  assert_stan_file(stan_file)
  tar_stan_compile_raw(
    name = name,
    stan_file = stan_file,
    quiet = quiet,
    stdout = stdout,
    stderr = substitute(stderr),
    dir = dir,
    pedantic = pedantic,
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
  stdout,
  stderr,
  dir,
  pedantic,
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
    stdout = stdout,
    stderr = stderr,
    dir = dir,
    pedantic = pedantic,
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
  stdout,
  stderr,
  dir,
  pedantic,
  include_paths,
  cpp_options,
  stanc_options,
  force_recompile
) {
  args <- list(
    call_ns("stantargets", "tar_stan_compile_run"),
    stan_file = stan_file,
    quiet = quiet,
    stdout = stdout,
    stderr = stderr,
    dir = dir,
    pedantic = pedantic,
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
#' @return Character of length 1, the value of `stan_file`.
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams cmdstanr::`model-method-compile`
#' @param stdout Character of length 1, file path to write the stdout stream
#'   of the model when it runs. Set to `NULL` to print to the console.
#'   Set to `R.utils::nullfile()` to suppress stdout.
#'   Does not apply to messages, warnings, or errors.
#' @param stderr Character of length 1, file path to write the stderr stream
#'   of the model when it runs. Set to `NULL` to print to the console.
#'   Set to `R.utils::nullfile()` to suppress stderr.
#'   Does not apply to messages, warnings, or errors.
tar_stan_compile_run <- function(
  stan_file,
  quiet = TRUE,
  stdout = NULL,
  stderr = NULL,
  dir = NULL,
  pedantic = FALSE,
  include_paths = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = FALSE
) {
  if (!is.null(stdout)) {
    withr::local_output_sink(new = stdout, append = TRUE)
  }
  if (!is.null(stderr)) {
    withr::local_message_sink(new = stderr, append = TRUE)
  }
  assert_stan_file(stan_file)
  model <- cmdstanr::cmdstan_model(
    stan_file = stan_file,
    compile = TRUE,
    quiet = quiet,
    dir = dir,
    pedantic = pedantic,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
  fs::path_rel(c(stan_file, model$exe_file()), start = getwd())
}
