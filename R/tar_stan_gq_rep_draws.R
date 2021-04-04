#' @title Multiple runs of generated quantities per model with draws
#' @export
#' @description `tar_stan_gq_rep_draws()` creates targets
#'   to run generated quantities multiple times and
#'   save only the draws from each run.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @family generated quantities
#' @return `tar_stan_gq_rep_draws()` returns a list of target objects.
#'   See the "Target objects" section for
#'   background.
#'   The target names use the `name` argument as a prefix, and the individual
#'   elements of `stan_files` appear in the suffixes where applicable.
#'   As an example, the specific target objects returned by
#' `tar_stan_gq_rep_draws(name = x, stan_files = "y.stan")`
#'   are as follows.
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of draws.
#'   * `x`: combine all the model-specific draws targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#' @inheritSection tar_stan_compile Target objects
#' @inheritParams tar_stan_gq_rep
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#' library(stantargets)
#' # Do not use temporary storage for stan files in real projects
#' # or else your targets will always rerun.
#' path <- tempfile(fileext = ".stan")
#' tar_stan_example_file(path = path)
#' list(
#'   tar_stan_mcmc(
#'     your_model,
#'     stan_files = c(x = path),
#'     data = tar_stan_example_data(),
#'     stdout = R.utils::nullfile(),
#'     stderr = R.utils::nullfile(),
#'     refresh = 0
#'   ),
#'   tar_stan_gq_rep_draws(
#'     generated_quantities,
#'     stan_files = path,
#'     data = tar_stan_example_data(),
#'     fitted_params = your_model_mcmc_x,
#'     batches = 2,
#'     reps = 2,
#'     stdout = R.utils::nullfile(),
#'     stderr = R.utils::nullfile()
#'   )
#' )
#' }, ask = FALSE)
#' targets::tar_make()
#' })
#' }
tar_stan_gq_rep_draws <- function(
  name,
  stan_files,
  data = list(),
  fitted_params,
  batches = 1L,
  reps = 1L,
  combine = FALSE,
  compile = c("original", "copy"),
  quiet = TRUE,
  stdout = NULL,
  stderr = NULL,
  dir = NULL,
  pedantic = FALSE,
  include_paths = NULL,
  cpp_options = list(),
  stanc_options = list(),
  force_recompile = FALSE,
  seed = NULL,
  output_dir = NULL,
  sig_figs = NULL,
  parallel_chains = getOption("mc.cores", 1),
  threads_per_chain = NULL,
  variables = NULL,
  data_copy = character(0),
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = "transient",
  garbage_collection = TRUE,
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  tar_stan_gq_rep(
    name = deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
    fitted_params = substitute(fitted_params),
    output_type = "draws",
    batches = batches,
    reps = reps,
    combine = combine,
    compile = compile,
    quiet = quiet,
    stdout = stdout,
    stderr = stderr,
    dir = dir,
    pedantic = pedantic,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile,
    seed = seed,
    output_dir = output_dir,
    sig_figs = sig_figs,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    data_copy = data_copy,
    variables = variables,
    summaries = NULL,
    summary_args = NULL,
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
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
