#' @title Multiple runs of generated quantities per model with summaries
#' @export
#' @description `tar_stan_gq_rep_summaries()` creates targets
#'   to run generated quantities multiple times and
#'   save only the summaries from each run.
#' @inheritSection tar_stan_mcmc_rep Seeds
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$generate_quantities()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @family generated quantities
#' @return `tar_stan_gq_rep_summaries()` returns a
#'   list of target objects. See the "Target objects" section for
#'   background.
#'   The target names use the `name` argument as a prefix, and the individual
#'   elements of `stan_files` appear in the suffixes where applicable.
#'   As an example, the specific target objects returned by
#'   `tar_stan_gq_rep_summary(name = x, stan_files = "y.stan")`
#'   returns a list of target objects:
#'   * `x_file_y`: reproducibly track the Stan model file. Returns
#'     a character vector with the paths to the
#'     model file and compiled executable.
#'   * `x_lines_y`: read the Stan model file for safe transport to
#'     parallel workers. Omitted if `compile = "original"`.
#'     Returns a character vector of lines in the model file.
#'   * `x_data`: use dynamic branching to generate multiple datasets
#'     by repeatedly running the R expression in the `data` argument.
#'     Each dynamic branch returns a batch of Stan data lists that `x_y`
#'     supplies to the model.
#'   * `x_y`: dynamic branching target to run generated quantities
#'     once per dataset.
#'     Each dynamic branch returns a tidy data frames of summaries
#'     corresponding to a batch of Stan data from `x_data`.
#'   * `x`: combine all branches of `x_y` into a single non-dynamic target.
#'     Suppressed if `combine` is `FALSE`.
#'     Returns a long tidy data frame of summaries.
#' @inheritSection tar_stan_compile Target objects
#' @inheritParams tar_stan_gq_rep
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' targets::tar_script({
#' library(stantargets)
#' # Do not use temporary storage for stan files in real projects
#' # or else your targets will always rerun.
#' path <- tempfile(pattern = "", fileext = ".stan")
#' tar_stan_example_file(path = path)
#' list(
#'   tar_stan_mcmc(
#'     your_model,
#'     stan_files = c(x = path),
#'     data = tar_stan_example_data(),
#'     stdout = R.utils::nullfile(),
#'     stderr = R.utils::nullfile()
#'   ),
#'   tar_stan_gq_rep_summary(
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
tar_stan_gq_rep_summary <- function(
  name,
  stan_files,
  data = list(),
  fitted_params,
  batches = 1L,
  reps = 1L,
  combine = TRUE,
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
  data_copy = character(0),
  variables = NULL,
  summaries = list(),
  summary_args = list(),
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = "qs",
  format_df = "fst_tbl",
  repository = targets::tar_option_get("repository"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  tar_stan_gq_rep(
    name = targets::tar_deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
    fitted_params = substitute(fitted_params),
    output_type = "summary",
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
    summaries = substitute(summaries),
    summary_args = substitute(summary_args),
    tidy_eval = tidy_eval,
    packages = packages,
    library = library,
    format = format,
    format_df = format_df,
    repository = repository,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
