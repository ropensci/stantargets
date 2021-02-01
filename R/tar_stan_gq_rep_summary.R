#' @title Multiple runs of generated quantities per model with summaries.
#' @export
#' @description Targets to run generated quantities multiple times and
#'   save only the summaries from each run.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_gq_rep_summary(name = x, stan_files = "y.stan")`
#'   returns a list of target objects:
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of summaries.
#'   * `x`: combine all the model-specific summaries targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#'
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_stan_gq_rep
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({
#' tar_stan_example_file()
#' targets::tar_script({
#' library(stantargets)
#' list(
#'   tar_stan_mcmc(
#'     your_model,
#'     stan_files = c(x = "stantargets_example.stan"),
#'     data = tar_stan_example_data(),
#'     log = R.utils::nullfile()
#'   ),
#'   tar_stan_gq_rep_summary(
#'     generated_quantities,
#'     stan_files = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     fitted_params = your_model_mcmc_x,
#'     batches = 2,
#'     reps = 2,
#'     log = R.utils::nullfile()
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
  log = NULL,
  dir = NULL,
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
  tar_stan_gq_rep(
    name = deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
    fitted_params = substitute(fitted_params),
    output = "summary",
    batches = batches,
    reps = reps,
    combine = combine,
    compile = compile,
    quiet = quiet,
    log = log,
    dir = dir,
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
