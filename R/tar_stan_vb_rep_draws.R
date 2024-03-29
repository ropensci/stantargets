#' @title Multiple variational Bayes runs per model with draws
#' @export
#' @description `tar_stan_vb_rep_draws()` creates targets to run
#'   variational Bayes multiple times per model and
#'   save only the draws from each run.
#' @inheritSection tar_stan_mcmc_rep Seeds
#' @details Draws could take up a lot of storage. If storage becomes
#'    excessive, please consider thinning the draws or using
#'   `tar_stan_vb_rep_summaries()` instead.
#'
#'   Most of the arguments are passed to the `$compile()`
#'   and `$variational()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @family variational Bayes
#' @return `tar_stan_vb_rep_summaries()` returns a
#'   list of target objects. See the "Target objects" section for
#'   background.
#'   The target names use the `name` argument as a prefix, and the individual
#'   elements of `stan_files` appear in the suffixes where applicable.
#'   As an example, the specific target objects returned by
#'   `tar_stan_vb_rep_draws(name = x, stan_files = "y.stan")`
#'   are as follows.
#'   * `x_file_y`: reproducibly track the Stan model file. Returns
#'     a character vector with paths to
#'     the model file and compiled executable.
#'   * `x_lines_y`: read the Stan model file for safe transport to
#'     parallel workers. Omitted if `compile = "original"`.
#'     Returns a character vector of lines in the model file.
#'   * `x_data`: use dynamic branching to generate multiple datasets
#'     by repeatedly running the R expression in the `data` argument.
#'     Each dynamic branch returns a batch of Stan data lists that `x_y`
#'     supplies to the model.
#'   * `x_y`: dynamic branching target to run variational Bayes
#'     once per dataset.
#'     Each dynamic branch returns a tidy data frames of draws
#'     corresponding to a batch of Stan data from `x_data`.
#'   * `x`: combine all branches of `x_y` into a single non-dynamic target.
#'     Suppressed if `combine` is `FALSE`.
#'     Returns a long tidy data frame of draws.
#' @inheritSection tar_stan_compile Target objects
#' @inheritParams tar_stan_vb_rep
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
#'   tar_stan_vb_rep_draws(
#'     your_model,
#'     stan_files = path,
#'     data = tar_stan_example_data(),
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
tar_stan_vb_rep_draws <- function(
  name,
  stan_files,
  data = list(),
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
  refresh = NULL,
  init = NULL,
  save_latent_dynamics = FALSE,
  output_dir = NULL,
  algorithm = NULL,
  iter = NULL,
  grad_samples = NULL,
  elbo_samples = NULL,
  eta = NULL,
  adapt_engaged = NULL,
  adapt_iter = NULL,
  tol_rel_obj = NULL,
  eval_elbo = NULL,
  output_samples = NULL,
  sig_figs = NULL,
  data_copy = character(0),
  variables = NULL,
  transform = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = "qs",
  format_df = "fst_tbl",
  repository = targets::tar_option_get("repository"),
  error = targets::tar_option_get("error"),
  memory = "transient",
  garbage_collection = TRUE,
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  tar_stan_vb_rep(
    name = targets::tar_deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
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
    refresh = refresh,
    init = init,
    save_latent_dynamics = save_latent_dynamics,
    output_dir = output_dir,
    algorithm = algorithm,
    iter = iter,
    grad_samples = grad_samples,
    elbo_samples = elbo_samples,
    eta = eta,
    adapt_engaged = adapt_engaged,
    adapt_iter = adapt_iter,
    tol_rel_obj = tol_rel_obj,
    eval_elbo = eval_elbo,
    output_samples = output_samples,
    sig_figs = sig_figs,
    data_copy = data_copy,
    variables = variables,
    summaries = NULL,
    summary_args = NULL,
    transform = substitute(transform),
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
