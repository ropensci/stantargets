#' @title Multiple iterations per model of variational Bayes with summaries.
#' @export
#' @description Targets to run variational Bayes multiple times and
#'   save only the summary output from each run.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_vb_rep_summary(name = x,  stan_files = "y.stan")`
#'   returns a list of `targets::tar_target()` objects:
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of VB summaries.
#'   * `x`: combine all the model-specific summary targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#' @inheritParams tar_stan_vb_rep
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' list(
#'   tar_stan_vb_rep_summary(
#'     your_model,
#'     stan_files = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     batches = 2,
#'     reps = 2
#'   )
#' )
tar_stan_vb_rep_summary <- function(
  name,
  stan_files,
  data = list(),
  batches = 1L,
  reps = 1L,
  combine = TRUE,
  compile = c("original", "copy"),
  quiet = TRUE,
  dir = NULL,
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
  copy_data = character(0),
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
  tar_stan_vb_rep(
    name = deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
    output = "summary",
    batches = batches,
    reps = reps,
    combine = combine,
    compile = compile,
    quiet = quiet,
    dir = dir,
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
    copy_data = copy_data,
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
