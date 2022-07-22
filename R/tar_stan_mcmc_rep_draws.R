#' @title Multiple MCMC runs per model with draws
#' @export
#' @description `tar_stan_mcmc_rep_draws()` creates targets
#'   to run MCMC multiple times per model and
#'   save only the draws from each run.
#' @details Draws could take up a lot of storage. If storage becomes
#'    excessive, please consider thinning the draws or using
#'   `tar_stan_mcmc_rep_summaries()` instead.
#'
#'   Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @family MCMC
#' @return `tar_stan_mcmc_rep_draws()` returns a
#'   list of target objects. See the "Target objects" section for
#'   background.
#'   The target names use the `name` argument as a prefix, and the individual
#'   elements of `stan_files` appear in the suffixes where applicable.
#'   As an example, the specific target objects returned by
#'   `tar_stan_mcmc_rep_draws(name = x, stan_files = "y.stan")`
#'   are as follows.
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
#'   * `x_y`: dynamic branching target to run MCMC once per dataset.
#'     Each dynamic branch returns a tidy data frames of draws
#'     corresponding to a batch of Stan data from `x_data`.
#'   * `x`: combine all branches of `x_y` into a single non-dynamic target.
#'     Suppressed if `combine` is `FALSE`.
#'     Returns a long tidy data frame of draws.
#' @inheritSection tar_stan_compile Target objects
#' @inheritParams tar_stan_mcmc_rep
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
#'   tar_stan_mcmc_rep_draws(
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
tar_stan_mcmc_rep_draws <- function(
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
  output_basename = NULL,
  sig_figs = NULL,
  chains = 4,
  parallel_chains = getOption("mc.cores", 1),
  chain_ids = seq_len(chains),
  threads_per_chain = NULL,
  opencl_ids = NULL,
  iter_warmup = NULL,
  iter_sampling = NULL,
  save_warmup = FALSE,
  thin = NULL,
  max_treedepth = NULL,
  adapt_engaged = TRUE,
  adapt_delta = NULL,
  step_size = NULL,
  metric = NULL,
  metric_file = NULL,
  inv_metric = NULL,
  init_buffer = NULL,
  term_buffer = NULL,
  window = NULL,
  fixed_param = FALSE,
  show_messages = TRUE,
  diagnostics = c("divergences", "treedepth", "ebfmi"),
  inc_warmup = FALSE,
  variables = NULL,
  data_copy = character(0),
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
  cue = targets::tar_option_get("cue")
) {
  tar_stan_mcmc_rep(
    name = targets::tar_deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
    output_type = "draws",
    batches = batches,
    reps = reps,
    combine = combine,
    compile = match.arg(compile),
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
    output_basename = output_basename,
    sig_figs = sig_figs,
    chains = chains,
    parallel_chains = parallel_chains,
    chain_ids = chain_ids,
    threads_per_chain = threads_per_chain,
    opencl_ids = opencl_ids,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    save_warmup = save_warmup,
    thin = thin,
    max_treedepth = max_treedepth,
    adapt_engaged = adapt_engaged,
    adapt_delta = adapt_delta,
    step_size = step_size,
    metric = metric,
    metric_file = metric_file,
    inv_metric = inv_metric,
    init_buffer = init_buffer,
    term_buffer = term_buffer,
    window = window,
    fixed_param = fixed_param,
    show_messages = show_messages,
    diagnostics = diagnostics,
    data_copy = data_copy,
    inc_warmup = inc_warmup,
    variables = variables,
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
    cue = cue
  )
}
