#' @title Multiple MCMCs per model with sampler diagnostics.
#' @export
#' @description Targets to run MCMC multiple times and
#'   save only the sampler diagnostics from each run. We recommend thinning
#'   if the number of MCMC runs is large.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_mcmc_rep_diagnostics(name = x, stan_files = "y.stan")`
#'   returns a list of target objects:
#'   * `x_file_y`: reproducibly track the Stan model file. Compile the model
#'     if `compile = "original"`.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of MCMC
#'     sampler diagnostics.
#'   * `x`: combine all the model-specific diagnostic targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#'  If you supply multiple models, you will get more (model-specific) targets.
#'  All the models share the same dataset.
#'
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_stan_mcmc_rep
#' @examples
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({
#' tar_stan_example_file()
#' targets::tar_script({
#' library(stantargets)
#' list(
#'   tar_stan_mcmc_rep_diagnostics(
#'     your_model,
#'     stan_files = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     batches = 2,
#'     reps = 2,
#'     log = R.utils::nullfile()
#'   )
#' )
#' }, ask = FALSE)
#' targets::tar_make()
#' })
#' }
tar_stan_mcmc_rep_diagnostics <- function(
  name,
  stan_files,
  data = list(),
  batches = 1L,
  reps = 1L,
  combine = FALSE,
  compile = c("original", "copy"),
  quiet = TRUE,
  log = NULL,
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
  chains = 4,
  parallel_chains = getOption("mc.cores", 1),
  chain_ids = seq_len(chains),
  threads_per_chain = NULL,
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
  sig_figs = NULL,
  validate_csv = TRUE,
  show_messages = TRUE,
  inc_warmup = FALSE,
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
  tar_stan_mcmc_rep(
    name = deparse_language(substitute(name)),
    stan_files = stan_files,
    data = substitute(data),
    output = "diagnostics",
    batches = batches,
    reps = reps,
    combine = combine,
    compile = match.arg(compile),
    quiet = quiet,
    log = log,
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
    chains = chains,
    parallel_chains = parallel_chains,
    chain_ids = chain_ids,
    threads_per_chain = threads_per_chain,
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
    sig_figs = sig_figs,
    validate_csv = validate_csv,
    show_messages = show_messages,
    inc_warmup = inc_warmup,
    data_copy = data_copy,
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
