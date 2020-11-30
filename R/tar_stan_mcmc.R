#' @title One MCMC with multiple outputs.
#' @export
#' @description Targets to run a Stan model once with MCMC
#'   and save multiple outputs.
#' @details Most of the arguments are passed to the `$compile()`,
#'  `$sample()`, and `$summary()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_mcmc(name = x, ...)` returns a list
#'   of `targets::tar_target()` objects:
#'   * `x_file`: reproducibly track the Stan model file.
#'   * `x_lines`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: data for the MCMC.
#'   * `x_mcmc`: `CmdStanMCMC` object with all the MCMC results.
#'   * `x_draws`: tidy data frame of MCMC draws. Omitted if `draws = FALSE`.
#'   * `x_summary`: tidy data frame of MCMC summaries.
#'     Omitted if `summary = FALSE`.
#'   * `x_diagnostics`: tidy data frame of MCMC sampler diagnostics.
#'     Omitted if `diagnostics = FALSE`.
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams tar_stan_compile_run
#' @inheritParams tar_stan_mcmc_run
#' @inheritParams tar_stan_summary
#' @inheritParams targets::tar_target
#' @param name Symbol, base name for the collection of targets.
#'   The name itself will be applied to the fit object itself,
#'   and there will be suffixes for various supporting targets.
#' @param data Code to generate the `data` argument of `$sample()`.
#' @param file Code to generate the `stan_file`
#'   argument of `$compile()`. Could just be a literal path to a
#'   Stan model file or the name of an upstream target
#'   defined by [tar_stan_compile()].
#' @param draws Logical, whether to create a target for posterior draws.
#'   Saves `posterior::as_draws_df(fit$draws())` to a compressed `tibble`.
#'   Convenient, but duplicates storage.
#' @param summary Logical, whether to create a target for
#'   `fit$summary()`.
#' @param inc_warmup `inc_warmup` argument to `$draws()`.
#' @param diagnostics Logical, whether to create a target for
#'   `posterior::as_draws_df(fit$sampler_diagnostics())`.
#'   Saves `posterior::as_draws_df(fit$draws())` to a compressed `tibble`.
#'   Convenient, but duplicates storage.
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_stan_mcmc(
#'     your_model,
#'     file = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     variables = "beta",
#'     summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
#'   )
#' )
tar_stan_mcmc <- function(
  name,
  file,
  data = list(),
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
  variables = NULL,
  inc_warmup = FALSE,
  summaries = list(),
  summary_args = list(),
  draws = TRUE,
  diagnostics = TRUE,
  summary = TRUE,
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
  envir <- tar_option_get("envir")
  compile <- match.arg(compile)
  name <- deparse_language(substitute(name))
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_data <- paste0(name, "_data")
  name_mcmc <- paste0(name, "_mcmc")
  name_draws <- paste0(name, "_draws")
  name_summary <- paste0(name, "_summary")
  name_diagnostics <- paste0(name, "_diagnostics")
  sym_file <- rlang::sym(name_file)
  sym_lines <- rlang::sym(name_lines)
  sym_data <- rlang::sym(name_data)
  sym_mcmc <- rlang::sym(name_mcmc)
  command_file <- tidy_eval(
    substitute(file),
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_lines <- call_function(
    "readLines",
    args = list(con = rlang::sym(name_file))
  )
  command_data <- tidy_eval(
    substitute(data),
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_draws <- substitute(
    tibble::as_tibble(posterior::as_draws_df(
      fit$draws(variables = variables, inc_warmup = inc_warmup)
    )),
    env = list(
      fit = sym_mcmc,
      variables = variables,
      inc_warmup = inc_warmup
    )
  )
  method_summary <- call_function("$", list(sym_mcmc, rlang::sym("summary")))
  args_summary <- list(method_summary)
  summaries <- as.list(substitute(summaries)[-1])
  for (index in seq_along(summaries)) {
    args_summary[[index + 1]] <- summaries[[index]]
  }
  args_summary$variables <- variables %||% quote(identity(NULL))
  args_summary$.args <- substitute(summary_args)
  command_summary <- as.expression(as.call(args_summary))
  command_diagnostics <- substitute(
    tibble::as_tibble(
      posterior::as_draws_df(.targets_mcmc$sampler_diagnostics())
    ),
    env = list(.targets_mcmc = sym_mcmc)
  )
  args_mcmc <- list(
    call_ns("stantargets", "tar_stan_mcmc_run"),
    file = sym_file,
    lines = trn(identical(compile, "original"), "", sym_lines),
    data = sym_data,
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
    variables = variables,
    inc_warmup = inc_warmup
  )
  command_mcmc <- as.expression(as.call(args_mcmc))
  target_file <- targets::tar_target_raw(
    name = name_file,
    command = command_file,
    packages = character(0),
    format = "file",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue
  )
  target_lines <- targets::tar_target_raw(
    name = name_lines,
    command = command_lines,
    packages = character(0),
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue
  )
  target_data <- targets::tar_target_raw(
    name = name_data,
    command = command_data,
    packages = packages,
    library = library,
    format = "qs",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  target_mcmc <- targets::tar_target_raw(
    name = name_mcmc,
    command = command_mcmc,
    format = "qs",
    packages = character(0),
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
  target_draws <- targets::tar_target_raw(
    name = name_draws,
    command = command_draws,
    packages = character(0),
    format = "fst_tbl",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  target_summary <- targets::tar_target_raw(
    name = name_summary,
    command = command_summary,
    packages = character(0),
    format = "fst_tbl",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  target_diagnostics <- targets::tar_target_raw(
    name = name_diagnostics,
    command = command_diagnostics,
    packages = character(0),
    format = "fst_tbl",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  list(
    target_file,
    trn(identical(compile, "original"), NULL, target_lines),
    target_data,
    target_mcmc,
    trn(identical(draws, TRUE), target_draws, NULL),
    trn(identical(summary, TRUE), target_summary, NULL),
    trn(identical(diagnostics, TRUE), target_diagnostics, NULL)
  )
}

#' @title Compile and run a Stan model and return the `CmdStanFit` object.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A `CmdStanFit` object.
#' @param file Character, Stan model file.
#' @param lines Character, lines of Stan model code.
#' @param data List of data to pass to the Stan model.
#' @param compile Character of length 1. If `"original"`, then
#'   `cmdstan` will compile the source file right before running
#'   it (or skip compilation if the binary is up to date). This
#'   assumes the worker has access to the file. If the worker
#'   is running on a remote computer that does not have access
#'   to the model file, set to `"copy"` instead. `compile = "copy"`
#'   means the pipeline will read the lines of the original Stan model file
#'   and send them to the worker. The worker writes the lines
#'   to a local copy and compiles the model from there, so it
#'   no longer needs access to the original Stan model file on your
#'   local machine. However, as a result, the Stan model re-compiles
#'   every time the main target reruns.
#' @param seed `seed` argument to `$sample()`.
#' @param refresh `refresh` argument to `$sample()`.
#' @param init `init` argument to `$sample()`.
#' @param save_latent_dynamics `save_latent_dynamics` argument to `$sample()`.
#' @param output_dir `output_dir` argument to `$sample()`.
#' @param chains `chains` argument to `$sample()`.
#' @param parallel_chains `parallel_chains` argument to `$sample()`.
#' @param chain_ids `chain_ids` argument to `$sample()`.
#' @param threads_per_chain `threads_per_chain` argument to `$sample()`.
#' @param iter_warmup `iter_warmup` argument to `$sample()`.
#' @param iter_sampling `iter_sampling` argument to `$sample()`.
#' @param save_warmup `save_warmup` argument to `$sample()`.
#' @param thin `thin` argument to `$sample()`.
#' @param max_treedepth `max_treedepth` argument to `$sample()`.
#' @param adapt_engaged `adapt_engaged` argument to `$sample()`.
#' @param adapt_delta `adapt_delta` argument to `$sample()`.
#' @param step_size `step_size` argument to `$sample()`.
#' @param metric `metric` argument to `$sample()`.
#' @param metric_file `metric_file` argument to `$sample()`.
#' @param inv_metric `inv_metric` argument to `$sample()`.
#' @param init_buffer `init_buffer` argument to `$sample()`.
#' @param term_buffer `term_buffer` argument to `$sample()`.
#' @param window `window` argument to `$sample()`.
#' @param fixed_param `fixed_param` argument to `$sample()`.
#' @param sig_figs `sig_figs` argument to `$sample()`.
#' @param validate_csv `validate_csv` argument to `$sample()`.
#' @param show_messages `show_messages` argument to `$sample()`.
#' @param variables `variables` argument to `$draws()` and `$summary()`
#'   on the `CmdStanMCMC` object.
tar_stan_mcmc_run <- function(
  file,
  lines,
  data,
  compile,
  quiet,
  dir,
  include_paths,
  cpp_options,
  stanc_options,
  force_recompile,
  seed,
  refresh,
  init,
  save_latent_dynamics,
  output_dir,
  chains,
  parallel_chains,
  chain_ids,
  threads_per_chain,
  iter_warmup,
  iter_sampling,
  save_warmup,
  thin,
  max_treedepth,
  adapt_engaged,
  adapt_delta,
  step_size,
  metric,
  metric_file,
  inv_metric,
  init_buffer,
  term_buffer,
  window,
  fixed_param,
  sig_figs,
  validate_csv,
  show_messages,
  variables,
  inc_warmup
) {
  if (identical(compile, "copy")) {
    file <- tempfile(fileext = ".stan")
    writeLines(lines, file)
  }
  model <- cmdstanr::cmdstan_model(
    stan_file = file,
    compile = TRUE,
    quiet = quiet,
    dir = dir,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
  if (is.null(seed)) {
    seed <- abs(targets::tar_seed()) + 1L
  }
  fit <- model$sample(
    data = data,
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
    show_messages = show_messages
  )
  # Load all the data and return the whole unserialized fit object:
  # https://github.com/stan-dev/cmdstanr/blob/d27994f804c493ff3047a2a98d693fa90b83af98/R/fit.R#L16-L18 # nolint
  fit$draws() # Do not specify variables or inc_warmup.
  try(fit$sampler_diagnostics(), silent = TRUE)
  try(fit$init(), silent = TRUE)
  fit
}
