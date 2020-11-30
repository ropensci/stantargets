#' @title Multiple MCMCs with summaries.
#' @export
#' @description Targets to run MCMC multiple times and
#'   save only the summary output from each run.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_mcmc_rep_summary(name = x, ...)` returns a list
#'   of `targets::tar_target()` objects:
#'   * `x_file`: reproducibly track the Stan model file.
#'   * `x_lines`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x`: dynamic branching target with tidy data frames of MCMC summaries.
#' @inheritParams tar_stan_mcmc
#' @param data Code to generate one simulated dataset for one rep of the model.
#' @param batches Number of batches. Each batch is a branch target
#'   that generates a dataset and runs the model `reps` times.
#' @param reps Number of model runs per batch.
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_stan_compile(compiled_model, "stantargets_example.stan"),
#'   tar_stan_mcmc_rep_summary(
#'     your_model,
#'     file = compiled_model,
#'     data = tar_stan_example_data(),
#'     batches = 2,
#'     reps = 2
#'   )
#' )
tar_stan_mcmc_rep_summary <- function(
  name,
  file,
  data = list(),
  batches = 1L,
  reps = 1L,
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
  envir <- tar_option_get("envir")
  compile <- match.arg(compile)
  name <- deparse_language(substitute(name))
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_batch <- paste0(name, "_batch")
  name_data <- paste0(name, "_data")
  sym_file <- rlang::sym(name_file)
  sym_lines <- rlang::sym(name_lines)
  sym_batch <- rlang::sym(name_batch)
  sym_data <- rlang::sym(name_data)
  command_file <- tidy_eval(
    substitute(file),
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_lines <- call_function(
    "readLines",
    args = list(con = rlang::sym(name_file))
  )
  command_batch <- substitute(seq_len(x), env = list(x = batches))
  command_rep <- tidy_eval(
    substitute(data),
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_data <- substitute(
    purrr::map(seq_len(.targets_reps), ~.targets_command),
    env = list(.targets_reps = reps, .targets_command = command_rep)
  )
  args <- list(
    call_ns("stantargets", "tar_stan_mcmc_rep_summary_run"),
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
    summaries = substitute(summaries),
    summary_args = substitute(summary_args)
  )
  command <- as.expression(as.call(args))
  pattern_data <- substitute(map(x), env = list(x = sym_batch))
  pattern <- substitute(map(x), env = list(x = sym_data))
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
  target_batch <- targets::tar_target_raw(
    name = name_batch,
    command = command_batch,
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
    pattern = pattern_data,
    packages = packages,
    library = library,
    format = "qs",
    iteration = "list",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  target <- targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = character(0),
    format = "fst_tbl",
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
  list(
    target_file,
    trn(identical(compile, "original"), NULL, target_lines),
    target_batch,
    target_data,
    target
  )
}

#' @title Run a Stan model and return only the summaries.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A data frame of posterior summaries.
#' @inheritParams tar_stan_mcmc_run
tar_stan_mcmc_rep_summary_run <- function(
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
  summaries,
  summary_args
) {
  stan_file <- file
  if (identical(compile, "copy")) {
    stan_file <- tempfile(fileext = ".stan")
    writeLines(lines, stan_file)
  }
  model <- cmdstanr::cmdstan_model(
    stan_file = stan_file,
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
  seeds <- seed + seq_along(data)
  out <- purrr::map2_dfr(
    .x = data,
    .y = seeds,
    ~tar_stan_mcmc_rep_summary_run_rep(
      data = .x,
      seed = .y,
      model = model,
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
      summaries = summaries,
      summary_args = summary_args
    )
  )
  out$.file <- file
  out
}

tar_stan_mcmc_rep_summary_run_rep <- function(
  data,
  seed,
  model,
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
  summaries,
  summary_args
) {
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
  args <- list(quote(fit$summary))
  for (index in seq_along(summaries)) {
    args[[index + 1]] <- summaries[[index]]
  }
  args$variables <- variables %||% quote(identity(NULL))
  args$.args <- summary_args
  command <- as.expression(as.call(args))
  out <- tibble::as_tibble(eval(command))
  out$.rep <- basename(tempfile(pattern = "rep_"))
  out
}
