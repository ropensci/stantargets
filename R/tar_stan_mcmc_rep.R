#' @title Multiple MCMCs per model with tidy output.
#' @keywords internal
#' @description Internal function for replicated MCMC.
#'   Users should not invoke directly.
#' @return `tar_stan_mcmc_rep(name = x, stan_files = "y.stan")`
#'   returns a list of target objects:
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of MCMC summaries.
#'   * `x`: combine all the model-specific summary targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#'
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please see the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_stan_mcmc_rep_run
#' @inheritParams tar_stan_summary
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams cmdstanr::`model-method-compile`
#' @inheritParams cmdstanr::`model-method-sample`
#' @inheritParams cmdstanr::`fit-method-draws`
#' @inheritParams targets::tar_target
#' @param stan_files Character vector of paths to known existing Stan model
#'   files created before running the pipeline.
#' @param data Code to generate a single replication of a simulated dataset.
#'   The workflow simulates multiple datasets, and each
#'   model runs on each dataset. To join data on to the model
#'   summaries, include a `.join_data`
#'   element of your Stan data list with names and dimensions corresponding
#'   to those of the model. For details, read
#'   <https://wlandau.github.io/stantargets/articles/mcmc_rep.html>.
#' @param batches Number of batches. Each batch is a branch target
#'   that generates a dataset and runs the model `reps` times.
#' @param reps Number of replications per batch.
#' @param combine Logical, whether to create a target to
#'   combine all the model results
#'   into a single data frame downstream. Convenient, but
#'   duplicates data.
tar_stan_mcmc_rep <- function(
  name,
  stan_files,
  data = list(),
  output = c("summary", "draws", "diagnostics"),
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
  data_copy = character(0),
  variables = NULL,
  inc_warmup = FALSE,
  summaries = NULL,
  summary_args = NULL,
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
  assert_chr(stan_files, "stan_files must be a character vector")
  assert_unique(stan_files, "stan_files must be unique")
  assert_chr(data_copy, "data_copy must be a character vector")
  name_stan <- produce_stan_names(stan_files)
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_batch <- paste0(name, "_batch")
  name_data <- paste0(name, "_data")
  sym_stan <- rlang::syms(name_stan)
  sym_file <- rlang::sym(name_file)
  sym_lines <- rlang::sym(name_lines)
  sym_batch <- rlang::sym(name_batch)
  sym_data <- rlang::sym(name_data)
  command_lines <- call_function(
    "readLines",
    args = list(con = rlang::sym(name_file))
  )
  command_batch <- substitute(seq_len(x), env = list(x = batches))
  command_rep <- tidy_eval(
    data,
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_data <- substitute(
    purrr::map(seq_len(.targets_reps), ~.targets_command),
    env = list(.targets_reps = reps, .targets_command = command_rep)
  )
  args <- list(
    call_ns("stantargets", "tar_stan_mcmc_rep_run"),
    stan_file = trn(identical(compile, "original"), sym_file, sym_lines),
    stan_name = quote(._stantargets_name_chr_50e43091),
    stan_path = quote(._stantargets_file_50e43091),
    data = sym_data,
    output = match.arg(output),
    compile = compile,
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
    variables = variables,
    summaries = summaries,
    summary_args = summary_args
  )
  command <- as.expression(as.call(args))
  pattern_data <- substitute(map(x), env = list(x = sym_batch))
  pattern <- substitute(map(x), env = list(x = sym_data))
  target_file <- targets::tar_target_raw(
    name = name_file,
    command = quote(._stantargets_file_50e43091),
    packages = character(0),
    format = "file",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = "main",
    priority = priority,
    cue = cue
  )
  target_compile <- tar_stan_compile_raw(
    name = name_file,
    stan_file = quote(._stantargets_file_50e43091),
    quiet = quiet,
    log = log,
    dir = dir,
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
  target_output <- targets::tar_target_raw(
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
  tar_stan_target_list_rep(
    name = name,
    name_batch = name_batch,
    name_data = name_data,
    name_stan = name_stan,
    sym_stan = sym_stan,
    stan_files = stan_files,
    compile = compile,
    combine = combine,
    target_batch = target_batch,
    target_compile = target_compile,
    target_file = target_file,
    target_lines = target_lines,
    target_data = target_data,
    target_output = target_output,
    packages = packages,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    priority = priority,
    resources = resources,
    cue = cue
  )
}

#' @title Run a Stan model and return only the summaries.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A data frame of posterior summaries.
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams cmdstanr::`model-method-compile`
#' @inheritParams cmdstanr::`model-method-sample`
#' @inheritParams cmdstanr::`fit-method-draws`
#' @inheritParams tar_stan_mcmc
#' @param stan_name Friendly suffix of the Stan model target.
#' @param stan_path Original path to the input Stan file.
#' @param output Type of output to create, either `"summaries"`,
#'   `"draws"`, or `"diagnostics"`.
#' @param data_copy Character vector of names of scalars in `data`.
#'   These values will be inserted as columns in the output data frame
#'   for each rep. To join more than just scalars, include a `.join_data`
#'   element of your Stan data list with names and dimensions corresponding
#'   to those of the model. For details, read
#'   <https://wlandau.github.io/stantargets/articles/mcmc_rep.html>.
tar_stan_mcmc_rep_run <- function(
  stan_file,
  stan_name,
  stan_path,
  data,
  output,
  compile,
  quiet,
  log,
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
  data_copy,
  inc_warmup,
  variables,
  summaries,
  summary_args
) {
  if (!is.null(log)) {
    sink(file = log, type = "output", append = TRUE)
    on.exit(sink(file = NULL, type = "output"))
  }
  file <- stan_file
  if (identical(compile, "copy")) {
    tmp <- tempfile(fileext = ".stan")
    writeLines(stan_file, tmp)
    file <- tmp
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
  seeds <- seed + seq_along(data)
  out <- purrr::map2_dfr(
    .x = data,
    .y = seeds,
    ~tar_stan_mcmc_rep_run_rep(
      data = .x,
      seed = .y,
      output = output,
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
      data_copy = data_copy,
      inc_warmup = inc_warmup,
      variables = variables,
      summaries = summaries,
      summary_args = summary_args
    )
  )
  out$.file <- stan_path
  out$.name <- stan_name
  out
}

tar_stan_mcmc_rep_run_rep <- function(
  data,
  output,
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
  inc_warmup,
  data_copy,
  summaries,
  summary_args
) {
  stan_data <- data
  stan_data$.join_data <- NULL
  fit <- model$sample(
    data = stan_data,
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
  tar_stan_output(
    fit = fit,
    output = output,
    summaries = summaries,
    summary_args = summary_args,
    variables = variables,
    inc_warmup = inc_warmup,
    data = data,
    data_copy = data_copy
  )
}
