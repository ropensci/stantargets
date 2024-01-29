#' @title Multiple MCMCs per model with tidy output
#' @keywords internal
#' @description Internal function for replicated MCMC.
#'   Users should not invoke directly.
#' @section Seeds:
#'   Rep-specific random number generator seeds for the data and models
#'   are automatically set based on the `seed` argument, batch, rep,
#'   parent target name, and `tar_option_get("seed")`. This ensures
#'   the rep-specific seeds do not change when you change the batching
#'   configuration (e.g. 40 batches of 10 reps each vs 20 batches of 20
#'   reps each). Each data seed is in the `.seed` list element of the output,
#'   and each Stan seed is in the .seed column of each Stan model output.
#' @return A list of target objects.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Developers can consult the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   to learn about the structure and composition of target objects.
#' @inheritParams tar_stan_mcmc_rep_run
#' @inheritParams tar_stan_summary
#' @inheritParams tar_stan_mcmc
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
#'   <https://docs.ropensci.org/stantargets/articles/simulation.html>.
#' @param batches Number of batches. Each batch is a sequence
#'   of branch targets containing multiple reps. Each rep
#'   generates a dataset and runs the model on it.
#' @param reps Number of replications per batch.
#' @param combine Logical, whether to create a target to
#'   combine all the model results
#'   into a single data frame downstream. Convenient, but
#'   duplicates data.
tar_stan_mcmc_rep <- function(
  name,
  stan_files,
  data = list(),
  output_type = c("summary", "draws", "diagnostics"),
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
  data_copy = character(0),
  variables = NULL,
  inc_warmup = FALSE,
  summaries = NULL,
  summary_args = NULL,
  transform = NULL,
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
  cue = targets::tar_option_get("cue")
) {
  envir <- tar_option_get("envir")
  compile <- match.arg(compile)
  targets::tar_assert_chr(stan_files, "stan_files must be a character vector")
  targets::tar_assert_unique(stan_files, "stan_files must be unique")
  targets::tar_assert_chr(data_copy, "data_copy must be a character vector")
  lapply(stan_files, assert_stan_file)
  assert_transform(transform)
  name_stan <- produce_stan_names(stan_files)
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_batch <- paste0(name, "_batch")
  name_data <- paste0(name, "_data")
  sym_stan <- as_symbols(name_stan)
  sym_file <- as.symbol(name_file)
  sym_lines <- as.symbol(name_lines)
  sym_batch <- as.symbol(name_batch)
  sym_data <- as.symbol(name_data)
  command_batch <- substitute(seq_len(x), env = list(x = batches))
  command_rep <- targets::tar_tidy_eval(
    data,
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_data <- substitute(
    stantargets::tar_stan_rep_data_batch(
      .targets_reps,
      .targets_batch,
      .targets_command
    ),
    env = list(
      .targets_reps = reps,
      .targets_batch = sym_batch,
      .targets_command = command_rep
    )
  )
  args <- list(
    call_ns("stantargets", "tar_stan_mcmc_rep_run"),
    stan_file = if_any(identical(compile, "original"), sym_file, sym_lines),
    stan_name = quote(._stantargets_name_chr_50e43091),
    stan_path = quote(._stantargets_file_50e43091),
    data = sym_data,
    output_type = match.arg(output_type),
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
    inc_warmup = inc_warmup,
    data_copy = data_copy,
    variables = variables,
    summaries = summaries,
    summary_args = summary_args,
    transform = transform
  )
  command <- as.expression(as.call(args))
  pattern_data <- substitute(map(x), env = list(x = sym_batch))
  pattern <- substitute(map(x), env = list(x = sym_data))
  target_file <- targets::tar_target_raw(
    name = name_file,
    command = quote(._stantargets_file_50e43091),
    packages = character(0),
    format = "file",
    repository = "local",
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
    stdout = stdout,
    stderr = stderr,
    dir = dir,
    pedantic = pedantic,
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
    command = command_lines(sym_file),
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
    format = format,
    repository = repository,
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
    format = format_df,
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
#' @param output_type Type of output to create, either `"summaries"`,
#'   `"draws"`, or `"diagnostics"`.
#' @param data_copy Character vector of names of scalars in `data`.
#'   These values will be inserted as columns in the output data frame
#'   for each rep. To join more than just scalars, include a `.join_data`
#'   element of your Stan data list with names and dimensions corresponding
#'   to those of the model. For details, read
#'   <https://docs.ropensci.org/stantargets/articles/simulation.html>.
#' @param transform Symbol or `NULL`, name of a function that accepts
#'   arguments `data` and `draws` and returns a data frame. Here,
#'   `data` is the JAGS data list supplied to the model, and `draws`
#'   is a data frame with one column per model parameter and one row
#'   per posterior sample. Any arguments other than `data` and `draws`
#'   must have valid default values because `stantargets` will not
#'   populate them. See the simulation-based calibration (SBC)
#'   section of the simulation vignette for an example.
tar_stan_mcmc_rep_run <- function(
  stan_file,
  stan_name,
  stan_path,
  data,
  output_type,
  compile,
  quiet,
  stdout,
  stderr,
  dir,
  pedantic,
  include_paths,
  cpp_options,
  stanc_options,
  force_recompile,
  seed,
  refresh,
  init,
  save_latent_dynamics,
  output_dir,
  output_basename,
  sig_figs,
  chains,
  parallel_chains,
  chain_ids,
  threads_per_chain,
  opencl_ids,
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
  show_messages,
  diagnostics,
  data_copy,
  inc_warmup,
  variables,
  summaries,
  summary_args,
  transform
) {
  if (!is.null(stdout)) {
    withr::local_output_sink(new = stdout, append = TRUE)
  }
  if (!is.null(stderr)) {
    withr::local_message_sink(new = stderr, append = TRUE)
  }
  file <- stan_file
  if (identical(compile, "copy")) {
    tmp <- tempfile(pattern = "", fileext = ".stan")
    writeLines(stan_file, tmp)
    file <- tmp
  }
  file <- grep("*.stan$", file, value = TRUE)
  model <- cmdstanr::cmdstan_model(
    stan_file = file,
    compile = TRUE,
    quiet = quiet,
    dir = dir,
    pedantic = pedantic,
    include_paths = include_paths,
    cpp_options = cpp_options,
    stanc_options = stanc_options,
    force_recompile = force_recompile
  )
  out <- purrr::map2_dfr(
    .x = seq_along(data),
    .y = data,
    ~tar_stan_mcmc_rep_run_rep(
      rep = .x,
      data = .y,
      model = model,
      output_type = output_type,
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
      summaries = summaries,
      summary_args = summary_args,
      transform = transform
    )
  )
  out$.file <- stan_path
  out$.name <- stan_name
  out
}

tar_stan_mcmc_rep_run_rep <- function(
  rep,
  data,
  model,
  output_type,
  seed,
  refresh,
  init,
  save_latent_dynamics,
  output_dir,
  output_basename,
  sig_figs,
  chains,
  parallel_chains,
  chain_ids,
  threads_per_chain,
  opencl_ids,
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
  show_messages,
  diagnostics,
  variables,
  inc_warmup,
  data_copy,
  summaries,
  summary_args,
  transform
) {
  stan_seed <- data$.seed + 1L
  stan_seed <- if_any(is.null(seed), stan_seed, stan_seed + seed)
  withr::local_seed(stan_seed[1])
  stan_data <- data
  stan_data$.dataset_id <- NULL
  stan_data$.join_data <- NULL
  stan_data$.seed <- NULL
  fit <- model$sample(
    data = stan_data,
    seed = stan_seed,
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
    diagnostics = diagnostics
  )
  out <- tar_stan_output(
    fit = fit,
    output_type = output_type,
    summaries = summaries,
    summary_args = summary_args,
    transform = transform,
    variables = variables,
    inc_warmup = inc_warmup,
    data = data,
    data_copy = data_copy,
    seed = stan_seed
  )
  remove_temp_files(fit)
  out
}
