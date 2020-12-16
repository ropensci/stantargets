#' @title Multiple runs of generated quantities per model with summaries.
#' @export
#' @description Targets to run generated quantities multiple times and
#'   save only the summaries from each run.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_gq_rep_summary(name = x, stan_files = "y.stan")`
#'   returns a list of `targets::tar_target()` objects:
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of summaries.
#'   * `x`: combine all the model-specific summaries targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#' @inheritParams tar_stan_gq
#' @inheritParams tar_stan_mcmc_rep_summary
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_stan_mcmc(
#'     your_model,
#'     stan_files = c(x = "stantargets_example.stan"),
#'     data = tar_stan_example_data()
#'   ),
#'   tar_stan_gq_rep_summary(
#'     generated_quantities,
#'     stan_files = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     fitted_params = your_model_mcmc_x,
#'     batches = 2,
#'     reps = 2
#'   )
#' )
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
  envir <- tar_option_get("envir")
  compile <- match.arg(compile)
  assert_chr(stan_files)
  assert_unique(stan_files)
  name <- deparse_language(substitute(name))
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
    substitute(data),
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_data <- substitute(
    purrr::map(seq_len(.targets_reps), ~.targets_command),
    env = list(.targets_reps = reps, .targets_command = command_rep)
  )
  args <- list(
    call_ns("stantargets", "tar_stan_gq_rep_summary_run"),
    stan_file = trn(identical(compile, "original"), sym_file, sym_lines),
    stan_name = quote(._stantargets_name_chr_50e43091),
    stan_path = quote(._stantargets_file_50e43091),
    data = sym_data,
    fitted_params = substitute(fitted_params),
    compile = compile,
    quiet = quiet,
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
    copy_data = copy_data,
    variables = variables,
    summaries = substitute(summaries),
    summary_args = substitute(summary_args)
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
  target_gq <- targets::tar_target_raw(
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
  out <- list(
    trn(identical(compile, "original"), target_compile, target_file),
    trn(identical(compile, "original"), NULL, target_lines),
    target_gq
  )
  out <- list_nonempty(out)
  values <- list(
    ._stantargets_file_50e43091 = stan_files,
    ._stantargets_name_50e43091 = sym_stan,
    ._stantargets_name_chr_50e43091 = name_stan
  )
  out <- tarchetypes::tar_map(
    values = values,
    names = ._stantargets_name_50e43091,
    unlist = TRUE,
    out
  )
  out[[name_data]] <- target_data
  out[[name_batch]] <- target_batch
  names_gq <- paste0(name, "_", name_stan)
  if (combine) {
    out[[name]] <- tarchetypes::tar_combine_raw(
      name = name,
      out[names_gq],
      packages = character(0),
      format = "fst_tbl",
      iteration = "vector",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = "main",
      priority = priority,
      resources = resources,
      storage = "main",
      retrieval = "main",
      cue = cue
    )
  }
  out
}

#' @title Run a Stan model and return only the summaries.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A data frame of posterior summaries.
#' @inheritParams tar_stan_gq
#' @inheritParams tar_stan_gq_rep_summary_run
tar_stan_gq_rep_summary_run <- function(
  stan_file,
  stan_name,
  stan_path,
  data,
  fitted_params,
  compile,
  quiet,
  dir,
  include_paths,
  cpp_options,
  stanc_options,
  force_recompile,
  seed,
  output_dir,
  sig_figs,
  parallel_chains,
  threads_per_chain,
  copy_data,
  variables,
  summaries,
  summary_args
) {
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
    ~tar_stan_gq_rep_summary_run_rep(
      data = .x,
      seed = .y,
      fitted_params = fitted_params,
      model = model,
      output_dir = output_dir,
      sig_figs = sig_figs,
      parallel_chains = parallel_chains,
      threads_per_chain = threads_per_chain,
      copy_data = copy_data,
      variables = variables,
      summaries = summaries,
      summary_args = summary_args
    )
  )
  out$.file <- stan_path
  out$.name <- stan_name
  out
}

tar_stan_gq_rep_summary_run_rep <- function(
  data,
  seed,
  model,
  fitted_params,
  output_dir,
  sig_figs,
  parallel_chains,
  threads_per_chain,
  copy_data,
  variables,
  summaries,
  summary_args
) {
  fit <- model$generate_quantities(
    fitted_params = fitted_params,
    data = data,
    seed = seed,
    output_dir = output_dir,
    sig_figs = sig_figs,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain
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
  out <- copy_data_scalars(out, data, copy_data)
  out
}
