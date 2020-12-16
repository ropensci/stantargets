#' @title Multiple optimization runs per model with draws.
#' @export
#' @description Targets to run vb multiple times and
#'   save only the MLEs in a wide-form "draws" data frame.
#' @details Most of the arguments are passed to the `$compile()`
#'   and `$sample()` methods of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_mcmc_rep_draws(name = x, stan_files = "y.stan")`
#'   returns a list of `targets::tar_target()` objects:
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: dynamic branching target with simulated datasets.
#'   * `x_y`: dynamic branching target with tidy data frames of MLE draws.
#'   * `x`: combine all the model-specific draws targets into
#'     a single data frame with columns to distinguish among the models.
#'     Suppressed if `combine` is `FALSE`.
#' @inheritParams tar_stan_mle
#' @inheritParams tar_stan_mcmc_rep_summary
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_stan_mle_rep_draws(
#'     your_model,
#'     stan_files = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     batches = 2,
#'     reps = 2
#'   )
#' )
tar_stan_mle_rep_draws <- function(
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
  init_alpha = NULL,
  iter = NULL,
  sig_figs = NULL,
  tol_obj = NULL,
  tol_rel_obj = NULL,
  tol_grad = NULL,
  tol_rel_grad = NULL,
  tol_param = NULL,
  history_size = NULL,
  variables = NULL,
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
    call_ns("stantargets", "tar_stan_mle_rep_draws_run"),
    stan_file = trn(identical(compile, "original"), sym_file, sym_lines),
    stan_name = quote(._stantargets_name_chr_50e43091),
    stan_path = quote(._stantargets_file_50e43091),
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
    algorithm = algorithm,
    init_alpha = init_alpha,
    iter = iter,
    sig_figs = sig_figs,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size,
    variables = variables
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
  target_mle <- targets::tar_target_raw(
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
    target_mle
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
  names_mle <- paste0(name, "_", name_stan)
  if (combine) {
    out[[name]] <- tarchetypes::tar_combine_raw(
      name = name,
      out[names_mle],
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
#' @inheritParams tar_stan_mle_run
#' @inheritParams tar_stan_mcmc_rep_summary_run
tar_stan_mle_rep_draws_run <- function(
  stan_file,
  stan_name,
  stan_path,
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
  algorithm,
  init_alpha,
  iter,
  sig_figs,
  tol_obj,
  tol_rel_obj,
  tol_grad,
  tol_rel_grad,
  tol_param,
  history_size,
  variables
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
    ~tar_stan_mle_rep_draws_run_rep(
      data = .x,
      seed = .y,
      model = model,
      refresh = refresh,
      init = init,
      save_latent_dynamics = save_latent_dynamics,
      output_dir = output_dir,
      algorithm = algorithm,
      init_alpha = init_alpha,
      iter = iter,
      sig_figs = sig_figs,
      tol_obj = tol_obj,
      tol_rel_obj = tol_rel_obj,
      tol_grad = tol_grad,
      tol_rel_grad = tol_rel_grad,
      tol_param = tol_param,
      history_size = history_size,
      variables = variables
    )
  )
  out$.file <- stan_path
  out$.name <- stan_name
  out
}

tar_stan_mle_rep_draws_run_rep <- function(
  data,
  seed,
  model,
  refresh,
  init,
  save_latent_dynamics,
  output_dir,
  algorithm,
  init_alpha,
  iter,
  sig_figs,
  tol_obj,
  tol_rel_obj,
  tol_grad,
  tol_rel_grad,
  tol_param,
  history_size,
  variables
) {
  fit <- model$optimize(
    data = data,
    seed = seed,
    refresh = refresh,
    init = init,
    save_latent_dynamics = save_latent_dynamics,
    output_dir = output_dir,
    algorithm = algorithm,
    init_alpha = init_alpha,
    iter = iter,
    sig_figs = sig_figs,
    tol_obj = tol_obj,
    tol_rel_obj = tol_rel_obj,
    tol_grad = tol_grad,
    tol_rel_grad = tol_rel_grad,
    tol_param = tol_param,
    history_size = history_size
  )
  out <- fit$draws(variables = variables)
  out <- tibble::as_tibble(posterior::as_draws_df(out))
  out$.rep <- basename(tempfile(pattern = "rep_"))
  out
}
