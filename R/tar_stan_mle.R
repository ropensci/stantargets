#' @title One optimization run per model with multiple outputs
#' @export
#' @description `tar_stan_mle()` creates targets to optimize a Stan model once
#'   per model and separately save draws-like output and summary-like output.
#' @details Most of the arguments are passed to the `$compile()`,
#'  `$optimize()`, and `$summary()` methods of the `CmdStanModel` class.
#'   If you previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @family optimization
#' @return `tar_stan_mle()` returns a
#'   list of target objects. See the "Target objects" section for
#'   background. The specific target objects returned by
#'   `tar_stan_mle(name = x, stan_files = "y.stan", ...)`
#'   are as follows.
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: data for the variational Bayes computation.
#'   * `x_mle_y`: `CmdStanMLE` object with the optimization results.
#'   * `x_draws_y`: wide-form data frame with the single MLE value.
#'     Omitted if `draws` is `FALSE`.
#'   * `x_summary_y`: long-form data frame with the single MLE value.
#'     Omitted if `summary = FALSE`.
#' @inheritSection tar_stan_compile Target objects
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams tar_stan_compile_run
#' @inheritParams tar_stan_mle_run
#' @inheritParams tar_stan_summary
#' @inheritParams tar_stan_mcmc
#' @inheritParams targets::tar_target
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
#'   tar_stan_mle(
#'     your_model,
#'     stan_files = path,
#'     data = tar_stan_example_data(),
#'     stdout = R.utils::nullfile(),
#'     stderr = R.utils::nullfile()
#'   )
#' )
#' }, ask = FALSE)
#' targets::tar_make()
#' })
#' }
tar_stan_mle <- function(
  name,
  stan_files,
  data = list(),
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
  init_alpha = NULL,
  iter = NULL,
  tol_obj = NULL,
  tol_rel_obj = NULL,
  tol_grad = NULL,
  tol_rel_grad = NULL,
  tol_param = NULL,
  history_size = NULL,
  sig_figs = NULL,
  variables = NULL,
  summaries = list(),
  summary_args = list(),
  draws = TRUE,
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
  assert_chr(stan_files)
  assert_unique(stan_files)
  lapply(stan_files, assert_stan_file)
  name <- deparse_language(substitute(name))
  name_stan <- produce_stan_names(stan_files)
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_data <- paste0(name, "_data")
  name_mle <- paste0(name, "_mle")
  name_draws <- paste0(name, "_draws")
  name_summary <- paste0(name, "_summary")
  sym_stan <- as_symbols(name_stan)
  sym_file <- as.symbol(name_file)
  sym_lines <- as.symbol(name_lines)
  sym_data <- as.symbol(name_data)
  sym_mle <- as.symbol(name_mle)
  command_data <- tar_tidy_eval(
    substitute(data),
    envir = envir,
    tidy_eval = tidy_eval
  )
  command_draws <- substitute(
    tibble::as_tibble(posterior::as_draws_df(
      fit$draws(variables = variables)
    )),
    env = list(fit = sym_mle, variables = variables)
  )
  command_summary <- tar_stan_summary_call(
    sym_fit = sym_mle,
    sym_data = sym_data,
    summaries = substitute(summaries),
    summary_args = substitute(summary_args),
    variables = variables
  )
  args_mle <- list(
    call_ns("stantargets", "tar_stan_mle_run"),
    stan_file = trn(identical(compile, "original"), sym_file, sym_lines),
    data = sym_data,
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
  command_mle <- as.expression(as.call(args_mle))
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
  target_output <- targets::tar_target_raw(
    name = name_mle,
    command = command_mle,
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
    packages = packages,
    format = "fst_tbl",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
  tar_stan_target_list(
    name_data = name_data,
    stan_files = stan_files,
    sym_stan = sym_stan,
    compile = compile,
    draws = draws,
    summary = summary,
    diagnostics = FALSE,
    target_file = target_file,
    target_lines = target_lines,
    target_data = target_data,
    target_output = target_output,
    target_draws = target_draws,
    target_summary = target_summary,
    target_diagnostics = NULL
  )
}

#' @title Compile and run a Stan model and return a `CmdStanMLE` object.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A `CmdStanFit` object.
#' @inheritParams tar_stan_mcmc_run
#' @inheritParams cmdstanr::`model-method-optimize`
tar_stan_mle_run <- function(
  stan_file,
  data,
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
  if (is.null(seed)) {
    seed <- abs(targets::tar_seed()) + 1L
  }
  stan_data <- data
  stan_data$.join_data <- NULL
  fit <- model$optimize(
    data = stan_data,
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
  # Load all the data and return the whole unserialized fit object:
  # https://github.com/stan-dev/cmdstanr/blob/d27994f804c493ff3047a2a98d693fa90b83af98/R/fit.R#L16-L18 # nolint
  fit$draws() # Do not specify variables.
  try(fit$init(), silent = TRUE)
  fit
}
