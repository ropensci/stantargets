#' @title One variational Bayes run per model with multiple outputs.
#' @export
#' @description Targets to run a Stan model once with
#'   variational Bayes and save multiple outputs.
#' @details Most of the arguments are passed to the `$compile()`,
#'  `$variational()`, and `$summary()` methods of the `CmdStanModel` class.
#'   If you previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_vb(name = x, stan_files = "y.stan", ...)` returns a list
#'   of `targets::tar_target()` objects:
#'   * `x_file_y`: reproducibly track the Stan model file.
#'   * `x_lines_y`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: data for the variational Bayes computation.
#'   * `x_vb_y`: `CmdStanVB` object with all the VB results.
#'   * `x_draws_y`: tidy data frame of VB draws. Omitted if `draws = FALSE`.
#'   * `x_summary_y`: tidy data frame of VB summaries.
#'     Omitted if `summary = FALSE`.
#'  If you supply multiple models, you will get more (model-specific) targets.
#'  All the models share the same dataset.
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams tar_stan_compile_run
#' @inheritParams tar_stan_vb_run
#' @inheritParams tar_stan_summary
#' @inheritParams tar_stan_mcmc
#' @inheritParams targets::tar_target
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_stan_vb(
#'     your_model,
#'     file = "stantargets_example.stan",
#'     data = tar_stan_example_data(),
#'     variables = "beta",
#'     summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
#'   )
#' )
tar_stan_vb <- function(
  name,
  stan_files,
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
  name <- deparse_language(substitute(name))
  name_stan <- produce_stan_names(stan_files)
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_data <- paste0(name, "_data")
  name_vb <- paste0(name, "_vb")
  name_draws <- paste0(name, "_draws")
  name_summary <- paste0(name, "_summary")
  sym_stan <- rlang::syms(name_stan)
  sym_file <- rlang::sym(name_file)
  sym_lines <- rlang::sym(name_lines)
  sym_data <- rlang::sym(name_data)
  sym_vb <- rlang::sym(name_vb)
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
      fit$draws(variables = variables)
    )),
    env = list(
      fit = sym_vb,
      variables = variables
    )
  )
  method_summary <- call_function("$", list(sym_vb, rlang::sym("summary")))
  args_summary <- list(method_summary)
  summaries <- as.list(substitute(summaries)[-1])
  for (index in seq_along(summaries)) {
    args_summary[[index + 1]] <- summaries[[index]]
  }
  args_summary$variables <- variables %||% quote(identity(NULL))
  args_summary$.args <- substitute(summary_args)
  command_summary <- as.expression(as.call(args_summary))
  args_vb <- list(
    call_ns("stantargets", "tar_stan_vb_run"),
    stan_file = trn(identical(compile, "original"), sym_file, sym_lines),
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
    variables = variables
  )
  command_vb <- as.expression(as.call(args_vb))
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
  target_vb <- targets::tar_target_raw(
    name = name_vb,
    command = command_vb,
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
  out <- list(
    target_file,
    trn(identical(compile, "original"), NULL, target_lines),
    target_vb,
    trn(identical(draws, TRUE), target_draws, NULL),
    trn(identical(summary, TRUE), target_summary, NULL)
  )
  out <- list_nonempty(out)
  values <- list(
    ._stantargets_file_50e43091 = stan_files,
    ._stantargets_name_50e43091 = sym_stan
  )
  out <- tarchetypes::tar_map(
    values = values,
    names = ._stantargets_name_50e43091,
    unlist = TRUE,
    out
  )
  out[[name_data]] <- target_data
  out
}

#' @title Compile and run a Stan model and return a `CmdStanVB` object.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A `CmdStanFit` object.
#' @inheritParams tar_stan_mcmc_run
#' @param seed `seed` argument to `$variational()`.
#' @param refresh `refresh` argument to `$variational()`.
#' @param init `init` argument to `$variational()`.
#' @param save_latent_dynamics `save_latent_dynamics`
#'   argument to `$variational()`.
#' @param output_dir `output_dir` argument to `$variational()`.
#' @param algorithm `algorithm` argument to `$variational()`.
#' @param iter `iter` argument to `$variational()`.
#' @param grad_samples `grad_samples` argument to `$variational()`.
#' @param elbo_samples `elbo_samples` argument to `$variational()`.
#' @param eta `eta` argument to `$variational()`.
#' @param adapt_engaged `adapt_engaged` argument to `$variational()`.
#' @param adapt_iter `adapt_iter` argument to `$variational()`.
#' @param tol_rel_obj `tol_rel_obj` argument to `$variational()`.
#' @param eval_elbo `eval_elbo` argument to `$variational()`.
#' @param output_samples `output_samples` argument to `$variational()`.
#' @param sig_figs `sig_figs` argument to `$variational()`.
#' @param variables `variables` argument to `$draws()` and `$summary()`
#'   on the `CmdStanVB` object.
tar_stan_vb_run <- function(
  stan_file,
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
  iter,
  grad_samples,
  elbo_samples,
  eta,
  adapt_engaged,
  adapt_iter,
  tol_rel_obj,
  eval_elbo,
  output_samples,
  sig_figs,
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
  fit <- model$variational(
    data = data,
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
    sig_figs = sig_figs
  )
  # Load all the data and return the whole unserialized fit object:
  # https://github.com/stan-dev/cmdstanr/blob/d27994f804c493ff3047a2a98d693fa90b83af98/R/fit.R#L16-L18 # nolint
  fit$draws() # Do not specify variables.
  try(fit$variationalr_diagnostics(), silent = TRUE)
  try(fit$init(), silent = TRUE)
  fit
}
