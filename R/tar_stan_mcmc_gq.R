#' @title Generated quantities on an existing MCMC object
#' @export
#' @description Targets to run the generated quantities of
#'   a Stan model and optionally save draws and summaries separately.
#' @details Most of the arguments are passed to the `$compile()`,
#'  `$generate_quantities()`, and `$summary()` methods
#'   of the `CmdStanModel` class. If you
#'   previously compiled the model in an upstream [tar_stan_compile()]
#'   target, then the model should not recompile.
#' @return `tar_stan_mcmc_gq(name = x, ...)` returns a list
#'   of `targets::tar_target()` objects:
#'   * `x_file`: reproducibly track the Stan model file.
#'   * `x_lines`: contents of the Stan model file.
#'     Omitted if `compile = "original"`.
#'   * `x_data`: data for the generated quantities.
#'   * `x_gq`: `CmdStanGQ` object with all the generated quantities results.
#'   * `x_draws`: tidy data frame of MCMC draws. Omitted if `draws = FALSE`.
#'   * `x_summary`: tidy data frame of MCMC summaries.
#'     Omitted if `summary = FALSE`.
#' @inheritParams cmdstanr::cmdstan_model
#' @inheritParams tar_stan_compile_run
#' @inheritParams tar_stan_mcmc_gq_run
#' @inheritParams tar_stan_summary
#' @inheritParams targets::tar_target
#' @param name Symbol, base name for the collection of targets.
#'   The name itself will be applied to the fit object itself,
#'   and there will be suffixes for various supporting targets.
#' @param fitted_params Symbol, name of a `CmdStanMCMC` object
#'   computed in a previous target: for example, the
#'   `*_mcmc` target from [tar_stan_mcmc()].
#' @param data Code to generate the `data`
#'   argument of `$generate_quantities()`.
#' @param file Code to generate the `stan_file`
#'   argument of `$compile()`. Could just be a literal path to a
#'   Stan model file or the name of an upstream target
#'   defined by [tar_stan_compile()].
#' @param draws Logical, whether to create a target for posterior draws.
#'   Saves `posterior::as_draws_df(fit$draws())` to a compressed `tibble`.
#'   Convenient, but duplicates storage.
#' @param summary Logical, whether to create a target for
#'   `fit$summary()`.
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' targets::tar_pipeline(
#'   tar_stan_mcmc(
#'     your_model,
#'     file = "stantargets_example.stan",
#'     data = tar_stan_example_data()
#'   ),
#'   tar_stan_mcmc_gq(
#'     custom_gq,
#'     fitted_params = your_model_mcmc,
#'     file = "stantargets_example.stan", # Can be a different model.
#'     data = your_model_data # Can be a different dataset.
#'   )
#' )
tar_stan_mcmc_gq <- function(
  name,
  fitted_params,
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
  output_dir = NULL,
  sig_figs = NULL,
  parallel_chains = getOption("mc.cores", 1),
  threads_per_chain = NULL,
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
  name_file <- paste0(name, "_file")
  name_lines <- paste0(name, "_lines")
  name_data <- paste0(name, "_data")
  name_gq <- paste0(name, "_gq")
  name_draws <- paste0(name, "_draws")
  name_summary <- paste0(name, "_summary")
  sym_file <- rlang::sym(name_file)
  sym_lines <- rlang::sym(name_lines)
  sym_data <- rlang::sym(name_data)
  sym_gq <- rlang::sym(name_gq)
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
      fit$draws(variables = variables)
    )),
    env = list(
      fit = sym_gq,
      variables = variables
    )
  )
  method_summary <- call_function("$", list(sym_gq, rlang::sym("summary")))
  args_summary <- list(method_summary)
  summaries <- as.list(substitute(summaries)[-1])
  for (index in seq_along(summaries)) {
    args_summary[[index + 1]] <- summaries[[index]]
  }
  args_summary$variables <- variables %||% quote(identity(NULL))
  args_summary$.args <- substitute(summary_args)
  command_summary <- as.expression(as.call(args_summary))
  args_gq <- list(
    call_ns("stantargets", "tar_stan_mcmc_gq_run"),
    fitted_params = substitute(fitted_params),
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
    output_dir = output_dir,
    sig_figs = sig_figs,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain,
    variables = variables
  )
  command_gq <- as.expression(as.call(args_gq))
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
  target_gq <- targets::tar_target_raw(
    name = name_gq,
    command = command_gq,
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
  list(
    target_file,
    trn(identical(compile, "original"), NULL, target_lines),
    target_data,
    target_gq,
    trn(identical(draws, TRUE), target_draws, NULL),
    trn(identical(summary, TRUE), target_summary, NULL)
  )
}

#' @title Compile and run a Stan model and return the `CmdStanFit` object.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A `CmdStanFit` object.
#' @param fitted_params An existing `CmdStanMCMC` with all draws loaded
#'   into memory.
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
#' @param seed `seed` argument to `$generate_quantities()`.
#' @param output_dir `output_dir` argument to `$generate_quantities()`.
#' @param sig_figs `sig_figs` argument to `$generate_quantities()`.
#' @param parallel_chains `parallel_chains` argument to
#'   `$generate_quantities()`.
#' @param threads_per_chain `threads_per_chain` argument to
#'   `$generate_quantities()`.
#' @param variables `variables` argument to `$draws()` and `$summary()`
#'   on the `CmdStanGQ` object.
tar_stan_mcmc_gq_run <- function(
  fitted_params,
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
  output_dir,
  sig_figs,
  parallel_chains,
  threads_per_chain,
  variables
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
  fit <- model$generate_quantities(
    fitted_params = fitted_params,
    data = data,
    seed = seed,
    output_dir = output_dir,
    sig_figs = sig_figs,
    parallel_chains = parallel_chains,
    threads_per_chain = threads_per_chain
  )
  # Load all the data and return the whole unserialized fit object:
  # https://github.com/stan-dev/cmdstanr/blob/d27994f804c493ff3047a2a98d693fa90b83af98/R/fit.R#L16-L18 # nolint
  fit$draws() # Do not specify variables.
  fit
}
