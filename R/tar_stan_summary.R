#' @title One summary of a `CmdStanFit` object.
#' @export
#' @description Target to run the `$summary()` method of a `CmdStanFit` object.
#' @details [tar_stan_mcmc()] etc. with `summary = TRUE` already gives you a
#'   target with a `$summary()` object.
#'   Use `tar_stan_summary()` to create additional specialized summaries.
#' @return A `targets::tar_target()` object to summarize a `CmdStanFit` object.
#' @inheritParams tar_stan_mcmc
#' @inheritParams targets::tar_target
#' @inheritParams cmdstanr::`fit-method-summary`
#' @param fit Symbol, name of a `CmdStanFit` object or an upstream target
#'   that returns a `CmdStanFit` object.
#' @param summaries Optional list of summary functions passed to `...` in
#'   `posterior::summarize_draws()` through `$summary()`
#'   on the `CmdStanFit` object.
#' @param summary_args Optional list of summary function arguments passed to
#'    `.args` in `posterior::summarize_draws()` through `$summary()`
#'    on the `CmdStanFit` object.
#' @examples
#' # First, write your Stan model file. Example:
#' # tar_stan_example_file() # Writes stantargets_example.stan
#' # Then in _targets.R, write the pipeline:
#' list(
#'   # Run a model and produce default summaries.
#'   tar_stan_mcmc(
#'     your_model,
#'     stan_files = "stantargets_example.stan",
#'     data = tar_stan_example_data()
#'   ),
#'   # Produce a more specialized summary
#'   tar_stan_summary(
#'     your_summary,
#'     fit = your_model,
#'     variables = "beta",
#'     summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
#'   )
#' )
tar_stan_summary <- function(
  name,
  fit,
  variables = NULL,
  summaries = NULL,
  summary_args = NULL,
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
  name <- deparse_language(substitute(name))
  fit <- deparse_language(substitute(fit))
  command <- tar_stan_summary_call(
    sym_fit = rlang::sym(fit),
    summaries = substitute(summaries),
    summary_args = substitute(summary_args),
    variables = variables
  )
  targets::tar_target_raw(
    name = name,
    command = command,
    packages = character(0),
    format = "fst_tbl",
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    cue = cue
  )
}

tar_stan_summary_call <- function(
  sym_fit,
  summaries,
  summary_args,
  variables
) {
  sym_summary <- rlang::sym("summary")
  if (!is.null(summaries)) {
    summaries <- trn(is.list(summaries), summaries, as.list(summaries[-1]))
  }
  method <- call_function("$", list(sym_fit, sym_summary))
  args <- list(method)
  args$variables <- variables %||% quote(identity(NULL))
  args$.args <- summary_args
  args <- c(args, summaries)
  expr <- as.call(list(quote(tibble::tibble), as.call(args)))
  as.expression(expr)
}
