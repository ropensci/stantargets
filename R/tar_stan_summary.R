#' @title One summary of a `CmdStanFit` object
#' @export
#' @description Create a target to run the `$summary()`
#'   method of a `CmdStanFit` object.
#' @details [tar_stan_mcmc()] etc. with `summary = TRUE` already gives you a
#'   target with output from the `$summary()` method.
#'   Use `tar_stan_summary()` to create additional specialized summaries.
#' @return `tar_stan_summary()` returns target object to
#'   summarize a `CmdStanFit` object. The return value of the target
#'   is a tidy data frame of summaries returned by the `$summary()`
#'   method of the `CmdStanFit` object.
#'   See the "Target objects" section for background.
#' @inheritSection tar_stan_compile Target objects
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
#' # First, write your Stan model file, e.g. model.stan.
#' # Then in _targets.R, write a pipeline like this:
#' if (Sys.getenv("TAR_LONG_EXAMPLES") == "true") {
#' targets::tar_dir({ # tar_dir() runs code from a temporary directory.
#' # Running inside a temporary directory to avoid
#' # modifying the user's file space. The file "model.stan"
#' # created below lives in a temporary directory.
#' # This satisfies CRAN policies.
#' tar_stan_example_file("model.stan")
#' targets::tar_script({
#' library(stantargets)
#' list(
#'   # Run a model and produce default summaries.
#'   tar_stan_mcmc(
#'     your_model,
#'     stan_files = "model.stan",
#'     data = tar_stan_example_data()
#'   ),
#'   # Produce a more specialized summary
#'   tar_stan_summary(
#'     your_summary,
#'     fit = your_model_mcmc_model,
#'     data = your_model_data_model,
#'     variables = "beta",
#'     summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
#'   )
#' )}, ask = FALSE)
#' targets::tar_make()
#' })
#' }
tar_stan_summary <- function(
  name,
  fit,
  data = NULL,
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
  command <- tar_stan_summary_call(
    sym_fit = substitute(fit),
    sym_data = substitute(data),
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
  sym_data,
  summaries,
  summary_args,
  variables
) {
  sym_summary <- as.symbol("summary")
  if (!is.null(summaries)) {
    summaries <- trn(is.list(summaries), summaries, as.list(summaries[-1]))
  }
  method <- call_function("$", list(sym_fit, sym_summary))
  args <- list(method)
  args$variables <- variables %|||% quote(identity(NULL))
  args$.args <- summary_args
  args <- c(args, summaries)
  expr <- as.call(list(quote(tibble::tibble), as.call(args)))
  expr <- as.call(
    list(
      quote(stantargets::tar_stan_summary_join_data),
      summaries = expr,
      data = sym_data
    )
  )
  as.expression(expr)
}

#' @title Join some Stan data to summary output
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A data frame of user-friendly Stan output.
#' @inheritParams tar_stan_mcmc
#' @param summaries A data frame of Stan posterior summaries.
tar_stan_summary_join_data <- function(summaries, data) {
  summaries$.join_data <- purrr::map_dbl(
    summaries$variable,
    ~tar_stan_summary_join_data_scalar(.x, data$.join_data)
  )
  summaries
}

tar_stan_summary_join_data_scalar <- function(text, data) {
  out <- try(eval(parse(text = text), envir = data), silent = TRUE)
  if (!is.vector(out) || length(out) != 1L) {
    out <- NA_real_
  }
  out
}
