#' @title Post-process Stan output
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A data frame of user-friendly Stan output.
#' @inheritParams tar_stan_summary
#' @inheritParams tar_stan_mcmc_rep
#' @param fit A Stan fit object.
#' @param data List, Stan dataset.
#' @param inc_warmup Logical, whether to include the warmup draws.
tar_stan_output <- function(
  fit,
  output,
  summaries,
  summary_args,
  variables,
  inc_warmup,
  data,
  data_copy
) {
  out <- switch(
    output,
    summary = tar_stan_summary(fit, data, summaries, summary_args, variables),
    draws = tar_stan_draws(fit, variables, inc_warmup),
    diagnostics = tar_stan_diagnostics(fit, inc_warmup)
  )
  out <- tibble::as_tibble(out)
  out <- tar_stan_rep_scalars(out, data, data_copy)
  out$.rep <- digest::digest(stats::runif(1), algo = "xxhash32")
  out
}

tar_stan_summary <- function(fit, data, summaries, summary_args, variables) {
  command <- tar_stan_summary_call(
    sym_fit = rlang::sym("fit"),
    summaries = summaries,
    summary_args = summary_args,
    variables = variables
  )
  eval(command)
}

tar_stan_draws <- function(fit, variables, inc_warmup) {
  out <- trn(
    is.null(inc_warmup),
    fit$draws(variables = variables),
    fit$draws(variables = variables, inc_warmup = inc_warmup)
  )
  tibble::as_tibble(posterior::as_draws_df(out))
}

tar_stan_diagnostics <- function(fit, inc_warmup) {
  out <- fit$sampler_diagnostics(inc_warmup = inc_warmup)
  tibble::as_tibble(posterior::as_draws_df(out))
}

tar_stan_rep_scalars <- function(x, data, data_copy) {
  for (var in data_copy) {
    msg <- paste(var, "in data_copy must have length 1 in data.")
    assert_scalar(data[[var]], msg)
    msg <- paste(var, "in data_copy must not already be in output.")
    assert_not_in(var, colnames(x), msg)
    x[[var]] <- data[[var]]
  }
  x
}
