tar_stan_rep_output <- function(
  fit,
  output,
  summaries,
  summary_args,
  variables,
  inc_warmup,
  data,
  copy_data
) {
  out <- switch(
    output,
    summary = tar_stan_rep_summary(fit, summaries, summary_args, variables),
    draws = tar_stan_rep_draws(fit, variables, inc_warmup),
    diagnostics = tar_stan_rep_diagnostics(fit, inc_warmup)
  )
  out <- tibble::as_tibble(out)
  out <- tar_stan_rep_scalars(out, data, copy_data)
  out$.rep <- digest::digest(stats::runif(1), algo = "xxhash32")
  out
}

tar_stan_rep_summary <- function(fit, summaries, summary_args, variables) {
  command <- tar_stan_summary_call(
    sym_fit = rlang::sym("fit"),
    summaries = summaries,
    summary_args = summary_args,
    variables = variables
  )
  eval(command)
}

tar_stan_rep_draws <- function(fit, variables, inc_warmup) {
  out <- trn(
    is.null(inc_warmup),
    fit$draws(variables = variables),
    fit$draws(variables = variables, inc_warmup = inc_warmup)
  )
  tibble::as_tibble(posterior::as_draws_df(out))
}

tar_stan_rep_diagnostics <- function(fit, inc_warmup) {
  out <- fit$sampler_diagnostics(inc_warmup = inc_warmup)
  tibble::as_tibble(posterior::as_draws_df(out))
}

tar_stan_rep_scalars <- function(x, data, copy_data) {
  for (var in copy_data) {
    msg <- paste(var, "in copy_data must have length 1 in data.")
    assert_scalar(data[[var]], msg)
    msg <- paste(var, "in copy_data must not already be in output.")
    assert_not_in(var, colnames(x), msg)
    x[[var]] <- data[[var]]
  }
  x
}
