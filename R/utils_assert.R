assert_stan_file <- function(stan_file) {
  targets::tar_assert_chr(stan_file)
  targets::tar_assert_path(stan_file)
  targets::tar_assert_not_dir(stan_file)
}

tar_stan_deprecate <- function(x, alternative) {
  if (!is.null(x)) {
    targets::tar_warn_deprecate(
      "Argument ",
      deparse(substitute(x)),
      " is deprecated. Use ",
      alternative,
      " instead."
    )
  }
}

assert_variables_fit <- function(variables, variables_fit) {
  if (length(setdiff(variables, variables_fit))) {
    msg <- paste(
      "the draws target must only have",
      "variables available in the CmdStanFit target. Control these",
      "variables with arguments variables and variables_fit."
    )
    targets::tar_throw_validate(msg)
  }
}

assert_inc_warmup_fit <- function(inc_warmup, inc_warmup_fit) {
 if (isTRUE(inc_warmup) && isFALSE(inc_warmup_fit)) {
    targets::tar_throw_validate(
      "inc_warmup cannot be TRUE if inc_warmup_fit is FALSE."
    )
  }
}
