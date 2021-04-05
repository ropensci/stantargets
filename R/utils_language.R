as_symbols <- function(x) {
  lapply(x, as.symbol)
}

call_ns <- function(pkg, fun) {
  call_function("::", as_symbols(c(pkg, fun)))
}

call_function <- function(name, args) {
  as.call(c(as.symbol(name), args))
}

deparse_language <- function(x) {
  if_any(!is.character(x) && !is.null(x), safe_deparse(x), x)
}

safe_deparse <- function(x, collapse = "\n", backtick = TRUE) {
  out <- direct_deparse(
    x,
    control = deparse_control_custom,
    backtick = backtick
  )
  if_any(length(out) > 1L, paste(out, collapse = collapse), out)
}

deparse_control_custom <- .deparseOpts(c("keepNA", "keepInteger"))

direct_deparse <- function(...) {
  produce_direct_deparse()(...)
}

produce_direct_deparse <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}

tar_tidy_eval <- function(expr, envir, tidy_eval) {
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  expr
}
