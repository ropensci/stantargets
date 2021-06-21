as_symbols <- function(x) {
  lapply(x, as.symbol)
}

call_ns <- function(pkg, fun) {
  call_function("::", as_symbols(c(pkg, fun)))
}

call_function <- function(name, args) {
  as.call(c(as.symbol(name), args))
}
