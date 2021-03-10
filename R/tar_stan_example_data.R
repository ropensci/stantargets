#' @title Example data for [tar_stan_example_file()]
#' @export
#' @description An example dataset compatible with the model file
#'   from [tar_stan_example_file()].
#' @return List, dataset compatible with the model file from
#'   [tar_stan_example_file()].
#' @param n Integer of length 1, number of data points.
#' @examples
#' tar_stan_example_data()
tar_stan_example_data <- function(n = 10L) {
  beta <- stats::rnorm(n = 1, mean = 0, sd = 1)
  x <- seq(from = -1, to = 1, length.out = n)
  y <- stats::rnorm(n, x * beta, 1)
  list(
    n = n,
    x = x,
    y = y,
    true_beta = beta,
    .join_data = list(beta = beta, y_rep = y)
  )
}
