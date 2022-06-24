#' @title Generate a batch of data
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A list of Stan datasets containing data and dataset IDs.
#' @param reps Positive integer of length 1, number of reps to run.
#' @param command R code to run to generate one dataset.
#' @examples
#' tar_stan_rep_data_batch(2, tar_stan_example_data())
tar_stan_rep_data_batch <- function(reps, command) {
  envir <- parent.frame()
  command <- substitute(command)
  purrr::map(seq_len(reps), ~tar_stan_rep_data_rep(.x, command, envir))
}

tar_stan_rep_data_rep <- function(rep, command, envir) {
  out <- eval(command, envir = envir)
  out$.dataset_id <- paste0(targets::tar_name(), "_", rep)
  out
}

list_nonempty <- function(list) {
  index <- vapply(list, FUN.VALUE = logical(1), FUN = function(x) {
    is.null(x)
  })
  list[!index]
}
