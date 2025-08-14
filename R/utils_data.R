#' @title Generate a batch of data
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @return A list of Stan datasets containing data and dataset IDs.
#' @param reps Positive integer of length 1, number of reps to run.
#' @param batch Positive integer of length 1, index of the current batch.
#' @param command R code to run to generate one dataset.
#' @examples
#' tar_stan_rep_data_batch(2, 1, tar_stan_example_data())
tar_stan_rep_data_batch <- function(reps, batch, command) {
  envir <- parent.frame()
  command <- substitute(command)
  purrr::map(
    seq_len(reps),
    ~ tar_stan_rep_data_rep(.x, reps, batch, command, envir)
  )
}

tar_stan_rep_data_rep <- function(rep, reps, batch, command, envir) {
  name <- targets::tar_definition()$pedigree$parent
  seed <- produce_seed_rep(name = name, batch = batch, rep = rep, reps = reps)
  out <- if_any(
    anyNA(seed),
    eval(command, envir = envir),
    withr::with_seed(
      seed = seed,
      code = eval(command, envir = envir)
    )
  )
  out$.dataset_id <- paste0(targets::tar_name(), "_", rep)
  out$.seed <- as.integer(seed)
  out
}

produce_seed_rep <- function(name, batch, rep, reps) {
  seed <- if_any(
    "seed" %in% names(formals(targets::tar_option_set)),
    targets::tar_option_get("seed"),
    0L
  )
  if (anyNA(seed)) {
    return(NA_integer_)
  }
  scalar <- paste(name, rep + reps * (batch - 1))
  abs(targets::tar_seed_create(as.character(scalar), global_seed = seed))
}

list_nonempty <- function(list) {
  index <- vapply(list, FUN.VALUE = logical(1), FUN = function(x) {
    is.null(x)
  })
  list[!index]
}
