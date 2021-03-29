throw_validate <- function(...) {
  stop(tar_condition_validate(...))
}

tar_condition_validate <- function(...) {
  structure(
    list(message = paste0(..., collapse = ""), call = NULL),
    class = c(
      "tar_condition_validate",
      "tar_condition_tarchetypes",
      "error",
      "condition"
    )
  )
}
