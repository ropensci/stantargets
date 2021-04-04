#' @title Write an example Stan model file.
#' @export
#' @description Overwrites the file at `path` with a built-in example
#'   Stan model file.
#' @family examples
#' @return `NULL` (invisibly).
#' @param path Character of length 1, file path to write the model file.
#' @examples
#' path <- tempfile(pattern = "", fileext = ".stan")
#' tar_stan_example_file(path = path)
#' writeLines(readLines(path))
tar_stan_example_file <- function(
  path = tempfile(pattern = "", fileext = ".stan")
) {
  src <- system.file("example.stan", package = "stantargets", mustWork = TRUE)
  file.copy(src, path, overwrite = TRUE, copy.date = TRUE)
  invisible()
}
