list_nonempty <- function(list) {
  index <- vapply(list, FUN.VALUE = logical(1), FUN = function(x) {
    is.null(x)
  })
  list[!index]
}
