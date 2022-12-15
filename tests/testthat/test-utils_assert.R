# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("assert_stan_file()", {
  tmp <- tempfile()
  tar_stan_example_file(tmp)
  expect_silent(assert_stan_file(tmp))
  expect_error(assert_stan_file(tempdir()), class = "tar_condition_validate")
})

targets::tar_test("tar_stan_deprecate()", {
  tmp <- tempfile()
  tar_stan_example_file(tmp)
  expect_silent(tar_stan_deprecate(NULL, "y"))
  expect_warning(
    tar_stan_deprecate("x", "y"),
    class = "tar_condition_deprecate"
  )
})

targets::tar_test("assert_transform()", {
  targets::tar_option_set(envir = new.env(parent = emptyenv()))
  envir <- targets::tar_option_get("envir")
  envir[["good1"]] <- function(data, draws) draws
  envir[["good2"]] <- function(draws, data, x) draws
  envir[["bad1"]] <- 123
  envir[["bad2"]] <- function(dat, draws) draws
  expect_silent(assert_transform(NULL))
  expect_silent(assert_transform(quote(good1)))
  expect_silent(assert_transform(quote(good2)))
  class <- "tar_condition_validate"
  expect_error(assert_transform(123), class = class)
  expect_error(assert_transform(quote(nothing)), class = class)
  expect_error(assert_transform(quote(bad1)), class = class)
  expect_error(assert_transform(quote(bad2)), class = class)
})
