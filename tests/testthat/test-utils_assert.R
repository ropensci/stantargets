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
