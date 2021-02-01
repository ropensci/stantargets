# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_compile()", {
  skip_on_cran()
  tar_stan_example_file("x.stan")
  targets::tar_script({
    list(
      tar_stan_compile(compile, stan_file = "x.stan")
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(compile), "x.stan")
})
