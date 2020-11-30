tar_test("tar_stan_compile()", {
  skip_on_cran()
  tar_stan_example_file()
  targets::tar_script({
    library(stantargets)
    tar_pipeline(
      tar_stan_compile(compile, stan_file = "stantargets_example.stan")
    )
  })
  targets::tar_make(callr_function = NULL)
  expect_equal(targets::tar_read(compile), "stantargets_example.stan")
})
