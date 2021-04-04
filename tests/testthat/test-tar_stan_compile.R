# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_compile()", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  restore_compiled_models()
  targets::tar_script({
    list(
      tar_stan_compile(
        compile,
        stan_file = "a.stan",
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  targets::tar_make(callr_function = NULL)
  out <- targets::tar_read(compile)
  expect_equal(length(out), 2)
  expect_equal(out[1], "a.stan")
})

targets::tar_test("stan file missing", {
  expect_error(
    tar_stan_compile(
      compile,
      stan_file = "a.stan",
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile()
    ),
    class = "tar_condition_validate"
  )
})
