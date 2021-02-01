# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_example_data()", {
  out <- tar_stan_example_data(n = 50L)
  expect_equal(out$n, 50L)
  expect_equal(length(out$x), 50L)
  expect_equal(length(out$y), 50L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
})
