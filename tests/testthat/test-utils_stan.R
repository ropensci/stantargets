# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("produce_stan_names()", {
  expect_equal(
    produce_stan_names(c(j = "x/a.stan", k = "x/b.stan")),
    c("j", "k")
  )
  expect_equal(
    produce_stan_names(c("x/a.stan", "x/b.stan")),
    c("a", "b")
  )
  expect_error(
    produce_stan_names(c("x/a.stan", "y/a.stan")),
    class = "condition_validate"
  )
  expect_equal(
    produce_stan_names(c(a = "x/a.stan", b = "y/a.stan")),
    c("a", "b")
  )
  expect_error(
    produce_stan_names(c("x/a.stan", y = "y/asdf.stan")),
    class = "condition_validate"
  )
})
