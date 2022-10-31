# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("list_nonempty()", {
  expect_equal(
    list_nonempty(list(a = 1, b = NULL, c = 2)),
    list(a = 1, c = 2)
  )
  expect_equal(
    list_nonempty(list(a = 1, c = 2)),
    list(a = 1, c = 2)
  )
  expect_equal(
    list_nonempty(list(a = 1)),
    list(a = 1)
  )
  expect_equal(
    list_nonempty(list()),
    list()
  )
})

targets::tar_test("produce_seed_rep()", {
  skip_on_cran()
  skip_if(!("seed" %in% names(formals(targets::tar_option_set))))
  on.exit(targets::tar_option_reset())
  targets::tar_option_set(seed = 0L)
  out <- produce_seed_rep("x", 1L, 2L, 3L)
  expect_true(is.integer(out))
  targets::tar_option_set(seed = NA_integer_)
  out <- produce_seed_rep("x", 1L, 2L, 3L)
  expect_true(is.integer(out))
})
