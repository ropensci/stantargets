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
