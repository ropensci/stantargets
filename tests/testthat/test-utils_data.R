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
