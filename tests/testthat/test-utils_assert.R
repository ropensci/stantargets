targets::tar_test("assert_chr()", {
  expect_silent(assert_chr(letters))
  expect_error(assert_chr(123), class = "condition_validate")
})

targets::tar_test("assert_not_in()", {
  expect_silent(assert_not_in("1", letters))
  expect_error(assert_not_in("a", letters), class = "condition_validate")
})

targets::tar_test("assert_nzchar()", {
  expect_silent(assert_nzchar(letters))
  expect_error(assert_nzchar(c("x", "")), class = "condition_validate")
})

targets::tar_test("assert_package()", {
  expect_error(assert_package("_illegal"), class = "condition_validate")
})

targets::tar_test("assert_path()", {
  file.create("x")
  expect_error(assert_path(c("x", "y")), class = "condition_validate")
  file.create("y")
  expect_silent(assert_path(c("x", "y")))
})

targets::tar_test("assert_scalar()", {
  expect_silent(assert_scalar(1))
  expect_error(assert_scalar(letters), class = "condition_validate")
})

targets::tar_test("assert_unique()", {
  expect_silent(assert_unique(letters))
  expect_error(assert_unique(c("1", "1")), class = "condition_validate")
})
