# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("assert_chr()", {
  expect_silent(assert_chr(letters))
  expect_error(assert_chr(123), class = "tar_condition_validate")
})

targets::tar_test("assert_nonempty()", {
  expect_silent(assert_nonempty(letters))
  expect_error(assert_nonempty(NULL), class = "tar_condition_validate")
})

targets::tar_test("assert_not_in()", {
  expect_silent(assert_not_in("1", letters))
  expect_error(assert_not_in("a", letters), class = "tar_condition_validate")
})

targets::tar_test("assert_nzchar()", {
  expect_silent(assert_nzchar(letters))
  expect_error(assert_nzchar(c("x", "")), class = "tar_condition_validate")
})

targets::tar_test("assert_package()", {
  expect_error(assert_package("_illegal"), class = "tar_condition_validate")
})

targets::tar_test("assert_path()", {
  file.create("x")
  expect_error(assert_path(c("x", "y")), class = "tar_condition_validate")
  file.create("y")
  expect_silent(assert_path(c("x", "y")))
})

targets::tar_test("assert_scalar()", {
  expect_silent(assert_scalar(1))
  expect_error(assert_scalar(letters), class = "tar_condition_validate")
})

targets::tar_test("assert_stan_file()", {
  tmp <- tempfile()
  tar_stan_example_file(tmp)
  expect_silent(assert_stan_file(tmp))
  expect_error(assert_stan_file(tempdir()), class = "tar_condition_validate")
})

targets::tar_test("assert_unique()", {
  expect_silent(assert_unique(letters))
  expect_error(assert_unique(c("1", "1")), class = "tar_condition_validate")
})
