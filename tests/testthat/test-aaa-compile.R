tar_test("precompile models", {
  skip_if_missing_cmdstan()
  compile_models()
  for (file in c("a.stan", "b.stan", "a", "b")) {
    expect_true(file.exists(compiled_path("a.stan")))
  }
})
