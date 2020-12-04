tar_test("tar_stan_summary() with defaults", {
  skip_on_cran()
  tar_stan_example_file("x.stan")
  tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    targets::tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = "x.stan",
        data = tar_stan_example_data(),
        init = 1,
        summary = FALSE,
        draws = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_summary(summary, fit = model_mcmc_x)
    )
  })
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  out <- tar_read(summary)
  expect_true(tibble::is_tibble(out))
  expect_true(nrow(out) > 1L)
  expect_true("lp__" %in% out$variable)
})

tar_test("tar_stan_summary() with custom settings", {
  skip_on_cran()
  tar_stan_example_file("x.stan")
  tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    targets::tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = "x.stan",
        data = tar_stan_example_data(),
        init = 1,
        summary = FALSE,
        draws = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_summary(
        summary,
        fit = model_mcmc_x,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
      )
    )
  })
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  out <- tar_read(summary)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 1L)
  expect_equal(out$variable, "beta")
  expect_equal(colnames(out), c("variable", "25%", "75%"))
})
