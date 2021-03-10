# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_summary() with defaults", {
  skip_on_cran()
  restore_compiled_models()
  tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        model,
        stan_files = "a.stan",
        data = tar_stan_example_data(),
        init = 1,
        summary = FALSE,
        draws = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_summary(summary, fit = model_mcmc_a)
    )
  })
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  out <- tar_read(summary)
  expect_true(tibble::is_tibble(out))
  expect_true(nrow(out) > 1L)
  expect_true("lp__" %in% out$variable)
})

targets::tar_test("tar_stan_summary() with custom summaries", {
  skip_on_cran()
  restore_compiled_models()
  tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        model,
        stan_files = "a.stan",
        data = tar_stan_example_data(),
        init = 1,
        summary = FALSE,
        draws = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_summary(
        summary,
        fit = model_mcmc_a,
        data = model_data,
        variables = "beta",
        summaries = list(
          ~quantile(.x, probs = c(0.25, 0.75)),
          custom = function(x, my_arg) my_arg
        ),
        summary_args = list(my_arg = 123L)
      )
    )
  })
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  out <- tar_read(summary)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 1L)
  expect_equal(out$variable, "beta")
  expect_equal(
    sort(colnames(out)),
    sort(c("variable", "25%", "75%", "custom", ".join_data"))
  )
  expect_true(all(out$custom == 123L))
  data <- tar_read(model_data)
  beta <- data$.join_data$beta
  expect_equal(out$.join_data, beta)
})
