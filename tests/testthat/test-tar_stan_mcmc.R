tar_test("tar_stan_mcmc(compile = \"original\")", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  tar_stan_example_file(path = "a.stan")
  tar_stan_example_file(path = "b.stan")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 100,
        iter_warmup = 50,
        chains = 4
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 11L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  exp <- tibble::tribble(
    ~from, ~to,
    "model_data", "model_mcmc_x",
    "model_data", "model_mcmc_y",
    "model_file_x", "model_mcmc_x",
    "model_file_y", "model_mcmc_y",
    "model_mcmc_x", "model_diagnostics_x",
    "model_mcmc_x", "model_draws_x",
    "model_mcmc_x", "model_summary_x",
    "model_mcmc_y", "model_diagnostics_y",
    "model_mcmc_y", "model_draws_y",
    "model_mcmc_y", "model_summary_y"
  )
  exp <- dplyr::arrange(exp, from, to)
  expect_equal(out, exp)
  # results
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(model_file_x), "a.stan")
  expect_equal(targets::tar_read(model_file_y), "b.stan")
  out <- targets::tar_read(model_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out_x <- targets::tar_read(model_mcmc_x)
  out_y <- targets::tar_read(model_mcmc_y)
  expect_true(inherits(out_x, "CmdStanMCMC"))
  expect_true(inherits(out_y, "CmdStanMCMC"))
  expect_true(inherits(out_x$draws(), "array"))
  expect_true(inherits(out_y$draws(), "array"))
  expect_true(inherits(out_x$sampler_diagnostics(), "array"))
  expect_true(inherits(out_y$sampler_diagnostics(), "array"))
  out_x <- targets::tar_read(model_draws_x)
  out_y <- targets::tar_read(model_draws_y)
  expect_true(tibble::is_tibble(out_x))
  expect_true(tibble::is_tibble(out_y))
  expect_equal(nrow(out_x), 400L)
  expect_equal(nrow(out_y), 400L)
  expect_true("lp__" %in% colnames(out_x))
  expect_true("lp__" %in% colnames(out_y))
  out_x <- targets::tar_read(model_summary_x)
  out_y <- targets::tar_read(model_summary_y)
  expect_true(tibble::is_tibble(out_x))
  expect_true(tibble::is_tibble(out_y))
  expect_true("lp__" %in% out_x$variable)
  expect_true("lp__" %in% out_y$variable)
  out_x <- targets::tar_read(model_diagnostics_x)
  out_y <- targets::tar_read(model_diagnostics_y)
  expect_true(tibble::is_tibble(out_x))
  expect_true(tibble::is_tibble(out_y))
  expect_equal(nrow(out_x), 400L)
  expect_equal(nrow(out_y), 400L)
  expect_true("treedepth__" %in% colnames(out_x))
  expect_true("treedepth__" %in% colnames(out_y))
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "a.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_x",
    "model_diagnostics_x",
    "model_summary_x",
    "model_draws_x",
    "model_mcmc_x"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 100,
        iter_warmup = 50,
        chains = 4
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    exp,
    "model_data",
    "model_diagnostics_y",
    "model_summary_y",
    "model_draws_y",
    "model_mcmc_y"
  )
  expect_equal(sort(out), sort(exp))
})

tar_test("tar_stan_mcmc(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  tar_stan_example_file()
  dir.create("csv_files")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        file = "stantargets_example.stan",
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter_sampling = 100,
        iter_warmup = 50,
        chains = 4,
        init = 1,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 7L)
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(model_file), "stantargets_example.stan")
  expect_equal(
    targets::tar_read(model_lines),
    readLines("stantargets_example.stan")
  )
  out <- targets::tar_read(model_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out <- targets::tar_read(model_mcmc)
  expect_true(inherits(out, "CmdStanMCMC"))
  expect_true(inherits(out$draws(), "array"))
  expect_true(inherits(out$sampler_diagnostics(), "array"))
  out <- targets::tar_read(model_draws)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 400L)
  expect_true("beta" %in% colnames(out))
  out <- targets::tar_read(model_summary)
  expect_equal(out$variable, "beta")
  expect_equal(colnames(out), c("variable", "25%", "75%"))
  out <- targets::tar_read(model_diagnostics)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 400L)
  expect_true("treedepth__" %in% colnames(out))
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_lines",
    "model_file",
    "model_diagnostics",
    "model_summary",
    "model_draws",
    "model_mcmc"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        file = "stantargets_example.stan",
        data = c(tar_stan_example_data()),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 100,
        iter_warmup = 50,
        chains = 4,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(exp, "model_data")
  expect_equal(sort(out), sort(exp))
})
