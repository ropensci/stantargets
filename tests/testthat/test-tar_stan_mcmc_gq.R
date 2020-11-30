tar_test("tar_stan_mcmc(compile = \"original\")", {
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
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        output_dir = "csv_files"
      ),
      tar_stan_mcmc_gq(
        gq,
        fitted_params = model_mcmc,
        compile = "original",
        file = "stantargets_example.stan",
        data = model_data,
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_manifest(starts_with("gq"), callr_function = NULL)
  expect_equal(nrow(out), 5L)
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(gq_file), "stantargets_example.stan")
  out <- targets::tar_read(gq_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out <- targets::tar_read(gq_gq)
  expect_true(inherits(out, "CmdStanGQ"))
  expect_true(inherits(out$draws(), "array"))
  out <- targets::tar_read(gq_draws)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 4000L)
  expect_true("y_rep[1]" %in% colnames(out))
  out <- targets::tar_read(gq_summary)
  expect_true(tibble::is_tibble(out))
  expect_true("y_rep[1]" %in% out$variable)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_file",
    "gq_gq",
    "gq_draws",
    "gq_summary",
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
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        output_dir = "csv_files"
      ),
      tar_stan_mcmc_gq(
        gq,
        fitted_params = model_mcmc,
        compile = "original",
        file = "stantargets_example.stan",
        data = tar_stan_example_data(),
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(exp, "gq_data")
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
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        init = 1,
        output_dir = "csv_files"
      ),
      tar_stan_mcmc_gq(
        gq,
        fitted_params = model_mcmc,
        file = "stantargets_example.stan",
        compile = "copy",
        data = model_data,
        output_dir = "csv_files",
        variables = "y_rep[1]",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
      )
    )
  })
  out <- targets::tar_manifest(starts_with("gq"), callr_function = NULL)
  expect_equal(nrow(out), 6L)
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(gq_file), "stantargets_example.stan")
  expect_equal(
    targets::tar_read(model_lines),
    readLines("stantargets_example.stan")
  )
  out <- targets::tar_read(gq_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out <- targets::tar_read(gq_gq)
  expect_true(inherits(out, "CmdStanGQ"))
  expect_true(inherits(out$draws(), "array"))
  out <- targets::tar_read(gq_draws)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 4000L)
  expect_true("y_rep[1]" %in% colnames(out))
  expect_false("y_rep[2]" %in% colnames(out))
  out <- targets::tar_read(gq_summary)
  expect_equal(out$variable, "y_rep[1]")
  expect_equal(colnames(out), c("variable", "25%", "75%"))
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_lines",
    "gq_file",
    "gq_summary",
    "gq_draws",
    "gq_gq",
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
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        output_dir = "csv_files"
      ),
      tar_stan_mcmc_gq(
        gq,
        fitted_params = model_mcmc,
        file = "stantargets_example.stan",
        compile = "copy",
        data = tar_stan_example_data(),
        output_dir = "csv_files",
        variables = "y_rep[1]",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(exp, "gq_data")
  expect_equal(sort(out), sort(exp))
})
