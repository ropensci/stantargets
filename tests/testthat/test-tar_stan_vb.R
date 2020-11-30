tar_test("tar_stan_vb(compile = \"original\")", {
  skip_on_cran()
  tar_stan_example_file()
  dir.create("csv_files")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb(
        model,
        file = "stantargets_example.stan",
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 5L)
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(model_file), "stantargets_example.stan")
  out <- targets::tar_read(model_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out <- targets::tar_read(model_vb)
  expect_true(inherits(out, "CmdStanVB"))
  expect_true(inherits(out$draws(), "draws"))
  out <- targets::tar_read(model_draws)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 1000L)
  expect_true("lp__" %in% colnames(out))
  out <- targets::tar_read(model_summary)
  expect_true(tibble::is_tibble(out))
  expect_true("lp__" %in% out$variable)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file",
    "model_summary",
    "model_draws",
    "model_vb"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb(
        model,
        file = "stantargets_example.stan",
        data = c(tar_stan_example_data()),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(exp, "model_data")
  expect_equal(sort(out), sort(exp))
})

tar_test("tar_stan_vb(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  tar_stan_example_file()
  dir.create("csv_files")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb(
        model,
        file = "stantargets_example.stan",
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 6L)
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
  out <- targets::tar_read(model_vb)
  expect_true(inherits(out, "CmdStanVB"))
  expect_true(inherits(out$draws(), "draws"))
  out <- targets::tar_read(model_draws)
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 1000L)
  expect_true("beta" %in% colnames(out))
  out <- targets::tar_read(model_summary)
  expect_equal(out$variable, "beta")
  expect_equal(colnames(out), c("variable", "25%", "75%"))
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_lines",
    "model_file",
    "model_summary",
    "model_draws",
    "model_vb"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb(
        model,
        file = "stantargets_example.stan",
        data = c(tar_stan_example_data()),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
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
