tar_test("tar_stan_vb_rep_summary(compile = \"original\")", {
  skip_on_cran()
  tar_stan_example_file()
  dir.create("csv_files")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb_rep_summary(
        model,
        file = "stantargets_example.stan",
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        output_samples = 1000,
        batches = 2,
        reps = 2,
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 4L)
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file), "stantargets_example.stan")
  out <- targets::tar_read(model_data)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 4L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  expect_true(is.numeric(out$true_beta))
  out <- targets::tar_read(model)
  expect_true(tibble::is_tibble(out))
  expect_true(any("lp__" %in% out$variable))
  expect_equal(length(unique(table(out$.rep))), 1L)
  expect_equal(length(table(out$.rep)), 4L)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb_rep_summary(
        model,
        file = "stantargets_example.stan",
        data = c(tar_stan_example_data()),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        output_samples = 1000,
        batches = 2,
        reps = 2,
        output_dir = "csv_files"
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(exp, "model_data")
  expect_equal(sort(out), sort(exp))
})

tar_test("tar_stan_vb_rep_summary(compile = \"copy\") custom summaries", {
  skip_on_cran()
  tar_stan_example_file()
  dir.create("csv_files")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb_rep_summary(
        model,
        file = "stantargets_example.stan",
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        output_samples = 1000,
        batches = 2,
        reps = 2,
        output_dir = "csv_files",
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
      )
    )
  })
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 5L)
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file), "stantargets_example.stan")
  out <- targets::tar_read(model_data)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 4L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  expect_true(is.numeric(out$true_beta))
  out <- targets::tar_read(model)
  expect_true(tibble::is_tibble(out))
  expect_equal(out$variable, rep("beta", 4))
  expect_equal(length(unique(table(out$.rep))), 1L)
  expect_equal(length(table(out$.rep)), 4L)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "stantargets_example.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file", "model_lines", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_vb_rep_summary(
        model,
        file = "stantargets_example.stan",
        data = c(tar_stan_example_data()),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        output_samples = 1000,
        batches = 2,
        reps = 2,
        output_dir = "csv_files",
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75)))
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(exp, "model_data")
  expect_equal(sort(out), sort(exp))
})
