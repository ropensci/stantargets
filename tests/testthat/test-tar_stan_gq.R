tar_test("tar_stan_gq(compile = \"original\")", {
  skip_on_cran()
  tar_stan_example_file("file1.stan")
  tar_stan_example_file("file2.stan")
  targets::tar_script({
    library(stantargets)
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = "file1.stan",
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        draws = FALSE,
        summary = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc_file1,
        compile = "original",
        stan_files = c("file1.stan", "file2.stan"),
        data = model_data
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 12L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from, ~to,
    "gq_data", "gq_gq_file1",
     "gq_data", "gq_gq_file2",
     "gq_file_file1", "gq_gq_file1",
     "gq_file_file2", "gq_gq_file2",
     "gq_gq_file1", "gq_draws_file1",
     "gq_gq_file1", "gq_summary_file1",
     "gq_gq_file2", "gq_draws_file2",
     "gq_gq_file2", "gq_summary_file2",
     "model_data", "gq_data",
     "model_data", "model_mcmc_file1",
     "model_file_file1", "model_mcmc_file1",
     "model_mcmc_file1", "gq_gq_file1",
     "model_mcmc_file1", "gq_gq_file2"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(gq_file_file1), "file1.stan")
  expect_equal(targets::tar_read(gq_file_file2), "file2.stan")
  out <- targets::tar_read(gq_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out1 <- targets::tar_read(gq_gq_file1)
  out2 <- targets::tar_read(gq_gq_file2)
  expect_true(inherits(out1, "CmdStanGQ"))
  expect_true(inherits(out2, "CmdStanGQ"))
  expect_true(inherits(out1$draws(), "array"))
  expect_true(inherits(out2$draws(), "array"))
  out1 <- targets::tar_read(gq_draws_file1)
  out2 <- targets::tar_read(gq_draws_file2)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 4000L)
  expect_equal(nrow(out2), 4000L)
  expect_true("y_rep[1]" %in% colnames(out1))
  expect_true("y_rep[1]" %in% colnames(out2))
  out1 <- targets::tar_read(gq_summary_file1)
  out2 <- targets::tar_read(gq_summary_file2)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_true("y_rep[1]" %in% out1$variable)
  expect_true("y_rep[1]" %in% out2$variable)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "file2.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_file_file2",
    "gq_gq_file2",
    "gq_draws_file2",
    "gq_summary_file2"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = "file1.stan",
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        draws = FALSE,
        diagnostics = FALSE,
        summary = FALSE
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc,
        compile = "original",
        stan_files = c("file1.stan", "file2.stan"),
        data = tar_stan_example_data()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_data",
    "gq_file_file2",
    "gq_gq_file1",
    "gq_gq_file2",
    "gq_draws_file1",
    "gq_draws_file2",
    "gq_summary_file1",
    "gq_summary_file2"
  )
  expect_equal(sort(out), sort(exp))
})

tar_test("tar_stan_gq(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  tar_stan_example_file("a.stan")
  tar_stan_example_file("b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = "a.stan",
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        init = 1,
        draws = FALSE,
        summary = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc_a,
        stan_files = c("a.stan", "b.stan"),
        compile = "copy",
        data = model_data,
        variables = "y_rep[1]",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 15L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from, ~to,
    "gq_data", "gq_gq_a",
    "gq_data", "gq_gq_b",
    "gq_file_a", "gq_lines_a",
    "gq_file_b", "gq_lines_b",
    "gq_gq_a", "gq_draws_a",
    "gq_gq_a", "gq_summary_a",
    "gq_gq_b", "gq_draws_b",
    "gq_gq_b", "gq_summary_b",
    "gq_lines_a", "gq_gq_a",
    "gq_lines_b", "gq_gq_b",
    "model_data", "gq_data",
    "model_data", "model_mcmc_a",
    "model_file_a", "model_lines_a",
    "model_lines_a", "model_mcmc_a",
    "model_mcmc_a", "gq_gq_a",
    "model_mcmc_a", "gq_gq_b"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(gq_file_a), "a.stan")
  expect_equal(targets::tar_read(gq_file_b), "b.stan")
  expect_equal(
    targets::tar_read(model_lines_a),
    readLines("a.stan")
  )
  expect_equal(
    targets::tar_read(gq_lines_a),
    readLines("a.stan")
  )
  expect_equal(
    targets::tar_read(gq_lines_b),
    readLines("b.stan")
  )
  out <- targets::tar_read(gq_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out1 <- targets::tar_read(gq_gq_a)
  out2 <- targets::tar_read(gq_gq_b)
  expect_true(inherits(out1, "CmdStanGQ"))
  expect_true(inherits(out2, "CmdStanGQ"))
  expect_true(inherits(out1$draws(), "array"))
  expect_true(inherits(out2$draws(), "array"))
  out1 <- targets::tar_read(gq_draws_a)
  out2 <- targets::tar_read(gq_draws_b)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 4000L)
  expect_equal(nrow(out2), 4000L)
  expect_true("y_rep[1]" %in% colnames(out1))
  expect_true("y_rep[1]" %in% colnames(out2))
  expect_false("y_rep[2]" %in% colnames(out1))
  expect_false("y_rep[2]" %in% colnames(out2))
  out1 <- targets::tar_read(gq_summary_a)
  out2 <- targets::tar_read(gq_summary_b)
  expect_equal(out1$variable, "y_rep[1]")
  expect_equal(out2$variable, "y_rep[1]")
  expect_equal(colnames(out1), c("variable", "25%", "75%"))
  expect_equal(colnames(out2), c("variable", "25%", "75%"))
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_lines_b",
    "gq_file_b",
    "gq_summary_b",
    "gq_draws_b",
    "gq_gq_b"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mcmc(
        model,
        stan_files = "a.stan",
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        init = 1,
        draws = FALSE,
        summary = FALSE,
        diagnostics = FALSE
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc_a,
        stan_files = c("a.stan", "b.stan"),
        compile = "copy",
        data = tar_stan_example_data(),
        variables = "y_rep[1]",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_data",
    "gq_lines_b",
    "gq_file_b",
    "gq_summary_a",
    "gq_summary_b",
    "gq_draws_a",
    "gq_draws_b",
    "gq_gq_a",
    "gq_gq_b"
  )
  expect_equal(sort(out), sort(exp))
})
