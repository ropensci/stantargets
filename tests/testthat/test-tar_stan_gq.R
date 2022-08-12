targets::tar_test("tar_stan_gq() *_fit args compat", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  file.create("x.stan")
  expect_error(
    tar_stan_gq(
      model,
      stan_files = "x.stan",
      data = tar_stan_example_data(),
      compile = "original",
      quiet = TRUE,
      stdout = R.utils::nullfile(),
      variables = c("a", "b"),
      variables_fit = "a"
    ),
    class = "tar_condition_validate"
  )
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_gq(compile = \"original\")", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  restore_compiled_models()
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        model,
        stan_files = "a.stan",
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 20,
        iter_warmup = 10,
        chains = 4,
        return_draws = FALSE,
        return_summary = FALSE,
        return_diagnostics = FALSE,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc_a,
        compile = "original",
        stan_files = c("a.stan", "b.stan"),
        data = model_data,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
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
    "gq_data", "gq_gq_a",
     "gq_data", "gq_gq_b",
     "gq_file_a", "gq_gq_a",
     "gq_file_b", "gq_gq_b",
     "gq_gq_a", "gq_draws_a",
     "gq_data", "gq_summary_a",
     "gq_data", "gq_summary_b",
     "gq_gq_a", "gq_summary_a",
     "gq_gq_b", "gq_draws_b",
     "gq_gq_b", "gq_summary_b",
     "model_data", "gq_data",
     "model_data", "model_mcmc_a",
     "model_file_a", "model_mcmc_a",
     "model_mcmc_a", "gq_gq_a",
     "model_mcmc_a", "gq_gq_b"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  suppressWarnings(targets::tar_make(callr_function = NULL))
  expect_equal(targets::tar_read(gq_file_a), "a.stan")
  expect_equal(targets::tar_read(gq_file_b), "b.stan")
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
  expect_equal(nrow(out1), 80L)
  expect_equal(nrow(out2), 80L)
  expect_true("y_rep[1]" %in% colnames(out1))
  expect_true("y_rep[1]" %in% colnames(out2))
  out1 <- targets::tar_read(gq_summary_a)
  out2 <- targets::tar_read(gq_summary_b)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_true("y_rep[1]" %in% out1$variable)
  expect_true("y_rep[1]" %in% out2$variable)
  original_data <- tar_read(model_data)
  y_rep <- original_data$.join_data$y_rep
  expect_equal(out1$.join_data[grepl("y_rep", out1$variable)], y_rep)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_file_b",
    "gq_gq_b",
    "gq_draws_b",
    "gq_summary_b"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        model,
        stan_files = "a.stan",
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 20,
        iter_warmup = 10,
        chains = 4,
        return_draws = FALSE,
        return_diagnostics = FALSE,
        return_summary = FALSE,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc,
        compile = "original",
        stan_files = c("a.stan", "b.stan"),
        data = tar_stan_example_data(),
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "gq_data",
    "gq_file_b",
    "gq_gq_a",
    "gq_gq_b",
    "gq_draws_a",
    "gq_draws_b",
    "gq_summary_a",
    "gq_summary_b"
  )
  expect_equal(sort(out), sort(exp))
})

targets::tar_test("tar_stan_gq(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_compile_copy()
  tar_stan_example_file("a.stan")
  tar_stan_example_file("b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
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
        return_draws = FALSE,
        return_summary = FALSE,
        return_diagnostics = FALSE,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc_a,
        stan_files = c("a.stan", "b.stan"),
        compile = "copy",
        data = model_data,
        variables = "y_rep[1]",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
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
    "gq_data", "gq_summary_a",
    "gq_data", "gq_summary_b",
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
  suppressWarnings(targets::tar_make(callr_function = NULL))
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
  expect_equal(
    sort(colnames(out1)),
    sort(c("variable", "25%", "75%", ".join_data"))
  )
  expect_equal(
    sort(colnames(out2)),
    sort(c("variable", "25%", "75%", ".join_data"))
  )
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
    list(
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
        return_draws = FALSE,
        return_summary = FALSE,
        return_diagnostics = FALSE,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      ),
      tar_stan_gq(
        gq,
        fitted_params = model_mcmc_a,
        stan_files = c("a.stan", "b.stan"),
        compile = "copy",
        data = tar_stan_example_data(),
        variables = "y_rep[1]",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
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

targets::tar_test("stan files missing", {
  expect_error(
    tar_stan_gq(
      gq,
      fitted_params = model_mcmc_a,
      stan_files = c("a.stan", "b.stan"),
      compile = "copy",
      data = tar_stan_example_data(),
      variables = "y_rep[1]",
      summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile()
    ),
    class = "tar_condition_validate"
  )
})
