targets::tar_test("tar_stan_mcmc() *_fit args compat", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  file.create("x.stan")
  expect_error(
    tar_stan_mcmc(
      model,
      stan_files = "x.stan",
      data = tar_stan_example_data(),
      compile = "original",
      quiet = TRUE,
      refresh = 0,
      init = 1,
      iter_sampling = 100,
      iter_warmup = 50,
      chains = 4,
      stdout = R.utils::nullfile(),
      variables = c("a", "b"),
      variables_fit = "a"
    ),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_stan_mcmc(
      model,
      stan_files = "x.stan",
      data = tar_stan_example_data(),
      compile = "original",
      quiet = TRUE,
      refresh = 0,
      init = 1,
      iter_sampling = 100,
      iter_warmup = 50,
      chains = 4,
      stdout = R.utils::nullfile(),
      inc_warmup = TRUE,
      inc_warmup_fit = FALSE
    ),
    class = "tar_condition_validate"
  )
})

# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_mcmc(compile = \"original\")", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  restore_compiled_models()
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
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
        chains = 4,
        stdout = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 11L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from,
    ~to,
    "model_data",
    "model_mcmc_x",
    "model_data",
    "model_mcmc_y",
    "model_data",
    "model_summary_x",
    "model_data",
    "model_summary_y",
    "model_file_x",
    "model_mcmc_x",
    "model_file_y",
    "model_mcmc_y",
    "model_mcmc_x",
    "model_diagnostics_x",
    "model_mcmc_x",
    "model_draws_x",
    "model_mcmc_x",
    "model_summary_x",
    "model_mcmc_y",
    "model_diagnostics_y",
    "model_mcmc_y",
    "model_draws_y",
    "model_mcmc_y",
    "model_summary_y"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  suppressWarnings(targets::tar_make(callr_function = NULL))
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
  original_data <- tar_read(model_data)
  beta <- original_data$.join_data$beta
  y_rep <- original_data$.join_data$y_rep
  expect_equal(out_x$.join_data[out_x$variable == "beta"], beta)
  expect_equal(out_x$.join_data[grepl("y_rep", out_x$variable)], y_rep)
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
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
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
        chains = 4,
        stdout = R.utils::nullfile()
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

targets::tar_test("tar_stan_mcmc(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  skip_compile_copy()
  tar_stan_example_file(path = "a.stan")
  tar_stan_example_file(path = "b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter_sampling = 100,
        iter_warmup = 50,
        chains = 4,
        init = 1,
        variables = "beta",
        summaries = list(~ quantile(.x, probs = c(0.25, 0.75))),
        stdout = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 13L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from,
    ~to,
    "model_data",
    "model_summary_a",
    "model_data",
    "model_summary_b",
    "model_data",
    "model_mcmc_a",
    "model_data",
    "model_mcmc_b",
    "model_file_a",
    "model_lines_a",
    "model_file_b",
    "model_lines_b",
    "model_lines_a",
    "model_mcmc_a",
    "model_lines_b",
    "model_mcmc_b",
    "model_mcmc_a",
    "model_diagnostics_a",
    "model_mcmc_a",
    "model_draws_a",
    "model_mcmc_a",
    "model_summary_a",
    "model_mcmc_b",
    "model_diagnostics_b",
    "model_mcmc_b",
    "model_draws_b",
    "model_mcmc_b",
    "model_summary_b"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  suppressWarnings(targets::tar_make(callr_function = NULL))
  expect_equal(targets::tar_read(model_file_a), "a.stan")
  expect_equal(targets::tar_read(model_file_b), "b.stan")
  expect_equal(
    targets::tar_read(model_lines_a),
    readLines("a.stan")
  )
  expect_equal(
    targets::tar_read(model_lines_b),
    readLines("b.stan")
  )
  out <- targets::tar_read(model_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out_a <- targets::tar_read(model_mcmc_a)
  out_b <- targets::tar_read(model_mcmc_b)
  expect_true(inherits(out_a, "CmdStanMCMC"))
  expect_true(inherits(out_b$draws(), "array"))
  expect_true(inherits(out_a$sampler_diagnostics(), "array"))
  expect_true(inherits(out_b$sampler_diagnostics(), "array"))
  out_a <- targets::tar_read(model_draws_a)
  out_b <- targets::tar_read(model_draws_b)
  expect_true(tibble::is_tibble(out_a))
  expect_true(tibble::is_tibble(out_b))
  expect_equal(nrow(out_a), 400L)
  expect_equal(nrow(out_b), 400L)
  expect_true("beta" %in% colnames(out_a))
  expect_true("beta" %in% colnames(out_b))
  out_a <- targets::tar_read(model_summary_a)
  out_b <- targets::tar_read(model_summary_b)
  expect_equal(out_a$variable, "beta")
  expect_equal(out_b$variable, "beta")
  expect_equal(
    sort(colnames(out_a)),
    sort(c("variable", "25%", "75%", ".join_data"))
  )
  expect_equal(
    sort(colnames(out_b)),
    sort(c("variable", "25%", "75%", ".join_data"))
  )
  out_a <- targets::tar_read(model_diagnostics_a)
  out_b <- targets::tar_read(model_diagnostics_b)
  expect_true(tibble::is_tibble(out_a))
  expect_true(tibble::is_tibble(out_b))
  expect_equal(nrow(out_a), 400L)
  expect_equal(nrow(out_b), 400L)
  expect_true("treedepth__" %in% colnames(out_a))
  expect_true("treedepth__" %in% colnames(out_b))
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_lines_b",
    "model_file_b",
    "model_diagnostics_b",
    "model_summary_b",
    "model_draws_b",
    "model_mcmc_b"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 100,
        iter_warmup = 50,
        chains = 4,
        variables = "beta",
        summaries = list(~ quantile(.x, probs = c(0.25, 0.75))),
        stdout = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_data",
    "model_lines_b",
    "model_file_b",
    "model_diagnostics_a",
    "model_summary_a",
    "model_draws_a",
    "model_mcmc_a",
    "model_diagnostics_b",
    "model_summary_b",
    "model_draws_b",
    "model_mcmc_b"
  )
  expect_equal(sort(out), sort(exp))
})

targets::tar_test("stan files missing", {
  expect_error(
    tar_stan_mcmc(
      model,
      stan_files = c("a.stan", "b.stan"),
      data = c(tar_stan_example_data()),
      compile = "copy",
      quiet = TRUE,
      refresh = 0,
      init = 1,
      iter_sampling = 100,
      iter_warmup = 50,
      chains = 4,
      variables = "beta",
      summaries = list(~ quantile(.x, probs = c(0.25, 0.75))),
      stdout = R.utils::nullfile()
    ),
    class = "tar_condition_validate"
  )
})
