# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_gq_rep_summary(compile = \"original\")", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  restore_compiled_models()
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        fit,
        stan_files = "a.stan",
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
        diagnostics = FALSE,
        log = R.utils::nullfile()
      ),
      tar_stan_gq_rep_summary(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = tar_stan_example_data(),
        fitted_params = fit_mcmc_a,
        compile = "original",
        quiet = TRUE,
        batches = 2,
        reps = 2,
        combine = TRUE,
        log = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 10L)
  out1 <- targets::tar_manifest(model_file_x, callr_function = NULL)
  out2 <- targets::tar_manifest(model_file_y, callr_function = NULL)
  expect_true(grepl("tar_stan_compile_run", out1$command))
  expect_true(grepl("tar_stan_compile_run", out2$command))
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from, ~to,
    "fit_data", "fit_mcmc_a",
    "fit_file_a", "fit_mcmc_a",
    "fit_mcmc_a", "model_x",
    "fit_mcmc_a", "model_y",
    "model_batch", "model_data",
    "model_data", "model_x",
    "model_data", "model_y",
    "model_file_x", "model_x",
    "model_file_y", "model_y",
    "model_x", "model",
    "model_y", "model"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file_x), "a.stan")
  expect_equal(targets::tar_read(model_file_y), "b.stan")
  out <- targets::tar_read(model_data)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 5L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  expect_true(is.numeric(out$true_beta))
  out1 <- targets::tar_read(model_x)
  out2 <- targets::tar_read(model_y)
  out <- targets::tar_read(model)
  expect_false("n" %in% colnames(out))
  expect_false("true_beta" %in% colnames(out))
  expect_equal(dplyr::bind_rows(out1, out2), out)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_true("rhat" %in% colnames(out1))
  expect_true("rhat" %in% colnames(out2))
  expect_equal(length(unique(table(out1$.rep))), 1L)
  expect_equal(length(unique(table(out2$.rep))), 1L)
  expect_equal(length(table(out1$.rep)), 4L)
  expect_equal(length(table(out2$.rep)), 4L)
  expect_equal(nrow(out1), 40L)
  expect_equal(nrow(out2), 40L)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_y", "model_y", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        fit,
        stan_files = "a.stan",
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
        diagnostics = FALSE,
        log = R.utils::nullfile()
      ),
      tar_stan_gq_rep_summary(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = c(tar_stan_example_data()),
        fitted_params = fit_mcmc_a,
        compile = "original",
        quiet = TRUE,
        batches = 2,
        reps = 2,
        combine = TRUE,
        log = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_y", "model_y", "model", "model_x", "model_data")
  expect_equal(sort(out), sort(exp))
})

targets::tar_test("tar_stan_gq_rep_summary(compile = \"copy\") custom", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_compile_copy()
  tar_stan_example_file("a.stan")
  tar_stan_example_file("b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        fit,
        stan_files = "a.stan",
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
        diagnostics = FALSE,
        log = R.utils::nullfile()
      ),
      tar_stan_gq_rep_summary(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = tar_stan_example_data(),
        fitted_params = fit_mcmc_a,
        compile = "copy",
        quiet = TRUE,
        batches = 2,
        reps = 2,
        combine = TRUE,
        data_copy = c("n", "true_beta"),
        variables = "y_rep[2]",
        summaries = list(
          ~quantile(.x, probs = c(0.25, 0.75)),
          custom = function(x, my_arg) my_arg
        ),
        summary_args = list(my_arg = 123L),
        log = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 12L)
  out1 <- targets::tar_manifest(model_file_a, callr_function = NULL)
  out2 <- targets::tar_manifest(model_file_b, callr_function = NULL)
  expect_false(grepl("tar_stan_compile_run", out1$command))
  expect_false(grepl("tar_stan_compile_run", out2$command))
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from, ~to,
    "fit_data", "fit_mcmc_a",
    "fit_file_a", "fit_mcmc_a",
    "fit_mcmc_a", "model_a",
    "fit_mcmc_a", "model_b",
    "model_a", "model",
    "model_b", "model",
    "model_batch", "model_data",
    "model_data", "model_a",
    "model_data", "model_b",
    "model_file_a", "model_lines_a",
    "model_file_b", "model_lines_b",
    "model_lines_a", "model_a",
    "model_lines_b", "model_b"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file_a), "a.stan")
  expect_equal(targets::tar_read(model_file_b), "b.stan")
  out <- targets::tar_read(model_data)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 5L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  expect_true(is.numeric(out$true_beta))
  out1 <- targets::tar_read(model_a)
  out2 <- targets::tar_read(model_b)
  out <- targets::tar_read(model)
  expect_true(all(is.finite(out$n)))
  expect_true(all(is.finite(out$true_beta)))
  expect_equal(dplyr::bind_rows(out1, out2), out)
  expect_true(tibble::is_tibble(out))
  expect_true("25%" %in% colnames(out))
  expect_true(all(out$custom == 123L))
  expect_equal(nrow(out), 8L)
  expect_equal(length(unique(table(out$.rep))), 1L)
  expect_equal(length(table(out$.rep)), 8L)
  expect_equal(unique(out1$.file), "a.stan")
  expect_equal(unique(out2$.file), "b.stan")
  expect_equal(unique(out1$.name), "a")
  expect_equal(unique(out2$.name), "b")
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_b", "model_lines_b", "model_b", "model")
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc(
        fit,
        stan_files = "a.stan",
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
        diagnostics = FALSE,
        log = R.utils::nullfile()
      ),
      tar_stan_gq_rep_summary(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = c(tar_stan_example_data()),
        fitted_params = fit_mcmc_a,
        compile = "copy",
        quiet = TRUE,
        batches = 2,
        reps = 2,
        combine = TRUE,
        data_copy = c("n", "true_beta"),
        variables = "y_rep[2]",
        summaries = list(
          ~quantile(.x, probs = c(0.25, 0.75)),
          custom = function(x, my_arg) my_arg
        ),
        summary_args = list(my_arg = 123L),
        log = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_b",
    "model_lines_b",
    "model_b",
    "model_a",
    "model_data",
    "model"
  )
  expect_equal(sort(out), sort(exp))
})
