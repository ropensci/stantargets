# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_mcmc_rep_summary(compile = \"original\")", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  restore_compiled_models()
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc_rep_summary(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 20,
        iter_warmup = 10,
        chains = 4,
        batches = 2,
        reps = 2,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 7L)
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
    "model_data", "model_x",
    "model_file_x", "model_x",
    "model_batch", "model_data",
    "model_data", "model_y",
    "model_file_y", "model_y",
    "model_x", "model",
    "model_y", "model"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  suppressWarnings(targets::tar_make(callr_function = NULL))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(length(targets::tar_read(model_file_x)), 2)
  expect_equal(length(targets::tar_read(model_file_y)), 2)
  expect_equal(targets::tar_read(model_file_x)[1], "a.stan")
  expect_equal(targets::tar_read(model_file_y)[1], "b.stan")
  # data
  out <- targets::tar_read(model_data)
  dataset_ids <- c(
    out[[1]][[1]]$.dataset_id,
    out[[1]][[2]]$.dataset_id,
    out[[2]][[1]]$.dataset_id,
    out[[2]][[2]]$.dataset_id
  )
  expect_equal(length(unique(dataset_ids)), 4)
  expect_equal(length(out), 2L)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 7L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  expect_true(is.numeric(out$true_beta))
  # model
  out1 <- targets::tar_read(model_x)
  out2 <- targets::tar_read(model_y)
  out <- targets::tar_read(model)
  expect_equal(unique(table(out$.dataset_id)), 24)
  expect_equal(length(unique(out$.dataset_id)), 4)
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
  expect_equal(nrow(out1), 48L)
  expect_equal(nrow(out2), 48L)
  expect_equal(unique(out1$.file), "a.stan")
  expect_equal(unique(out2$.file), "b.stan")
  expect_equal(unique(out1$.name), "x")
  expect_equal(unique(out2$.name), "y")
  original_data <- tar_read(model_data)
  beta <- original_data[[1]][[1]]$.join_data$beta
  y_rep <- original_data[[1]][[1]]$.join_data$y_rep
  out1 <- out1[out1$.rep == out1$.rep[1], ]
  expect_equal(out1$.join_data[out1$variable == "beta"], beta)
  expect_equal(out1$.join_data[grepl("y_rep", out1$variable)], y_rep)
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
      tar_stan_mcmc_rep_summary(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 20,
        iter_warmup = 10,
        chains = 4,
        batches = 2,
        reps = 2,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c("model_file_y", "model_y", "model", "model_x", "model_data")
  expect_equal(sort(out), sort(exp))
})

targets::tar_test("tar_stan_mcmc_rep_summary(compile = \"copy\") custom", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  skip_compile_copy()
  tar_stan_example_file("a.stan")
  tar_stan_example_file("b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc_rep_summary(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        init = 1,
        batches = 2,
        reps = 2,
        data_copy = c("n", "true_beta"),
        variables = "beta",
        summaries = list(
          ~quantile(.x, probs = c(0.25, 0.75)),
          custom = function(x, my_arg) my_arg
        ),
        summary_args = list(my_arg = 123L),
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 9L)
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
  suppressWarnings(targets::tar_make(callr_function = NULL))
  meta <- tar_meta(starts_with("model_data_"))
  expect_equal(nrow(meta), 2L)
  expect_equal(targets::tar_read(model_file_a), "a.stan")
  expect_equal(targets::tar_read(model_file_b), "b.stan")
  expect_equal(targets::tar_read(model_lines_a), readLines("a.stan"))
  expect_equal(targets::tar_read(model_lines_b), readLines("b.stan"))
  out <- targets::tar_read(model_data)
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_equal(length(out), 2L)
  out <- out[[2]]
  expect_true(is.list(out))
  expect_equal(length(out), 7L)
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  expect_true(is.numeric(out$true_beta))
  out1 <- targets::tar_read(model_a)
  out2 <- targets::tar_read(model_b)
  out <- targets::tar_read(model)
  expect_equal(unique(table(out$.dataset_id)), 2)
  expect_equal(length(unique(out$.dataset_id)), 4)
  expect_true(all(is.finite(out$n)))
  expect_true(all(is.finite(out$true_beta)))
  expect_equal(dplyr::bind_rows(out1, out2), out)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 4L)
  expect_equal(nrow(out2), 4L)
  out <- dplyr::bind_rows(out1, out2)
  expect_true("25%" %in% colnames(out))
  expect_true(all(out$custom == 123L))
  expect_equal(length(unique(table(out1$.rep))), 1L)
  expect_equal(length(unique(table(out2$.rep))), 1L)
  expect_equal(length(table(out1$.rep)), 4L)
  expect_equal(length(table(out2$.rep)), 4L)
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
      tar_stan_mcmc_rep_summary(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter_sampling = 1000,
        iter_warmup = 500,
        chains = 4,
        init = 1,
        batches = 2,
        reps = 2,
        data_copy = c("n", "true_beta"),
        variables = "beta",
        summaries = list(
          ~quantile(.x, probs = c(0.25, 0.75)),
          custom = function(x, my_arg) my_arg
        ),
        summary_args = list(my_arg = 123L),
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
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

targets::tar_test("stan files missing", {
  expect_error(
    tar_stan_mcmc_rep_summary(
      model,
      stan_files = c("a.stan", "b.stan"),
      data = c(tar_stan_example_data()),
      compile = "copy",
      quiet = TRUE,
      refresh = 0,
      iter_sampling = 1000,
      iter_warmup = 500,
      chains = 4,
      init = 1,
      batches = 2,
      reps = 2,
      data_copy = c("n", "true_beta"),
      variables = "beta",
      summaries = list(
        ~quantile(.x, probs = c(0.25, 0.75)),
        custom = function(x, my_arg) my_arg
      ),
      summary_args = list(my_arg = 123L),
      stdout = R.utils::nullfile(),
      stderr = R.utils::nullfile()
    ),
    class = "tar_condition_validate"
  )
})

targets::tar_test("tar_stan_mcmc_rep_summary() seed resilience", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  restore_compiled_models()
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc_rep_summary(
        model,
        stan_files = c(x = "a.stan"),
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 20,
        iter_warmup = 10,
        chains = 4,
        batches = 2,
        reps = 2,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  suppressWarnings(targets::tar_make(callr_function = NULL))
  data1 <- tar_read(model_data)
  expect_equal(length(data1), 2L)
  model1 <- tar_read(model_x)
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_mcmc_rep_summary(
        model,
        stan_files = c(x = "a.stan"),
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        init = 1,
        iter_sampling = 20,
        iter_warmup = 10,
        chains = 4,
        batches = 1,
        reps = 4,
        stdout = R.utils::nullfile(),
        stderr = R.utils::nullfile()
      )
    )
  })
  suppressWarnings(targets::tar_make(callr_function = NULL))
  data2 <- tar_read(model_data)
  expect_equal(length(data2), 1L)
  model2 <- tar_read(model_x)
  data_list1 <- list(
    data1[[1]][[1]],
    data1[[1]][[2]],
    data1[[2]][[1]],
    data1[[2]][[2]]
  )
  for (index in seq_len(4)) {
    data_list1[[index]]$.dataset_id <- NULL
    data2[[1]][[index]]$.dataset_id <- NULL
  }
  expect_equal(data_list1, data2[[1]])
  for (field in c(".dataset_id", ".rep")) {
    model1[[field]] <- NULL
    model2[[field]] <- NULL
  }
  expect_equal(model1, model2)
})
