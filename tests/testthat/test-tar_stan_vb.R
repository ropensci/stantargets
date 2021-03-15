# targets::tar_test() runs the test code inside a temporary directory
# to avoid accidentally writing to the user's file space.
targets::tar_test("tar_stan_vb(compile = \"original\")", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  restore_compiled_models()
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_vb(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        log = R.utils::nullfile()
      )
    )
  })
  # manifest
  out <- targets::tar_manifest(callr_function = NULL)
  expect_equal(nrow(out), 9L)
  # graph
  out <- targets::tar_network(callr_function = NULL, targets_only = TRUE)$edges
  out <- dplyr::arrange(out, from, to)
  rownames(out) <- NULL
  exp <- tibble::tribble(
    ~from, ~to,
    "model_data", "model_summary_x",
    "model_data", "model_summary_y",
    "model_data", "model_vb_x",
    "model_file_x", "model_vb_x",
    "model_data", "model_vb_y",
    "model_file_y", "model_vb_y",
    "model_vb_x", "model_summary_x",
    "model_vb_y", "model_summary_y",
    "model_vb_x", "model_draws_x",
    "model_vb_y", "model_draws_y"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
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
  out1 <- targets::tar_read(model_vb_x)
  out2 <- targets::tar_read(model_vb_y)
  expect_true(inherits(out1, "CmdStanVB"))
  expect_true(inherits(out2, "CmdStanVB"))
  expect_true(inherits(out1$draws(), "draws"))
  expect_true(inherits(out2$draws(), "draws"))
  out1 <- targets::tar_read(model_draws_x)
  out2 <- targets::tar_read(model_draws_y)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 1000L)
  expect_equal(nrow(out2), 1000L)
  expect_true("lp__" %in% colnames(out1))
  expect_true("lp__" %in% colnames(out2))
  out1 <- targets::tar_read(model_summary_x)
  out2 <- targets::tar_read(model_summary_y)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_true("lp__" %in% out1$variable)
  expect_true("lp__" %in% out2$variable)
  original_data <- tar_read(model_data)
  beta <- original_data$.join_data$beta
  y_rep <- original_data$.join_data$y_rep
  expect_equal(out1$.join_data[out1$variable == "beta"], beta)
  expect_equal(out1$.join_data[grepl("y_rep", out1$variable)], y_rep)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_y",
    "model_summary_y",
    "model_draws_y",
    "model_vb_y"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_vb(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "original",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        log = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_y",
    "model_summary_y",
    "model_draws_y",
    "model_vb_y",
    "model_summary_x",
    "model_draws_x",
    "model_vb_x",
    "model_data"
  )
  expect_equal(sort(out), sort(exp))
})

targets::tar_test("tar_stan_vb(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  skip_if_missing_cmdstan()
  skip_if_not_installed("dplyr")
  skip_compile_copy()
  tar_stan_example_file(path = "a.stan")
  tar_stan_example_file(path = "b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_vb(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = tar_stan_example_data(),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        log = R.utils::nullfile()
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
    ~from, ~to,
    "model_data", "model_summary_a",
    "model_data", "model_summary_b",
    "model_data", "model_vb_a",
    "model_data", "model_vb_b",
    "model_file_a", "model_lines_a",
    "model_file_b", "model_lines_b",
    "model_lines_a", "model_vb_a",
    "model_lines_b", "model_vb_b",
    "model_vb_a", "model_draws_a",
    "model_vb_a", "model_summary_a",
    "model_vb_b", "model_draws_b",
    "model_vb_b", "model_summary_b"
  )
  exp <- dplyr::arrange(exp, from, to)
  rownames(exp) <- NULL
  expect_equal(out, exp)
  # results
  capture.output(suppressWarnings(targets::tar_make(callr_function = NULL)))
  expect_equal(targets::tar_read(model_file_a), "a.stan")
  expect_equal(targets::tar_read(model_file_b), "b.stan")
  expect_equal(targets::tar_read(model_lines_a), readLines("a.stan"))
  expect_equal(targets::tar_read(model_lines_b), readLines("b.stan"))
  out <- targets::tar_read(model_data)
  expect_true(is.list(out))
  expect_equal(out$n, 10L)
  expect_equal(length(out$x), 10L)
  expect_equal(length(out$y), 10L)
  expect_true(is.numeric(out$x))
  expect_true(is.numeric(out$y))
  out1 <- targets::tar_read(model_vb_a)
  out2 <- targets::tar_read(model_vb_b)
  expect_true(inherits(out1, "CmdStanVB"))
  expect_true(inherits(out2, "CmdStanVB"))
  expect_true(inherits(out1$draws(), "draws"))
  expect_true(inherits(out2$draws(), "draws"))
  out1 <- targets::tar_read(model_draws_a)
  out2 <- targets::tar_read(model_draws_b)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 1000L)
  expect_equal(nrow(out2), 1000L)
  expect_true("beta" %in% colnames(out1))
  expect_true("beta" %in% colnames(out2))
  out1 <- targets::tar_read(model_summary_a)
  out2 <- targets::tar_read(model_summary_b)
  expect_equal(out1$variable, "beta")
  expect_equal(out2$variable, "beta")
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
    "model_lines_b",
    "model_file_b",
    "model_summary_b",
    "model_draws_b",
    "model_vb_b"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    list(
      tar_stan_vb(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "copy",
        quiet = TRUE,
        refresh = 0,
        iter = 1000,
        variables = "beta",
        summaries = list(~quantile(.x, probs = c(0.25, 0.75))),
        log = R.utils::nullfile()
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_lines_b",
    "model_file_b",
    "model_summary_b",
    "model_draws_b",
    "model_vb_b",
    "model_summary_a",
    "model_draws_a",
    "model_vb_a",
    "model_data"
  )
  expect_equal(sort(out), sort(exp))
})
