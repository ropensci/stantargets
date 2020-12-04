tar_test("tar_stan_mle(compile = \"original\")", {
  skip_on_cran()
  tar_stan_example_file(path = "a.stan")
  tar_stan_example_file(path = "b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mle(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = tar_stan_example_data(),
        compile = "original",
        quiet = TRUE,
        refresh = 0
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
    "model_data", "model_mle_x",
    "model_file_x", "model_mle_x",
    "model_data", "model_mle_y",
    "model_file_y", "model_mle_y",
    "model_mle_x", "model_summary_x",
    "model_mle_y", "model_summary_y",
    "model_mle_x", "model_draws_x",
    "model_mle_y", "model_draws_y"
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
  out1 <- targets::tar_read(model_mle_x)
  out2 <- targets::tar_read(model_mle_y)
  expect_true(inherits(out1, "CmdStanMLE"))
  expect_true(inherits(out2, "CmdStanMLE"))
  expect_true(inherits(out1$draws(), "draws"))
  expect_true(inherits(out2$draws(), "draws"))
  out1 <- targets::tar_read(model_draws_x)
  out2 <- targets::tar_read(model_draws_y)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 1L)
  expect_equal(nrow(out2), 1L)
  expect_true("lp__" %in% colnames(out1))
  expect_true("lp__" %in% colnames(out2))
  out1 <- targets::tar_read(model_summary_x)
  out2 <- targets::tar_read(model_summary_y)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_true("lp__" %in% out1$variable)
  expect_true("lp__" %in% out2$variable)
  # Everything should be up to date.
  expect_equal(targets::tar_outdated(callr_function = NULL), character(0))
  # Change the model.
  write("", file = "b.stan", append = TRUE)
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_y",
    "model_summary_y",
    "model_draws_y",
    "model_mle_y"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mle(
        model,
        stan_files = c(x = "a.stan", y = "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "original",
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_file_y",
    "model_summary_y",
    "model_draws_y",
    "model_mle_y",
    "model_summary_x",
    "model_draws_x",
    "model_mle_x",
    "model_data"
  )
  expect_equal(sort(out), sort(exp))
})

tar_test("tar_stan_mle(compile = \"copy\") with custom summaries", {
  skip_on_cran()
  tar_stan_example_file(path = "a.stan")
  tar_stan_example_file(path = "b.stan")
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mle(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = tar_stan_example_data(),
        compile = "copy",
        refresh = 0,
        variables = "beta",
        summaries = list(~c(mean = -10000)),
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
    "model_data", "model_mle_a",
    "model_data", "model_mle_b",
    "model_file_a", "model_lines_a",
    "model_file_b", "model_lines_b",
    "model_lines_a", "model_mle_a",
    "model_lines_b", "model_mle_b",
    "model_mle_a", "model_draws_a",
    "model_mle_a", "model_summary_a",
    "model_mle_b", "model_draws_b",
    "model_mle_b", "model_summary_b"
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
  out1 <- targets::tar_read(model_mle_a)
  out2 <- targets::tar_read(model_mle_b)
  expect_true(inherits(out1, "CmdStanMLE"))
  expect_true(inherits(out2, "CmdStanMLE"))
  expect_true(inherits(out1$draws(), "draws"))
  expect_true(inherits(out2$draws(), "draws"))
  out1 <- targets::tar_read(model_draws_a)
  out2 <- targets::tar_read(model_draws_b)
  expect_true(tibble::is_tibble(out1))
  expect_true(tibble::is_tibble(out2))
  expect_equal(nrow(out1), 1L)
  expect_equal(nrow(out2), 1L)
  expect_true("beta" %in% colnames(out1))
  expect_true("beta" %in% colnames(out2))
  out1 <- targets::tar_read(model_summary_a)
  out2 <- targets::tar_read(model_summary_b)
  expect_equal(out1$variable, "beta")
  expect_equal(out2$variable, "beta")
  expect_equal(colnames(out1), c("variable", "estimate"))
  expect_equal(colnames(out2), c("variable", "estimate"))
  expect_equal(out1$estimate, -10000)
  expect_equal(out2$estimate, -10000)
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
    "model_mle_b"
  )
  expect_equal(sort(out), sort(exp))
  # Change the data code.
  targets::tar_script({
    tar_option_set(memory = "transient", garbage_collection = TRUE)
    tar_pipeline(
      tar_stan_mle(
        model,
        stan_files = c("a.stan", "b.stan"),
        data = c(tar_stan_example_data()),
        compile = "copy",
        refresh = 0,
        variables = "beta",
        summaries = list(~c(mean = -10000))
      )
    )
  })
  out <- targets::tar_outdated(callr_function = NULL)
  exp <- c(
    "model_lines_b",
    "model_file_b",
    "model_summary_b",
    "model_draws_b",
    "model_mle_b",
    "model_summary_a",
    "model_draws_a",
    "model_mle_a",
    "model_data"
  )
  expect_equal(sort(out), sort(exp))
})
