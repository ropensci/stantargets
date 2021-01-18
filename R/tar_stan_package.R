#' targets: Targets Archetypes for Stan
#' @docType package
#' @description Bayesian data analysis usually incurs long runtimes
#'   and cumbersome custom code. A specialized pipeline toolkit for
#'   Bayesians, the `stantargets` R package leverages
#'   `targets` and `cmdstanr` to ease these burdens.
#'   `stantargets` makes it super easy to set up useful scalable
#'   Stan pipelines that automatically parallelize the computation
#'   and skip expensive steps when the results are already up to date.
#'   Minimal custom code is required, and there is no need to manually
#'   configure branching, so usage is much easier than `targets` alone.
#'   `stantargets` can access all of `cmdstanr`'s major algorithms
#'   (MCMC, variational Bayes, and optimization) and it supports
#'   both single-fit workflows and multi-rep simulation studies.
#' @name stantargets-package
#' @importFrom cmdstanr cmdstan_model
#' @importFrom digest digest
#' @importFrom fst read_fst
#' @importFrom qs qread
#' @importFrom posterior as_draws_df
#' @importFrom purrr map
#' @importFrom rlang as_function sym
#' @importFrom stats rnorm runif
#' @importFrom targets tar_cue tar_dir tar_load tar_option_get tar_path
#'   tar_read tar_script tar_target tar_target_raw tar_test
#' @importFrom tarchetypes tar_combine tar_map
NULL

utils::globalVariables(
  c(
    "._stantargets_file_50e43091",
    "._stantargets_name_50e43091",
    "._stantargets_name_chr_50e43091"
  )
)
