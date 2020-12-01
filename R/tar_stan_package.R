#' targets: Targets Archetypes for Stan
#' @docType package
#' @description The `stantargets` R package is an extension to
#'   `targets` and `cmdstanr` for Bayesian data analysis.
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
#' @importFrom fst read_fst
#' @importFrom qs qread
#' @importFrom posterior as_draws_df
#' @importFrom purrr map
#' @importFrom rlang as_function sym
#' @importFrom stats rnorm
#' @importFrom targets tar_cue tar_dir tar_load tar_option_get tar_path
#'   tar_pipeline tar_read tar_script tar_target tar_target_raw
NULL
