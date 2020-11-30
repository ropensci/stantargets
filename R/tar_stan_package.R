#' targets: Targets Archetypes for Stan
#' @docType package
#' @description The `targets` package is a pipeline toolkit that brings together
#' function-oriented programming and Make-like declarative workflows for
#' Statistics and data science in R. The `stantargets` package provides
#' convenient user-side functions to create Bayesian data analysis
#' workflows with `targets` and Stan. These functions reduce
#' cognitive overhead and micromanagement so the user can focus
#' more on model development than software development.
#' `stantargets` invokes Stan through the `cmdstanr` package:
#' <https://mc-stan.org/cmdstanr>.
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
