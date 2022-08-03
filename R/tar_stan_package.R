#' targets: Targets Archetypes for Stan
#' @docType package
#' @description Bayesian data analysis usually incurs long runtimes
#'   and cumbersome custom code. A pipeline toolkit tailored to
#'   Bayesian statisticians, the `stantargets` R package leverages
#'   `targets` and `cmdstanr` to ease these burdens.
#'   `stantargets` makes it super easy to set up scalable
#'   Stan pipelines that automatically parallelize the computation
#'   and skip expensive steps when the results are already up to date.
#'   Minimal custom code is required, and there is no need to manually
#'   configure branching, so usage is much easier than `targets` alone.
#'   `stantargets` can access all of `cmdstanr`'s major algorithms
#'   (MCMC, variational Bayes, and optimization) and it supports
#'   both single-fit workflows and multi-rep simulation studies.
#' @name stantargets-package
#' @seealso <https://docs.ropensci.org/stantargets/>, [tar_stan_mcmc()]
#' @importFrom cmdstanr cmdstan_model
#' @importFrom digest digest
#' @importFrom fs path_ext_remove path_rel
#' @importFrom fst read_fst
#' @importFrom qs qread
#' @importFrom parallel detectCores
#' @importFrom posterior as_draws_df
#' @importFrom purrr map map_dbl map2_dfr
#' @importFrom rlang check_installed expr quo_squash
#' @importFrom stats rnorm runif
#' @importFrom targets tar_assert_chr tar_assert_nonempty
#'   tar_assert_not_dir tar_assert_not_in
#'   tar_assert_nzchar tar_assert_path
#'   tar_assert_scalar tar_assert_unique
#'   tar_cue tar_deparse_safe tar_dir tar_load tar_option_get tar_path
#'   tar_read tar_script tar_target tar_target_raw tar_test tar_tidy_eval
#'   tar_throw_validate
#' @importFrom tarchetypes tar_combine_raw tar_map
#' @importFrom withr local_message_sink local_output_sink
NULL

utils::globalVariables(
  c(
    "._stantargets_file_50e43091",
    "._stantargets_name_50e43091",
    "._stantargets_name_chr_50e43091"
  )
)
