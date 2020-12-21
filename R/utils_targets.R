tar_stan_target_list <- function(
  name_data,
  stan_files,
  sym_stan,
  compile,
  draws,
  summary,
  diagnostics,
  target_file,
  target_lines,
  target_data,
  target_output,
  target_draws,
  target_summary,
  target_diagnostics
) {
  out <- list(
    target_file,
    trn(identical(compile, "original"), NULL, target_lines),
    target_output,
    trn(identical(draws, TRUE), target_draws, NULL),
    trn(identical(summary, TRUE), target_summary, NULL),
    trn(identical(diagnostics, TRUE), target_diagnostics, NULL)
  )
  out <- list_nonempty(out)
  values <- list(
    ._stantargets_file_50e43091 = stan_files,
    ._stantargets_name_50e43091 = sym_stan
  )
  out <- tarchetypes::tar_map(
    values = values,
    names = ._stantargets_name_50e43091,
    unlist = TRUE,
    out
  )
  out[[name_data]] <- target_data
  out
}
