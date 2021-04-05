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
    if_any(identical(compile, "original"), NULL, target_lines),
    target_output,
    if_any(identical(draws, TRUE), target_draws, NULL),
    if_any(identical(summary, TRUE), target_summary, NULL),
    if_any(identical(diagnostics, TRUE), target_diagnostics, NULL)
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

tar_stan_target_list_rep <- function(
  name,
  name_batch,
  name_data,
  name_stan,
  sym_stan,
  stan_files,
  compile,
  combine,
  target_batch,
  target_compile,
  target_file,
  target_lines,
  target_data,
  target_output,
  packages,
  error,
  memory,
  garbage_collection,
  priority,
  resources,
  cue
) {
  out <- list(
    if_any(identical(compile, "original"), target_compile, target_file),
    if_any(identical(compile, "original"), NULL, target_lines),
    target_output
  )
  out <- list_nonempty(out)
  values <- list(
    ._stantargets_file_50e43091 = stan_files,
    ._stantargets_name_50e43091 = sym_stan,
    ._stantargets_name_chr_50e43091 = name_stan
  )
  out <- tarchetypes::tar_map(
    values = values,
    names = ._stantargets_name_50e43091,
    unlist = TRUE,
    out
  )
  out[[name_data]] <- target_data
  out[[name_batch]] <- target_batch
  names_output <- paste0(name, "_", name_stan)
  if (combine) {
    out[[name]] <- tarchetypes::tar_combine_raw(
      name = name,
      out[names_output],
      packages = packages,
      format = "fst_tbl",
      iteration = "vector",
      error = error,
      memory = memory,
      garbage_collection = garbage_collection,
      deployment = "main",
      priority = priority,
      resources = resources,
      storage = "main",
      retrieval = "main",
      cue = cue
    )
  }
  out
}
