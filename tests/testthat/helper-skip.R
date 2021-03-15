skip_if_missing_cmdstan <- function(version = "2.25.0") {
  installed <- tryCatch(
    cmdstanr::cmdstan_version(),
    error = function(e) {
      skip("CmdStan not found.")
    }
  )
  if (installed < version) {
    msg <- sprintf(
      "Insufficient CmdStan version (%s < %s).",
      installed,
      version
    )
    skip(msg)
  }
}
