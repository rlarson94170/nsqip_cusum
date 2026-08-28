# =============================================================================
# run_tests.R
#
# Runs the test suite. From RStudio:  source("run_tests.R")
# From a shell:                       Rscript run_tests.R
#
# The suite uses only synthetic fixtures — it never reads data/, so it runs
# on a fresh clone with no PHI present.
# =============================================================================

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat is not installed.\n  install.packages(\"testthat\")")
}

results <- testthat::test_dir(
  "tests/testthat",
  reporter        = "summary",
  stop_on_failure = FALSE
)

df <- as.data.frame(results)
n_fail <- sum(df$failed) + sum(df$error)

if (n_fail > 0) {
  message("\n", n_fail, " test(s) failed.")
  # Non-zero exit for Rscript / CI; harmless when sourced interactively
  if (!interactive()) quit(status = 1)
} else {
  message("\nAll tests passed.")
}
