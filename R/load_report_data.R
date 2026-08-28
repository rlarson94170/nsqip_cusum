# =============================================================================
# Shared report data loading
#
# The Quarto report and the beamer slide renderer need exactly the same inputs
# prepared exactly the same way. They used to carry byte-identical copies of
# this block, which meant any change had to be made twice and the two outputs
# could silently disagree.
#
# Loading is also the slow step: a full run renders one PDF and one deck per
# specialty and per division, and each render re-read the same multi-megabyte
# Case Details workbook. Since Quarto renders in a separate process, an
# in-memory cache buys nothing across renders, so results are cached to disk
# and keyed on the inputs' size and mtime. Touch or replace any input file and
# the cache misses on the next run.
# =============================================================================

library(dplyr)

REPORT_CACHE_DIR <- ".cache"

#' Load and prepare everything a report needs from the raw inputs
#'
#' @param data_file Path to the Case Details Report xlsx
#' @param site_sar_file Path to the Site SAR/ISAR Summary, or "" for none
#' @param surgeon_mapping_file Path to the surgeon-division CSV, or "" for none
#' @param benchmark_type "site_expected" or "national_observed"
#' @param specialties Specialties to retain from the Case Details file
#' @param cache_dir Directory for the on-disk cache; "" or NULL disables it
#' @return A list with:
#'   \describe{
#'     \item{case_data}{cases with division and procedure_category assigned}
#'     \item{benchmark_rates}{specialty-level benchmark table}
#'     \item{targeted_data}{parsed targeted SAR, or NULL}
#'     \item{ot_data}{parsed Over-Time O/E trends, or NULL}
#'     \item{site_sar_available}{whether a readable site SAR was supplied}
#'   }
load_report_data <- function(data_file,
                             site_sar_file = "",
                             surgeon_mapping_file = "",
                             benchmark_type = "site_expected",
                             specialties = c("General Surgery", "Vascular",
                                             "Thoracic", "Plastics"),
                             cache_dir = REPORT_CACHE_DIR) {

  key <- .report_cache_key(data_file, site_sar_file, surgeon_mapping_file,
                           benchmark_type, specialties)

  cached <- .report_cache_get(cache_dir, key)
  if (!is.null(cached)) {
    message("  Using cached report data (", nrow(cached$case_data), " cases)")
    return(cached)
  }

  case_data <- process_case_details(
    filepath    = data_file,
    specialties = specialties
  )

  if (nchar(surgeon_mapping_file) > 0 && file.exists(surgeon_mapping_file)) {
    surgeon_map <- load_surgeon_mapping(surgeon_mapping_file)
    case_data <- assign_divisions(case_data, surgeon_map)
  } else {
    case_data$division <- NA_character_
  }

  case_data <- assign_procedure_categories(case_data)

  site_sar_available <- nchar(site_sar_file) > 0 && file.exists(site_sar_file)

  benchmark_rates <- get_benchmark_rates(
    site_sar_path  = if (site_sar_available) site_sar_file else NULL,
    benchmark_type = benchmark_type
  )

  # These two are decorative if absent — a missing or malformed sheet should
  # cost the report a section, not the whole render.
  ot_data <- targeted_data <- NULL
  if (site_sar_available) {
    ot_data <- tryCatch(parse_over_time(site_sar_file),
                        error = function(e) NULL)
    targeted_data <- tryCatch(parse_targeted_sar(site_sar_file),
                              error = function(e) NULL)
  }

  out <- list(
    case_data          = case_data,
    benchmark_rates    = benchmark_rates,
    targeted_data      = targeted_data,
    ot_data            = ot_data,
    site_sar_available = site_sar_available
  )

  .report_cache_put(cache_dir, key, out)
  out
}


#' Targeted rates to use for case-level p0, given the benchmark mode
#'
#' Targeted expected rates are risk-adjusted, so they belong only in
#' site_expected mode; national_observed stays unadjusted throughout.
targeted_for_mode <- function(bundle, benchmark_type) {
  if (identical(benchmark_type, "site_expected")) bundle$targeted_data else NULL
}


# ---- Cache internals --------------------------------------------------------

# Identity of the inputs, not their contents: size and mtime are enough to
# notice a replaced download, and are far cheaper than hashing a 2 MB workbook.
.report_cache_key <- function(data_file, site_sar_file, surgeon_mapping_file,
                              benchmark_type, specialties) {
  stamp <- function(path) {
    if (is.null(path) || is.na(path) || nchar(path) == 0 ||
        !file.exists(path)) {
      return(paste0(path, ":absent"))
    }
    info <- file.info(path)
    paste0(normalizePath(path), ":", info$size, ":",
           format(info$mtime, "%Y-%m-%d %H:%M:%OS3"))
  }

  paste(
    stamp(data_file), stamp(site_sar_file), stamp(surgeon_mapping_file),
    benchmark_type, paste(sort(specialties), collapse = ","),
    sep = "||"
  )
}


.report_cache_file <- function(cache_dir) {
  file.path(cache_dir, "report_data.rds")
}


.report_cache_get <- function(cache_dir, key) {
  if (is.null(cache_dir) || nchar(cache_dir) == 0) return(NULL)
  path <- .report_cache_file(cache_dir)
  if (!file.exists(path)) return(NULL)

  tryCatch({
    entry <- readRDS(path)
    if (!is.list(entry) || !identical(entry$key, key)) return(NULL)
    if (!all(c("case_data", "benchmark_rates") %in% names(entry$value))) {
      return(NULL)
    }
    entry$value
  }, error = function(e) NULL)   # a corrupt cache is a miss, never a failure
}


.report_cache_put <- function(cache_dir, key, value) {
  if (is.null(cache_dir) || nchar(cache_dir) == 0) return(invisible(FALSE))

  tryCatch({
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    # Write to a temporary file and rename, so an interrupted write cannot
    # leave a half-written cache that the next run would have to recover from.
    tmp <- paste0(.report_cache_file(cache_dir), ".tmp", Sys.getpid())
    saveRDS(list(key = key, value = value), tmp)
    file.rename(tmp, .report_cache_file(cache_dir))
    invisible(TRUE)
  }, error = function(e) {
    message("  Could not write report cache: ", conditionMessage(e))
    invisible(FALSE)
  })
}


#' Remove the cached report data
clear_report_cache <- function(cache_dir = REPORT_CACHE_DIR) {
  path <- .report_cache_file(cache_dir)
  if (file.exists(path)) unlink(path)
  invisible(TRUE)
}
