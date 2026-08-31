# =============================================================================
# Test setup
#
# This project is a set of scripts rather than a package, so there is no
# namespace to load — the helpers are sourced directly. testthat runs with the
# working directory set to tests/testthat, so paths climb two levels.
# =============================================================================

.proj_root <- normalizePath(file.path("..", ".."), mustWork = TRUE)

suppressMessages({
  source(file.path(.proj_root, "R", "version.R"))
  source(file.path(.proj_root, "R", "benchmarks.R"))
  source(file.path(.proj_root, "R", "data_processing.R"))
  source(file.path(.proj_root, "R", "cusum_functions.R"))
  source(file.path(.proj_root, "R", "triage.R"))
  source(file.path(.proj_root, "R", "load_report_data.R"))
})


# ---- Synthetic fixtures -----------------------------------------------------
# Everything below is fabricated. No test touches data/, so the suite runs on
# a clone with no PHI present.

#' Minimal benchmark table in the shape get_benchmark_rates() returns
fake_benchmarks <- function(spec = "General Surgery",
                            rates = c(SSI = 0.04, Sepsis = 0.01,
                                      Mortality = 0.02, Morbidity = 0.08)) {
  tibble::tibble(
    specialty        = spec,
    complication     = names(rates),
    p0               = unname(rates),
    p0_pct           = unname(rates) * 100,
    benchmark_source = "Site expected (risk-adjusted)"
  )
}

#' Minimal targeted-SAR table in the shape parse_targeted_sar() returns
fake_targeted <- function(spec = "General Surgery") {
  tibble::tribble(
    ~targeted_procedure, ~procedure_category, ~complication,   ~complication_std, ~n_cases, ~exp_rate,
    "Colectomy",         "Colectomy",         "SSI",           "SSI",             100L,     0.06,
    "Proctectomy",       "Proctectomy",       "SSI",           "SSI",             20L,      0.14,
    # Distinctive sentinels: these are non-binary models that must never be
    # used as a p0, so tests assert their exact values never appear.
    "Colectomy",         "Colectomy",         "Anastomotic Leak", NA_character_,  100L,     0.7777,
    "Colectomy",         "Colectomy",         "Length of Stay",   NA_character_,  100L,     0.8888,
    "Colectomy",         "Colectomy",         "Sepsis",        "Sepsis",          100L,     NA_real_,
    "Major Hepatectomy", "Hepatectomy",       "SSI",           "SSI",             10L,      0.10,
    "Partial Hepatectomy", "Hepatectomy",     "SSI",           "SSI",             30L,      0.02
  ) |>
    dplyr::mutate(specialty = spec)
}

#' Case data in the shape process_case_details() + assign_* produce
#'
#' @param n_by_cat Named vector: procedure_category -> number of cases
#' @param events Named list: complication column -> integer vector of 0/1,
#'   recycled to the total case count
#' @param cpt CPT code per case, recycled to the total case count. Defaults to
#'   one code for every case, which is enough for anything not testing the
#'   per-flag procedure breakdown.
fake_cases <- function(n_by_cat = c(Colectomy = 6, Other = 4),
                       events = list(),
                       spec = "General Surgery",
                       div = "Colorectal",
                       cpt = 44950) {
  cats <- rep(names(n_by_cat), times = unname(n_by_cat))
  n <- length(cats)

  codes <- rep_len(as.character(cpt), n)

  base <- tibble::tibble(
    case_id            = seq_len(n),
    op_date            = as.Date("2026-01-01") + seq_len(n),
    specialty          = spec,
    division           = div,
    procedure_category = cats,
    cpt_code           = codes,
    cpt_desc           = paste0("Procedure ", codes),
    # Case-detail columns, needed by anything that renders a case list.
    # Benign defaults; override via the returned frame where a test cares.
    lmrn               = sprintf("%08d", seq_len(n)),
    surgeon            = "SURGEON,A (1)",
    asa_class          = "ASA II - Mild Disturb",
    los                = 2L,
    readmit_related    = 0L,
    readmit_unrelated  = 0L
  )

  for (v in names(complication_labels)) base[[v]] <- 0L
  for (v in names(events)) base[[v]] <- as.integer(rep_len(events[[v]], n))
  base
}

#' Place `k` events at chosen positions in a length-n 0/1 vector
events_at <- function(n, positions) {
  x <- integer(n)
  x[positions] <- 1L
  x
}


#' A raw Case Details frame with every required column at a benign default
#'
#' Defaults are a complete, uncomplicated General Surgery case. Override any
#' column by name to build the scenario under test:
#'
#'   raw_cases(2, `# of Postop Pneumonia` = c(1, 0))
#'
#' @param n Number of rows
#' @param ... Column overrides, recycled to length n
raw_cases <- function(n = 1, ...) {
  overrides <- list(...)

  df <- list(
    `Completion Status`  = "Complete",
    `Surgical Specialty` = "General Surgery",
    `Operation Date`     = format(as.Date("2026-01-01") + seq_len(n) - 1,
                                  "%m/%d/%Y"),
    `Case Number`        = as.character(seq_len(n)),
    LMRN                 = sprintf("%08d", seq_len(n)),
    `Attending/Staff Surgeon`  = "SURGEON,A (1)",
    `CPT Code`                 = "44950",
    `CPT Description`          = "Appendectomy",
    `Age at Time of Surgery`   = 55,
    `ASA Classification`       = "ASA II - Mild Disturb",
    `Hospital Length of Stay`  = 2,
    `Postop Death w/in 30 days of Procedure` = "No"
  )

  # Occurrence counts and the targeted-module text fields all default to
  # "nothing happened"
  zero_cols <- setdiff(
    REQUIRED_CASE_COLUMNS,
    c(names(df), "Colectomy Primary Indication for Surgery",
      "Proctectomy Preop Patient Marked for Stoma",
      "Colectomy Postop Anastomotic Leak", "Proctectomy Postop Anastomotic Leak",
      "Colectomy Prolonged Postoperative NPO or NGT Use",
      "Proctectomy Prolonged Postoperative NPO or NGT Use")
  )
  for (col in zero_cols) df[[col]] <- 0

  for (col in c("Colectomy Primary Indication for Surgery",
                "Proctectomy Preop Patient Marked for Stoma",
                "Colectomy Postop Anastomotic Leak",
                "Proctectomy Postop Anastomotic Leak",
                "Colectomy Prolonged Postoperative NPO or NGT Use",
                "Proctectomy Prolonged Postoperative NPO or NGT Use")) {
    df[[col]] <- NA_character_
  }

  for (col in names(overrides)) df[[col]] <- overrides[[col]]

  tibble::as_tibble(lapply(df, function(v) rep_len(v, n)))
}

#' derive_case_indicators() on a fixture, quietly
derive_raw <- function(...) {
  derive_case_indicators(raw_cases(...), quiet = TRUE)
}
