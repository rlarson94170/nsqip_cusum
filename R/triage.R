# =============================================================================
# Triage: turning charts into a chart-review worklist
#
# The CUSUM answers "when did events cluster", which is what makes a case list
# actionable. It does not answer "is this division's rate elevated" — it fires
# on clustering even when the cumulative rate is at or below expected. Acting
# on the CUSUM alone therefore sends reviewers after runs that carry no excess
# (in the 2026-H1 data it flagged Unplanned Readmission at 28 observed vs 32.3
# expected).
#
# So each complication is scored on two independent gates:
#
#   magnitude  cumulative events >= MIN_EVENTS and one-sided Poisson
#              p < alpha against the summed per-case expected rate
#   timing     the CUSUM signalled
#
#   Tier 1  Review now      both gates
#   Tier 2  Worth a look    magnitude only  (elevated, no recent cluster)
#   Tier 3  Watch           timing only, and observed >= expected
#   --      Suppressed      timing only, observed < expected — clustering noise
#
# Because reports run quarterly over a trailing ~6 months, consecutive reports
# overlap by about three months and every event is seen twice. Flags are
# therefore matched against the previous report for the same scope and marked
# new or carried over, so a division does not investigate a cluster twice.
# =============================================================================

library(dplyr)
library(tibble)

# Gate defaults. MIN_EVENTS is the floor below which no ratio is believable:
# two events against one expected is a 2x "excess" and pure noise.
TRIAGE_ALPHA      <- 0.10
TRIAGE_MIN_EVENTS <- 3L

TRIAGE_TIER_LABELS <- c(
  "1" = "Review now",
  "2" = "Worth a look",
  "3" = "Watch"
)


#' One-sided upper-tail Poisson p-value for observed vs expected events
#'
#' The sum of per-case Bernoulli probabilities is used as the Poisson mean.
#' At these rates and case counts the Poisson approximation to the
#' Poisson-binomial is close, and it keeps the test cheap and explainable.
.triage_p <- function(obs, expected) {
  if (is.na(obs) || is.na(expected) || expected <= 0) return(NA_real_)
  stats::ppois(obs - 1, lambda = expected, lower.tail = FALSE)
}


#' Smallest event count that would flag, given the expected count
#'
#' Reported so that "nothing flagged" is interpretable: a division can see
#' that, say, mortality could not have flagged below 3 events against 0.6
#' expected, rather than reading silence as reassurance.
#'
#' @return Integer event count, or NA if unreachable within a sane range
min_events_to_flag <- function(expected, alpha = TRIAGE_ALPHA,
                               min_events = TRIAGE_MIN_EVENTS) {
  if (is.na(expected) || expected <= 0) return(NA_integer_)
  for (k in seq(min_events, max(min_events, ceiling(expected * 25) + 25L))) {
    if (!is.na(.triage_p(k, expected)) && .triage_p(k, expected) < alpha) {
      return(as.integer(k))
    }
  }
  NA_integer_
}


#' Build the triage table for one specialty (and optionally one division)
#'
#' @param data Processed case data with division and procedure_category
#' @param spec Specialty name
#' @param div Division name, or NULL/"" for the whole specialty
#' @param benchmark_rates Specialty-level rates from get_benchmark_rates()
#' @param targeted_rates Targeted SAR from parse_targeted_sar(), or NULL
#' @param odds_ratio Odds ratio defining p1
#' @param target_arl Target in-control ARL for the CUSUM gate
#' @param alpha Significance level for the magnitude gate
#' @param min_events Minimum events before anything can flag
#' @param cusum_h Fixed decision boundary for the timing gate. NULL (default)
#'   calibrates one per complication from `target_arl`. Supply a value to pin
#'   the boundary across reports, or to make results deterministic.
#' @return A tibble, one row per monitored complication, ordered by tier
build_triage <- function(data, spec, div = NULL, benchmark_rates,
                         targeted_rates = NULL, odds_ratio = 2.0,
                         target_arl = 1500, alpha = TRIAGE_ALPHA,
                         min_events = TRIAGE_MIN_EVENTS, cusum_h = NULL) {

  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) df <- df |> filter(division == div)
  df <- df |> arrange(op_date)

  if (nrow(df) == 0) return(NULL)

  rows <- list()

  for (var_name in names(complication_labels)) {
    comp_label <- unname(complication_labels[var_name])
    if (!(var_name %in% names(df))) next

    cp <- build_case_p0(
      case_data       = df,
      comp_label      = comp_label,
      spec            = spec,
      benchmark_rates = benchmark_rates,
      targeted_rates  = targeted_rates
    )
    if (all(is.na(cp$p0))) next
    cp$p0[is.na(cp$p0)] <- mean(cp$p0, na.rm = TRUE)
    if (mean(cp$p0) < 0.0001) next

    obs      <- sum(df[[var_name]], na.rm = TRUE)
    expected <- sum(cp$p0)
    p_val    <- .triage_p(obs, expected)

    cusum_sig <- tryCatch({
      r <- compute_cusum(
        outcomes = df[[var_name]], p0 = cp$p0, h = cusum_h,
        odds_ratio = odds_ratio, reset = TRUE, target_arl = target_arl
      )
      sum(r$signal) > 0
    }, error = function(e) NA)

    gate_magnitude <- !is.na(p_val) && obs >= min_events && p_val < alpha
    gate_timing    <- isTRUE(cusum_sig)

    tier <- if (gate_magnitude && gate_timing) 1L
            else if (gate_magnitude)           2L
            else if (gate_timing && obs >= expected) 3L
            else 0L

    rows[[length(rows) + 1]] <- tibble(
      complication  = comp_label,
      var           = var_name,
      n_cases       = nrow(df),
      observed      = as.integer(obs),
      expected      = round(expected, 1),
      oe            = if (expected > 0) round(obs / expected, 2) else NA_real_,
      p_value       = round(p_val, 3),
      cusum_signal  = gate_timing,
      suppressed    = gate_timing && !gate_magnitude && obs < expected,
      tier          = tier,
      tier_label    = ifelse(tier == 0, NA_character_,
                             unname(TRIAGE_TIER_LABELS[as.character(tier)])),
      need_events   = min_events_to_flag(expected, alpha, min_events)
    ) |>
      mutate(fold = ifelse(is.na(need_events) | expected <= 0, NA_real_,
                           round(need_events / expected, 1)))
  }

  if (length(rows) == 0) return(NULL)

  bind_rows(rows) |>
    arrange(ifelse(tier == 0, 99L, tier), p_value)
}


# ---- Carry-over tracking ----------------------------------------------------

TRIAGE_HISTORY_FILE <- "output/triage_history.csv"

# Stand-in for "no division" in the history file's division column
SPECIALTY_LEVEL_KEY <- "(all)"

#' Record this report's flags and mark each as new or carried over
#'
#' Flags are keyed on (specialty, division, complication). A flag is "carried
#' over" when the same complication was flagged for the same scope in the most
#' recent *earlier* report, which given the quarterly cadence and trailing
#' 6-month window is the common case for a genuine ongoing problem.
#'
#' Writing is an upsert on (report_date, specialty, division, complication),
#' so the PDF and slide renders of the same scope do not create duplicates.
#'
#' @param triage Output of build_triage()
#' @param spec Specialty name
#' @param div Division name, or NULL/"" for specialty level
#' @param report_date Date of this report (defaults to today)
#' @param path History CSV; set to "" or NULL to disable persistence
#' @return `triage` with a `status` column added ("New" / "Carried over ...")
annotate_carryover <- function(triage, spec, div = NULL,
                               report_date = Sys.Date(),
                               path = TRIAGE_HISTORY_FILE) {

  if (is.null(triage) || nrow(triage) == 0) return(triage)

  # A non-empty sentinel: an empty string round-trips through read.csv() as a
  # logical NA column when every row is specialty-level, which then breaks the
  # bind_rows() upsert on type.
  div_key <- if (is.null(div) || nchar(div) == 0) SPECIALTY_LEVEL_KEY else div
  flagged <- triage |> filter(tier > 0)

  prior <- .read_triage_history(path)

  # `report_date` names both an argument and a history column, so pull the
  # argument into a distinct local before any data-masked comparison.
  this_date <- as.Date(report_date)
  prev_flags <- character(0)
  last_date  <- NULL

  if (!is.null(prior) && nrow(prior) > 0) {
    prev <- prior |>
      filter(
        .data$specialty == spec,
        .data$division  == div_key,
        as.Date(.data$report_date) < this_date
      )
    if (nrow(prev) > 0) {
      last_date  <- max(as.Date(prev$report_date))
      prev_flags <- prev |>
        filter(as.Date(.data$report_date) == last_date, .data$tier > 0) |>
        pull(complication)
    }
  }

  triage$status <- ifelse(
    triage$tier > 0,
    ifelse(
      triage$complication %in% prev_flags,
      paste0("Carried over (", format(last_date, "%b %Y"), ")"),
      "New"
    ),
    NA_character_
  )

  if (!is.null(path) && nchar(path) > 0 && nrow(flagged) > 0) {
    .write_triage_history(flagged, spec, div_key, report_date, path, prior)
  }

  triage
}


.read_triage_history <- function(path) {
  if (is.null(path) || nchar(path) == 0 || !file.exists(path)) return(NULL)
  required <- c("report_date", "specialty", "division", "complication", "tier")

  tryCatch({
    h <- utils::read.csv(path, stringsAsFactors = FALSE) |> as_tibble()

    # read.csv will parse almost anything into *some* data frame, so a corrupt
    # or schema-drifted file arrives without error and only blows up later in
    # the filter. Validate up front and discard rather than fail the render —
    # the worst case is that this report's flags all read as New.
    missing <- setdiff(required, names(h))
    if (length(missing) > 0) {
      message("  Ignoring triage history at ", path,
              " — missing column(s): ", paste(missing, collapse = ", "))
      return(NULL)
    }

    # Force the key columns to character: read.csv infers types per column and
    # will hand back logical NA for a column it reads as entirely blank.
    for (col in setdiff(required, "tier")) h[[col]] <- as.character(h[[col]])
    h$tier <- suppressWarnings(as.integer(h$tier))
    h
  }, error = function(e) {
    message("  Could not read triage history (", conditionMessage(e), ")")
    NULL
  })
}


.write_triage_history <- function(flagged, spec, div_key, report_date, path,
                                  prior) {
  new_rows <- flagged |>
    transmute(
      report_date  = as.character(as.Date(report_date)),
      specialty    = spec,
      division     = div_key,
      complication = complication,
      tier         = tier,
      observed     = observed,
      expected     = expected
    )

  this_date_chr <- as.character(as.Date(report_date))

  combined <- if (is.null(prior) || nrow(prior) == 0) {
    new_rows
  } else {
    # Upsert: drop any existing rows for this exact report/scope first, so the
    # PDF and slide renders of the same scope do not duplicate each other.
    prior |>
      filter(!(.data$report_date == this_date_chr &
               .data$specialty   == spec &
               .data$division    == div_key)) |>
      bind_rows(new_rows)
  }

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tryCatch(
    utils::write.csv(combined, path, row.names = FALSE),
    error = function(e) message("  Could not write triage history: ",
                                conditionMessage(e))
  )
}
