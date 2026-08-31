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
library(stringr)

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


# ---- Per-flag procedure breakdown -------------------------------------------
#
# A flag names a complication, not a place to look. Thirteen SSIs spread evenly
# across a service and thirteen concentrated in two procedures call for
# completely different chart reviews, and the flag alone cannot tell them apart.
#
# Cases are grouped on the raw CPT rather than `procedure_category`, because
# PROCEDURE_CPT_MAP covers General Surgery only — every Plastics case falls
# through to "Other", which is exactly where this breakdown is most needed.
#
# The rollup row is the comparison: "these two procedures ran 28% and 33%
# against 4.3% for everything else" is the finding, and a rate with nothing to
# sit against is not interpretable. That makes the rollup load-bearing, and it
# is only meaningful if single-event CPTs stay inside it — one event is a case,
# not a concentration, and listing it separately both adds a 1-of-1 row at 100%
# and drains the baseline it should be measured against. In the 2026-H1
# Plastics data, listing the three singletons moved the rest-of-service rate
# from 4.3% to 0.0% and made the two real outliers look starker than they are.

FLAG_PROCEDURE_TOP_N   <- 5L
FLAG_PROCEDURE_MIN_EV  <- 2L
FLAG_PROCEDURE_DESC_CH <- 38L

# Label for the rollup row; also used by the report to style it differently
FLAG_PROCEDURE_OTHER <- "All other CPTs"


#' Most frequent non-missing description for a CPT
#'
#' A code should carry one description, but free-text edits across a download
#' produce near-duplicates, so take the commonest rather than the first.
.modal_desc <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}


#' Top CPTs by event count for each flagged complication
#'
#' Ranked by event count, not rate: ranking by rate puts every 1-of-1 procedure
#' at the top of the table and buries the concentration worth reviewing.
#' Denominators are always shown rather than filtered on — the concentrations
#' that motivated this sat at n = 15 and n = 18, so any floor high enough to
#' make a rate "reliable" would discard the finding.
#'
#' @param data Processed case data with cpt_code and cpt_desc
#' @param spec Specialty name
#' @param div Division name, or NULL/"" for the whole specialty
#' @param triage Output of build_triage()
#' @param top_n CPTs listed per complication before the rollup row
#' @param min_cpt_events Events a CPT needs before it is listed separately
#'   rather than folded into the rollup
#' @param max_desc Description truncation width
#' @return A tibble with one row per (complication, CPT) plus a rollup row per
#'   complication, or NULL if nothing flagged, no complication has a CPT
#'   carrying repeat events, or CPTs are unavailable
build_flag_procedures <- function(data, spec, div = NULL, triage,
                                  top_n = FLAG_PROCEDURE_TOP_N,
                                  min_cpt_events = FLAG_PROCEDURE_MIN_EV,
                                  max_desc = FLAG_PROCEDURE_DESC_CH) {

  if (is.null(triage) || nrow(triage) == 0) return(NULL)
  flagged <- triage |> filter(tier > 0)
  if (nrow(flagged) == 0) return(NULL)

  # Older cached case data predates the CPT columns; degrade to no table
  # rather than failing the render.
  if (!all(c("cpt_code", "cpt_desc") %in% names(data))) return(NULL)

  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) df <- df |> filter(division == div)
  if (nrow(df) == 0) return(NULL)

  df$.cpt <- ifelse(is.na(df$cpt_code), "(missing)", as.character(df$cpt_code))

  rows <- list()

  for (i in seq_len(nrow(flagged))) {
    v <- flagged$var[i]
    if (!(v %in% names(df))) next

    by_cpt <- df |>
      summarise(
        events      = sum(.data[[v]], na.rm = TRUE),
        cases       = dplyr::n(),
        description = .modal_desc(cpt_desc),
        .by         = .cpt
      ) |>
      mutate(rate_pct = round(events / cases * 100, 1))

    top <- by_cpt |>
      filter(events >= min_cpt_events) |>
      arrange(desc(events), desc(rate_pct)) |>
      head(top_n)

    # Every event a one-off: there is no concentration to point a reviewer at,
    # and a table of rollup-only would imply otherwise.
    if (nrow(top) == 0) next

    rest <- by_cpt |> filter(!(.cpt %in% top$.cpt))

    out <- top |>
      transmute(
        cpt         = .cpt,
        description = str_trunc(coalesce(description, ""), max_desc),
        events      = as.integer(events),
        cases       = as.integer(cases),
        rate_pct    = rate_pct,
        is_other    = FALSE
      )

    # Only when there is something left to compare against: with every case
    # already listed, a 0-of-0 rollup row is noise.
    if (nrow(rest) > 0 && sum(rest$cases) > 0) {
      out <- bind_rows(out, tibble(
        cpt         = paste0("(", nrow(rest), ngettext(nrow(rest),
                                                       " CPT)", " CPTs)")),
        description = FLAG_PROCEDURE_OTHER,
        events      = as.integer(sum(rest$events)),
        cases       = as.integer(sum(rest$cases)),
        rate_pct    = round(sum(rest$events) / sum(rest$cases) * 100, 1),
        is_other    = TRUE
      ))
    }

    rows[[length(rows) + 1]] <- out |>
      mutate(complication = flagged$complication[i], var = v, .before = 1)
  }

  if (length(rows) == 0) return(NULL)
  bind_rows(rows)
}


# ---- Composite overlap ------------------------------------------------------
#
# Morbidity is an OR over the individual complications, so when SSI is elevated
# morbidity is elevated too and the worklist shows two flags for one problem.
# Whether that is true has to be checked rather than assumed: in the 2026-H1
# data Plastics morbidity was entirely SSI, while General Surgery was about
# half, with a substantial block of morbidity events no other flag accounted
# for. The first is one chart review, the second is two.
#
# Overlap is measured only against complications that themselves flagged. The
# question is whether one flag on the worklist subsumes another, not whether
# the composite is definitionally a union — it always is.

# Monitored complications that feed the morbidity composite, per its derivation
# in derive_case_indicators(). Wound disruption and stroke also feed it but are
# not separately monitored, so they can only ever appear as unexplained events.
COMPOSITE_COMPONENTS <- list(
  morbidity = c("ssi", "pneumonia", "unplanned_intubation", "vent48",
                "renal_failure", "uti", "cardiac", "sepsis")
)


.and_list <- function(x) {
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x[1])
  if (length(x) == 2) return(paste(x, collapse = " or "))
  paste0(paste(x[-length(x)], collapse = ", "), ", or ", x[length(x)])
}


#' Note where a flagged composite is explained by another flag
#'
#' @param triage Output of build_triage()
#' @param data Processed case data
#' @param spec Specialty name
#' @param div Division name, or NULL/"" for the whole specialty
#' @return `triage` with `overlap_note` and `overlap_full` columns added
#'   (NA/FALSE where the complication is not a composite, or where no component
#'   complication flagged). `overlap_full` marks the case where the composite
#'   adds no patients at all, so the report can suppress its duplicate detail.
annotate_composite_overlap <- function(triage, data, spec, div = NULL) {

  if (is.null(triage) || nrow(triage) == 0) return(triage)

  triage$overlap_note <- NA_character_
  triage$overlap_full <- FALSE

  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) df <- df |> filter(division == div)
  if (nrow(df) == 0) return(triage)

  flagged_vars <- triage$var[triage$tier > 0]

  for (i in seq_len(nrow(triage))) {
    v <- triage$var[i]
    if (triage$tier[i] == 0) next
    if (!(v %in% names(COMPOSITE_COMPONENTS))) next
    if (!(v %in% names(df))) next

    components <- intersect(COMPOSITE_COMPONENTS[[v]], flagged_vars)
    components <- intersect(components, names(df))
    if (length(components) == 0) next

    has_comp <- df[[v]] == 1L & !is.na(df[[v]])
    n_total  <- sum(has_comp)
    if (n_total == 0) next

    covered <- rep(FALSE, nrow(df))
    for (cv in components) covered <- covered | (df[[cv]] == 1L & !is.na(df[[cv]]))

    n_overlap <- sum(has_comp & covered)
    n_only    <- n_total - n_overlap
    if (n_overlap == 0) next

    labels <- .and_list(unname(complication_labels[components]))

    triage$overlap_full[i] <- n_only == 0

    triage$overlap_note[i] <- if (n_only == 0) {
      paste0(n_total, " events, all also counted under ", labels,
             " — one chart review, not two.")
    } else {
      paste0(n_overlap, " of ", n_total, " events (",
             round(n_overlap / n_total * 100), "%) also counted under ",
             labels, "; ", n_only,
             ngettext(n_only, " event is", " events are"),
             " not explained by another flag.")
    }
  }

  triage
}


# ---- Case list for the flagged complications ---------------------------------
#
# The full report's appendix lists every complication in the trailing three
# months, which is the right thing there and the wrong thing in a two-page
# handout. Two differences:
#
#   - Only the complications that flagged. A summary that reproduced every
#     occurrence would just be the appendix again.
#   - The whole report window, not the trailing three months, because the flag
#     was computed over the whole window. "13 SSIs" over a seven-row table is
#     a discrepancy the reader has to resolve, and they should not have to.
#
# Cases are matched on the triage row's own `var`, the same column build_triage()
# counted, so the row count reconciles with the Obs column by construction.

# `overlap_full` is logical but arrives NA for non-composites, and filter()
# drops NA rows. Wanted here is "not TRUE", NA included.
isTRUE_vec <- function(x) !is.na(x) & x


#' Case list restricted to the complications that flagged
#'
#' @param data Processed case data
#' @param spec Specialty name
#' @param div Division name, or NULL/"" for the whole specialty
#' @param triage Output of build_triage(), ideally after
#'   annotate_composite_overlap()
#' @param tiers Which tiers to include; default all flagged
#' @param drop_redundant Drop composites that add no patients over the flag
#'   explaining them, matching how the procedure breakdown is suppressed. Their
#'   cases are already listed under the explaining complication.
#' @return A tibble ready for display, or NULL if nothing flagged or no case
#'   carries a flagged complication
build_flag_caselist <- function(data, spec, div = NULL, triage,
                                tiers = 1:3, drop_redundant = TRUE) {

  if (is.null(triage) || nrow(triage) == 0) return(NULL)

  flagged <- triage |> filter(.data$tier %in% tiers)

  if (drop_redundant && "overlap_full" %in% names(triage)) {
    flagged <- flagged |> filter(!isTRUE_vec(.data$overlap_full))
  }
  if (nrow(flagged) == 0) return(NULL)

  df <- data |> filter(.data$specialty == spec)
  if (!is.null(div) && nchar(div) > 0) df <- df |> filter(.data$division == div)
  if (nrow(df) == 0) return(NULL)

  vars <- flagged$var[flagged$var %in% names(df)]
  if (length(vars) == 0) return(NULL)

  labels <- setNames(flagged$complication, flagged$var)[vars]

  # One occurrence string per case, naming only the flagged complications it
  # carries. Built column-wise rather than by apply()-ing over the data frame,
  # which coerces every column to character on the way in.
  hits <- lapply(vars, function(v) {
    on <- !is.na(df[[v]]) & df[[v]] == 1
    lbl <- unname(labels[v])
    if (v == "unplanned_readmit") {
      rel   <- !is.na(df$readmit_related)   & df$readmit_related   == 1
      unrel <- !is.na(df$readmit_unrelated) & df$readmit_unrelated == 1
      lbl <- ifelse(rel & unrel, "Readmission (related + unrelated)",
             ifelse(rel,         "Readmission (related)",
             ifelse(unrel,       "Readmission (unrelated)", lbl)))
    }
    ifelse(on, lbl, NA_character_)
  })

  occ <- apply(do.call(cbind, hits), 1, function(r) {
    r <- r[!is.na(r)]
    if (length(r) == 0) "" else paste(r, collapse = ", ")
  })

  df$.occ <- occ
  df <- df |> filter(nchar(.data$.occ) > 0)
  if (nrow(df) == 0) return(NULL)

  df |>
    arrange(.data$op_date) |>
    transmute(
      MRN         = .data$lmrn,
      `Op Date`   = format(.data$op_date, "%m/%d/%y"),
      Surgeon     = sapply(.data$surgeon, format_surgeon_name),
      CPT         = .data$cpt_code,
      ASA         = sapply(.data$asa_class, format_asa_class),
      LOS         = as.integer(.data$los),
      Occurrences = .data$.occ
    )
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
