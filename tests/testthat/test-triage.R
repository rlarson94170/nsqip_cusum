# =============================================================================
# Triage tiering and carry-over tracking
#
# The tiering tests pin cusum_h so the timing gate is deterministic and the
# suite does not pay for boundary calibration; calibration itself is covered
# in test-cusum.R.
# =============================================================================

# A boundary low enough that any clustered pair of events signals
H_SENSITIVE <- 0.5
# A boundary no realistic run in these fixtures can reach
H_INERT     <- 50

triage_for <- function(events, n_by_cat = c(Other = 200), h = H_INERT, ...) {
  cases <- fake_cases(n_by_cat, events = events)
  build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
               targeted_rates = NULL, cusum_h = h, ...)
}

row_for <- function(tri, comp) tri[tri$complication == comp, ]


# ---- Magnitude gate ---------------------------------------------------------

test_that("a clear excess with no clustering is Tier 2", {
  # 200 cases, Sepsis p0 = 1% -> 2 expected; 9 observed
  tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
  r <- row_for(tri, "Sepsis")

  expect_equal(r$observed, 9L)
  expect_equal(r$expected, 2.0)
  expect_lt(r$p_value, TRIAGE_ALPHA)
  expect_false(r$cusum_signal)
  expect_equal(r$tier, 2L)
  expect_equal(r$tier_label, "Worth a look")
})

test_that("an excess that also clusters is Tier 1", {
  tri <- triage_for(list(sepsis = events_at(200, 1:9)), h = H_SENSITIVE)
  r <- row_for(tri, "Sepsis")

  expect_true(r$cusum_signal)
  expect_lt(r$p_value, TRIAGE_ALPHA)
  expect_equal(r$tier, 1L)
  expect_equal(r$tier_label, "Review now")
})

test_that("the minimum-event floor blocks a large ratio on tiny counts", {
  # 2 events against 2 expected is not a finding; nor is 2 against 0.4
  tri <- triage_for(list(sepsis = events_at(200, c(1, 2))),
                    n_by_cat = c(Other = 40), h = H_SENSITIVE)
  r <- row_for(tri, "Sepsis")

  expect_equal(r$observed, 2L)
  expect_lt(r$observed, TRIAGE_MIN_EVENTS)
  expect_true(r$tier %in% c(0L, 3L))   # never Tier 1 or 2
  expect_false(identical(r$tier, 1L))
  expect_false(identical(r$tier, 2L))
})


# ---- Timing gate and the suppression rule -----------------------------------

test_that("clustering with observed at or above expected is Tier 3", {
  # Morbidity p0 = 8% -> 16 expected over 200; 17 observed, tightly clustered
  tri <- triage_for(list(morbidity = events_at(200, 1:17)), h = H_SENSITIVE)
  r <- row_for(tri, "Morbidity")

  expect_true(r$cusum_signal)
  expect_gte(r$observed, r$expected)
  expect_gt(r$p_value, TRIAGE_ALPHA)   # not a significant excess
  expect_equal(r$tier, 3L)
  expect_equal(r$tier_label, "Watch")
})

test_that("clustering with observed BELOW expected is suppressed", {
  # This is the wild-goose-chase case: a real run of events, no excess behind
  # it. The live data showed Unplanned Readmission at 28 obs vs 32.3 expected.
  tri <- triage_for(list(morbidity = events_at(200, 1:8)), h = H_SENSITIVE)
  r <- row_for(tri, "Morbidity")

  expect_true(r$cusum_signal)
  expect_lt(r$observed, r$expected)
  expect_true(r$suppressed)
  expect_equal(r$tier, 0L)
  expect_true(is.na(r$tier_label))
})

test_that("no events and no clustering is untiered", {
  tri <- triage_for(list())
  expect_true(all(tri$tier == 0L))
  expect_true(all(is.na(tri$tier_label)))
  expect_false(any(tri$suppressed))
})

test_that("results are ordered with flagged rows first", {
  tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
  expect_equal(tri$tier[1], 2L)
  expect_true(all(diff(ifelse(tri$tier == 0, 99L, tri$tier)) >= 0))
})


# ---- Detection floor --------------------------------------------------------

test_that("min_events_to_flag respects the floor and the alpha", {
  # Never returns fewer than the minimum, however small the expectation
  expect_gte(min_events_to_flag(0.1), TRIAGE_MIN_EVENTS)

  # The returned count is significant and one fewer is not
  k <- min_events_to_flag(5)
  expect_lt(ppois(k - 1, 5, lower.tail = FALSE), TRIAGE_ALPHA)
  expect_gte(ppois(k - 2, 5, lower.tail = FALSE), TRIAGE_ALPHA)

  # A larger expectation needs more events but a smaller fold excess
  expect_gt(min_events_to_flag(20), min_events_to_flag(2))
  expect_lt(min_events_to_flag(20) / 20, min_events_to_flag(2) / 2)
})

test_that("min_events_to_flag handles a degenerate expectation", {
  expect_true(is.na(min_events_to_flag(0)))
  expect_true(is.na(min_events_to_flag(NA_real_)))
})

test_that("every scored complication reports a detection floor", {
  tri <- triage_for(list())
  expect_true(all(!is.na(tri$need_events)))
  expect_true(all(tri$fold > 1))
})


# ---- Carry-over tracking ----------------------------------------------------

with_history <- function(code) {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  code(path)
}

test_that("a first-ever flag is marked New and persisted", {
  with_history(function(path) {
    tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
    out <- annotate_carryover(tri, "General Surgery", NULL,
                              as.Date("2026-08-27"), path)

    expect_equal(row_for(out, "Sepsis")$status, "New")
    expect_true(all(is.na(out$status[out$tier == 0])))

    hist <- read.csv(path, stringsAsFactors = FALSE)
    expect_equal(nrow(hist), 1)
    expect_equal(hist$complication, "Sepsis")
    expect_equal(hist$division, SPECIALTY_LEVEL_KEY)
  })
})

test_that("a flag seen in the previous report is marked carried over", {
  with_history(function(path) {
    tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
    annotate_carryover(tri, "General Surgery", NULL, as.Date("2026-08-27"), path)
    out <- annotate_carryover(tri, "General Surgery", NULL,
                              as.Date("2026-11-27"), path)

    expect_match(row_for(out, "Sepsis")$status, "^Carried over")
    expect_match(row_for(out, "Sepsis")$status, "Aug 2026")
  })
})

test_that("re-rendering the same report does not duplicate history", {
  # The PDF and the slide deck both call annotate_carryover for one report.
  with_history(function(path) {
    tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
    for (i in 1:3) {
      annotate_carryover(tri, "General Surgery", NULL, as.Date("2026-08-27"), path)
    }
    expect_equal(nrow(read.csv(path, stringsAsFactors = FALSE)), 1)
  })
})

test_that("specialty-level scope round-trips through the history file", {
  # Regression: an empty division string came back from read.csv as a logical
  # NA column, which broke the bind_rows() upsert on type.
  with_history(function(path) {
    tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
    annotate_carryover(tri, "General Surgery", "",   as.Date("2026-08-27"), path)
    expect_no_error(
      annotate_carryover(tri, "General Surgery", "", as.Date("2026-11-27"), path)
    )
    hist <- read.csv(path, stringsAsFactors = FALSE)
    expect_type(hist$division, "character")
    expect_true(all(hist$division == SPECIALTY_LEVEL_KEY))
  })
})

test_that("divisions are tracked independently of each other", {
  with_history(function(path) {
    tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
    annotate_carryover(tri, "General Surgery", "Colorectal",
                       as.Date("2026-08-27"), path)
    # A different division in the next quarter has no history of its own
    out <- annotate_carryover(tri, "General Surgery", "ACSS",
                              as.Date("2026-11-27"), path)

    expect_equal(row_for(out, "Sepsis")$status, "New")
    expect_equal(nrow(read.csv(path, stringsAsFactors = FALSE)), 2)
  })
})

test_that("a report with no flags writes nothing and marks nothing", {
  with_history(function(path) {
    out <- annotate_carryover(triage_for(list()), "General Surgery", NULL,
                              as.Date("2026-08-27"), path)
    expect_true(all(is.na(out$status)))
    expect_false(file.exists(path))
  })
})

test_that("persistence can be switched off", {
  tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
  out <- annotate_carryover(tri, "General Surgery", NULL,
                            as.Date("2026-08-27"), path = "")
  expect_equal(row_for(out, "Sepsis")$status, "New")
})

test_that("a corrupt history file degrades to New rather than failing", {
  with_history(function(path) {
    writeLines(c("this is not,a valid history", "1,2,3"), path)
    tri <- triage_for(list(sepsis = events_at(200, seq(1, 180, by = 20))))
    expect_no_error(
      out <- annotate_carryover(tri, "General Surgery", NULL,
                                as.Date("2026-08-27"), path)
    )
    expect_equal(row_for(out, "Sepsis")$status, "New")
  })
})


# ---- Per-flag procedure breakdown -------------------------------------------
#
# The scenario throughout is the one that motivated the feature: a service-wide
# SSI flag whose events are concentrated in two low-volume procedures.

concentrated_cases <- function(n = 200) {
  cpts <- c(rep("19318", 20), rep("19364", 15), rep("15734", n - 35))
  ssi  <- integer(n)
  ssi[c(1:5, 21:25, 40:42)] <- 1L      # 5 of 20, 5 of 15, 3 of 165
  fake_cases(c(Other = n), events = list(ssi = ssi), cpt = cpts)
}

procs_for <- function(cases, benchmarks = fake_benchmarks(), ...) {
  tri <- build_triage(cases, "General Surgery", NULL, benchmarks,
                      cusum_h = H_INERT)
  build_flag_procedures(cases, "General Surgery", NULL, tri, ...)
}


test_that("events are attributed to the CPTs carrying them", {
  p <- procs_for(concentrated_cases())

  expect_equal(sum(p$events[!p$is_other]), 13L)
  expect_equal(p$cpt[1:2], c("19364", "19318"))
  expect_equal(p$rate_pct[p$cpt == "19364"], 33.3)
  expect_equal(p$rate_pct[p$cpt == "19318"], 25.0)
})

test_that("ties on event count are broken by rate, not by CPT order", {
  # 19318 and 19364 both carry 5 events; the denser one must lead
  p <- procs_for(concentrated_cases())
  expect_equal(p$events[1], p$events[2])
  expect_gt(p$rate_pct[1], p$rate_pct[2])
})

# 8 CPTs of 10 cases carrying 2 events each: 16 events against 3.2 expected,
# so SSI flags and 3 of the 8 CPTs fall past the top-5 cut.
spread_cases <- function() {
  n <- 80
  cpts <- rep(as.character(19301:19308), each = 10)
  ssi  <- integer(n)
  ssi[c(outer(c(1, 2), seq(0, 70, by = 10), "+"))] <- 1L
  fake_cases(c(Other = n), events = list(ssi = ssi), cpt = cpts)
}

test_that("the rollup row aggregates every CPT not listed", {
  p <- procs_for(spread_cases())
  other <- p[p$is_other, ]

  expect_equal(nrow(p), 6L)                     # 5 listed + rollup
  expect_equal(nrow(other), 1L)
  expect_equal(other$events, 6L)                # the 3 unlisted CPTs
  expect_equal(other$cases, 30L)
  expect_equal(other$description, FLAG_PROCEDURE_OTHER)
  expect_equal(sum(p$cases[!p$is_other]) + other$cases, 80L)
})

test_that("zero-event CPTs still form the comparison row", {
  # Every event sits in one procedure; the rest of the service is the contrast
  n <- 200
  ssi <- integer(n); ssi[1:13] <- 1L
  cases <- fake_cases(c(Other = n), events = list(ssi = ssi),
                      cpt = c(rep("19318", 20), rep("15734", 180)))

  p <- procs_for(cases)
  other <- p[p$is_other, ]

  expect_equal(nrow(other), 1L)
  expect_equal(other$events, 0L)
  expect_equal(other$rate_pct, 0)
  expect_equal(other$cpt, "(1 CPT)")           # singular
})

test_that("a CPT carrying one event is folded into the rollup, not listed", {
  # The rollup is the baseline, so singletons must not be promoted out of it:
  # doing so both adds a 1-of-1 row at 100% and drains the comparison.
  n <- 200
  # Two concentrations, then four single-case CPTs each with one event
  cpts <- c(rep("19318", 20), rep("19364", 15),
            "26615", "26620", "26625", "26630", rep("15734", n - 39))
  ssi  <- integer(n)
  ssi[c(1:5, 21:25, 36:39)] <- 1L       # 5 of 20, 5 of 15, and four 1-of-1s
  cases <- fake_cases(c(Other = n), events = list(ssi = ssi), cpt = cpts)

  p <- procs_for(cases)

  expect_false(any(c("26615", "26620", "26625", "26630") %in% p$cpt))
  expect_setequal(p$cpt[!p$is_other], c("19364", "19318"))
  expect_equal(p$events[p$is_other], 4L)       # the four singletons
  expect_equal(p$cases[p$is_other], 165L)      # 161 filler + the 4
  expect_equal(p$rate_pct[p$is_other], 2.4)
})

test_that("a complication whose events are all one-offs gets no table", {
  n <- 200
  cpts <- as.character(20000 + seq_len(n))     # every case its own CPT
  cases <- fake_cases(c(Other = n), events = list(ssi = events_at(n, 1:13)),
                      cpt = cpts)
  expect_null(procs_for(cases))
})

test_that("no rollup row appears when every case is already listed", {
  p <- procs_for(concentrated_cases())
  expect_false(any(p$is_other))                # only 3 CPTs, all carry events
})

test_that("top_n caps the rows listed before the rollup", {
  p <- procs_for(spread_cases(), top_n = 2)
  expect_equal(sum(!p$is_other), 2L)
  expect_equal(p$events[p$is_other], 12L)       # the other 6 CPTs
})

test_that("only flagged complications get a breakdown", {
  p <- procs_for(concentrated_cases())
  expect_setequal(unique(p$complication), "SSI")
  expect_setequal(unique(p$var), "ssi")
})

test_that("nothing flagged means no table rather than an empty one", {
  cases <- fake_cases(c(Other = 200))          # no events anywhere
  expect_null(procs_for(cases))
})

test_that("case data without CPT columns degrades to no table", {
  cases <- concentrated_cases()
  tri <- build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
                      cusum_h = H_INERT)
  bare <- cases |> dplyr::select(-cpt_code, -cpt_desc)

  expect_null(build_flag_procedures(bare, "General Surgery", NULL, tri))
})

test_that("a missing CPT code is grouped rather than dropped", {
  n <- 200
  ssi <- integer(n); ssi[1:13] <- 1L
  cases <- fake_cases(c(Other = n), events = list(ssi = ssi),
                      cpt = c(rep("19318", 20), rep("15734", 180)))
  cases$cpt_code[1:6] <- NA

  p <- procs_for(cases)
  expect_true("(missing)" %in% p$cpt)
  expect_equal(sum(p$events), 13L)             # no event lost to the NA group
})

test_that("long procedure descriptions are truncated", {
  cases <- concentrated_cases()
  cases$cpt_desc <- strrep("Breast reconstruction with free flap ", 3)

  p <- procs_for(cases, max_desc = 20)
  expect_true(all(nchar(p$description[!p$is_other]) <= 20))
})


# ---- Composite overlap ------------------------------------------------------

# Morbidity at 4% against 200 cases expects 8, so 13 events is a real excess.
# At the fake_benchmarks default of 8% it would sit below expected and never
# flag, which is a property of the fixture rather than of the feature.
bm_morbidity <- function() {
  fake_benchmarks(rates = c(SSI = 0.04, Sepsis = 0.01,
                            Mortality = 0.02, Morbidity = 0.04))
}

overlap_for <- function(events, n = 200) {
  cases <- fake_cases(c(Other = n), events = events)
  tri <- build_triage(cases, "General Surgery", NULL, bm_morbidity(),
                      cusum_h = H_INERT)
  annotate_composite_overlap(tri, cases, "General Surgery", NULL)
}


test_that("a composite explained entirely by another flag says so", {
  ssi <- events_at(200, 1:13)
  tri <- overlap_for(list(ssi = ssi, morbidity = ssi))
  r <- row_for(tri, "Morbidity")

  expect_true(r$overlap_full)
  expect_match(r$overlap_note, "^13 events, all also counted under SSI")
})

test_that("a partly explained composite reports the unexplained remainder", {
  # 29 morbidity events, 13 of them the SSIs
  tri <- overlap_for(list(
    ssi       = events_at(200, 1:13),
    morbidity = events_at(200, c(1:13, 50:65))
  ))
  r <- row_for(tri, "Morbidity")

  expect_false(r$overlap_full)
  expect_match(r$overlap_note, "13 of 29 events \\(45%\\)")
  expect_match(r$overlap_note, "16 events are not explained")
})

test_that("every flagged component is named in the note", {
  # SSI and Sepsis both flag and both feed morbidity
  tri <- overlap_for(list(
    ssi       = events_at(200, 1:13),
    sepsis    = events_at(200, 20:25),
    morbidity = events_at(200, c(1:13, 20:25))
  ))
  r <- row_for(tri, "Morbidity")

  expect_match(r$overlap_note, "SSI")
  expect_match(r$overlap_note, "Sepsis")
})

test_that("overlap is measured against flags, not against definitions", {
  # SSI drives morbidity but is itself too small to flag, so morbidity is a
  # finding in its own right and must not be explained away
  tri <- overlap_for(list(
    ssi       = events_at(200, 1:2),
    morbidity = events_at(200, 1:13)
  ))

  expect_equal(row_for(tri, "SSI")$tier, 0L)
  expect_true(is.na(row_for(tri, "Morbidity")$overlap_note))
})

test_that("disjoint flags produce no note", {
  tri <- overlap_for(list(
    ssi       = events_at(200, 1:13),
    morbidity = events_at(200, 50:78)
  ))
  expect_true(is.na(row_for(tri, "Morbidity")$overlap_note))
})

test_that("complications that are not composites are never annotated", {
  ssi <- events_at(200, 1:13)
  tri <- overlap_for(list(ssi = ssi, morbidity = ssi))

  expect_true(all(is.na(tri$overlap_note[tri$var != "morbidity"])))
  expect_false(any(tri$overlap_full[tri$var != "morbidity"]))
})

test_that("the columns are added even when nothing flags", {
  tri <- overlap_for(list())
  expect_true(all(c("overlap_note", "overlap_full") %in% names(tri)))
  expect_true(all(is.na(tri$overlap_note)))
})

test_that("VTE and readmission are not treated as morbidity components", {
  # Neither feeds the composite in derive_case_indicators(), so neither can
  # explain it away
  expect_false("vte" %in% COMPOSITE_COMPONENTS$morbidity)
  expect_false("unplanned_readmit" %in% COMPOSITE_COMPONENTS$morbidity)
  expect_false("mortality" %in% COMPOSITE_COMPONENTS$morbidity)
  expect_true(all(COMPOSITE_COMPONENTS$morbidity %in% names(complication_labels)))
})
