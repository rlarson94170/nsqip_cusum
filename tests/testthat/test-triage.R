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
