# =============================================================================
# Case list for the flagged complications (executive summary)
#
# The property that matters is reconciliation: the summary prints the flag's
# Obs count and the case rows on the same page, so a reader can count them.
# If those two ever disagree the handout undermines itself.
# =============================================================================

# A boundary no run in these fixtures can reach: the timing gate is not what
# this file tests, and pinning it keeps the suite off boundary calibration.
H_INERT_CL <- 50

# 200 cases, Sepsis p0 = 1% -> 2 expected. Nine spread-out events clear the
# magnitude gate without clustering, which is Tier 2.
sepsis_scenario <- function(h = H_INERT_CL, ...) {
  cases <- fake_cases(c(Other = 200),
                      events = list(sepsis = events_at(200, seq(1, 180, by = 20))),
                      ...)
  tri <- build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
                      targeted_rates = NULL, cusum_h = h)
  list(cases = cases, triage = tri)
}


test_that("row count reconciles with the flag's observed count", {
  sc <- sepsis_scenario()
  cl <- build_flag_caselist(sc$cases, "General Surgery", NULL, sc$triage)

  obs <- sc$triage$observed[sc$triage$complication == "Sepsis"]
  expect_equal(nrow(cl), obs)
  expect_true(all(cl$Occurrences == "Sepsis"))
})

test_that("cases carrying only unflagged complications are excluded", {
  cases <- fake_cases(
    c(Other = 200),
    events = list(
      sepsis = events_at(200, seq(1, 180, by = 20)),   # flags
      ssi    = events_at(200, c(5, 6))                 # 2 events, below the floor
    )
  )
  tri <- build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
                      targeted_rates = NULL, cusum_h = H_INERT_CL)

  expect_equal(tri$tier[tri$complication == "SSI"], 0L)

  cl <- build_flag_caselist(cases, "General Surgery", NULL, tri)
  expect_equal(nrow(cl), 9L)
  expect_false(any(grepl("SSI", cl$Occurrences)))
})

test_that("a case carrying two flagged complications lists both", {
  cases <- fake_cases(
    c(Other = 200),
    events = list(
      sepsis    = events_at(200, seq(1, 180, by = 20)),
      mortality = events_at(200, c(1, 40, 60, 80, 100, 120, 140, 160))
    )
  )
  tri <- build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
                      targeted_rates = NULL, cusum_h = H_INERT_CL)
  cl <- build_flag_caselist(cases, "General Surgery", NULL, tri)

  # Case 1 has both; it appears once, naming both.
  both <- cl$Occurrences[cl$MRN == "00000001"]
  expect_length(both, 1L)
  expect_match(both, "Sepsis")
  expect_match(both, "Mortality")
})

test_that("nothing flagged yields NULL", {
  cases <- fake_cases(c(Other = 200), events = list(sepsis = events_at(200, 1)))
  tri <- build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
                      targeted_rates = NULL, cusum_h = H_INERT_CL)

  expect_true(all(tri$tier == 0L))
  expect_null(build_flag_caselist(cases, "General Surgery", NULL, tri))
})

test_that("NULL triage yields NULL rather than erroring", {
  cases <- fake_cases(c(Other = 10))
  expect_null(build_flag_caselist(cases, "General Surgery", NULL, NULL))
})

test_that("a fully-overlapping composite does not duplicate its cases", {
  # Every Morbidity event is also an SSI event, so Morbidity adds no patients.
  # Listing it would double every row.
  hits  <- events_at(200, seq(1, 120, by = 8))
  cases <- fake_cases(c(Other = 200),
                      events = list(ssi = hits, morbidity = hits))
  # Rates chosen so both the composite and its component clearly flag; at the
  # default Morbidity p0 the composite never reaches the magnitude gate and
  # the overlap logic is never exercised.
  bench <- fake_benchmarks(rates = c(SSI = 0.02, Morbidity = 0.03))
  tri <- build_triage(cases, "General Surgery", NULL, bench,
                      targeted_rates = NULL, cusum_h = H_INERT_CL)
  tri <- annotate_composite_overlap(tri, cases, "General Surgery", NULL)

  expect_true(tri$overlap_full[tri$complication == "Morbidity"])

  cl <- build_flag_caselist(cases, "General Surgery", NULL, tri)
  expect_equal(nrow(cl), sum(hits))
  expect_equal(anyDuplicated(cl$MRN), 0L)
  expect_false(any(grepl("Morbidity", cl$Occurrences)))

  # Opting out lists it, so the suppression is the caller's choice, not a
  # property of the data.
  cl2 <- build_flag_caselist(cases, "General Surgery", NULL, tri,
                             drop_redundant = FALSE)
  expect_true(any(grepl("Morbidity", cl2$Occurrences)))
})

test_that("a partially-overlapping composite is kept", {
  ssi_hits <- events_at(200, seq(1, 120, by = 8))
  morb     <- pmax(ssi_hits, events_at(200, c(3, 11, 19, 27, 35)))
  cases <- fake_cases(c(Other = 200),
                      events = list(ssi = ssi_hits, morbidity = morb))
  bench <- fake_benchmarks(rates = c(SSI = 0.02, Morbidity = 0.03))
  tri <- build_triage(cases, "General Surgery", NULL, bench,
                      targeted_rates = NULL, cusum_h = H_INERT_CL)
  tri <- annotate_composite_overlap(tri, cases, "General Surgery", NULL)

  expect_false(isTRUE(tri$overlap_full[tri$complication == "Morbidity"]))

  cl <- build_flag_caselist(cases, "General Surgery", NULL, tri)
  expect_equal(nrow(cl), sum(morb))
  expect_true(any(grepl("Morbidity", cl$Occurrences)))
})

test_that("readmission relatedness is spelled out", {
  cases <- fake_cases(c(Other = 200),
                      events = list(unplanned_readmit = events_at(200, 1:12)))
  cases$readmit_related[1]     <- 1L
  cases$readmit_unrelated[2]   <- 1L
  cases$readmit_related[3]     <- 1L
  cases$readmit_unrelated[3]   <- 1L

  bench <- fake_benchmarks(rates = c(`Unplanned Readmission` = 0.02))
  tri <- build_triage(cases, "General Surgery", NULL, bench,
                      targeted_rates = NULL, cusum_h = H_INERT_CL)
  cl <- build_flag_caselist(cases, "General Surgery", NULL, tri)

  expect_equal(cl$Occurrences[1], "Readmission (related)")
  expect_equal(cl$Occurrences[2], "Readmission (unrelated)")
  expect_equal(cl$Occurrences[3], "Readmission (related + unrelated)")
  expect_equal(cl$Occurrences[4], "Unplanned Readmission")
})

test_that("the division filter is respected", {
  a <- fake_cases(c(Other = 200), div = "Colorectal",
                  events = list(sepsis = events_at(200, seq(1, 180, by = 20))))
  b <- fake_cases(c(Other = 200), div = "ACSS",
                  events = list(sepsis = events_at(200, 1:9)))
  b$lmrn <- sprintf("B%07d", seq_len(nrow(b)))
  cases <- dplyr::bind_rows(a, b)

  tri <- build_triage(cases, "General Surgery", "Colorectal", fake_benchmarks(),
                      targeted_rates = NULL, cusum_h = H_INERT_CL)
  cl <- build_flag_caselist(cases, "General Surgery", "Colorectal", tri)

  expect_equal(nrow(cl), 9L)
  expect_false(any(grepl("^B", cl$MRN)))
})

test_that("the window is the whole report period, not the trailing months", {
  # The full report's appendix shows three months; the summary must not, or the
  # row count stops matching the flag's Obs. Events here span seven months.
  cases <- fake_cases(c(Other = 200),
                      events = list(sepsis = events_at(200, seq(1, 180, by = 20))))
  cases$op_date <- as.Date("2026-01-01") + seq_len(nrow(cases)) - 1

  tri <- build_triage(cases, "General Surgery", NULL, fake_benchmarks(),
                      targeted_rates = NULL, cusum_h = H_INERT_CL)

  cl   <- build_flag_caselist(cases, "General Surgery", NULL, tri)
  appx <- build_complication_caselist(cases, "General Surgery", NULL, months = 3)

  expect_equal(nrow(cl), 9L)
  expect_lt(nrow(appx), nrow(cl))
  expect_equal(min(as.Date(cl$`Op Date`, format = "%m/%d/%y")),
               as.Date("2026-01-01"))
})

test_that("tiers can be restricted", {
  sc <- sepsis_scenario()   # Sepsis is Tier 2
  expect_equal(sc$triage$tier[sc$triage$complication == "Sepsis"], 2L)

  expect_null(build_flag_caselist(sc$cases, "General Surgery", NULL,
                                  sc$triage, tiers = 1L))
  expect_equal(
    nrow(build_flag_caselist(sc$cases, "General Surgery", NULL,
                             sc$triage, tiers = 2L)),
    9L
  )
})

test_that("surgeon and ASA are formatted for display", {
  sc <- sepsis_scenario()
  cl <- build_flag_caselist(sc$cases, "General Surgery", NULL, sc$triage)

  expect_equal(unique(cl$Surgeon), "Surgeon, A")
  expect_equal(unique(cl$ASA), "II")
})
