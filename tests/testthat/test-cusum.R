# =============================================================================
# Bernoulli CUSUM: scoring, accumulation, and boundary calibration
# =============================================================================

test_that("scores match the log-likelihood ratio definition", {
  p0 <- 0.05
  or0 <- p0 / (1 - p0); p1 <- (2 * or0) / (1 + 2 * or0)
  s_event   <- log((p1 * (1 - p0)) / (p0 * (1 - p1)))
  s_noevent <- log((1 - p1) / (1 - p0))

  r <- compute_cusum(c(1L, 0L), p0 = p0, h = 99)

  expect_equal(r$score[1], s_event)
  expect_equal(r$score[2], s_noevent)
  expect_gt(s_event, 0)      # an event pushes the statistic up
  expect_lt(s_noevent, 0)    # a clean case pulls it down
})

test_that("the statistic accumulates and never goes negative", {
  r <- compute_cusum(rep(0L, 50), p0 = 0.05, h = 99)
  expect_true(all(r$cusum >= 0))
  expect_equal(r$cusum[50], 0)   # a clean run is pinned at the reflecting zero
})

test_that("a signal fires exactly when the boundary is reached", {
  # h chosen so two consecutive events cross it but one does not
  p0 <- 0.05
  one <- compute_cusum(c(1L), p0 = p0, h = 99)$cusum[1]
  r <- compute_cusum(c(1L, 1L), p0 = p0, h = one * 1.5)

  expect_false(r$signal[1])
  expect_true(r$signal[2])
})

test_that("reset zeroes the statistic after a signal, and reset = FALSE does not", {
  p0 <- 0.05
  one <- compute_cusum(1L, p0 = p0, h = 99)$cusum[1]
  h <- one * 0.5   # a single event signals

  with_reset <- compute_cusum(c(1L, 0L), p0 = p0, h = h, reset = TRUE)
  no_reset   <- compute_cusum(c(1L, 0L), p0 = p0, h = h, reset = FALSE)

  expect_true(with_reset$signal[1])
  expect_lt(with_reset$cusum[2], no_reset$cusum[2])
  expect_equal(with_reset$cusum[2], 0)   # reset, then a clean case cannot go below 0
})

test_that("a scalar p0 is recycled and a vector p0 is used per case", {
  scalar <- compute_cusum(c(1L, 1L), p0 = 0.05, h = 99)
  expect_equal(unique(scalar$p0), 0.05)

  # A rarer baseline scores an event more heavily
  mixed <- compute_cusum(c(1L, 1L), p0 = c(0.01, 0.20), h = 99)
  expect_equal(mixed$p0, c(0.01, 0.20))
  expect_gt(mixed$score[1], mixed$score[2])
})

test_that("invalid p0 is rejected rather than silently recycled", {
  expect_error(compute_cusum(c(0L, 1L, 0L), p0 = c(0.05, 0.05)), "length 1 or")
  expect_error(compute_cusum(c(0L, 1L), p0 = c(0.05, NA)), "NA")
  expect_error(compute_cusum(c(0L, 1L), p0 = c(0.05, 0)), "between 0 and 1")
  expect_error(compute_cusum(c(0L, 1L), p0 = c(0.05, 1)), "between 0 and 1")
  expect_error(compute_cusum(integer(0), p0 = 0.05), "empty")
})

test_that("calibration diagnostics travel with the result", {
  r <- compute_cusum(rep(0L, 20), p0 = 0.08, target_arl = 200)
  expect_false(is.null(attr(r, "arl0")))
  expect_false(is.null(attr(r, "events_to_signal")))
  expect_gte(attr(r, "events_to_signal"), 1)
})


# ---- calibrate_h ------------------------------------------------------------

test_that("h hits the requested in-control ARL", {
  h <- calibrate_h(0.08, target_arl = 300, n_sim = 200)
  # Monte Carlo, so allow a generous band around the target
  expect_gt(attr(h, "arl0"), 300 * 0.7)
  expect_lt(attr(h, "arl0"), 300 * 1.4)
})

test_that("h increases with the requested ARL", {
  h_low  <- calibrate_h(0.08, target_arl = 300,  n_sim = 200)
  h_high <- calibrate_h(0.08, target_arl = 1500, n_sim = 200)
  expect_gt(as.numeric(h_high), as.numeric(h_low))
})

test_that("h responds to the odds ratio", {
  # This is the regression the old lookup table could not express: it was
  # generated for OR = 2 only and returned the same h for every OR.
  h_or15 <- calibrate_h(0.05, odds_ratio = 1.5, target_arl = 500, n_sim = 200)
  h_or30 <- calibrate_h(0.05, odds_ratio = 3.0, target_arl = 500, n_sim = 200)
  expect_false(isTRUE(all.equal(as.numeric(h_or15), as.numeric(h_or30))))
})

test_that("h is continuous across the old lookup table's 4.5% break", {
  # The replaced table jumped from h = 4.5 to h = 2.5 between p0 = 0.044 and
  # 0.046, giving ARLs of ~4,400 and ~600 for near-identical rates.
  h_below <- as.numeric(calibrate_h(0.044, target_arl = 500, n_sim = 200))
  h_above <- as.numeric(calibrate_h(0.046, target_arl = 500, n_sim = 200))
  expect_lt(abs(h_below - h_above), 0.5)
})

test_that("an unreachable ARL warns instead of silently missing", {
  # At a low rate the statistic moves in steps of ~log(OR), so a short ARL
  # cannot be achieved at any boundary.
  expect_warning(
    calibrate_h(0.01, target_arl = 50, n_sim = 100),
    "closest achievable"
  )
})

test_that("calibration is memoised and deterministic", {
  a <- calibrate_h(0.07, target_arl = 400, n_sim = 150)
  b <- calibrate_h(0.07, target_arl = 400, n_sim = 150)
  expect_identical(as.numeric(a), as.numeric(b))
})

test_that("calibration leaves the global RNG stream undisturbed", {
  # calibrate_h sets a fixed seed for reproducibility; it must restore the
  # caller's stream so it cannot silently change downstream sampling.
  set.seed(123)
  before <- runif(3)

  set.seed(123)
  invisible(calibrate_h(0.06, target_arl = 300, n_sim = 100))
  after <- runif(3)

  expect_equal(before, after)
})

test_that("rejects p0 outside the open unit interval", {
  expect_error(calibrate_h(0, target_arl = 500), "between 0 and 1")
  expect_error(calibrate_h(1, target_arl = 500), "between 0 and 1")
  expect_error(calibrate_h(NA_real_, target_arl = 500), "no usable p0")
})


# ---- Chart display titles ---------------------------------------------------
#
# generate_specialty_charts() keys its list on the column name, so any caller
# rendering a heading has to translate. The slide decks did not, and titled
# frames "unplanned_reop" for three releases.

test_that("chart titles resolve to display labels", {
  expect_equal(chart_frame_title("unplanned_reop"), "Unplanned Reoperation")
  expect_equal(chart_frame_title("cdiff"), "C.diff Colitis")
  expect_equal(chart_frame_title("vent48"), "Ventilator > 48h")
})

test_that("an unrecognised name passes through rather than becoming NA", {
  expect_equal(chart_frame_title("not_a_complication"), "not_a_complication")
})

test_that("every monitored complication has a title", {
  titles <- chart_frame_title(names(complication_labels))
  expect_false(any(is.na(titles)))
  expect_false(any(titles == names(complication_labels)))
})
