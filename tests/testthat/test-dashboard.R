# =============================================================================
# Monthly complication dashboard
# =============================================================================

#' Case data spanning `n_months` from `start`, one case per month, with an
#' event in every month so per-month counts are trivially predictable.
monthly_cases <- function(n_months, start = as.Date("2026-01-01"),
                          per_month = 1L) {
  starts <- seq(start, by = "month", length.out = n_months)
  dates  <- rep(starts, each = per_month)
  n      <- length(dates)

  df <- tibble::tibble(
    op_date            = dates,
    specialty          = "General Surgery",
    division           = "Colorectal",
    procedure_category = "Other"
  )
  for (v in names(complication_labels)) df[[v]] <- 0L
  for (v in c("d_ssi_superficial", "d_ssi_deep", "d_ssi_organ", "d_septic_shock",
              "renal_insuff", "postop_dialysis_ind", "cardiac_arrest", "mi",
              "dvt", "pe", "wound_disruption", "stroke_cva_ind")) {
    df[[v]] <- 0L
  }
  df$uti <- 1L   # one UTI per case, so a month's count equals its case count
  df
}

test_that("a within-year window labels months without the year", {
  d <- build_dashboard(monthly_cases(6), "General Surgery")
  expect_equal(d$months, c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))
})

test_that("a window crossing a year boundary disambiguates the labels", {
  d <- build_dashboard(monthly_cases(4, start = as.Date("2025-11-01")),
                       "General Surgery")
  expect_equal(d$months, c("Nov 25", "Dec 25", "Jan 26", "Feb 26"))
})

test_that("months beyond twelve get one column each, not merged", {
  # Regression: labelling with "%b" alone gave two "Jan" columns whose counts
  # were summed together, so an 18-month window reported 12 months of data.
  n <- 18
  d <- build_dashboard(monthly_cases(n), "General Surgery")

  expect_equal(length(d$months), n)
  expect_equal(anyDuplicated(d$months), 0)
  expect_equal(sum(names(d$table) %in% d$months), n)
})

test_that("per-month counts are attributed to the right month", {
  # 14 months, with a distinctive number of cases in the second January
  cases <- rbind(
    monthly_cases(14, start = as.Date("2026-01-01")),
    monthly_cases(1,  start = as.Date("2027-01-01"), per_month = 5L)
  )
  d <- build_dashboard(cases, "General Surgery")

  uti <- d$table[d$table$Complication == "UTI", ]
  expect_equal(as.integer(uti[["Jan 26"]]), 1L)
  expect_equal(as.integer(uti[["Jan 27"]]), 6L)   # 1 from the span + 5 added

  cases_row <- d$table[d$table$Complication == "Cases Reviewed", ]
  expect_equal(as.integer(cases_row[["Jan 26"]]), 1L)
  expect_equal(as.integer(cases_row[["Jan 27"]]), 6L)
})

test_that("monthly counts and the total reconcile", {
  d <- build_dashboard(monthly_cases(15, per_month = 2L), "General Surgery")

  uti <- d$table[d$table$Complication == "UTI", ]
  monthly_sum <- sum(as.integer(unlist(uti[d$months])))

  expect_equal(monthly_sum, as.integer(uti$Total))
  expect_equal(as.integer(uti$Total), 30L)
  expect_equal(d$n_total, 30L)
})

test_that("a month with no cases in the range is simply absent", {
  # A gap month should not appear as a phantom zero column
  cases <- rbind(
    monthly_cases(2, start = as.Date("2026-01-01")),
    monthly_cases(2, start = as.Date("2026-06-01"))
  )
  d <- build_dashboard(cases, "General Surgery")
  expect_equal(d$months, c("Jan", "Feb", "Jun", "Jul"))
})

test_that("an empty scope returns NULL rather than erroring", {
  expect_null(build_dashboard(monthly_cases(3), "Vascular"))
})
