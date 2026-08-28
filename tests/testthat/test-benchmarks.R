# =============================================================================
# Case-level (procedure-matched) benchmark assignment
# =============================================================================

test_that("without targeted rates every case gets the specialty rate", {
  cases <- fake_cases(c(Colectomy = 3, Other = 2))
  cp <- build_case_p0(cases, "SSI", "General Surgery", fake_benchmarks(), NULL)

  expect_equal(cp$p0, rep(0.04, 5))
  expect_equal(cp$n_matched, 0)
  expect_true(all(grepl("\\(specialty\\)$", cp$cohort)))
})

test_that("a matching procedure category takes the targeted rate", {
  cases <- fake_cases(c(Colectomy = 3, Other = 2))
  cp <- build_case_p0(cases, "SSI", "General Surgery",
                      fake_benchmarks(), fake_targeted())

  expect_equal(cp$p0, c(rep(0.06, 3), rep(0.04, 2)))
  expect_equal(cp$n_matched, 3)
})

test_that("distinct targeted procedures keep distinct rates", {
  cases <- fake_cases(c(Colectomy = 2, Proctectomy = 2, Other = 1))
  cp <- build_case_p0(cases, "SSI", "General Surgery",
                      fake_benchmarks(), fake_targeted())

  # Proctectomy SSI is more than twice Colectomy SSI — collapsing them into a
  # single colorectal rate is exactly what this design avoids.
  expect_equal(cp$p0, c(0.06, 0.06, 0.14, 0.14, 0.04))
})

test_that("several targeted models in one category combine case-weighted", {
  cases <- fake_cases(c(Hepatectomy = 2))
  cp <- build_case_p0(cases, "SSI", "General Surgery",
                      fake_benchmarks(), fake_targeted())

  # Major (n=10, 0.10) and Partial (n=30, 0.02): (10*0.10 + 30*0.02) / 40
  expect_equal(unique(cp$p0), 0.04)
  expect_match(unique(cp$cohort), "Major Hepatectomy / Partial Hepatectomy")
})

test_that("non-binary targeted models are never used as p0", {
  # Anastomotic Leak and Length of Stay carry complication_std = NA and have no
  # specialty-level counterpart; they must not leak into any complication.
  cases <- fake_cases(c(Colectomy = 2))
  for (comp in c("Mortality", "Morbidity")) {
    cp <- build_case_p0(cases, comp, "General Surgery",
                        fake_benchmarks(), fake_targeted())
    expect_false(any(cp$p0 %in% c(0.7777, 0.8888)),
                 info = paste("leaked into", comp))
  }
})

test_that("a targeted model with no expected rate falls back", {
  # fake_targeted() gives Colectomy Sepsis an NA exp_rate
  cases <- fake_cases(c(Colectomy = 3))
  cp <- build_case_p0(cases, "Sepsis", "General Surgery",
                      fake_benchmarks(), fake_targeted())

  expect_equal(cp$p0, rep(0.01, 3))
  expect_equal(cp$n_matched, 0)
})

test_that("a complication with no benchmark at all yields NA", {
  cases <- fake_cases(c(Colectomy = 2))
  cp <- build_case_p0(cases, "C.diff Colitis", "General Surgery",
                      fake_benchmarks(), fake_targeted())
  expect_true(all(is.na(cp$p0)))
})

test_that("the cohort mix accounts for every case", {
  cases <- fake_cases(c(Colectomy = 4, Proctectomy = 1, Other = 3))
  cp <- build_case_p0(cases, "SSI", "General Surgery",
                      fake_benchmarks(), fake_targeted())

  expect_equal(sum(cp$mix$n), 8)
  expect_equal(nrow(cp$mix), 3)
  expect_equal(length(cp$p0), 8)
})

test_that("describe_case_p0 reports the mix only when there is one", {
  cases <- fake_cases(c(Colectomy = 3, Other = 2))

  single <- describe_case_p0(
    build_case_p0(cases, "SSI", "General Surgery", fake_benchmarks(), NULL))
  expect_match(single, "^p₀ = 4%$")
  expect_false(grepl("procedure-matched", single))

  mixed <- describe_case_p0(
    build_case_p0(cases, "SSI", "General Surgery",
                  fake_benchmarks(), fake_targeted()))
  expect_match(mixed, "3/5 procedure-matched")
  expect_match(mixed, "mean")
})

test_that("targeted rates for another specialty are ignored", {
  cases <- fake_cases(c(Colectomy = 3))
  other <- fake_targeted(spec = "Vascular")
  cp <- build_case_p0(cases, "SSI", "General Surgery",
                      fake_benchmarks(), other)

  expect_equal(cp$p0, rep(0.04, 3))
  expect_equal(cp$n_matched, 0)
})
