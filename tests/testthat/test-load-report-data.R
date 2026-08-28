# =============================================================================
# Shared report loading and its on-disk cache
#
# The loader itself reads a Case Details workbook, which is PHI and cannot be
# committed as a fixture. What is covered here is the cache contract — key
# construction, hit/miss, invalidation, and failure modes — since a cache that
# returns stale data would silently produce reports from the wrong download.
# =============================================================================

# A stand-in for the loaded bundle
fake_bundle <- function(n = 3) {
  list(
    case_data          = tibble::tibble(case_id = seq_len(n)),
    benchmark_rates    = fake_benchmarks(),
    targeted_data      = fake_targeted(),
    ot_data            = NULL,
    site_sar_available = TRUE
  )
}

with_files <- function(code) {
  dir <- tempfile(); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  data_file <- file.path(dir, "cases.xlsx")
  writeLines("placeholder", data_file)
  code(dir, data_file)
}


test_that("the cache key changes when an input file changes", {
  with_files(function(dir, data_file) {
    k1 <- .report_cache_key(data_file, "", "", "site_expected", "General Surgery")

    Sys.sleep(0.01)
    writeLines(c("placeholder", "more content"), data_file)
    k2 <- .report_cache_key(data_file, "", "", "site_expected", "General Surgery")

    expect_false(identical(k1, k2))
  })
})

test_that("the cache key changes with the benchmark mode and specialties", {
  with_files(function(dir, data_file) {
    base <- .report_cache_key(data_file, "", "", "site_expected", "General Surgery")

    expect_false(identical(base,
      .report_cache_key(data_file, "", "", "national_observed", "General Surgery")))
    expect_false(identical(base,
      .report_cache_key(data_file, "", "", "site_expected", c("General Surgery", "Vascular"))))
  })
})

test_that("specialty order does not change the key", {
  with_files(function(dir, data_file) {
    a <- .report_cache_key(data_file, "", "", "site_expected", c("Vascular", "General Surgery"))
    b <- .report_cache_key(data_file, "", "", "site_expected", c("General Surgery", "Vascular"))
    expect_identical(a, b)
  })
})

test_that("an absent optional input is distinct from a present one", {
  with_files(function(dir, data_file) {
    sar <- file.path(dir, "sar.xlsx")
    absent <- .report_cache_key(data_file, sar, "", "site_expected", "General Surgery")
    writeLines("sar", sar)
    present <- .report_cache_key(data_file, sar, "", "site_expected", "General Surgery")
    expect_false(identical(absent, present))
  })
})

test_that("a stored bundle is returned for a matching key", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    .report_cache_put(cache, "key-A", fake_bundle(7))

    hit <- .report_cache_get(cache, "key-A")
    expect_false(is.null(hit))
    expect_equal(nrow(hit$case_data), 7)
  })
})

test_that("a different key misses rather than returning stale data", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    .report_cache_put(cache, "key-A", fake_bundle())
    expect_null(.report_cache_get(cache, "key-B"))
  })
})

test_that("writing a new key replaces the old entry", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    .report_cache_put(cache, "key-A", fake_bundle(3))
    .report_cache_put(cache, "key-B", fake_bundle(9))

    expect_null(.report_cache_get(cache, "key-A"))
    expect_equal(nrow(.report_cache_get(cache, "key-B")$case_data), 9)
  })
})

test_that("a corrupt cache file is a miss, not a failure", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    dir.create(cache)
    writeLines("not an rds file", .report_cache_file(cache))

    expect_no_error(hit <- .report_cache_get(cache, "any-key"))
    expect_null(hit)
  })
})

test_that("a bundle missing required elements is rejected", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    .report_cache_put(cache, "key-A", list(something_else = 1))
    expect_null(.report_cache_get(cache, "key-A"))
  })
})

test_that("caching can be switched off", {
  with_files(function(dir, data_file) {
    expect_false(.report_cache_put("", "key-A", fake_bundle()))
    expect_null(.report_cache_get("", "key-A"))
  })
})

test_that("a missing cache directory is a miss", {
  expect_null(.report_cache_get(tempfile(), "key-A"))
})

test_that("clear_report_cache removes the entry", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    .report_cache_put(cache, "key-A", fake_bundle())
    expect_true(file.exists(.report_cache_file(cache)))

    clear_report_cache(cache)
    expect_false(file.exists(.report_cache_file(cache)))
    expect_null(.report_cache_get(cache, "key-A"))
  })
})

test_that("no temporary write artefacts are left behind", {
  with_files(function(dir, data_file) {
    cache <- file.path(dir, "cache")
    .report_cache_put(cache, "key-A", fake_bundle())
    expect_length(list.files(cache, pattern = "\\.tmp"), 0)
  })
})


# ---- targeted_for_mode ------------------------------------------------------

test_that("targeted rates are used only in site_expected mode", {
  bundle <- fake_bundle()
  expect_false(is.null(targeted_for_mode(bundle, "site_expected")))
  expect_null(targeted_for_mode(bundle, "national_observed"))
})


# ---- Version ----------------------------------------------------------------

test_that("the version label is defined in exactly one place", {
  expect_match(NSQIP_CUSUM_VERSION, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_equal(nsqip_version_label(),
               paste0("NSQIP CUSUM Monitoring System v", NSQIP_CUSUM_VERSION))

  # No output may carry its own hard-coded literal: the report footer and the
  # deck's closing frame each had one, and both sat at v1.4 through v1.5.0.
  sources <- c(file.path(.proj_root, "nsqip_cusum_report.qmd"),
               file.path(.proj_root, "R", "render_beamer_slides.R"))
  for (f in sources) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_false(grepl("Monitoring System v[0-9]", txt),
                 info = paste("hard-coded version string in", basename(f)))
  }
})
