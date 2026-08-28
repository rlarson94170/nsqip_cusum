# =============================================================================
# Procedure classification and division assignment
#
# process_case_details() is not covered here: it reads a Case Details xlsx and
# every such file contains PHI, so there is nothing to commit as a fixture.
# The pure functions downstream of it are covered instead.
# =============================================================================

test_that("NSQIP targeted flags outrank CPT classification", {
  # 44140 is a colectomy CPT, but the module flag is what the SAR cohort uses
  expect_equal(classify_procedure(44140, colectomy_flag = 1), "Colectomy")
  expect_equal(classify_procedure(44950, colectomy_flag = 1), "Colectomy")
  expect_equal(classify_procedure(44950, proctectomy_flag = 1), "Proctectomy")

  # Proctectomy wins only when colectomy is not also flagged
  expect_equal(
    classify_procedure(44950, colectomy_flag = 1, proctectomy_flag = 1),
    "Colectomy"
  )
})

test_that("CPT codes map to their categories", {
  expect_equal(classify_procedure(44950), "Appendectomy")
  expect_equal(classify_procedure(47562), "Cholecystectomy")
  expect_equal(classify_procedure(49505), "Inguinal Hernia Repair")
  expect_equal(classify_procedure(49591), "Ventral Hernia Repair")
})

test_that("unmapped and missing CPT codes fall through to Other", {
  expect_equal(classify_procedure(99999), "Other")
  expect_equal(classify_procedure(NA), "Other")
})

test_that("flags absent or NA do not derail classification", {
  expect_equal(classify_procedure(44950, colectomy_flag = 0), "Appendectomy")
  expect_equal(classify_procedure(44950, colectomy_flag = NA), "Appendectomy")
})

test_that("assign_procedure_categories classifies a whole frame", {
  df <- tibble::tibble(
    cpt_code         = c(44950, 47562, 99999, 12345),
    colectomy_flag   = c(0L, 0L, 1L, 0L),
    proctectomy_flag = c(0L, 0L, 0L, 1L)
  )
  out <- assign_procedure_categories(df)

  expect_equal(
    out$procedure_category,
    c("Appendectomy", "Cholecystectomy", "Colectomy", "Proctectomy")
  )
  expect_equal(nrow(out), nrow(df))
})


# ---- Division assignment ----------------------------------------------------

test_that("get_divisions applies the minimum-case threshold", {
  df <- tibble::tibble(
    specialty = "General Surgery",
    division  = c(rep("Colorectal", 12), rep("ACSS", 5), rep("Transplant", 1))
  )

  expect_equal(get_divisions(df, "General Surgery", min_cases = 10), "Colorectal")
  expect_setequal(
    get_divisions(df, "General Surgery", min_cases = 5),
    c("Colorectal", "ACSS")
  )
  expect_length(get_divisions(df, "General Surgery", min_cases = 100), 0)
})

test_that("get_divisions orders by descending volume and drops unmapped", {
  df <- tibble::tibble(
    specialty = "General Surgery",
    division  = c(rep("ACSS", 20), rep("Colorectal", 30), rep(NA, 40))
  )
  expect_equal(
    get_divisions(df, "General Surgery", min_cases = 10),
    c("Colorectal", "ACSS")
  )
})

test_that("assign_divisions leaves unmatched surgeons as NA", {
  data <- tibble::tibble(surgeon = c("A", "B", "C"))
  mapping <- tibble::tibble(
    surgeon = c("A", "B"), specialty = "General Surgery",
    division = c("Colorectal", "ACSS")
  )
  out <- suppressMessages(assign_divisions(data, mapping))

  expect_equal(out$division, c("Colorectal", "ACSS", NA))
  expect_equal(nrow(out), 3)   # no rows gained or lost
})

test_that("assign_divisions with no mapping yields an all-NA column", {
  data <- tibble::tibble(surgeon = c("A", "B"))
  out <- assign_divisions(data, NULL)

  expect_true("division" %in% names(out))
  expect_true(all(is.na(out$division)))
})


# ---- Complication label contract --------------------------------------------

test_that("complication labels agree with the benchmark vocabulary", {
  # build_case_p0() joins on these labels, so a drift between the two would
  # silently drop a complication's benchmark and leave p0 as NA.
  expect_setequal(unname(complication_labels), COMPLICATIONS)
})

test_that("targeted complication names map onto the same vocabulary", {
  mapped <- TARGETED_COMP_STD[!is.na(TARGETED_COMP_STD)]
  expect_true(all(mapped %in% COMPLICATIONS))
  # "Ventilator > 48 Hours" in the targeted sheets vs "> 48h" everywhere else
  expect_equal(unname(TARGETED_COMP_STD["Ventilator > 48 Hours"]),
               "Ventilator > 48h")
})


# ---- Locating the Case Details download -------------------------------------
#
# NSQIP appends the download date and site ID, so the configured path is a
# prefix. These fixtures are empty files: resolution is a filename question and
# never opens the workbook.

case_dir <- function(names) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  file.create(file.path(d, names))
  d
}

test_that("the only file in the folder is used, whatever it is called", {
  # The operating assumption is one download in data/ at a time, so this is
  # the property that actually matters: the suffix NSQIP appends is never
  # required to parse. Ranking only runs when there is a choice to make.
  odd <- c("Case_Details_Report-28-Aug-2026-1201.xlsx",  # as downloaded
           "Case_Details_Report-28-Aug-2026-9999.xlsx",  # not a valid HHMM
           "Case_Details_Report-28-Aug-2026-123456.xlsx",# not four digits
           "Case_Details_Report-28-Aug-2026-ABCD.xlsx",  # not digits
           "Case_Details_Report-2026-08-28-1201.xlsx",   # no month name
           "Case_Details_Report (1).xlsx",               # browser duplicate
           "Case_Details_Report.xlsx")                   # no suffix at all

  for (nm in odd) {
    d <- withr::local_tempdir()
    file.create(file.path(d, nm))
    expect_equal(basename(resolve_case_file(d, quiet = TRUE)), nm)
  }
})

test_that("the download date is read out of the filename", {
  expect_equal(case_file_date("Case_Details_Report-17-Aug-2026-1503.xlsx"),
               as.Date("2026-08-17"))
  expect_equal(case_file_date("data/Case_Details_Report-1-Jan-2025-99.xlsx"),
               as.Date("2025-01-01"))
})

test_that("a filename with no date yields NA rather than an error", {
  expect_true(is.na(case_file_date("Case_Details_Report.xlsx")))
  expect_true(is.na(case_file_date("Case_Details_Report-32-Xyz-2026-1.xlsx")))
})

test_that("dates are read the same way under a non-English locale", {
  # as.Date(format = "%b") goes through LC_TIME and would return NA here
  withr::local_locale(c(LC_TIME = "de_DE.UTF-8"))
  expect_equal(case_file_date("Case_Details_Report-17-Aug-2026-1503.xlsx"),
               as.Date("2026-08-17"))
})

test_that("the newest download wins", {
  d <- case_dir(c("Case_Details_Report-15-Nov-2025-1503.xlsx",
                  "Case_Details_Report-17-Aug-2026-1503.xlsx"))
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report-17-Aug-2026-1503.xlsx")
})

test_that("whatever follows the date is ignored", {
  # The trailing digits carry no meaning here; only the date is read, and it
  # must not be confused by digits sitting next to it.
  expect_equal(case_file_date("Case_Details_Report-27-Aug-2026-1513.xlsx"),
               as.Date("2026-08-27"))
  expect_equal(case_file_date("Case_Details_Report-27-Aug-2026-2027.xlsx"),
               as.Date("2026-08-27"))
  expect_equal(case_file_date("Case_Details_Report-27-Aug-2026-9999.xlsx"),
               as.Date("2026-08-27"))
  expect_equal(case_file_date("Case_Details_Report-27-Aug-2026-ABCD.xlsx"),
               as.Date("2026-08-27"))
})

test_that("same-date files fall through to write time", {
  d <- case_dir(c("Case_Details_Report-27-Aug-2026-1513.xlsx",
                  "Case_Details_Report-27-Aug-2026-0830.xlsx"))
  Sys.setFileTime(file.path(d, "Case_Details_Report-27-Aug-2026-0830.xlsx"),
                  Sys.time() - 3600)
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report-27-Aug-2026-1513.xlsx")
})

test_that("undated files fall back to write time among themselves", {
  d <- case_dir(c("Case_Details_Report.xlsx", "Case_Details_Report-old.xlsx"))
  Sys.setFileTime(file.path(d, "Case_Details_Report-old.xlsx"),
                  Sys.time() - 3600)
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report.xlsx")
})

test_that("an undated leftover never outranks a real download", {
  # The failure this guards: a stale Case_Details_Report.xlsx from an earlier
  # run silently winning over the file just downloaded.
  d <- case_dir(c("Case_Details_Report.xlsx",
                  "Case_Details_Report-17-Aug-2026-1503.xlsx"))
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report-17-Aug-2026-1503.xlsx")
})

test_that("a configured prefix resolves like a directory", {
  d <- case_dir(c("Case_Details_Report.xlsx",
                  "Case_Details_Report-17-Aug-2026-1503.xlsx"))
  expect_equal(
    basename(resolve_case_file(file.path(d, "Case_Details_Report.xlsx"),
                               quiet = TRUE)),
    "Case_Details_Report-17-Aug-2026-1503.xlsx"
  )
})

test_that("naming one download pins the run to it", {
  d <- case_dir(c("Case_Details_Report-15-Nov-2025-1503.xlsx",
                  "Case_Details_Report-17-Aug-2026-1503.xlsx"))
  pinned <- file.path(d, "Case_Details_Report-15-Nov-2025-1503.xlsx")
  expect_equal(basename(resolve_case_file(pinned, quiet = TRUE)),
               "Case_Details_Report-15-Nov-2025-1503.xlsx")
})

test_that("an open workbook's lock file is not mistaken for a download", {
  d <- case_dir(c("Case_Details_Report-17-Aug-2026-1503.xlsx",
                  "~$Case_Details_Report-17-Aug-2026-1503.xlsx"))
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report-17-Aug-2026-1503.xlsx")
})

test_that("only the undated file present is still usable", {
  d <- case_dir("Case_Details_Report.xlsx")
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report.xlsx")
})

test_that("unrelated workbooks in the folder are ignored", {
  d <- case_dir(c("SAR_Site_Summary.xlsx",
                  "Case_Details_Report-17-Aug-2026-1503.xlsx"))
  expect_equal(basename(resolve_case_file(d, quiet = TRUE)),
               "Case_Details_Report-17-Aug-2026-1503.xlsx")
})

test_that("an empty folder fails with the folder named", {
  d <- case_dir("SAR_Site_Summary.xlsx")
  expect_error(resolve_case_file(d, quiet = TRUE), "No Case Details file")
})

test_that("a missing folder fails distinctly from an empty one", {
  expect_error(resolve_case_file("no/such/folder/Case_Details_Report.xlsx"),
               "folder not found")
})
