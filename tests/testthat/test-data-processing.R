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
