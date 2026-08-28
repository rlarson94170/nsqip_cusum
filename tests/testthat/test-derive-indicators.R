# =============================================================================
# Case-level indicator derivation: PATOS exclusions and SAR composites
#
# These are the definitions the whole report rests on — a wrong composite
# silently changes every benchmark comparison, every CUSUM, and every triage
# tier. They became testable once derive_case_indicators() was separated from
# the Excel read, since no real Case Details file can be committed as a
# fixture.
# =============================================================================

# ---- Filtering and parsing --------------------------------------------------

test_that("only completed cases in the target specialties are retained", {
  raw <- raw_cases(
    4,
    `Completion Status`  = c("Complete", "In Progress", "Complete", "Complete"),
    `Surgical Specialty` = c("General Surgery", "General Surgery",
                             "Vascular", "Urology")
  )
  out <- derive_case_indicators(raw, specialties = c("General Surgery", "Vascular"),
                                quiet = TRUE)

  expect_equal(nrow(out), 2)
  expect_setequal(out$specialty, c("General Surgery", "Vascular"))
})

test_that("cases come back in chronological order", {
  raw <- raw_cases(3, `Operation Date` = c("03/15/2026", "01/05/2026", "02/20/2026"))
  out <- derive_case_indicators(raw, quiet = TRUE)

  expect_equal(out$op_date, as.Date(c("2026-01-05", "2026-02-20", "2026-03-15")))
  expect_false(is.unsorted(out$op_date))
})

test_that("an empty result is returned without error", {
  out <- derive_case_indicators(raw_cases(2), specialties = "Neurosurgery",
                                quiet = TRUE)
  expect_equal(nrow(out), 0)
})


# ---- The column contract ----------------------------------------------------

test_that("a renamed column is named in the error, not buried", {
  raw <- raw_cases(1)
  names(raw)[names(raw) == "# of Postop Pneumonia"] <- "# of Postoperative Pneumonia"

  expect_error(
    derive_case_indicators(raw, quiet = TRUE),
    "# of Postop Pneumonia"
  )
  expect_error(derive_case_indicators(raw, quiet = TRUE), "missing 1 required")
})

test_that("several missing columns are all reported at once", {
  raw <- raw_cases(1)
  raw[["# of Postop UTI"]] <- NULL
  raw[["# of Postop Sepsis"]] <- NULL

  expect_error(derive_case_indicators(raw, quiet = TRUE), "missing 2 required")
})


# ---- PATOS exclusions -------------------------------------------------------

test_that("a complication present at time of surgery is not counted", {
  # Two cases with pneumonia; the first was PATOS
  out <- derive_raw(2,
    `# of Postop Pneumonia`       = c(1, 1),
    `# of Postop Pneumonia PATOS` = c(1, 0)
  )
  expect_equal(out$pneumonia, c(0L, 1L))
})

test_that("PATOS is applied to every complication the SAR excludes", {
  cases <- list(
    pneumonia = c("# of Postop Pneumonia", "# of Postop Pneumonia PATOS"),
    vent48    = c("# of Postop On Ventilator > 48 hours",
                  "# of Postop On Ventilator > 48 hours PATOS"),
    uti       = c("# of Postop UTI", "# of Postop UTI PATOS"),
    sepsis    = c("# of Postop Sepsis", "# of Postop Sepsis PATOS")
  )
  for (var in names(cases)) {
    cols <- cases[[var]]
    args <- list(2); args[[cols[1]]] <- c(1, 1); args[[cols[2]]] <- c(1, 0)
    out <- do.call(derive_raw, args)
    expect_equal(out[[var]], c(0L, 1L), info = var)
  }
})

test_that("each SSI component is PATOS-adjusted independently", {
  # Superficial is PATOS, deep is not — the composite must still fire
  out <- derive_raw(1,
    `# of Postop Superficial Incisional SSI`       = 1,
    `# of Postop Superficial Incisional SSI PATOS` = 1,
    `# of Postop Deep Incisional SSI`             = 1,
    `# of Postop Deep Incisional SSI PATOS`       = 0
  )
  expect_equal(out$ssi, 1L)
  expect_equal(out$d_ssi_superficial, 0L)
  expect_equal(out$d_ssi_deep, 1L)
})

test_that("an all-PATOS SSI does not count as an SSI", {
  out <- derive_raw(1,
    `# of Postop Superficial Incisional SSI`        = 1,
    `# of Postop Superficial Incisional SSI PATOS`  = 1,
    `# of Postop Organ/Space SSI`                   = 1,
    `# of Postop Organ/Space SSI PATOS`             = 1
  )
  expect_equal(out$ssi, 0L)
})

test_that("PATOS does not suppress complications the SAR does not exclude", {
  # Renal insufficiency and MI have no PATOS counterpart in the SAR definition
  out <- derive_raw(1,
    `# of Postop Renal Insufficiency` = 1,
    `# of Myocardial Infarction`      = 1
  )
  expect_equal(out$renal_failure, 1L)
  expect_equal(out$cardiac, 1L)
})


# ---- Composite definitions --------------------------------------------------

test_that("SSI is the union of its three components", {
  for (col in c("# of Postop Superficial Incisional SSI",
                "# of Postop Deep Incisional SSI",
                "# of Postop Organ/Space SSI")) {
    args <- list(1); args[[col]] <- 1
    expect_equal(do.call(derive_raw, args)$ssi, 1L, info = col)
  }
  expect_equal(derive_raw(1)$ssi, 0L)
})

test_that("Cardiac is arrest or MI", {
  expect_equal(derive_raw(1, `# of Cardiac Arrest Requiring CPR` = 1)$cardiac, 1L)
  expect_equal(derive_raw(1, `# of Myocardial Infarction` = 1)$cardiac, 1L)
  expect_equal(derive_raw(1)$cardiac, 0L)
})

test_that("VTE is PE or venous thrombosis", {
  expect_equal(derive_raw(1, `# of Postop Pulmonary Embolism` = 1)$vte, 1L)
  expect_equal(
    derive_raw(1, `# of Postop Venous Thrombosis Requiring Therapy` = 1)$vte, 1L)
  expect_equal(derive_raw(1)$vte, 0L)
})

test_that("Renal Failure is insufficiency or dialysis", {
  expect_equal(derive_raw(1, `# of Postop Renal Insufficiency` = 1)$renal_failure, 1L)
  expect_equal(derive_raw(1, `# of Postop Dialysis` = 1)$renal_failure, 1L)
  expect_equal(derive_raw(1)$renal_failure, 0L)
})

test_that("Morbidity fires on each of its twelve components", {
  components <- c(
    "# of Postop Superficial Incisional SSI", "# of Postop Wound Disruption",
    "# of Postop Pneumonia", "# of Postop Unplanned Intubation",
    "# of Postop On Ventilator > 48 hours", "# of Postop Dialysis",
    "# of Postop Renal Insufficiency", "# of Postop UTI",
    "# of Stroke/Cerebral Vascular Accident (CVA)",
    "# of Cardiac Arrest Requiring CPR", "# of Myocardial Infarction",
    "# of Postop Sepsis"
  )
  for (col in components) {
    args <- list(1); args[[col]] <- 1
    expect_equal(do.call(derive_raw, args)$morbidity, 1L, info = col)
  }
})

test_that("Morbidity is not triggered by non-components", {
  # Readmission, reoperation, C.diff, VTE and mortality sit outside the
  # SAR morbidity composite
  for (col in c("# of Unplanned Readmissions", "Total # of Unplanned Returns to OR",
                "# of Postop C. diff", "# of Postop Pulmonary Embolism")) {
    args <- list(1); args[[col]] <- 1
    expect_equal(do.call(derive_raw, args)$morbidity, 0L, info = col)
  }
  expect_equal(
    derive_raw(1, `Postop Death w/in 30 days of Procedure` = "Yes")$morbidity, 0L)
})

test_that("a PATOS-only complication does not reach Morbidity", {
  out <- derive_raw(1,
    `# of Postop Pneumonia` = 1, `# of Postop Pneumonia PATOS` = 1)
  expect_equal(out$pneumonia, 0L)
  expect_equal(out$morbidity, 0L)
})

test_that("Mortality reads the 30-day death flag", {
  out <- derive_raw(2, `Postop Death w/in 30 days of Procedure` = c("Yes", "No"))
  expect_equal(out$mortality, c(1L, 0L))
})

test_that("reoperation and readmission count any occurrence", {
  out <- derive_raw(3, `Total # of Unplanned Returns to OR` = c(0, 1, 2))
  expect_equal(out$unplanned_reop, c(0L, 1L, 1L))

  out <- derive_raw(3, `# of Unplanned Readmissions` = c(0, 1, 3))
  expect_equal(out$unplanned_readmit, c(0L, 1L, 1L))
})

test_that("readmission relatedness is captured separately", {
  out <- derive_raw(3,
    `# of Unplanned Readmissions` = c(1, 1, 1),
    `# of Readmissions likely related to Primary Procedure`   = c(1, 0, 1),
    `# of Readmissions likely unrelated to Primary Procedure` = c(0, 1, 1)
  )
  expect_equal(out$readmit_related,   c(1L, 0L, 1L))
  expect_equal(out$readmit_unrelated, c(0L, 1L, 1L))
})


# ---- Targeted module handling -----------------------------------------------

test_that("targeted module flags are set from their marker columns", {
  out <- derive_raw(2,
    `Colectomy Primary Indication for Surgery`   = c("Colon cancer", NA),
    `Proctectomy Preop Patient Marked for Stoma` = c(NA, "Yes")
  )
  expect_equal(out$colectomy_flag, c(1L, 0L))
  expect_equal(out$proctectomy_flag, c(0L, 1L))
})

test_that("anastomotic leak reads either targeted module", {
  out <- derive_raw(3, `Colectomy Postop Anastomotic Leak` =
                      c("Leak, treated", "No leak", NA))
  expect_equal(out$anastomotic_leak, c(1L, 0L, 0L))

  out <- derive_raw(1, `Proctectomy Postop Anastomotic Leak` = "Leak, treated")
  expect_equal(out$anastomotic_leak, 1L)
})

test_that("prolonged NPO reads either targeted module", {
  out <- derive_raw(3,
    `Colectomy Prolonged Postoperative NPO or NGT Use` = c("Yes", "No", NA))
  expect_equal(out$prolonged_npo, c(1L, 0L, 0L))
})


# ---- Missing data -----------------------------------------------------------

test_that("missing occurrence counts become zero, not NA", {
  out <- derive_raw(1,
    `# of Postop Pneumonia` = NA, `# of Postop UTI` = NA,
    `Total # of Unplanned Returns to OR` = NA
  )
  comp_cols <- intersect(names(complication_labels), names(out))
  expect_false(any(is.na(out[comp_cols])))
  expect_equal(out$pneumonia, 0L)
  expect_equal(out$unplanned_reop, 0L)
})

test_that("an event with a missing PATOS value is not silently dropped", {
  # PATOS is blank rather than 0 in some exports; the occurrence still counts
  out <- derive_raw(1,
    `# of Postop Pneumonia` = 1, `# of Postop Pneumonia PATOS` = NA)
  expect_equal(out$pneumonia, 1L)
})

test_that("identifiers survive derivation intact", {
  out <- derive_raw(2, LMRN = c("00123456", "20655620"))
  expect_equal(out$lmrn, c("00123456", "20655620"))
})


# ---- Contract between the column list and the PATOS rules -------------------

test_that("every PATOS-excluded complication has both columns declared", {
  # PATOS_PAIRS documents which complications the SAR excludes when present at
  # surgery. Adding one without declaring its paired column would fail the
  # required-column check at run time on real data only.
  for (col in PATOS_PAIRS) {
    expect_true(col %in% REQUIRED_CASE_COLUMNS, info = col)
    expect_true(paste(col, "PATOS") %in% REQUIRED_CASE_COLUMNS,
                info = paste(col, "PATOS"))
  }
})

test_that("PATOS_PAIRS matches the exclusions the derivation actually applies", {
  # Each listed occurrence must be suppressed when its PATOS flag is set
  var_for <- c(
    "# of Postop Superficial Incisional SSI"      = "d_ssi_superficial",
    "# of Postop Deep Incisional SSI"             = "d_ssi_deep",
    "# of Postop Organ/Space SSI"                 = "d_ssi_organ",
    "# of Postop Pneumonia"                       = "pneumonia",
    "# of Postop On Ventilator > 48 hours"        = "vent48",
    "# of Postop UTI"                             = "uti",
    "# of Postop Sepsis"                          = "sepsis",
    "# of Postop Septic Shock"                    = "d_septic_shock"
  )
  expect_setequal(names(var_for), PATOS_PAIRS)

  for (col in PATOS_PAIRS) {
    args <- list(1); args[[col]] <- 1; args[[paste(col, "PATOS")]] <- 1
    expect_equal(do.call(derive_raw, args)[[var_for[[col]]]], 0L, info = col)
  }
})

test_that("no required column is declared twice", {
  expect_equal(anyDuplicated(REQUIRED_CASE_COLUMNS), 0)
})
