# =============================================================================
# NSQIP Case Details Data Processing
#
# Reads the Case Details Report xlsx and derives binary complication indicators.
# Handles PATOS exclusions per SAR methodology.
# =============================================================================

library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)

# Every column the derivation reads. NSQIP renames columns between releases,
# and without this check a rename surfaces as an opaque failure deep inside the
# transmute below rather than as "this column is missing".
REQUIRED_CASE_COLUMNS <- c(
  # Filtering and identifiers
  "Completion Status", "Surgical Specialty", "Operation Date",
  "Case Number", "LMRN", "Attending/Staff Surgeon",
  "CPT Code", "CPT Description", "Age at Time of Surgery",
  "ASA Classification", "Hospital Length of Stay",
  # Readmission detail
  "# of Readmissions likely related to Primary Procedure",
  "# of Readmissions likely unrelated to Primary Procedure",
  "# of Unplanned Readmissions", "Total # of Unplanned Returns to OR",
  # Targeted module flags and their procedure-specific outcomes
  "Colectomy Primary Indication for Surgery",
  "Proctectomy Preop Patient Marked for Stoma",
  "Colectomy Postop Anastomotic Leak", "Proctectomy Postop Anastomotic Leak",
  "Colectomy Prolonged Postoperative NPO or NGT Use",
  "Proctectomy Prolonged Postoperative NPO or NGT Use",
  # Mortality
  "Postop Death w/in 30 days of Procedure",
  # Occurrences, each paired with its PATOS counterpart where one exists
  "# of Postop Superficial Incisional SSI",
  "# of Postop Superficial Incisional SSI PATOS",
  "# of Postop Deep Incisional SSI", "# of Postop Deep Incisional SSI PATOS",
  "# of Postop Organ/Space SSI", "# of Postop Organ/Space SSI PATOS",
  "# of Postop Pneumonia", "# of Postop Pneumonia PATOS",
  "# of Postop On Ventilator > 48 hours",
  "# of Postop On Ventilator > 48 hours PATOS",
  "# of Postop UTI", "# of Postop UTI PATOS",
  "# of Postop Sepsis", "# of Postop Sepsis PATOS",
  "# of Postop Septic Shock", "# of Postop Septic Shock PATOS",
  "# of Postop Unplanned Intubation", "# of Postop Renal Insufficiency",
  "# of Postop Dialysis", "# of Cardiac Arrest Requiring CPR",
  "# of Myocardial Infarction", "# of Postop Pulmonary Embolism",
  "# of Postop Venous Thrombosis Requiring Therapy",
  "# of Postop Wound Disruption",
  "# of Stroke/Cerebral Vascular Accident (CVA)", "# of Postop C. diff"
)

# Complications excluded when Present At Time Of Surgery, per SAR methodology
PATOS_PAIRS <- c(
  "# of Postop Superficial Incisional SSI",
  "# of Postop Deep Incisional SSI",
  "# of Postop Organ/Space SSI",
  "# of Postop Pneumonia",
  "# of Postop On Ventilator > 48 hours",
  "# of Postop UTI",
  "# of Postop Sepsis",
  "# of Postop Septic Shock"
)


# ---- Locating the Case Details download -------------------------------------
#
# NSQIP appends the download date and a further four digits to the filename,
# so a download arrives as "Case_Details_Report-28-Aug-2026-1201.xlsx". The
# file is kept under that name rather than renamed: the date records when the
# data was pulled, and renaming to a fixed filename throws it away and has to
# be redone every quarter. So the configured path is treated as a prefix —
# anything matching "Case_Details_Report*.xlsx" is ingested, whatever the
# suffix turns out to be.

CASE_FILE_PREFIX <- "Case_Details_Report"

# Locale-independent month lookup. as.Date(format = "%b") reads %b through
# LC_TIME, so it silently returns NA under a non-English locale — the dates in
# these filenames are always English abbreviations regardless of the machine.
.MONTH_ABB <- setNames(1:12, c("jan", "feb", "mar", "apr", "may", "jun",
                               "jul", "aug", "sep", "oct", "nov", "dec"))


#' Download date encoded in a Case Details filename
#'
#' Only the date is read. NSQIP also appends four more digits, but nothing
#' depends on what they are — the expected case is a single download in
#' `data/`, and a lone file is used whatever it is called.
#'
#' @param path One or more file paths
#' @return A Date vector, NA where the filename carries no parseable date
case_file_date <- function(path) {
  base <- basename(path)
  pat  <- "[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}"

  out   <- rep(NA_real_, length(path))
  found <- regexpr(pat, base) > 0
  if (!any(found)) return(as.Date(out, origin = "1970-01-01"))

  parts <- strsplit(regmatches(base, regexpr(pat, base)), "-", fixed = TRUE)
  vals <- vapply(parts, function(p) {
    mo <- .MONTH_ABB[tolower(p[2])]
    if (is.na(mo)) return(NA_real_)
    d <- suppressWarnings(as.Date(sprintf("%s-%02d-%02d", p[3], mo,
                                          as.integer(p[1]))))
    if (is.na(d)) NA_real_ else as.numeric(d)
  }, numeric(1))

  out[found] <- vals
  as.Date(out, origin = "1970-01-01")
}


#' Resolve the configured path to an actual Case Details download
#'
#' The configured path is a prefix, not an exact name: `data/` or
#' `data/Case_Details_Report.xlsx` both select the newest
#' `Case_Details_Report*.xlsx` sitting beside it. Naming a specific download
#' still works, because a full filename is a prefix that matches only itself.
#'
#' One download in `data/` at a time is the expected case, and a lone file is
#' returned whatever it is named — nothing in the suffix has to parse. The
#' ranking below only decides between files when more than one is present.
#'
#' Ranking is by the date in the filename, newest first, with modification
#' time breaking ties. A file with no date ranks below every dated one — a
#' bare `Case_Details_Report.xlsx` left over from an earlier run must not
#' outrank a fresh download, since silently using stale data is the failure
#' this is meant to prevent.
#'
#' @param path Directory, prefix, or exact file path
#' @param quiet Suppress the message naming the file chosen
#' @return The resolved path to a single .xlsx file
resolve_case_file <- function(path, quiet = FALSE) {

  if (is.null(path) || length(path) != 1 || is.na(path) || !nzchar(path)) {
    stop("No Case Details path configured.")
  }

  if (dir.exists(path)) {
    dir <- path
    stem <- CASE_FILE_PREFIX
  } else {
    dir  <- dirname(path)
    stem <- sub("\\.xlsx$", "", basename(path), ignore.case = TRUE)
  }

  if (!dir.exists(dir)) {
    stop("Case Details folder not found: ", dir)
  }

  hits <- list.files(dir, pattern = paste0("^", stem, ".*\\.xlsx$"),
                     ignore.case = TRUE, full.names = TRUE)

  # Excel writes a "~$name.xlsx" lock file while the workbook is open. It
  # matches any sensible glob and is not a readable workbook.
  hits <- hits[!startsWith(basename(hits), "~$")]

  if (length(hits) == 0) {
    stop("No Case Details file found in ", dir, "/ matching \"", stem,
         "*.xlsx\".\n",
         "Place the download from NSQIP in that folder; the name it arrives ",
         "with is fine.")
  }

  dates  <- case_file_date(hits)
  mtimes <- file.info(hits)$mtime
  # na.last keeps undated files below dated ones even under decreasing order;
  # mtime is the second key, so same-date files resolve to the newer one.
  chosen <- hits[order(dates, mtimes, decreasing = TRUE, na.last = TRUE)][1]

  if (!quiet && length(hits) > 1) {
    message("  ", length(hits), " Case Details files present; using the ",
            "newest: ", basename(chosen))
  }

  chosen
}


#' Read the raw Case Details Report
#'
#' Kept separate from the derivation so the derivation can be exercised
#' without an xlsx — every real Case Details file contains PHI, so none can
#' serve as a test fixture.
#'
#' @param filepath Path to the Case Details Report .xlsx file
#' @return The raw `report_data` sheet as a tibble
read_case_details <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("Case Details file not found: ", filepath)
  }
  read_excel(filepath, sheet = "report_data")
}


#' Derive case-level indicators from a raw Case Details frame
#'
#' Filters to completed cases in the target specialties, parses the operation
#' date, and derives every binary complication indicator, applying the PATOS
#' exclusions and the SAR composite definitions.
#'
#' @param raw Raw Case Details data, as returned by read_case_details()
#' @param specialties Character vector of specialties to include
#' @param quiet Suppress the progress messages
#' @return A tibble with one row per retained case
derive_case_indicators <- function(
    raw,
    specialties = c("General Surgery", "Vascular", "Thoracic", "Plastics"),
    quiet = FALSE
) {

  say <- function(...) if (!quiet) message(...)

  missing_cols <- setdiff(REQUIRED_CASE_COLUMNS, names(raw))
  if (length(missing_cols) > 0) {
    stop(
      "Case Details data is missing ", length(missing_cols), " required ",
      "column(s):\n  ", paste(missing_cols, collapse = "\n  "),
      "\n\nNSQIP renames columns between releases. Compare the headings in ",
      "the\n'report_data' sheet against REQUIRED_CASE_COLUMNS in ",
      "R/data_processing.R."
    )
  }

  # --- Filter to target specialties and completed cases ---
  df <- raw |>
    filter(
      `Completion Status` == "Complete",
      `Surgical Specialty` %in% specialties
    )
  say("  Cases after filtering (complete, target specialties): ", nrow(df))

  # --- Parse operation date ---
  df <- df |>
    mutate(
      op_date = mdy(`Operation Date`)
    ) |>
    arrange(op_date)

  # --- Helper: safe numeric conversion (handles NA, character "0"/"1") ---
  safe_binary <- function(x) {
    as.integer(as.numeric(x) > 0)
  }

  # --- Helper: apply a PATOS exclusion to an occurrence ---
  # A blank PATOS field means "not present at time of surgery". Comparing it
  # with == 0 yields NA, which the trailing replace_na() then turned into 0 --
  # silently discarding a real occurrence. Treat missing as not-PATOS instead.
  not_patos <- function(event, patos) {
    as.integer(
      ifelse(is.na(event), 0L, event) == 1L &
      ifelse(is.na(patos), 0L, patos) == 0L
    )
  }

  # --- Helper: preserve an MRN exactly as recorded ---
  # as.integer() dropped leading zeros, so "00123456" was written to the case
  # list as "123456" and would not be found in the chart system.
  as_mrn <- function(x) {
    if (is.character(x)) return(trimws(x))
    ifelse(is.na(x), NA_character_,
           trimws(format(x, scientific = FALSE, trim = TRUE)))
  }

  # --- Derive binary complication indicators ---
  processed <- df |>
    transmute(
      # Identifiers
      case_id    = `Case Number`,
      lmrn       = as_mrn(`LMRN`),
      op_date    = op_date,
      specialty  = `Surgical Specialty`,
      surgeon    = `Attending/Staff Surgeon`,
      cpt_code   = `CPT Code`,
      cpt_desc   = `CPT Description`,
      age        = `Age at Time of Surgery`,
      asa_class  = `ASA Classification`,
      los        = as.numeric(`Hospital Length of Stay`),
      readmit_related   = as.integer(as.numeric(`# of Readmissions likely related to Primary Procedure`) > 0),
      readmit_unrelated = as.integer(as.numeric(`# of Readmissions likely unrelated to Primary Procedure`) > 0),
      
      # NSQIP Targeted procedure module flags (used for procedure classification)
      colectomy_flag   = as.integer(!is.na(`Colectomy Primary Indication for Surgery`)),
      proctectomy_flag = as.integer(!is.na(`Proctectomy Preop Patient Marked for Stoma`)),
      
      # Targeted procedure-specific complications
      anastomotic_leak = as.integer(
        grepl("^Leak", `Colectomy Postop Anastomotic Leak`, ignore.case = TRUE) |
        grepl("^Leak", `Proctectomy Postop Anastomotic Leak`, ignore.case = TRUE)
      ),
      prolonged_npo = as.integer(
        `Colectomy Prolonged Postoperative NPO or NGT Use` == "Yes" |
        `Proctectomy Prolonged Postoperative NPO or NGT Use` == "Yes"
      ),
      
      # Granular sub-complication indicators for dashboard
      # (these are pre-PATOS; PATOS adjustment applied below)
      ssi_superficial_raw = safe_binary(`# of Postop Superficial Incisional SSI`),
      ssi_superficial_pat = safe_binary(`# of Postop Superficial Incisional SSI PATOS`),
      ssi_deep_raw        = safe_binary(`# of Postop Deep Incisional SSI`),
      ssi_deep_pat        = safe_binary(`# of Postop Deep Incisional SSI PATOS`),
      ssi_organ_raw       = safe_binary(`# of Postop Organ/Space SSI`),
      ssi_organ_pat       = safe_binary(`# of Postop Organ/Space SSI PATOS`),
      septic_shock_raw    = safe_binary(`# of Postop Septic Shock`),
      septic_shock_pat    = safe_binary(`# of Postop Septic Shock PATOS`),
      renal_insuff        = safe_binary(`# of Postop Renal Insufficiency`),
      postop_dialysis_ind = safe_binary(`# of Postop Dialysis`),
      cardiac_arrest      = safe_binary(`# of Cardiac Arrest Requiring CPR`),
      mi                  = safe_binary(`# of Myocardial Infarction`),
      dvt                 = safe_binary(`# of Postop Venous Thrombosis Requiring Therapy`),
      pe                  = safe_binary(`# of Postop Pulmonary Embolism`),
      wound_disruption    = safe_binary(`# of Postop Wound Disruption`),
      stroke_cva_ind      = safe_binary(`# of Stroke/Cerebral Vascular Accident (CVA)`),
      
      # PATOS-adjusted dashboard sub-indicators
      d_ssi_superficial = not_patos(ssi_superficial_raw, ssi_superficial_pat),
      d_ssi_deep        = not_patos(ssi_deep_raw, ssi_deep_pat),
      d_ssi_organ       = not_patos(ssi_organ_raw, ssi_organ_pat),
      d_septic_shock    = not_patos(septic_shock_raw, septic_shock_pat),
      
      # ---- Individual complication indicators ----
      
      # 1. Mortality: 30-day death
      mortality = as.integer(`Postop Death w/in 30 days of Procedure` == "Yes"),
      
      # 2. Cardiac: Cardiac arrest + MI (SAR definition 3)
      cardiac = as.integer(
        safe_binary(`# of Cardiac Arrest Requiring CPR`) |
        safe_binary(`# of Myocardial Infarction`)
      ),
      
      # 3. Pneumonia (exclude PATOS)
      pneumonia_raw  = safe_binary(`# of Postop Pneumonia`),
      pneumonia_patos = safe_binary(`# of Postop Pneumonia PATOS`),
      pneumonia = not_patos(pneumonia_raw, pneumonia_patos),
      
      # 4. Unplanned Intubation
      unplanned_intubation = safe_binary(`# of Postop Unplanned Intubation`),
      
      # 5. Ventilator > 48 Hours (exclude PATOS)
      vent48_raw   = safe_binary(`# of Postop On Ventilator > 48 hours`),
      vent48_patos = safe_binary(`# of Postop On Ventilator > 48 hours PATOS`),
      vent48 = not_patos(vent48_raw, vent48_patos),
      
      # 6. VTE: Pulmonary embolism + Venous thrombosis (SAR definition 4)
      vte = as.integer(
        safe_binary(`# of Postop Pulmonary Embolism`) |
        safe_binary(`# of Postop Venous Thrombosis Requiring Therapy`)
      ),
      
      # 7. Renal Failure: Renal insufficiency + Dialysis (SAR definition 5)
      renal_failure = as.integer(
        safe_binary(`# of Postop Renal Insufficiency`) |
        safe_binary(`# of Postop Dialysis`)
      ),
      
      # 8. UTI (exclude PATOS)
      uti_raw   = safe_binary(`# of Postop UTI`),
      uti_patos = safe_binary(`# of Postop UTI PATOS`),
      uti = not_patos(uti_raw, uti_patos),
      
      # 9. SSI: Superficial + Deep + Organ/Space (SAR definition 6)
      #    Exclude PATOS for organ/space (and superficial if PATOS exists)
      ssi_superficial     = safe_binary(`# of Postop Superficial Incisional SSI`),
      ssi_superficial_pat = safe_binary(`# of Postop Superficial Incisional SSI PATOS`),
      ssi_deep            = safe_binary(`# of Postop Deep Incisional SSI`),
      ssi_deep_patos      = safe_binary(`# of Postop Deep Incisional SSI PATOS`),
      ssi_organ           = safe_binary(`# of Postop Organ/Space SSI`),
      ssi_organ_patos     = safe_binary(`# of Postop Organ/Space SSI PATOS`),
      ssi = as.integer(
        not_patos(ssi_superficial, ssi_superficial_pat) == 1L |
        not_patos(ssi_deep, ssi_deep_patos) == 1L |
        not_patos(ssi_organ, ssi_organ_patos) == 1L
      ),
      
      # 10. Sepsis: Worsening sepsis/septic shock (SAR definition 7)
      #     Exclude PATOS
      sepsis_raw   = safe_binary(`# of Postop Sepsis`),
      sepsis_patos = safe_binary(`# of Postop Sepsis PATOS`),
      sepsis = not_patos(sepsis_raw, sepsis_patos),
      
      # 11. C.diff Colitis
      cdiff = safe_binary(`# of Postop C. diff`),
      
      # 12. Unplanned Reoperation (any unplanned return to OR)
      unplanned_reop = as.integer(
        !is.na(`Total # of Unplanned Returns to OR`) &
        as.numeric(`Total # of Unplanned Returns to OR`) > 0
      ),
      
      # 13. Unplanned Readmission
      unplanned_readmit = as.integer(
        !is.na(`# of Unplanned Readmissions`) &
        as.numeric(`# of Unplanned Readmissions`) > 0
      ),
      
      # 14. Morbidity (composite per SAR definition 1):
      #     Any of: SSI, wound disruption, pneumonia, unplanned intubation,
      #     vent>48h, dialysis, renal insufficiency, UTI, stroke/CVA,
      #     cardiac arrest, MI, systemic sepsis
      #     (Using PATOS-adjusted individual indicators where applicable)
      wound_disruption = safe_binary(`# of Postop Wound Disruption`),
      stroke_cva       = safe_binary(`# of Stroke/Cerebral Vascular Accident (CVA)`),
      postop_dialysis  = safe_binary(`# of Postop Dialysis`),
      
      morbidity = as.integer(
        ssi == 1 | wound_disruption == 1 | pneumonia == 1 |
        unplanned_intubation == 1 | vent48 == 1 |
        postop_dialysis == 1 |
        (safe_binary(`# of Postop Renal Insufficiency`) == 1) |
        uti == 1 | stroke_cva == 1 |
        (safe_binary(`# of Cardiac Arrest Requiring CPR`) == 1) |
        (safe_binary(`# of Myocardial Infarction`) == 1) |
        sepsis == 1
      )
    ) |>
    # Drop intermediate working columns
    select(
      case_id, lmrn, op_date, specialty, surgeon, cpt_code, cpt_desc,
      age, asa_class, los, readmit_related, readmit_unrelated,
      colectomy_flag, proctectomy_flag, anastomotic_leak, prolonged_npo,
      # Dashboard sub-indicators (PATOS-adjusted)
      d_ssi_superficial, d_ssi_deep, d_ssi_organ,
      d_septic_shock, renal_insuff, postop_dialysis_ind,
      cardiac_arrest, mi, dvt, pe, wound_disruption, stroke_cva_ind,
      # Standard CUSUM indicators
      mortality, morbidity, cardiac, pneumonia, unplanned_intubation,
      vent48, vte, renal_failure, uti, ssi, sepsis, cdiff,
      unplanned_reop, unplanned_readmit
    )
  
  # Replace any remaining NAs in complication columns with 0
  complication_cols <- c(
    "mortality", "morbidity", "cardiac", "pneumonia", "unplanned_intubation",
    "vent48", "vte", "renal_failure", "uti", "ssi", "sepsis", "cdiff",
    "unplanned_reop", "unplanned_readmit",
    "readmit_related", "readmit_unrelated",
    "anastomotic_leak", "prolonged_npo",
    "d_ssi_superficial", "d_ssi_deep", "d_ssi_organ",
    "d_septic_shock", "renal_insuff", "postop_dialysis_ind",
    "cardiac_arrest", "mi", "dvt", "pe", "wound_disruption", "stroke_cva_ind"
  )
  processed <- processed |>
    mutate(across(all_of(complication_cols), ~replace_na(., 0L)))

  if (nrow(processed) > 0) {
    say("  Date range: ", min(processed$op_date, na.rm = TRUE),
        " to ", max(processed$op_date, na.rm = TRUE))
    say("  Cases by specialty:")
    for (s in specialties) {
      say("    ", s, ": ", sum(processed$specialty == s))
    }
  }

  processed
}


#' Read and process the NSQIP Case Details Report
#'
#' Thin orchestration over read_case_details() and derive_case_indicators().
#'
#' @param filepath Path to the Case Details Report .xlsx file
#' @param specialties Character vector of specialties to include
#' @return A tibble with one row per case, columns for identifiers, dates,
#'         specialty, and binary (0/1) complication indicators
process_case_details <- function(
    filepath,
    specialties = c("General Surgery", "Vascular", "Thoracic", "Plastics")
) {
  message("Reading Case Details Report: ", filepath)
  raw <- read_case_details(filepath)
  message("  Total cases in file: ", nrow(raw))

  derive_case_indicators(raw, specialties = specialties)
}

#' Map internal complication variable names to display labels
complication_labels <- c(
  mortality            = "Mortality",
  morbidity            = "Morbidity",
  cardiac              = "Cardiac",
  pneumonia            = "Pneumonia",
  unplanned_intubation = "Unplanned Intubation",
  vent48               = "Ventilator > 48h",
  vte                  = "VTE",
  renal_failure        = "Renal Failure",
  uti                  = "UTI",
  ssi                  = "SSI",
  sepsis               = "Sepsis",
  cdiff                = "C.diff Colitis",
  unplanned_reop       = "Unplanned Reoperation",
  unplanned_readmit    = "Unplanned Readmission"
)


#' Load surgeon-to-division mapping
#'
#' @param filepath Path to surgeon_division_mapping.csv
#' @return A tibble with surgeon, specialty, division columns
load_surgeon_mapping <- function(filepath) {
  if (!file.exists(filepath)) {
    message("  Surgeon mapping file not found: ", filepath)
    return(NULL)
  }
  mapping <- read.csv(filepath, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  
  # Normalize column names: strip BOM artifacts, replace dots/slashes
  cnames <- names(mapping)
  cnames <- gsub("^X\\.+", "", cnames)           # Remove leading X... from BOM
  cnames <- gsub("\\.", " ", cnames)              # Dots to spaces
  cnames <- trimws(cnames)
  names(mapping) <- cnames
  
  # Find the surgeon column (contains "Attending" or "Surgeon")
  surgeon_col <- grep("Attending|Surgeon", cnames, value = TRUE, ignore.case = TRUE)[1]
  spec_col    <- grep("Specialty", cnames, value = TRUE, ignore.case = TRUE)[1]
  div_col     <- grep("Division", cnames, value = TRUE, ignore.case = TRUE)[1]
  
  if (any(is.na(c(surgeon_col, spec_col, div_col)))) {
    message("  Could not identify required columns in mapping file.")
    message("  Found columns: ", paste(cnames, collapse = ", "))
    return(NULL)
  }
  
  mapping <- mapping |>
    as_tibble() |>
    transmute(
      surgeon   = trimws(.data[[surgeon_col]]),
      specialty = trimws(.data[[spec_col]]),
      division  = trimws(.data[[div_col]])
    )
  
  message("  Loaded surgeon mapping: ", nrow(mapping), " entries, ",
          length(unique(mapping$division)), " divisions")
  mapping
}


#' Assign divisions to case data using surgeon mapping
#'
#' @param data Processed case data (must include surgeon column)
#' @param mapping Surgeon mapping tibble from load_surgeon_mapping()
#' @return Data with division column added
assign_divisions <- function(data, mapping) {
  if (is.null(mapping)) {
    data$division <- NA_character_
    return(data)
  }
  
  result <- data |>
    left_join(
      mapping |> select(surgeon, division),
      by = "surgeon"
    )
  
  n_mapped <- sum(!is.na(result$division))
  n_unmapped <- sum(is.na(result$division))
  if (n_unmapped > 0) {
    message("  Division assignment: ", n_mapped, " mapped, ",
            n_unmapped, " unmapped")
    # Show unmapped surgeons
    unmapped_surgeons <- result |>
      filter(is.na(division)) |>
      pull(surgeon) |>
      unique()
    message("  Unmapped surgeons: ",
            paste(unmapped_surgeons, collapse = "; "))
  } else {
    message("  Division assignment: all ", n_mapped, " cases mapped")
  }
  
  result
}


#' Get distinct divisions within a specialty
#'
#' @param data Case data with division column
#' @param spec Specialty name
#' @param min_cases Minimum case count to include a division (default 10)
#' @return Character vector of division names
get_divisions <- function(data, spec, min_cases = 10) {
  divs <- data |>
    filter(specialty == spec, !is.na(division)) |>
    count(division, name = "n") |>
    filter(n >= min_cases) |>
    arrange(desc(n)) |>
    pull(division)
  divs
}


#' Generate a summary table of observed rates
#'
#' @param data Processed case data
#' @param spec Specialty name
#' @param div Optional division name (NULL for all cases in specialty)
#' @return A tibble with complication, n_cases, n_events, observed_rate_pct
observed_rates_summary <- function(data, spec, div = NULL) {
  spec_data <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) {
    spec_data <- spec_data |> filter(division == div)
  }
  n_total <- nrow(spec_data)
  
  comps <- names(complication_labels)
  
  tibble(
    complication = complication_labels[comps],
    n_cases   = n_total,
    n_events  = sapply(comps, function(c) sum(spec_data[[c]], na.rm = TRUE)),
    observed_rate_pct = round(n_events / n_cases * 100, 2)
  )
}


#' Format a surgeon name for display: "LASTNAME,FIRST (12345)" -> "Lastname, First"
#'
#' Top-level rather than local because the executive summary's case list
#' formats the same column and a second copy would be a second thing to keep
#' in step.
format_surgeon_name <- function(s) {
  s <- gsub("\\s*\\(\\d+\\)$", "", s)
  parts <- strsplit(s, ",")[[1]]
  if (length(parts) == 2) {
    last  <- paste0(toupper(substr(trimws(parts[1]), 1, 1)),
                    tolower(substr(trimws(parts[1]), 2, nchar(trimws(parts[1])))))
    first <- paste0(toupper(substr(trimws(parts[2]), 1, 1)),
                    tolower(substr(trimws(parts[2]), 2, nchar(trimws(parts[2])))))
    paste0(last, ", ", first)
  } else {
    s
  }
}

#' Reduce an ASA class string to its roman numeral
format_asa_class <- function(a) {
  m <- regmatches(a, regexpr("[IV]+", a))
  if (length(m) > 0) m else a
}


#' Build a case-level complication list for the appendix
#'
#' Returns one row per case that had at least one complication (excluding
#' morbidity composite), with a human-readable occurrence string.
#' Only includes cases from the most recent `months` of data.
#'
#' @param data Processed case data
#' @param spec Specialty name
#' @param div Optional division name
#' @param months Number of trailing months to include (default 3)
#' @return A tibble ready for table display, or NULL if no cases
build_complication_caselist <- function(data, spec, div = NULL, months = 3) {
  
  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) {
    df <- df |> filter(division == div)
  }
  
  # Filter to trailing N months
  cutoff <- max(df$op_date, na.rm = TRUE) %m-% months(months)
  df <- df |> filter(op_date > cutoff)
  
  if (nrow(df) == 0) return(NULL)
  
  # Individual complications to check (exclude morbidity composite)
  comp_vars <- c(
    "mortality"            = "Death",
    "cardiac"              = "Cardiac",
    "pneumonia"            = "Pneumonia",
    "unplanned_intubation" = "Unplanned Intubation",
    "vent48"               = "Vent > 48h",
    "vte"                  = "VTE",
    "renal_failure"        = "Renal Failure",
    "uti"                  = "UTI",
    "ssi"                  = "SSI",
    "sepsis"               = "Sepsis",
    "cdiff"                = "C.diff",
    "unplanned_reop"       = "Return to OR"
  )
  
  # Build occurrence string for each case using apply
  occ_strings <- apply(df, 1, function(row) {
    hits <- character()
    for (vname in names(comp_vars)) {
      val <- suppressWarnings(as.integer(row[[vname]]))
      if (!is.na(val) && val == 1L) {
        hits <- c(hits, comp_vars[vname])
      }
    }
    # Handle readmission with relatedness
    ra <- suppressWarnings(as.integer(row[["unplanned_readmit"]]))
    if (!is.na(ra) && ra == 1L) {
      rel   <- suppressWarnings(as.integer(row[["readmit_related"]]))
      unrel <- suppressWarnings(as.integer(row[["readmit_unrelated"]]))
      rel   <- ifelse(is.na(rel), 0L, rel)
      unrel <- ifelse(is.na(unrel), 0L, unrel)
      if (rel == 1L && unrel == 1L) {
        hits <- c(hits, "Readmission (related + unrelated)")
      } else if (rel == 1L) {
        hits <- c(hits, "Readmission (related)")
      } else if (unrel == 1L) {
        hits <- c(hits, "Readmission (unrelated)")
      } else {
        hits <- c(hits, "Readmission")
      }
    }
    paste(hits, collapse = ", ")
  })
  
  df$.comp_list <- occ_strings
  df <- df |> filter(nchar(.comp_list) > 0)
  
  if (nrow(df) == 0) return(NULL)
  
  result <- df |>
    mutate(
      surgeon_short = sapply(surgeon, format_surgeon_name),
      asa_short     = sapply(asa_class, format_asa_class)
    ) |>
    arrange(op_date) |>
    transmute(
      MRN         = lmrn,
      `Op Date`   = format(op_date, "%m/%d/%y"),
      Surgeon     = surgeon_short,
      CPT         = cpt_code,
      ASA         = asa_short,
      LOS         = as.integer(los),
      Occurrences = .comp_list
    )
  
  result
}


# =============================================================================
# Procedure Classification
# =============================================================================

# CPT-based procedure category definitions for General Surgery
# NSQIP targeted flags (colectomy_flag, proctectomy_flag) take precedence
PROCEDURE_CPT_MAP <- list(
  "Appendectomy"               = c(44950, 44960, 44970),
  "Cholecystectomy"            = c(47562, 47563, 47564, 47600, 47605, 47610),
  "Breast"                     = c(19120, 19125, 19301, 19302, 19303, 19304,
                                   19305, 19306, 19307),
  "Thyroid/Parathyroid"        = c(60210, 60220, 60225, 60240, 60252, 60254,
                                   60260, 60270, 60271, 60500, 60502),
  "Adrenalectomy"              = c(60650),
  "Pancreatectomy"             = c(48140, 48145, 48146, 48148, 48150, 48152,
                                   48153, 48154, 48155),
  "Hepatectomy"                = c(47120, 47122, 47125, 47130),
  "Bariatric"                  = c(43644, 43645, 43775, 43659, 43770, 43771,
                                   43842, 43843, 43845, 43846),
  "Ventral Hernia Repair"      = c(49591, 49592, 49593, 49594, 49595, 49596,
                                   49613, 49614, 49615, 49616, 49617, 49618,
                                   49652, 49653, 49654, 49655, 49656, 49657),
  "Inguinal Hernia Repair"     = c(49505, 49507, 49520, 49521, 49525, 49550,
                                   49553, 49650, 49651),
  "Hiatal/PEH Repair"          = c(43280, 43281, 43282),
  "Enterostomy Closure"        = c(44620, 44625, 44626, 44227),
  "Small Bowel Resection"      = c(44120, 44121, 44125, 44130, 44202),
  "Esophagectomy"              = c(43107, 43108, 43112, 43113, 43116, 43117,
                                   43118, 43121, 43122, 43123, 43124, 43287,
                                   43288, 43289),
  "Transplant/Donor"           = c(50360, 50365, 50370, 47135, 47136, 47140,
                                   48160, 48550, 48554, 48556, 50300, 50320,
                                   50323, 50325, 50327, 50328, 50329, 50340,
                                   50543, 50544, 50545, 50547, 50225)
)

# Build reverse lookup: CPT -> category
.cpt_to_category <- local({
  lut <- new.env(hash = TRUE, parent = emptyenv())
  for (cat in names(PROCEDURE_CPT_MAP)) {
    for (cpt in PROCEDURE_CPT_MAP[[cat]]) {
      assign(as.character(cpt), cat, envir = lut)
    }
  }
  lut
})

#' Classify a case into a procedure category
#'
#' NSQIP targeted module flags (colectomy, proctectomy) take precedence
#' over CPT-based classification.
#'
#' @param cpt_code CPT code (numeric)
#' @param colectomy_flag 1 if NSQIP colectomy module, 0 otherwise
#' @param proctectomy_flag 1 if NSQIP proctectomy module, 0 otherwise
#' @return Character: procedure category name
classify_procedure <- function(cpt_code, colectomy_flag = 0, proctectomy_flag = 0) {
  if (!is.na(colectomy_flag) && colectomy_flag == 1) return("Colectomy")
  if (!is.na(proctectomy_flag) && proctectomy_flag == 1) return("Proctectomy")
  cpt_str <- as.character(as.integer(cpt_code))
  cat <- tryCatch(get(cpt_str, envir = .cpt_to_category), error = function(e) NULL)
  if (!is.null(cat)) cat else "Other"
}

#' Classify all cases in a data frame
#' @param data Case data with cpt_code, colectomy_flag, proctectomy_flag columns
#' @return Data with procedure_category column added
assign_procedure_categories <- function(data) {
  data |>
    mutate(procedure_category = mapply(
      classify_procedure, cpt_code, colectomy_flag, proctectomy_flag
    ))
}


#' Build procedure mix profile table for a specialty/division
#'
#' @param data Processed case data (with procedure_category column)
#' @param spec Specialty name
#' @param div Optional division name
#' @return A tibble with one row per procedure category, or NULL
build_procedure_mix <- function(data, spec, div = NULL) {
  
  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) {
    df <- df |> filter(division == div)
  }
  if (nrow(df) == 0) return(NULL)
  
  n_total <- nrow(df)
  
  # Any-complication indicator (excluding morbidity composite)
  comp_cols <- c("mortality", "cardiac", "pneumonia", "unplanned_intubation",
                 "vent48", "vte", "renal_failure", "uti", "ssi", "sepsis",
                 "cdiff", "unplanned_reop", "unplanned_readmit")
  df$any_complication <- as.integer(rowSums(df[, comp_cols], na.rm = TRUE) > 0)
  
  proc_summary <- df |>
    group_by(procedure_category) |>
    summarise(
      n          = n(),
      pct_total  = round(n() / n_total * 100, 1),
      n_comp     = sum(any_complication, na.rm = TRUE),
      comp_rate  = round(sum(any_complication, na.rm = TRUE) / n() * 100, 1),
      median_los = round(median(los, na.rm = TRUE), 0),
      .groups    = "drop"
    ) |>
    arrange(desc(n))
  
  proc_summary |>
    transmute(
      Procedure   = procedure_category,
      N           = n,
      `% Total`   = pct_total,
      `w/ Comp`   = n_comp,
      `Comp %`    = comp_rate,
      `Med LOS`   = median_los
    )
}


#' Build division-level complication rates by targeted procedure category
#'
#' For each targeted procedure type the division performs, computes the
#' division's own observed complication rates. Designed to sit alongside
#' the site-level targeted SAR benchmarks for comparison.
#'
#' @param data Processed case data (with procedure_category column)
#' @param spec Specialty name
#' @param div Optional division name
#' @param targeted_data Parsed targeted SAR (to know which complications to show)
#' @return A named list of tibbles (one per procedure), or NULL
build_division_procedure_rates <- function(data, spec, div = NULL, targeted_data = NULL) {
  
  if (is.null(targeted_data)) return(NULL)
  
  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) {
    df <- df |> filter(division == div)
  }
  if (nrow(df) == 0) return(NULL)
  
  # Only targeted procedures relevant to this specialty
  td <- targeted_data |> filter(specialty == spec)
  if (nrow(td) == 0) return(NULL)
  
  # Map from targeted complication names to our column names
  comp_col_map <- c(
    "Mortality"              = "mortality",
    "Morbidity"              = "morbidity",
    "Cardiac"                = "cardiac",
    "Pneumonia"              = "pneumonia",
    "Unplanned Intubation"   = "unplanned_intubation",
    "Ventilator > 48 Hours"  = "vent48",
    "VTE"                    = "vte",
    "Renal Failure"          = "renal_failure",
    "UTI"                    = "uti",
    "SSI"                    = "ssi",
    "Sepsis"                 = "sepsis",
    "C.diff Colitis"         = "cdiff",
    "Unplanned Reoperation"  = "unplanned_reop",
    "Unplanned Readmission"  = "unplanned_readmit",
    "Anastomotic Leak"       = "anastomotic_leak",
    "Prolonged NPO/NGT Use"  = "prolonged_npo"
  )
  
  results <- list()
  
  for (proc_name in unique(td$targeted_procedure)) {
    proc_cat <- unique(td$procedure_category[td$targeted_procedure == proc_name])
    if (length(proc_cat) == 0) next
    
    proc_cases <- df |> filter(procedure_category %in% proc_cat)
    n <- nrow(proc_cases)
    if (n == 0) next
    
    # Get the complications tracked for this targeted procedure
    proc_comps <- td |>
      filter(targeted_procedure == proc_name) |>
      pull(complication) |>
      unique()
    # Drop Length of Stay (not a binary complication)
    proc_comps <- proc_comps[proc_comps != "Length of Stay"]
    
    rows <- list()
    for (comp in proc_comps) {
      col <- comp_col_map[comp]
      if (is.na(col) || !(col %in% names(proc_cases))) next
      
      n_events <- sum(proc_cases[[col]], na.rm = TRUE)
      obs_pct <- round(n_events / n * 100, 2)
      
      rows[[length(rows) + 1]] <- tibble(
        Complication = comp,
        N            = n,
        Events       = n_events,
        `Obs %`      = obs_pct
      )
    }
    
    if (length(rows) > 0) {
      results[[proc_name]] <- bind_rows(rows)
    }
  }
  
  if (length(results) == 0) return(NULL)
  results
}


# =============================================================================
# Monthly Complication Dashboard
# =============================================================================

#' Dashboard complication row definitions
#'
#' Each entry: display_name, data_column, group, sar_complication (for rate lookup)
#' sar_complication is the matching SAR complication name, or NA if no direct match
DASHBOARD_ROWS <- list(
  # Group: top-level
  list("Cases Reviewed",           NA,                    "Volume",            NA),
  list("Mortality",                "mortality",           "Volume",            "Mortality"),
  # Group: Infection Related
  list("Superficial SSI",          "d_ssi_superficial",   "Infection Related", NA),
  list("Deep SSI",                 "d_ssi_deep",          "Infection Related", NA),
  list("Organ/Space SSI",          "d_ssi_organ",         "Infection Related", NA),
  list("UTI",                      "uti",                 "Infection Related", "UTI"),
  list("Sepsis",                   "sepsis",              "Infection Related", "Sepsis"),
  list("Septic Shock",             "d_septic_shock",      "Infection Related", NA),
  list("C.diff Colitis",           "cdiff",               "Infection Related", "C.diff Colitis"),
  list("Wound Disruption",         "wound_disruption",    "Infection Related", NA),
  # Group: Respiratory
  list("Pneumonia",                "pneumonia",           "Respiratory",       "Pneumonia"),
  list("Unplanned Intubation",     "unplanned_intubation","Respiratory",       "Unplanned Intubation"),
  list("Prolonged Vent (>48h)",    "vent48",              "Respiratory",       "Ventilator > 48h"),
  # Group: Renal
  list("Renal Insufficiency (AKI)","renal_insuff",        "Renal",             NA),
  list("Renal Failure (Dialysis)", "postop_dialysis_ind", "Renal",             NA),
  # Group: Cardiac
  list("Cardiac Arrest",           "cardiac_arrest",      "Cardiac",           NA),
  list("Myocardial Infarction",    "mi",                  "Cardiac",           NA),
  # Group: VTE/PE
  list("Venous Thromboembolism",   "dvt",                 "VTE/PE",            NA),
  list("Pulmonary Embolism",       "pe",                  "VTE/PE",            NA),
  # Group: Readm/ROR
  list("Unplanned Readmission",    "unplanned_readmit",   "Readm/ROR",        "Unplanned Readmission"),
  list("Return to OR",             "unplanned_reop",      "Readm/ROR",        "Unplanned Reoperation")
)

# SAR complication names that map to dashboard groups (for group-level rates)
DASHBOARD_GROUP_SAR <- list(
  "Infection Related" = "SSI",
  "Respiratory"       = NA,
  "Renal"             = "Renal Failure",
  "Cardiac"           = "Cardiac",
  "VTE/PE"            = "VTE"
)


#' Build monthly complication dashboard table
#'
#' @param data Processed case data
#' @param spec Specialty name
#' @param div Optional division name
#' @param benchmark_rates Benchmark rates tibble (for SAR rate column)
#' @return A list with: table (tibble), groups (for kableExtra pack_rows),
#'         months (character vector of month labels)
build_dashboard <- function(data, spec, div = NULL, benchmark_rates = NULL) {
  
  df <- data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) {
    df <- df |> filter(division == div)
  }
  if (nrow(df) == 0) return(NULL)
  
  n_total <- nrow(df)
  
  # Create month labels from the data range
  # Months are keyed on the month's start date, never on the label. A "%b"
  # label alone collides once the window exceeds twelve months ("Jan" from two
  # different years), which silently merged their counts into one column.
  # The year is shown whenever the window crosses a calendar year, so a label
  # is never ambiguous to a reader either.
  df <- df |> mutate(month_start = floor_date(op_date, "month"))

  month_starts <- sort(unique(df$month_start))
  multi_year   <- length(unique(format(month_starts, "%Y"))) > 1
  month_order  <- format(month_starts, if (multi_year) "%b %y" else "%b")

  # Monthly case counts, matched on the date key
  monthly_n <- vapply(
    month_starts, function(ms) sum(df$month_start == ms, na.rm = TRUE),
    integer(1)
  )
  names(monthly_n) <- month_order
  
  # Build each row
  rows <- list()
  group_info <- list()  # track group -> row indices for pack_rows
  current_group <- ""
  row_idx <- 0
  
  for (entry in DASHBOARD_ROWS) {
    display_name <- entry[[1]]
    col_name     <- entry[[2]]
    group        <- entry[[3]]
    sar_comp     <- entry[[4]]
    
    row_idx <- row_idx + 1
    
    # Track groups
    if (group != current_group) {
      current_group <- group
      if (!(group %in% names(group_info))) {
        group_info[[group]] <- c(start = row_idx, end = row_idx)
      }
    }
    group_info[[group]]["end"] <- row_idx
    
    if (is.na(col_name)) {
      # Cases Reviewed row
      monthly_vals <- as.integer(monthly_n[month_order])
      total_val <- n_total
      rate_val <- NA_real_
    } else {
      # Complication row: count events per month
      monthly_vals <- vapply(
        month_starts,
        function(ms) sum(df[[col_name]][df$month_start == ms], na.rm = TRUE),
        numeric(1)
      )
      total_val <- sum(df[[col_name]], na.rm = TRUE)
      rate_val <- round(total_val / n_total * 100, 1)
    }
    
    # SAR rate lookup
    sar_rate <- NA_real_
    if (!is.null(benchmark_rates) && !is.na(sar_comp)) {
      match <- benchmark_rates |>
        filter(specialty == spec, complication == sar_comp)
      if (nrow(match) > 0) {
        sar_rate <- round(match$p0_pct[1], 1)
      }
    }
    
    row_data <- c(
      list(Complication = display_name),
      as.list(setNames(as.integer(monthly_vals), month_order)),
      list(Total = as.integer(total_val),
           `Rate %` = rate_val,
           `SAR %` = sar_rate)
    )
    
    rows[[row_idx]] <- as_tibble(row_data)
  }
  
  result_table <- bind_rows(rows)
  
  # Clean up group_info: remove "Volume" since Mortality isn't really a group header
  # Instead, make Cases/Mortality standalone, and group the rest
  pack_info <- list()
  for (g in names(group_info)) {
    if (g == "Volume") next
    pack_info[[g]] <- c(group_info[[g]]["start"], group_info[[g]]["end"])
  }
  
  list(
    table      = result_table,
    pack_rows  = pack_info,
    months     = month_order,
    n_total    = n_total
  )
}
