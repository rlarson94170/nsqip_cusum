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

#' Read and process the NSQIP Case Details Report
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
  raw <- read_excel(filepath, sheet = "report_data")
  message("  Total cases in file: ", nrow(raw))
  
  # --- Filter to target specialties and completed cases ---
  df <- raw |>
    filter(
      `Completion Status` == "Complete",
      `Surgical Specialty` %in% specialties
    )
  message("  Cases after filtering (complete, target specialties): ", nrow(df))
  
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
  
  # --- Derive binary complication indicators ---
  processed <- df |>
    transmute(
      # Identifiers
      case_id    = `Case Number`,
      lmrn       = as.character(as.integer(`LMRN`)),
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
      pneumonia = as.integer(pneumonia_raw == 1 & pneumonia_patos == 0),
      
      # 4. Unplanned Intubation
      unplanned_intubation = safe_binary(`# of Postop Unplanned Intubation`),
      
      # 5. Ventilator > 48 Hours (exclude PATOS)
      vent48_raw   = safe_binary(`# of Postop On Ventilator > 48 hours`),
      vent48_patos = safe_binary(`# of Postop On Ventilator > 48 hours PATOS`),
      vent48 = as.integer(vent48_raw == 1 & vent48_patos == 0),
      
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
      uti = as.integer(uti_raw == 1 & uti_patos == 0),
      
      # 9. SSI: Superficial + Deep + Organ/Space (SAR definition 6)
      #    Exclude PATOS for organ/space (and superficial if PATOS exists)
      ssi_superficial     = safe_binary(`# of Postop Superficial Incisional SSI`),
      ssi_superficial_pat = safe_binary(`# of Postop Superficial Incisional SSI PATOS`),
      ssi_deep            = safe_binary(`# of Postop Deep Incisional SSI`),
      ssi_deep_patos      = safe_binary(`# of Postop Deep Incisional SSI PATOS`),
      ssi_organ           = safe_binary(`# of Postop Organ/Space SSI`),
      ssi_organ_patos     = safe_binary(`# of Postop Organ/Space SSI PATOS`),
      ssi = as.integer(
        (ssi_superficial == 1 & ssi_superficial_pat == 0) |
        (ssi_deep == 1 & ssi_deep_patos == 0) |
        (ssi_organ == 1 & ssi_organ_patos == 0)
      ),
      
      # 10. Sepsis: Worsening sepsis/septic shock (SAR definition 7)
      #     Exclude PATOS
      sepsis_raw   = safe_binary(`# of Postop Sepsis`),
      sepsis_patos = safe_binary(`# of Postop Sepsis PATOS`),
      sepsis = as.integer(sepsis_raw == 1 & sepsis_patos == 0),
      
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
    "anastomotic_leak", "prolonged_npo"
  )
  processed <- processed |>
    mutate(across(all_of(complication_cols), ~replace_na(., 0L)))
  
  message("  Date range: ", min(processed$op_date, na.rm = TRUE),
          " to ", max(processed$op_date, na.rm = TRUE))
  message("  Cases by specialty:")
  for (s in specialties) {
    n <- sum(processed$specialty == s)
    message("    ", s, ": ", n)
  }
  
  processed
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
  
  # Format surgeon name: "LAST, First" without ID number
  format_surgeon <- function(s) {
    # Remove parenthetical ID: "LASTNAME,FIRST (12345)" -> "LASTNAME,FIRST"
    s <- gsub("\\s*\\(\\d+\\)$", "", s)
    # Title case: "LASTNAME,FIRST" -> "Lastname, First"
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
  
  # Format ASA: extract just the class number
  format_asa <- function(a) {
    m <- regmatches(a, regexpr("[IV]+", a))
    if (length(m) > 0) m else a
  }
  
  result <- df |>
    mutate(
      surgeon_short = sapply(surgeon, format_surgeon),
      asa_short     = sapply(asa_class, format_asa)
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
