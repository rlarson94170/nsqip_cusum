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
      op_date    = op_date,
      specialty  = `Surgical Specialty`,
      surgeon    = `Attending/Staff Surgeon`,
      cpt_code   = `CPT Code`,
      cpt_desc   = `CPT Description`,
      age        = `Age at Time of Surgery`,
      asa_class  = `ASA Classification`,
      
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
      case_id, op_date, specialty, surgeon, cpt_code, cpt_desc, age, asa_class,
      mortality, morbidity, cardiac, pneumonia, unplanned_intubation,
      vent48, vte, renal_failure, uti, ssi, sepsis, cdiff,
      unplanned_reop, unplanned_readmit
    )
  
  # Replace any remaining NAs in complication columns with 0
  complication_cols <- c(
    "mortality", "morbidity", "cardiac", "pneumonia", "unplanned_intubation",
    "vent48", "vte", "renal_failure", "uti", "ssi", "sepsis", "cdiff",
    "unplanned_reop", "unplanned_readmit"
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
