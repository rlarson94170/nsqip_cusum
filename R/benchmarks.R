# =============================================================================
# NSQIP SAR Benchmark Rates
#
# Two benchmark sources:
#   1. National observed rates from SAR Summary Report (unadjusted)
#   2. Site-specific expected rates from Site SAR (risk-adjusted for case mix)
#
# The site expected rates are the preferred p0 because they account for
# the complexity of your patient population.
# =============================================================================

library(tibble)
library(dplyr)
library(readxl)

# Standardized complication names used throughout the project
COMPLICATIONS <- c(
  "Mortality", "Morbidity", "Cardiac", "Pneumonia",
  "Unplanned Intubation", "Ventilator > 48h", "VTE",
  "Renal Failure", "UTI", "SSI", "Sepsis", "C.diff Colitis",
  "Unplanned Reoperation", "Unplanned Readmission"
)

# ---- National Observed Rates (January 2026 SAR Summary) --------------------

get_national_rates <- function() {
  
  allcases <- c(
    Mortality = 0.82, Morbidity = 6.51, Cardiac = 0.53,
    Pneumonia = 0.85, `Unplanned Intubation` = 0.44,
    `Ventilator > 48h` = 0.52, VTE = 0.83,
    `Renal Failure` = 0.77, UTI = 1.13, SSI = 3.10,
    Sepsis = 0.98, `C.diff Colitis` = 0.21,
    `Unplanned Reoperation` = 2.36, `Unplanned Readmission` = 4.63
  )
  
  spec_rates <- list(
    "General Surgery" = c(
      Mortality = 1.12, Morbidity = 7.53, Cardiac = 0.61,
      Pneumonia = 1.06, `Unplanned Intubation` = 0.61,
      `Ventilator > 48h` = 0.87, VTE = 0.95,
      `Renal Failure` = 1.15, UTI = 0.75, SSI = 3.88,
      Sepsis = 1.45, `C.diff Colitis` = 0.34,
      `Unplanned Reoperation` = 2.57, `Unplanned Readmission` = 5.42
    ),
    "Vascular" = c(
      Mortality = 2.59, Morbidity = 11.12, Cardiac = 2.25,
      Pneumonia = 1.73, `Unplanned Intubation` = 1.37,
      `Ventilator > 48h` = 1.22, VTE = 0.94,
      `Renal Failure` = 1.94, UTI = 0.97, SSI = 3.78,
      Sepsis = 2.09, `C.diff Colitis` = 0.34,
      `Unplanned Reoperation` = 7.79, `Unplanned Readmission` = 9.37
    ),
    "Thoracic" = c(
      Mortality = 1.06, Morbidity = 8.51, Cardiac = NA,
      Pneumonia = 3.66, `Unplanned Intubation` = 1.63,
      `Ventilator > 48h` = 1.24, VTE = 1.11,
      `Renal Failure` = 0.90, UTI = 0.74, SSI = 2.85,
      Sepsis = 1.70, `C.diff Colitis` = 0.19,
      `Unplanned Reoperation` = 3.93, `Unplanned Readmission` = 6.35
    ),
    "Plastics" = c(
      Mortality = NA, Morbidity = 6.50, Cardiac = NA,
      Pneumonia = 0.22, `Unplanned Intubation` = NA,
      `Ventilator > 48h` = 0.07, VTE = NA,
      `Renal Failure` = 0.11, UTI = 0.32, SSI = 5.21,
      Sepsis = 0.33, `C.diff Colitis` = 0.08,
      `Unplanned Reoperation` = 3.99, `Unplanned Readmission` = 3.02
    )
  )
  
  rates <- tibble(
    specialty = character(), complication = character(),
    national_rate_pct = numeric(), source = character()
  )
  
  for (spec in names(spec_rates)) {
    for (comp in COMPLICATIONS) {
      rate <- spec_rates[[spec]][comp]
      if (is.na(rate)) {
        rates <- bind_rows(rates, tibble(
          specialty = spec, complication = comp,
          national_rate_pct = allcases[comp],
          source = "ALLCASES (fallback)"
        ))
      } else {
        rates <- bind_rows(rates, tibble(
          specialty = spec, complication = comp,
          national_rate_pct = rate,
          source = "Specialty-specific"
        ))
      }
    }
  }
  
  rates$national_rate <- rates$national_rate_pct / 100
  rates
}


# ---- Site-Specific Expected Rates (from Site SAR) --------------------------

#' Parse the Site SAR Summary to extract expected rates and assessments
parse_site_sar <- function(filepath) {
  
  sheet_map <- c(
    "General" = "General Surgery", "Vascular" = "Vascular",
    "Thoracic" = "Thoracic", "Plastic" = "Plastics"
  )
  
  comp_map <- c(
    "Mortality"              = "Mortality",
    "Morbidity"              = "Morbidity",
    "Cardiac"                = "Cardiac",
    "Pneumonia"              = "Pneumonia",
    "Unplanned Intubation"   = "Unplanned Intubation",
    "Ventilator > 48 Hours"  = "Ventilator > 48h",
    "VTE"                    = "VTE",
    "Renal Failure"          = "Renal Failure",
    "UTI"                    = "UTI",
    "SSI"                    = "SSI",
    "Sepsis"                 = "Sepsis",
    "C.diff Colitis"         = "C.diff Colitis",
    "Unplanned Reoperation"  = "Unplanned Reoperation",
    "Unplanned Readmission"  = "Unplanned Readmission"
  )
  
  all_site_rates <- tibble()
  
  for (sheet_name in names(sheet_map)) {
    spec_name <- sheet_map[sheet_name]
    tryCatch({
      df <- read_excel(filepath, sheet = sheet_name)
      for (i in seq_len(nrow(df))) {
        model <- as.character(df$`Model Name`[i])
        if (is.na(model)) next
        if (grepl("^Emergency", model, ignore.case = TRUE)) next
        
        matched_comp <- NA
        for (sar_suffix in names(comp_map)) {
          if (grepl(paste0(sar_suffix, "(\\s*\\(.*\\))?$"), model)) {
            matched_comp <- comp_map[sar_suffix]
            break
          }
        }
        if (is.na(matched_comp)) next
        
        all_site_rates <- bind_rows(all_site_rates, tibble(
          specialty        = spec_name,
          complication     = matched_comp,
          sar_model        = model,
          n_cases_sar      = as.numeric(df$`Total Cases`[i]),
          observed_events  = as.numeric(df$`Observed Events`[i]),
          observed_rate_sar = as.numeric(df$`Observed Rate`[i]),
          expected_rate    = as.numeric(df$`Expected Rate`[i]),
          odds_ratio_sar   = as.numeric(df$`Odds Ratio`[i]),
          ci_lower         = as.numeric(df$`95% CI Lower`[i]),
          ci_upper         = as.numeric(df$`95% CI Upper`[i]),
          outlier          = ifelse(is.na(df$Outlier[i]), "", as.character(df$Outlier[i])),
          decile           = as.numeric(df$Decile[i]),
          adj_percentile   = as.numeric(df$`Adjusted Percentile`[i]),
          adj_quartile     = as.numeric(df$`Adjusted Quartile`[i]),
          assessment       = as.character(df$Assessment[i])
        ))
      }
    }, error = function(e) {
      message("  Warning: could not read sheet '", sheet_name, "': ", e$message)
    })
  }
  
  all_site_rates
}


#' Parse the Over-Time sheet for O/E trend data
parse_over_time <- function(filepath) {
  
  df <- read_excel(filepath, sheet = "Over-Time")
  
  spec_prefixes <- c(
    "GEN " = "General Surgery", "VASC " = "Vascular",
    "THOR " = "Thoracic", "PLAST " = "Plastics"
  )
  
  comp_suffixes <- c(
    "Mortality" = "Mortality", "Morbidity" = "Morbidity",
    "Cardiac" = "Cardiac", "Pneumonia" = "Pneumonia",
    "Unplanned Intubation" = "Unplanned Intubation",
    "Ventilator > 48 Hours" = "Ventilator > 48h",
    "VTE" = "VTE", "Renal Failure" = "Renal Failure",
    "UTI" = "UTI", "SSI" = "SSI", "Sepsis" = "Sepsis",
    "C.diff Colitis" = "C.diff Colitis",
    "Unplanned Reoperation" = "Unplanned Reoperation",
    "Unplanned Readmission" = "Unplanned Readmission"
  )
  
  results <- tibble()
  
  for (i in seq_len(nrow(df))) {
    model <- as.character(df$Model[i])
    if (is.na(model)) next
    
    matched_spec <- NA
    for (pfx in names(spec_prefixes)) {
      if (startsWith(model, pfx)) { matched_spec <- spec_prefixes[pfx]; break }
    }
    if (is.na(matched_spec)) next
    
    matched_comp <- NA
    for (sfx in names(comp_suffixes)) {
      if (endsWith(model, sfx)) { matched_comp <- comp_suffixes[sfx]; break }
    }
    if (is.na(matched_comp)) next
    
    period_cols <- names(df)[names(df) != "Model"]
    for (pc in period_cols) {
      val_raw <- as.character(df[[pc]][i])
      if (is.na(val_raw) || val_raw == "nan" || val_raw == "NaN") next
      is_high <- grepl("H$", val_raw)
      is_low  <- grepl("L$", val_raw)
      val_num <- suppressWarnings(as.numeric(gsub("[HL]$", "", val_raw)))
      if (is.na(val_num)) next
      
      results <- bind_rows(results, tibble(
        specialty = matched_spec, complication = matched_comp,
        period = pc, oe_ratio = val_num,
        is_outlier_high = is_high, is_outlier_low = is_low
      ))
    }
  }
  
  results
}


#' Build the unified benchmark table
#'
#' @param site_sar_path Path to SAR_Site_Summary.xlsx (NULL to use national only)
#' @param benchmark_type "site_expected" (preferred) or "national_observed"
get_benchmark_rates <- function(site_sar_path = NULL,
                                benchmark_type = "site_expected") {
  
  national <- get_national_rates()
  
  if (is.null(site_sar_path) || benchmark_type == "national_observed") {
    rates <- national |>
      transmute(
        specialty, complication,
        p0_pct = national_rate_pct, p0 = national_rate,
        national_rate_pct, national_rate,
        benchmark_source = paste0("National observed (", source, ")"),
        assessment = NA_character_,
        odds_ratio_sar = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
        adj_percentile = NA_real_, outlier = NA_character_
      )
    return(rates)
  }
  
  site_rates <- parse_site_sar(site_sar_path)
  
  rates <- national |>
    left_join(
      site_rates |> select(
        specialty, complication, expected_rate,
        odds_ratio_sar, ci_lower, ci_upper,
        assessment, n_cases_sar, observed_rate_sar,
        adj_percentile, outlier
      ),
      by = c("specialty", "complication")
    ) |>
    mutate(
      p0_pct = ifelse(!is.na(expected_rate), expected_rate * 100, national_rate_pct),
      p0     = ifelse(!is.na(expected_rate), expected_rate, national_rate),
      benchmark_source = ifelse(
        !is.na(expected_rate),
        "Site expected (risk-adjusted)",
        paste0("National observed (", source, ")")
      )
    )
  
  rates
}

# =============================================================================
# Targeted SAR Parsing
# =============================================================================

# Map targeted sheet names to specialty names
TARGETED_SHEET_MAP <- c(
  "Targeted-General"    = "General Surgery",
  "Targeted-Vascular"   = "Vascular",
  "Targeted-Thoracic"   = "Thoracic",
  "Targeted-Plastic"    = "Plastics"
)

# Map targeted procedure names (from SAR model names) to procedure categories
# (matching the categories in data_processing.R)
TARGETED_PROC_MAP <- c(
  "Colectomy"              = "Colectomy",
  "Proctectomy"            = "Proctectomy",
  "VHR"                    = "Ventral Hernia Repair",
  "Major Hepatectomy"      = "Hepatectomy",
  "Partial Hepatectomy"    = "Hepatectomy",
  "Distal Pancreatectomy"  = "Pancreatectomy",
  "Whipple Pancreatectomy" = "Pancreatectomy",
  "Esophagectomy"          = "Esophagectomy",
  "Lung Resection"         = "Lung Resection",
  "Flap"                   = "Flap"
)


#' Parse Targeted SAR data from the Site Summary file
#'
#' @param sar_file Path to SAR_Site_Summary.xlsx
#' @return A tibble with targeted procedure benchmark data
parse_targeted_sar <- function(sar_file) {
  
  if (is.null(sar_file) || !file.exists(sar_file)) return(NULL)
  
  sheets <- readxl::excel_sheets(sar_file)
  results <- list()
  
  for (sheet_name in names(TARGETED_SHEET_MAP)) {
    if (!(sheet_name %in% sheets)) next
    spec <- TARGETED_SHEET_MAP[sheet_name]
    
    raw <- readxl::read_excel(sar_file, sheet = sheet_name)
    if (nrow(raw) == 0 || !("Model Name" %in% names(raw))) next
    
    for (i in seq_len(nrow(raw))) {
      model <- raw$`Model Name`[i]
      if (is.na(model)) next
      
      # Skip emergency sub-models
      if (grepl("^Emergency ", model)) next
      
      # Parse: remove prefix patterns like "T GEN ", "T THOR ", "GEN ", "VASC "
      # Only strip known specialty abbreviation prefixes
      clean <- model
      # Pattern 1: "T <SPEC> " (targeted with specialty)
      if (grepl("^T\\s+[A-Z]+\\s+", clean)) {
        clean <- sub("^T\\s+[A-Z]+\\s+", "", clean)
      } else {
        # Pattern 2: "GEN ", "VASC ", "THOR ", "PLAST " (LOS-only models)
        clean <- sub("^(GEN|VASC|THOR|PLAST|NSG|ORTHO|GYN|OB|URO)\\s+", "", clean)
      }
      
      # Match known complications from the end
      known_comps <- c("Mortality", "Morbidity", "Cardiac", "Pneumonia",
                       "Unplanned Intubation", "Ventilator > 48 Hours",
                       "VTE", "Renal Failure", "UTI", "SSI", "Sepsis",
                       "C.diff Colitis", "Unplanned Reoperation",
                       "Unplanned Readmission", "Length of Stay",
                       "Anastomotic Leak", "Prolonged NPO/NGT Use")
      
      proc_name <- clean
      comp_name <- NA_character_
      for (comp in known_comps) {
        if (endsWith(clean, comp)) {
          proc_name <- trimws(sub(paste0(comp, "$"), "", clean))
          comp_name <- comp
          break
        }
      }
      
      # Map to our procedure categories
      proc_cat <- if (proc_name %in% names(TARGETED_PROC_MAP)) {
        TARGETED_PROC_MAP[proc_name]
      } else {
        proc_name
      }
      
      results[[length(results) + 1]] <- tibble(
        specialty          = spec,
        targeted_procedure = proc_name,
        procedure_category = as.character(proc_cat),
        complication       = comp_name,
        n_cases            = as.integer(raw$`Total Cases`[i]),
        obs_rate           = as.numeric(raw$`Observed Rate`[i]),
        exp_rate           = as.numeric(raw$`Expected Rate`[i]),
        odds_ratio         = as.numeric(raw$`Odds Ratio`[i]),
        percentile         = as.numeric(raw$`Adjusted Percentile`[i]),
        assessment         = as.character(raw$`Assessment`[i])
      )
    }
  }
  
  if (length(results) == 0) return(NULL)
  bind_rows(results)
}


#' Build targeted SAR summary for a specific division
#'
#' Filters targeted SAR data to procedures that the division actually performs,
#' based on the case data.
#'
#' @param targeted_data Parsed targeted SAR (from parse_targeted_sar)
#' @param case_data Case data with procedure_category and division columns
#' @param spec Specialty
#' @param div Division name (NULL for specialty-level report)
#' @return A formatted tibble for display, or NULL
build_targeted_summary <- function(targeted_data, case_data, spec, div = NULL) {
  
  if (is.null(targeted_data)) return(NULL)
  
  # Filter targeted data to this specialty
  td <- targeted_data |> filter(specialty == spec)
  if (nrow(td) == 0) return(NULL)
  
  # Determine which procedure categories this division actually does
  df <- case_data |> filter(specialty == spec)
  if (!is.null(div) && nchar(div) > 0) {
    df <- df |> filter(division == div)
  }
  if (nrow(df) == 0) return(NULL)
  
  # Count cases per procedure category in our data
  div_procs <- df |>
    count(procedure_category, name = "div_cases") |>
    filter(div_cases >= 1)
  
  # Match targeted procedures to what the division does
  relevant <- td |>
    filter(procedure_category %in% div_procs$procedure_category) |>
    left_join(div_procs, by = "procedure_category")
  
  if (nrow(relevant) == 0) return(NULL)
  
  # Format for display
  relevant |>
    mutate(
      obs_pct = round(obs_rate * 100, 2),
      exp_pct = round(exp_rate * 100, 2),
      oe      = round(odds_ratio, 2),
      pctl    = round(percentile)
    ) |>
    transmute(
      Procedure    = targeted_procedure,
      Complication  = complication,
      `N (SAR)`    = n_cases,
      `Obs %`      = obs_pct,
      `Exp %`      = exp_pct,
      `O/E`        = oe,
      Pctl         = pctl,
      Assessment   = assessment
    )
}
