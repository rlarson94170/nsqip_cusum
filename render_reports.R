# =============================================================================
# render_reports.R
#
# Master script to render CUSUM monitoring PDFs for each specialty and
# (optionally) for divisions within specialties.
#
# Usage:
#   1. Place Case Details Report xlsx in data/
#   2. Place SAR/ISAR Site Summary xlsx in data/ (optional but recommended)
#   3. Place surgeon_division_mapping.csv in data/ (for division reports)
#   4. Update paths below if needed
#   5. Run: source("render_reports.R")
#   6. PDFs will be saved to output/
# =============================================================================

library(quarto)
library(tinytex)

# ---- CONFIGURATION ----------------------------------------------------------

# Path to the latest Case Details Report download
data_file <- "data/Case_Details_Report.xlsx"

# Path to Site SAR/ISAR Summary (set to NULL if not available)
# Use whichever is most recent — SAR and ISAR have compatible formats
site_sar_file <- "data/SAR_Site_Summary.xlsx"

# Surgeon-to-division mapping (set to "" to skip division reports)
surgeon_mapping_file <- "data/surgeon_division_mapping.csv"

# Benchmark type: "site_expected" (risk-adjusted, preferred) or
#                 "national_observed" (unadjusted national rates)
benchmark_type <- "site_expected"

# Specialties to generate reports for
specialties <- c("General Surgery", "Vascular", "Thoracic", "Plastics")

# Specialties to break out by division (requires surgeon mapping)
# Each specialty listed here will get one PDF per division in addition
# to the overall specialty PDF
division_specialties <- c("General Surgery")

# Minimum cases to generate a division report
min_division_cases <- 10

# CUSUM parameters
odds_ratio <- 2.0     # p1 = OR of 2x relative to p0
target_arl <- 500     # Average run length before false alarm

# Output directory
output_dir <- "output"

# -----------------------------------------------------------------------------

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Validate files
if (!file.exists(data_file)) {
  stop("Case Details file not found: ", data_file,
       "\nPlace your Case Details Report .xlsx in the data/ folder.")
}

if (!is.null(site_sar_file) && !file.exists(site_sar_file)) {
  message("NOTE: Site SAR file not found: ", site_sar_file)
  message("  Falling back to national observed rates.")
  site_sar_file <- NULL
  benchmark_type <- "national_observed"
}

has_mapping <- nchar(surgeon_mapping_file) > 0 && file.exists(surgeon_mapping_file)
if (!has_mapping && length(division_specialties) > 0) {
  message("NOTE: Surgeon mapping not found: ", surgeon_mapping_file)
  message("  Division reports will be skipped.")
}

message("\n", strrep("=", 65))
message("  NSQIP CUSUM Report Generation")
message(strrep("=", 65))
message("Data file:      ", data_file)
message("Site SAR:       ", ifelse(is.null(site_sar_file), "(not provided)", site_sar_file))
message("Surgeon map:    ", ifelse(has_mapping, surgeon_mapping_file, "(not provided)"))
message("Benchmark type: ", benchmark_type)
message("Specialties:    ", paste(specialties, collapse = ", "))
message("Divisions for:  ", ifelse(has_mapping, paste(division_specialties, collapse = ", "), "(none)"))
message("Parameters:     OR = ", odds_ratio, ", Target ARL = ", target_arl)
message("Output:         ", output_dir, "/")
message(strrep("=", 65), "\n")

# ---- Build list of reports to render ----------------------------------------

# Helper to render a single report
render_one <- function(spec, div = "", label = NULL) {
  if (is.null(label)) {
    label <- if (nchar(div) > 0) paste0(spec, " — ", div) else spec
  }
  
  spec_clean <- gsub("[/ ]", "_", tolower(spec))
  div_clean  <- gsub("[/ ]", "_", tolower(div))
  
  filename <- if (nchar(div) > 0) {
    paste0("NSQIP_CUSUM_", spec_clean, "_", div_clean, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  } else {
    paste0("NSQIP_CUSUM_", spec_clean, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  }
  output_file <- file.path(output_dir, filename)
  
  message("\n--- Rendering: ", label, " ---")
  
  tryCatch({
    quarto_render(
      input = "nsqip_cusum_report.qmd",
      output_format = "pdf",
      execute_params = list(
        specialty            = spec,
        division             = div,
        data_file            = data_file,
        site_sar_file        = ifelse(is.null(site_sar_file), "", site_sar_file),
        surgeon_mapping_file = ifelse(has_mapping, surgeon_mapping_file, ""),
        benchmark_type       = benchmark_type,
        odds_ratio           = odds_ratio,
        target_arl           = target_arl
      )
    )
    
    rendered_pdf <- "nsqip_cusum_report.pdf"
    if (file.exists(rendered_pdf)) {
      file.copy(rendered_pdf, output_file, overwrite = TRUE)
      file.remove(rendered_pdf)
      message("  \u2713 Success: ", output_file)
    }
    
  }, error = function(e) {
    message("  \u2717 ERROR: ", e$message)
  })
}

# ---- Render specialty-level reports -----------------------------------------

for (spec in specialties) {
  render_one(spec)
}

# ---- Render division-level reports ------------------------------------------

if (has_mapping) {
  
  # Load data to discover divisions and their case counts
  source("R/data_processing.R")
  
  case_data <- process_case_details(data_file, specialties)
  surgeon_map <- load_surgeon_mapping(surgeon_mapping_file)
  case_data <- assign_divisions(case_data, surgeon_map)
  
  for (spec in division_specialties) {
    divs <- get_divisions(case_data, spec, min_cases = min_division_cases)
    
    if (length(divs) == 0) {
      message("\n  No divisions with >= ", min_division_cases,
              " cases found for ", spec)
      next
    }
    
    message("\n", strrep("-", 50))
    message("  Division reports for ", spec, ": ",
            paste(divs, collapse = ", "))
    message(strrep("-", 50))
    
    for (div in divs) {
      n <- sum(case_data$specialty == spec & case_data$division == div,
               na.rm = TRUE)
      message("  (", div, ": ", n, " cases)")
      render_one(spec, div)
    }
  }
}

# ---- Done -------------------------------------------------------------------

message("\n", strrep("=", 65))
message("  Report generation complete!")
message("  Check '", output_dir, "/' for PDFs.")
message(strrep("=", 65))
