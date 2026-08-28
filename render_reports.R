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

# Helpers, in dependency order. These are sourced explicitly rather than
# relied on as a side effect of calling the slide renderer — division
# discovery needs them even when render_slides is FALSE.
source("R/benchmarks.R")
source("R/data_processing.R")
source("R/cusum_functions.R")
source("R/triage.R")
source("R/load_report_data.R")

# The direct beamer slide renderer (bypasses Quarto/pandoc)
source("R/render_beamer_slides.R")

# Verify the LaTeX environment before doing any work. Set strict = FALSE
# to downgrade missing packages from an error to a warning.
source("R/preflight.R")
preflight_latex(strict = TRUE)

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

# In-control ARL, in cases, for the CUSUM decision boundary. The ARL is per
# chart and a full run produces dozens of charts, so this is a multiplicity
# setting as much as a sensitivity one: at 500 roughly 3 charts per division
# report flag by chance, at 1500 roughly 0.8. The CUSUM is the timing gate
# only — a complication reaches the review list when its cumulative rate is
# also elevated (see R/triage.R) — so this does not need to carry the whole
# false-alarm budget on its own.
target_arl <- 1500

# Output formats: set render_slides = TRUE to also produce beamer slide decks
render_slides <- TRUE

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
message("Triage gates:   >= ", TRIAGE_MIN_EVENTS, " events and p < ",
        TRIAGE_ALPHA, ", plus CUSUM timing")
message("Slide decks:    ", ifelse(render_slides, "Yes", "No"))
message("Output:         ", output_dir, "/")
message(strrep("=", 65), "\n")

# ---- Render outcome ledger --------------------------------------------------
# Every render's outcome is recorded so the run can end with an accurate
# summary and a non-zero exit. Previously a failed render printed an error and
# the script still finished with "Report generation complete!", which made a
# partial set of PDFs indistinguishable from a full one.

.render_log <- new.env(parent = emptyenv())
.render_log$rows <- list()

.record_render <- function(label, kind, ok) {
  .render_log$rows[[length(.render_log$rows) + 1]] <-
    list(label = label, kind = kind, ok = isTRUE(ok))
  invisible(NULL)
}

.render_failures <- function() {
  Filter(function(r) !r$ok, .render_log$rows)
}


# ---- Build list of reports to render ----------------------------------------

# Helper to render a single PDF report via Quarto
render_one <- function(spec, div = "", label = NULL) {
  if (is.null(label)) {
    label <- if (nchar(div) > 0) paste0(spec, " — ", div) else spec
  }
  
  spec_clean <- gsub("[/ ]", "_", tolower(spec))
  div_clean  <- gsub("[/ ]", "_", tolower(div))
  
  base_name <- if (nchar(div) > 0) {
    paste0("NSQIP_CUSUM_", spec_clean, "_", div_clean, "_", format(Sys.Date(), "%Y%m%d"))
  } else {
    paste0("NSQIP_CUSUM_", spec_clean, "_", format(Sys.Date(), "%Y%m%d"))
  }
  
  filename <- paste0(base_name, ".pdf")
  output_file <- file.path(output_dir, filename)
  
  message("\n--- Rendering: ", label, " ---")
  
  ok <- tryCatch({
    quarto_render(
      input = "nsqip_cusum_report.qmd",
      execute_params = list(
        specialty            = spec,
        division             = div,
        data_file            = data_file,
        site_sar_file        = ifelse(is.null(site_sar_file), "", site_sar_file),
        surgeon_mapping_file = ifelse(has_mapping, surgeon_mapping_file, ""),
        benchmark_type       = benchmark_type,
        specialties          = specialties,
        odds_ratio           = odds_ratio,
        target_arl           = target_arl
      )
    )
    
    rendered_pdf <- "nsqip_cusum_report.pdf"
    if (!file.exists(rendered_pdf)) {
      # Quarto returned without error but produced nothing. Treated as a
      # failure: staying silent here would leave the previous run's PDF in
      # output/ looking like a fresh one.
      stop("quarto_render() produced no PDF")
    }
    if (!file.copy(rendered_pdf, output_file, overwrite = TRUE)) {
      stop("could not copy the rendered PDF to ", output_file)
    }
    file.remove(rendered_pdf)
    message("  \u2713 Success: ", output_file)
    TRUE

  }, error = function(e) {
    message("  \u2717 ERROR: ", conditionMessage(e))
    FALSE
  })

  .record_render(label, "report", ok)
  invisible(ok)
}

# Helper to render a slide deck (direct R → LaTeX → PDF, no Quarto)
render_slides_one <- function(spec, div = "") {
  label <- if (nchar(div) > 0) paste0(spec, " \u2014 ", div) else spec
  
  spec_clean <- gsub("[/ ]", "_", tolower(spec))
  div_clean  <- gsub("[/ ]", "_", tolower(div))
  
  base_name <- if (nchar(div) > 0) {
    paste0("NSQIP_CUSUM_", spec_clean, "_", div_clean, "_", format(Sys.Date(), "%Y%m%d"))
  } else {
    paste0("NSQIP_CUSUM_", spec_clean, "_", format(Sys.Date(), "%Y%m%d"))
  }
  
  filename <- paste0(base_name, "_slides.pdf")
  out_file <- file.path(output_dir, filename)
  
  message("\n--- Rendering: ", label, " [slides] ---")
  
  ok <- tryCatch({
    render_beamer_slides(
      spec               = spec,
      div                = div,
      data_file          = data_file,
      site_sar_file      = ifelse(is.null(site_sar_file), "", site_sar_file),
      surgeon_mapping_file = ifelse(has_mapping, surgeon_mapping_file, ""),
      benchmark_type     = benchmark_type,
      specialties        = specialties,
      odds_ratio         = odds_ratio,
      target_arl         = target_arl,
      output_file        = out_file
    )
    if (!file.exists(out_file)) {
      stop("render_beamer_slides() produced no PDF")
    }
    message("  \u2713 Success: ", out_file)
    TRUE
  }, error = function(e) {
    message("  \u2717 ERROR: ", conditionMessage(e))
    FALSE
  })

  .record_render(label, "slides", ok)
  invisible(ok)
}

# ---- Render specialty-level reports -----------------------------------------

# A specialty with no cases renders a structurally valid but empty PDF, which
# is how a typo in `specialties` used to pass unnoticed. Check up front and
# skip, rather than shipping an empty report. The load is cached, so this
# costs nothing on top of the renders that follow.
available <- load_report_data(
  data_file            = data_file,
  site_sar_file        = if (is.null(site_sar_file)) "" else site_sar_file,
  surgeon_mapping_file = if (has_mapping) surgeon_mapping_file else "",
  benchmark_type       = benchmark_type,
  specialties          = specialties
)$case_data

spec_counts <- table(available$specialty)
empty_specs <- setdiff(specialties, names(spec_counts))

if (length(empty_specs) > 0) {
  message("\n", strrep("!", 65))
  message("  Skipping ", length(empty_specs), " specialty/specialties with no ",
          "cases in the data:")
  for (e in empty_specs) message("    - ", e)
  # The loaded data is already filtered to `specialties`, so this lists which
  # of the configured names matched — not everything the workbook holds.
  message("  Configured names that did match: ",
          if (length(spec_counts) > 0) paste(sort(names(spec_counts)), collapse = ", ")
          else "(none)")
  message("  Check the spelling against the 'Surgical Specialty' column.")
  message(strrep("!", 65))
}

for (spec in setdiff(specialties, empty_specs)) {
  render_one(spec)
  if (render_slides) {
    render_slides_one(spec)
  }
}

# ---- Render division-level reports ------------------------------------------

if (has_mapping) {
  
  # Load data to discover divisions and their case counts. This shares the
  # on-disk cache with the renders, so the workbook is read once per run
  # rather than once per output.
  case_data <- load_report_data(
    data_file            = data_file,
    site_sar_file        = if (is.null(site_sar_file)) "" else site_sar_file,
    surgeon_mapping_file = surgeon_mapping_file,
    benchmark_type       = benchmark_type,
    specialties          = specialties
  )$case_data
  
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
      if (render_slides) {
        render_slides_one(spec, div)
      }
    }
  }
}

# ---- Done -------------------------------------------------------------------

n_total    <- length(.render_log$rows)
failures   <- .render_failures()
n_failed   <- length(failures)
n_ok       <- n_total - n_failed

message("\n", strrep("=", 65))
if (n_failed == 0) {
  message("  Report generation complete: ", n_ok, "/", n_total, " succeeded.")
  message("  Check '", output_dir, "/' for PDFs.")
  message(strrep("=", 65))
} else {
  message("  Report generation INCOMPLETE: ", n_ok, "/", n_total,
          " succeeded, ", n_failed, " failed.")
  message(strrep("-", 65))
  for (f in failures) message("    \u2717 ", f$label, " [", f$kind, "]")
  message(strrep("-", 65))
  message("  Any older PDF for a failed report is still in '", output_dir,
          "/' and is now stale.")
  message(strrep("=", 65))

  # Non-zero exit for Rscript and cron; harmless when sourced in RStudio.
  if (!interactive()) quit(status = 1)
}
