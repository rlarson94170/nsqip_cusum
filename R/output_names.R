# =============================================================================
# Output file naming
#
# One definition, because there were nearly four: the report renderer, the deck
# renderer, and the summary renderer each build the same stem, and the summary's
# footer names the accompanying report file so the reader knows what to go and
# open. That last one is the reason this lives in R/ rather than inside
# render_reports.R — Quarto renders in a separate process and can only reach
# what the template sources, so a copy in the .qmd would be a copy that drifts,
# and the failure mode is a handout pointing at a filename that does not exist.
# =============================================================================

#' Output filename stem for one reporting scope
#'
#' @param spec Specialty name
#' @param div Division name, or "" for specialty level
#' @param date Report date; the stem carries it as YYYYMMDD
#' @return Character stem, without extension or output suffix
scope_base_name <- function(spec, div = "", date = Sys.Date()) {
  spec_clean <- gsub("[/ ]", "_", tolower(spec))
  div_clean  <- gsub("[/ ]", "_", tolower(div))
  stamp      <- format(date, "%Y%m%d")
  if (nchar(div) > 0) {
    paste0("NSQIP_CUSUM_", spec_clean, "_", div_clean, "_", stamp)
  } else {
    paste0("NSQIP_CUSUM_", spec_clean, "_", stamp)
  }
}

#' Human-readable label for one reporting scope
scope_label <- function(spec, div = "") {
  if (nchar(div) > 0) paste0(spec, " — ", div) else spec
}

#' Escape a filename for literal display in LaTeX
#'
#' These stems are full of underscores, which are subscript operators in LaTeX.
latex_escape_filename <- function(x) gsub("_", "\\\\_", x)
