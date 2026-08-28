# =============================================================================
# Release version
#
# One definition, because there were two: the PDF report footer and the slide
# deck's closing frame each carried their own hard-coded literal, and both sat
# at v1.4 through the v1.5.0 release. A version string that has to be updated
# in more than one place is a version string that goes stale.
#
# Bump this alongside the Version History section in README.md.
# =============================================================================

NSQIP_CUSUM_VERSION <- "1.5.0"

#' Version string as it appears in report and deck footers
nsqip_version_label <- function() {
  paste0("NSQIP CUSUM Monitoring System v", NSQIP_CUSUM_VERSION)
}
