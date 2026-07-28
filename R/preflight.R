# =============================================================================
# preflight.R
#
# Verifies the LaTeX environment BEFORE rendering, so a missing package
# produces one clear message instead of a wall of lualatex output.
#
# Two failure modes this catches:
#   1. Missing .sty files (BasicTeX ships minimal; kableExtra needs a lot)
#   2. Stale TeX Live — a new TeX Live is released each March, and tlmgr
#      refuses cross-release installs, so an older tree silently cannot
#      install anything new.
#
# Usage: source("R/preflight.R"); preflight_latex()
# =============================================================================

# Every .sty the report and slides require, including transitive
# dependencies that tlmgr does NOT resolve automatically.
#
# Note: ltablex is loaded internally by xltabular and is NOT declared as a
# TeX Live dependency of it — this is the one that most recently broke a run.
# tabularx ships inside the `tools` bundle, not as its own package.
REQUIRED_STY <- c(
  # Core document
  "scrartcl.cls", "geometry.sty", "fancyhdr.sty", "xcolor.sty",
  "graphicx.sty", "amsmath.sty", "amssymb.sty", "etoolbox.sty",
  "caption.sty", "subcaption.sty", "microtype.sty", "parskip.sty",
  "bookmark.sty", "footnotehyper.sty", "upquote.sty", "xurl.sty",
  "unicode-math.sty", "lmodern.sty",
  # Tables — the kableExtra stack
  "longtable.sty", "booktabs.sty", "array.sty", "colortbl.sty",
  "multirow.sty", "wrapfig.sty", "float.sty", "pdflscape.sty",
  "tabularx.sty", "xltabular.sty", "ltablex.sty",
  "threeparttable.sty", "threeparttablex.sty", "makecell.sty",
  "ulem.sty", "environ.sty", "trimspaces.sty", "varwidth.sty"
)

# Map .sty -> the TeX Live package that provides it, where the names differ.
STY_TO_PKG <- c(
  "scrartcl.cls"    = "koma-script",
  "tabularx.sty"    = "tools",          # already present in any install
  "unicode-math.sty"= "unicode-math",
  "lmodern.sty"     = "lm"
)


#' Check the LaTeX environment and stop with actionable guidance if broken.
#'
#' @param strict If TRUE (default), stop on missing packages. If FALSE,
#'   warn only and let the render proceed.
preflight_latex <- function(strict = TRUE) {

  message("\n--- LaTeX preflight ---")

  # ---- 1. Is a TeX distribution reachable at all? -------------------------
  tlmgr_path <- Sys.which("tlmgr")
  if (nchar(tlmgr_path) == 0) {
    stop("No TeX distribution found on PATH.\n",
         "  Install one with: tinytex::install_tinytex()")
  }

  # ---- 2. Which distribution, and is it current? --------------------------
  ver_out <- tryCatch(
    system2("tlmgr", "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) character(0)
  )

  tl_year <- suppressWarnings(
    as.integer(sub(".*version\\s+(\\d{4}).*", "\\1",
                   grep("version\\s+\\d{4}", ver_out, value = TRUE)[1]))
  )
  tl_root <- sub(".*installation:\\s*", "",
                 grep("installation:", ver_out, value = TRUE)[1])

  if (!is.na(tl_year)) {
    message("  TeX Live ", tl_year, "  (", tl_root, ")")

    # TeX Live releases each March. If we're past March and the tree is
    # from a prior year, tlmgr can no longer install from the live repo.
    now <- Sys.Date()
    current_release <- as.integer(format(now, "%Y")) -
      if (as.integer(format(now, "%m")) < 4) 1 else 0

    if (tl_year < current_release) {
      message("\n  !! TeX Live ", tl_year, " is older than the current ",
              "release (", current_release, ").")
      message("     tlmgr cannot install packages across releases, so any ",
              "missing\n     package will fail silently. Upgrade before ",
              "troubleshooting anything else:")
      message("       tinytex::install_tinytex(force = TRUE)   # or reinstall MacTeX/BasicTeX")
    }
  }

  # ---- 3. Will automatic package installation actually work? --------------
  needs_sudo <- !is.na(tl_root) && grepl("^/usr/local|^/opt", tl_root)
  if (needs_sudo) {
    message("  Note: distribution lives in ", dirname(tl_root),
            " — tlmgr needs sudo.")
    message("        Quarto's auto-install runs unprivileged and will fail; ",
            "install\n        missing packages manually (command shown below ",
            "if any are absent).")
  }

  # ---- 4. Are all required files present? ---------------------------------
  found <- vapply(REQUIRED_STY, function(f) {
    out <- suppressWarnings(
      system2("kpsewhich", shQuote(f), stdout = TRUE, stderr = FALSE)
    )
    length(out) > 0 && nchar(out[1]) > 0
  }, logical(1))

  missing_sty <- REQUIRED_STY[!found]

  if (length(missing_sty) == 0) {
    message("  All ", length(REQUIRED_STY), " required LaTeX files found.")
    message("--- preflight OK ---\n")
    return(invisible(TRUE))
  }

  # ---- 5. Report what's missing, with the exact fix ------------------------
  pkgs <- vapply(missing_sty, function(f) {
    if (f %in% names(STY_TO_PKG)) STY_TO_PKG[[f]] else tools::file_path_sans_ext(f)
  }, character(1))
  pkgs <- unique(unname(pkgs))

  prefix <- if (needs_sudo) "sudo " else ""
  cmd <- paste0(prefix, "tlmgr install ", paste(pkgs, collapse = " "))

  msg <- paste0(
    "Missing ", length(missing_sty), " LaTeX file(s): ",
    paste(missing_sty, collapse = ", "), "\n\n",
    "  Install with:\n    ", cmd, "\n\n",
    "  If more turn up afterwards, install the whole bundle instead:\n    ",
    prefix, "tlmgr install collection-latexextra collection-fontsrecommended\n"
  )

  if (strict) stop(msg) else warning(msg)
  invisible(FALSE)
}
