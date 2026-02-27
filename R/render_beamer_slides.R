# =============================================================================
# render_beamer_slides.R
#
# Generates beamer slide deck directly from R — no Quarto/pandoc.
# Produces a complete .tex file and compiles with lualatex via tinytex.
#
# This avoids pandoc's beamer writer, which interferes with raw
# \begin{frame}...\end{frame} emitted from R chunks.
# =============================================================================

render_beamer_slides <- function(
    spec,
    div            = "",
    data_file,
    site_sar_file  = "",
    surgeon_mapping_file = "",
    benchmark_type = "site_expected",
    odds_ratio     = 2.0,
    target_arl     = 500,
    output_file    = NULL
) {

  # ---- Load helpers --------------------------------------------------------
  # (idempotent — safe to re-source)
  source("R/benchmarks.R",       local = FALSE)
  source("R/data_processing.R",  local = FALSE)
  source("R/cusum_functions.R",  local = FALSE)

  suppressPackageStartupMessages({
    library(knitr)
    library(kableExtra)
    library(ggplot2)
    library(dplyr)
    library(lubridate)
  })

  # ---- Helper: strip table float wrapper for beamer compatibility ----------
  kable_to_tex <- function(k) {
    s <- as.character(k)
    s <- sub("\\\\begin\\{table\\}[^\n]*\n", "", s)
    s <- sub("\\\\end\\{table\\}\n?", "", s)
    s
  }

  # ---- Load & prepare data -------------------------------------------------
  case_data <- process_case_details(
    filepath    = data_file,
    specialties = c("General Surgery", "Vascular", "Thoracic", "Plastics")
  )

  if (nchar(surgeon_mapping_file) > 0 && file.exists(surgeon_mapping_file)) {
    surgeon_map <- load_surgeon_mapping(surgeon_mapping_file)
    case_data <- assign_divisions(case_data, surgeon_map)
  } else {
    case_data$division <- NA_character_
  }

  case_data <- assign_procedure_categories(case_data)

  site_sar_available <- nchar(site_sar_file) > 0 && file.exists(site_sar_file)

  benchmark_rates <- get_benchmark_rates(
    site_sar_path  = if (site_sar_available) site_sar_file else NULL,
    benchmark_type = benchmark_type
  )

  ot_data <- NULL
  if (site_sar_available) {
    ot_data <- tryCatch(parse_over_time(site_sar_file), error = function(e) NULL)
  }

  targeted_data <- NULL
  if (site_sar_available) {
    targeted_data <- tryCatch(parse_targeted_sar(site_sar_file), error = function(e) NULL)
  }

  div_val <- if (nchar(div) > 0) div else NULL

  spec_data <- case_data |> filter(specialty == spec)
  if (!is.null(div_val)) {
    spec_data <- spec_data |> filter(division == div_val)
  }

  n_cases  <- nrow(spec_data)
  date_min <- min(spec_data$op_date, na.rm = TRUE)
  date_max <- max(spec_data$op_date, na.rm = TRUE)
  report_name <- if (!is.null(div_val)) paste0(spec, " --- ", div_val) else spec

  bench_desc <- if (benchmark_type == "site_expected" && site_sar_available) {
    "Site risk-adjusted expected rates (SAR/ISAR)"
  } else {
    "National observed rates (SAR)"
  }

  # ---- Pre-generate plots --------------------------------------------------
  plot_dir <- tempfile("slide_plots_")
  dir.create(plot_dir)
  on.exit(unlink(plot_dir, recursive = TRUE), add = TRUE)

  # Volume chart
  monthly <- spec_data |>
    mutate(month = floor_date(op_date, "month")) |>
    count(month, name = "cases")

  p_vol <- ggplot(monthly, aes(x = month, y = cases)) +
    geom_col(fill = "#4292c6", width = 25) +
    geom_text(aes(label = cases), vjust = -0.3, size = 3.5) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "1 month") +
    labs(x = NULL, y = "Cases",
         title = paste0("Monthly Case Volume")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(size = 14, face = "bold"))
  vol_file <- file.path(plot_dir, "volume.pdf")
  ggsave(vol_file, p_vol, width = 9.5, height = 3.5, device = "pdf")

  # CUSUM charts
  charts <- generate_specialty_charts(
    data = case_data, spec = spec, rates = benchmark_rates,
    odds_ratio = odds_ratio, div = div_val
  )
  chart_files <- character(length(charts))
  for (i in seq_along(charts)) {
    p <- charts[[i]] +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 9),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 9))
    f <- file.path(plot_dir, paste0("cusum_", i, ".pdf"))
    ggsave(f, p, width = 9.5, height = 3.5, device = "pdf")
    chart_files[i] <- f
  }

  # O/E trend plots
  oe_plots <- list()
  oe_files <- character(0)
  if (!is.null(ot_data)) {
    spec_ot <- ot_data |> filter(specialty == spec)
    if (nrow(spec_ot) > 0) {
      ni_comps <- benchmark_rates |>
        filter(specialty == spec, assessment == "Needs Improvement") |>
        pull(complication)
      ho_comps <- spec_ot |>
        filter(is_outlier_high) |> pull(complication) |> unique()
      priority_comps <- unique(c(ni_comps, ho_comps))

      for (comp in priority_comps) {
        comp_ot <- spec_ot |> filter(complication == comp) |> arrange(period)
        if (nrow(comp_ot) >= 2) {
          p <- plot_oe_trend(comp_ot, comp)
          if (!is.null(p)) {
            p <- p + theme(plot.title = element_text(size = 13, face = "bold"),
                           axis.text = element_text(size = 9),
                           axis.text.x = element_text(angle = 45, hjust = 1))
            oe_plots[[comp]] <- p
          }
        }
      }
      oe_files <- character(length(oe_plots))
      for (i in seq_along(oe_plots)) {
        f <- file.path(plot_dir, paste0("oe_", i, ".pdf"))
        ggsave(f, oe_plots[[i]], width = 9.5, height = 3.0, device = "pdf")
        oe_files[i] <- f
      }
    }
  }

  # ---- Escape helper -------------------------------------------------------
  tex_escape <- function(x) {
    x <- gsub("&", "\\\\&", x)
    x <- gsub("%", "\\\\%", x)
    x <- gsub("#", "\\\\#", x)
    x <- gsub("_", "\\\\_", x)
    x
  }

  # ---- Build .tex document -------------------------------------------------
  tex <- character()
  add <- function(...) tex <<- c(tex, paste0(...))

  # -- Preamble --
  add("\\documentclass[9pt,aspectratio=169]{beamer}")
  add("\\usetheme{default}")
  add("\\usecolortheme{seahorse}")
  add("\\usefonttheme{professionalfonts}")
  add("")
  add("\\usepackage{booktabs}")
  add("\\usepackage{longtable}")
  add("\\usepackage{colortbl}")
  add("\\usepackage{graphicx}")
  add("\\usepackage{amsmath}")
  add("")
  add("\\definecolor{sigred}{HTML}{CB181D}")
  add("\\definecolor{okgreen}{HTML}{238B45}")
  add("\\definecolor{vcublue}{HTML}{003366}")
  add("\\definecolor{vcugold}{HTML}{FFB300}")
  add("\\definecolor{lightgray}{HTML}{F0F0F0}")
  add("")
  add("\\setbeamercolor{frametitle}{bg=vcublue,fg=white}")
  add("\\setbeamercolor{title}{bg=vcublue,fg=white}")
  add("\\setbeamercolor{subtitle}{fg=vcugold}")
  add("\\setbeamercolor{structure}{fg=vcublue}")
  add("\\setbeamercolor{section in toc}{fg=vcublue}")
  add("\\setbeamercolor{footline}{fg=gray}")
  add("\\setbeamerfont{frametitle}{size=\\large,series=\\bfseries}")
  add("\\setbeamerfont{title}{size=\\Large,series=\\bfseries}")
  add("\\setbeamertemplate{navigation symbols}{}")
  add("\\setbeamertemplate{footline}{\\hfill\\insertframenumber\\,/\\,\\inserttotalframenumber\\kern1em\\vskip2pt}")
  add("\\setbeamertemplate{itemize items}[circle]")
  add("\\renewcommand{\\arraystretch}{1.1}")
  add("")
  add("\\title{NSQIP CUSUM Outcome Monitoring}")
  add("\\subtitle{", tex_escape(report_name), "}")
  add("\\author{Surgical Quality Improvement}")
  add("\\date{", format(Sys.Date(), "%B %d, %Y"), "}")
  add("")
  add("\\begin{document}")
  add("")
  add("\\begin{frame}")
  add("\\titlepage")
  add("\\end{frame}")
  add("")

  # -- Report Summary --
  add("\\section{Overview}")
  add("")
  add("\\begin{frame}{Report Summary}")
  add("\\textbf{\\large ", tex_escape(report_name), "}")
  add("")
  add("\\vspace{0.4cm}")
  add("")
  add("\\begin{tabular}{ll}")
  add("\\textbf{Period:} & ", format(date_min, "%b %d, %Y"), " -- ",
      format(date_max, "%b %d, %Y"), " \\\\")
  add("\\textbf{Cases:} & ", n_cases, " \\\\")
  add("\\textbf{Benchmark:} & ", bench_desc, " \\\\")
  add("\\textbf{CUSUM OR:} & ", odds_ratio,
      "$\\times$ \\quad ARL$_0$ $\\approx$ ", target_arl, " \\\\")
  add("\\end{tabular}")
  add("\\end{frame}")
  add("")

  # -- Volume --
  add("\\begin{frame}{Monthly Case Volume}")
  add("\\centering")
  add("\\includegraphics[width=0.95\\textwidth]{", vol_file, "}")
  add("\\end{frame}")
  add("")

  # -- SAR Context --
  add("\\section{SAR Context}")
  add("")
  add("\\begin{frame}{Prior SAR Performance}")
  if (site_sar_available) {
    site_sar_data <- parse_site_sar(site_sar_file) |> filter(specialty == spec)
    if (nrow(site_sar_data) > 0) {
      sar_tbl <- site_sar_data |>
        mutate(obs_pct = round(observed_rate_sar * 100, 2),
               exp_pct = round(expected_rate * 100, 2),
               oe = round(odds_ratio_sar, 2),
               pctl = round(adj_percentile)) |>
        select(Complication = complication, `Obs %` = obs_pct, `Exp %` = exp_pct,
               `O/E` = oe, Pctl = pctl, Assessment = assessment)

      k <- kable(sar_tbl, format = "latex", booktabs = TRUE,
                  align = c("l","r","r","r","r","l"), linesep = "") |>
        kable_styling(font_size = 6, latex_options = c("scale_down"))
      for (i in seq_len(nrow(sar_tbl))) {
        if (sar_tbl$Assessment[i] == "Needs Improvement") {
          k <- k |> row_spec(i, color = "sigred")
        } else if (sar_tbl$Assessment[i] == "Exemplary") {
          k <- k |> row_spec(i, color = "okgreen")
        }
      }
      add(kable_to_tex(k))

      ni_comps <- site_sar_data |>
        filter(assessment == "Needs Improvement") |> pull(complication)
      if (length(ni_comps) > 0) {
        add("\\vspace{0.2cm}")
        add("\\textcolor{sigred}{\\textbf{Needs Improvement (",
            length(ni_comps), "):}} ", paste(ni_comps, collapse = ", "))
      }
    }
  } else {
    add("Site SAR not available.")
  }
  add("\\end{frame}")
  add("")

  # -- Current Performance: Observed vs Benchmarks --
  add("\\section{Current Performance}")
  add("")
  add("\\begin{frame}{Observed Rates vs.\\ Benchmarks}")

  obs_summary <- observed_rates_summary(case_data, spec, div_val)
  bench_for_spec <- benchmark_rates |>
    filter(specialty == spec) |>
    select(complication, p0_pct, benchmark_source)
  summary_tbl <- obs_summary |>
    left_join(bench_for_spec, by = "complication") |>
    mutate(benchmark_pct = round(p0_pct, 2),
           ratio = ifelse(p0_pct > 0, round(observed_rate_pct / p0_pct, 2), NA)) |>
    select(Complication = complication, N = n_cases, Events = n_events,
           `Obs %` = observed_rate_pct, `Bench %` = benchmark_pct, `O/B` = ratio)

  k <- kable(summary_tbl, format = "latex", booktabs = TRUE,
             align = c("l","r","r","r","r","r"), linesep = "") |>
    kable_styling(font_size = 6, latex_options = c("scale_down"))
  add(kable_to_tex(k))
  add("\\end{frame}")
  add("")

  # -- Monthly Dashboard --
  add("\\begin{frame}{Monthly Complication Dashboard}")
  dashboard <- build_dashboard(case_data, spec, div_val, benchmark_rates)
  if (!is.null(dashboard)) {
    dtbl_display <- dashboard$table
    month_cols <- dashboard$months
    for (mc in c(month_cols, "Total")) {
      dtbl_display[[mc]] <- as.character(dtbl_display[[mc]])
    }
    dtbl_display$`Rate %` <- ifelse(
      is.na(dashboard$table$`Rate %`), "",
      format(dashboard$table$`Rate %`, nsmall = 1))
    dtbl_display$`SAR %` <- ifelse(
      is.na(dashboard$table$`SAR %`), "",
      format(dashboard$table$`SAR %`, nsmall = 1))

    k <- kable(dtbl_display, format = "latex", booktabs = TRUE,
               align = c("l", rep("c", length(month_cols)), "r", "r", "r"),
               linesep = "") |>
      kable_styling(font_size = 5, latex_options = c("scale_down")) |>
      row_spec(1, bold = TRUE, background = "lightgray") |>
      row_spec(2, bold = TRUE)
    for (g in names(dashboard$pack_rows)) {
      k <- k |> pack_rows(g, dashboard$pack_rows[[g]][1], dashboard$pack_rows[[g]][2],
                           bold = TRUE, italic = FALSE,
                           hline_before = TRUE, hline_after = FALSE,
                           latex_gap_space = "0.15em")
    }
    add(kable_to_tex(k))
  }
  add("\\end{frame}")
  add("")

  # -- CUSUM Charts --
  add("\\section{CUSUM Charts}")
  add("")
  add("\\begin{frame}{CUSUM Monitoring Overview}")
  add("\\begin{itemize}")
  add("\\item ", length(charts), " complications monitored with Bernoulli CUSUM")
  add("\\item Benchmark: ", bench_desc)
  add("\\item Detection target: OR = ", odds_ratio, "$\\times$ relative to benchmark")
  add("\\item Decision boundary calibrated for ARL$_0$ $\\approx$ ", target_arl)
  add("\\end{itemize}")
  add("\\end{frame}")
  add("")

  for (i in seq_along(chart_files)) {
    chart_name <- tex_escape(names(charts)[i])
    add("\\begin{frame}{", chart_name, "}")
    add("\\centering")
    add("\\includegraphics[width=\\textwidth]{", chart_files[i], "}")
    add("\\end{frame}")
    add("")
  }

  # -- O/E Trends --
  if (length(oe_plots) > 0) {
    add("\\section{O/E Trends}")
    add("")
    for (i in seq_along(oe_plots)) {
      comp_name <- names(oe_plots)[i]
      add("\\begin{frame}{O/E Trend: ", comp_name, "}")
      if (i == 1) {
        if (!is.null(div_val)) {
          add("\\small Historical O/E ratios from the \\textbf{",
              tex_escape(spec), "} SAR model (specialty-level). ",
              "Red dots = high outlier.\\\\[0.3em]")
        } else {
          add("\\small Red dots indicate periods flagged as high outlier.\\\\[0.3em]")
        }
      }
      add("\\centering")
      add("\\includegraphics[width=\\textwidth]{", oe_files[i], "}")
      add("\\end{frame}")
      add("")
    }
  }

  # -- Appendix: Procedure Mix --
  add("\\section{Appendix}")
  add("")

  proc_mix <- build_procedure_mix(case_data, spec, div_val)
  if (!is.null(proc_mix) && nrow(proc_mix) > 0) {
    add("\\begin{frame}{Procedure Mix Profile}")
    k <- kable(proc_mix, format = "latex", booktabs = TRUE,
               align = c("l","r","r","r","r","r"), linesep = "") |>
      kable_styling(font_size = 6, latex_options = c("scale_down"))
    add(kable_to_tex(k))
    add("\\end{frame}")
    add("")
  }

  # -- Appendix: Targeted SAR --
  if (!is.null(targeted_data)) {
    tgt_summary <- build_targeted_summary(targeted_data, case_data, spec, div_val)
    div_proc_rates <- build_division_procedure_rates(
      case_data, spec, div_val, targeted_data)

    if (!is.null(tgt_summary) && nrow(tgt_summary) > 0) {
      for (proc in unique(tgt_summary$Procedure)) {
        proc_sar <- tgt_summary |> filter(Procedure == proc) |> select(-Procedure)
        proc_div <- div_proc_rates[[proc]]

        add("\\begin{frame}{Targeted: ", tex_escape(proc), "}")
        if (!is.null(proc_div)) {
          merged <- proc_sar |>
            left_join(
              proc_div |> select(Complication, `Div N` = N,
                                 `Div Events` = Events, `Div Obs %` = `Obs %`),
              by = "Complication")
          k <- kable(merged, format = "latex", booktabs = TRUE,
                     align = c("l","r","r","r","r","r","l","r","r","r"),
                     linesep = "") |>
            kable_styling(font_size = 5, latex_options = c("scale_down")) |>
            add_header_above(c(" " = 1,
                               "Site SAR (Risk-Adjusted)" = 6,
                               "Division (Unadjusted)" = 3))
        } else {
          k <- kable(proc_sar, format = "latex", booktabs = TRUE,
                     align = c("l","r","r","r","r","r","l"),
                     linesep = "") |>
            kable_styling(font_size = 6, latex_options = c("scale_down"))
        }
        add(kable_to_tex(k))
        add("\\end{frame}")
        add("")
      }
    }
  }

  # -- Closing slide --
  add("\\begin{frame}[plain]")
  add("\\begin{center}")
  add("\\vspace{1.5cm}")
  add("{\\Large\\bfseries\\color{vcublue} NSQIP CUSUM Monitoring System v1.4}")
  add("")
  add("\\vspace{0.5cm}")
  add("")
  add("{\\normalsize ", tex_escape(report_name), "}")
  add("")
  add("\\vspace{0.3cm}")
  add("")
  add("{\\small ", bench_desc, "}")
  add("")
  add("\\vspace{0.3cm}")
  add("")
  add("{\\small Report generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), "}")
  add("")
  add("\\vspace{1cm}")
  add("")
  add("{\\footnotesize\\color{gray} Surgical Quality Improvement Program}")
  add("\\end{center}")
  add("\\end{frame}")
  add("")
  add("\\end{document}")

  # ---- Write .tex and compile ----------------------------------------------
  tex_file <- tempfile(fileext = ".tex")
  writeLines(tex, tex_file)

  # Compile with lualatex (two passes for page numbers)
  tryCatch({
    tinytex::lualatex(tex_file)

    # Move PDF to output location
    pdf_file <- sub("\\.tex$", ".pdf", tex_file)
    if (file.exists(pdf_file) && !is.null(output_file)) {
      file.copy(pdf_file, output_file, overwrite = TRUE)
    }
    invisible(output_file)
  }, finally = {
    # Cleanup temp .tex and aux files
    for (ext in c(".tex", ".aux", ".log", ".nav", ".snm", ".toc", ".out")) {
      tmp <- sub("\\.tex$", ext, tex_file)
      if (file.exists(tmp)) file.remove(tmp)
    }
  })
}
