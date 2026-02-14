# =============================================================================
# Bernoulli CUSUM Functions for Surgical Outcome Monitoring
#
# Implements the log-likelihood ratio CUSUM for binary outcomes.
# Enhanced with site SAR context: O/E trend sparklines, assessment badges.
# =============================================================================

library(ggplot2)
library(scales)
library(dplyr)

#' Compute Bernoulli CUSUM for a binary outcome vector
#'
#' @param outcomes Integer vector of 0/1 outcomes in chronological order
#' @param p0 Acceptable (expected) event rate (proportion, not %)
#' @param odds_ratio The odds ratio for p1 relative to p0 (default 2.0)
#' @param h Decision boundary. If NULL, auto-calibrated.
#' @param reset Logical: reset to 0 after crossing h?
#' @return A tibble with columns: case_num, outcome, score, cusum, signal, h
compute_cusum <- function(outcomes, p0, odds_ratio = 2.0, h = NULL, reset = TRUE) {
  
  n <- length(outcomes)
  
  or0 <- p0 / (1 - p0)
  or1 <- odds_ratio * or0
  p1 <- or1 / (1 + or1)
  
  s_event   <- log((p1 * (1 - p0)) / (p0 * (1 - p1)))
  s_noevent <- log((1 - p1) / (1 - p0))
  
  if (is.null(h)) {
    h <- calibrate_h(p0, p1, target_arl = 500)
  }
  
  # Vectorize scores, then loop only for the cumulative max(0, ...) with reset
  scores <- ifelse(outcomes == 1, s_event, s_noevent)
  
  cusum_vals <- numeric(n)
  signals    <- logical(n)
  
  C_prev <- 0
  for (i in seq_len(n)) {
    C_prev <- max(0, C_prev + scores[i])
    cusum_vals[i] <- C_prev
    if (C_prev >= h) {
      signals[i] <- TRUE
      if (reset) C_prev <- 0
    }
  }
  
  tibble(
    case_num = 1:n, outcome = outcomes, score = scores,
    cusum = cusum_vals, signal = signals, h = h, p0 = p0, p1 = p1
  )
}


#' Calibrate h for a target ARL₀ ≈ 500 with odds ratio = 2
#'
#' Uses a pre-computed lookup table derived from Monte Carlo simulation
#' (1000 runs × 3000 cases, cumsum/cummin vectorized method).
#' This eliminates all simulation at runtime — h is returned instantly.
#'
#' The table was generated for OR=2.0 and ARL₀≈500.
#' For p₀ < 4.5%, h=4.5 is appropriate because at these low rates,
#' individual events carry substantial weight in the log-likelihood.
#'
#' @param p0 Acceptable rate (proportion)
#' @param p1 Unacceptable rate (proportion, used only for validation)
#' @param target_arl Target ARL (default 500; used only for documentation)
#' @return Calibrated h value
calibrate_h <- function(p0, p1, target_arl = 500) {
  
  # Lookup table: h as a function of p0, for OR=2, ARL₀≈500
  # Generated via vectorized Monte Carlo (1000 sims × 3000 cases)
  if      (p0 < 0.045) h <- 4.5
  else if (p0 < 0.070) h <- 2.5
  else if (p0 < 0.095) h <- 3.0
  else if (p0 < 0.115) h <- 3.5
  else if (p0 < 0.135) h <- 4.0
  else                  h <- 4.5
  
  h
}


#' Create a CUSUM chart with SAR context
#'
#' @param cusum_data Output from compute_cusum()
#' @param specialty_name Display name
#' @param complication_name Display name
#' @param dates Date vector aligned with cases
#' @param benchmark_source Description of benchmark used
#' @param assessment SAR assessment string (optional)
#' @param sar_oe SAR odds ratio (optional)
#' @param sar_percentile SAR adjusted percentile (optional)
#' @return A ggplot object
plot_cusum <- function(cusum_data, specialty_name, complication_name,
                       dates = NULL, benchmark_source = "",
                       assessment = NA, sar_oe = NA, sar_percentile = NA) {
  
  h_val     <- cusum_data$h[1]
  p0_pct    <- round(cusum_data$p0[1] * 100, 2)
  p1_pct    <- round(cusum_data$p1[1] * 100, 2)
  n_events  <- sum(cusum_data$outcome)
  n_cases   <- nrow(cusum_data)
  obs_rate  <- round(n_events / n_cases * 100, 2)
  n_signals <- sum(cusum_data$signal)
  
  plot_df <- cusum_data
  if (!is.null(dates)) plot_df$date <- dates
  
  # Build subtitle
  sub_parts <- paste0(
    "p\u2080 = ", p0_pct, "%  |  p\u2081 = ", p1_pct,
    "%  |  Observed: ", obs_rate, "% (", n_events, "/", n_cases, ")"
  )
  
  # Benchmark type indicator
  if (grepl("Site expected", benchmark_source)) {
    sub_parts <- paste0(sub_parts, "  |  Benchmark: site risk-adjusted")
  } else if (grepl("ALLCASES", benchmark_source)) {
    sub_parts <- paste0(sub_parts, "  |  Benchmark: ALLCASES (no specialty model)")
  }
  
  # SAR assessment badge
  if (!is.na(assessment)) {
    sar_info <- paste0("SAR: ", assessment)
    if (!is.na(sar_oe)) {
      sar_info <- paste0(sar_info, " (O/E=", round(sar_oe, 2), ")")
    }
    if (!is.na(sar_percentile)) {
      sar_info <- paste0(sar_info, " [P", round(sar_percentile), "]")
    }
    sub_parts <- paste0(sub_parts, "\n", sar_info)
  }
  
  # Base plot
  if (!is.null(dates)) {
    p <- ggplot(plot_df, aes(x = date, y = cusum)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
      labs(x = "Operation Date")
  } else {
    p <- ggplot(plot_df, aes(x = case_num, y = cusum)) +
      labs(x = "Case Number")
  }
  
  p <- p +
    geom_line(color = "#2171b5", linewidth = 0.6) +
    geom_hline(yintercept = h_val, linetype = "dashed", color = "#cb181d",
               linewidth = 0.7)
  
  # Decision boundary label
  x_pos <- if (!is.null(dates)) min(dates, na.rm = TRUE) else 1
  p <- p +
    annotate("text", x = x_pos, y = h_val + 0.2,
             label = paste0("h = ", round(h_val, 2)),
             hjust = 0, vjust = 0, size = 2.8, color = "#cb181d")
  
  # Signal markers
  signal_df <- plot_df |> filter(signal)
  if (nrow(signal_df) > 0) {
    p <- p + geom_point(data = signal_df, color = "#cb181d", size = 2.5, shape = 17)
  }
  
  # Event tick marks along bottom
  event_df <- plot_df |> filter(outcome == 1)
  if (nrow(event_df) > 0) {
    p <- p + geom_point(data = event_df, y = -0.1, color = "#fd8d3c",
                        size = 1.2, shape = "|", stroke = 0.8)
  }
  
  # Assessment-based title color
  title_color <- "black"
  if (!is.na(assessment)) {
    if (assessment == "Needs Improvement") title_color <- "#cb181d"
    else if (assessment == "Exemplary") title_color <- "#238b45"
  }
  
  p <- p +
    labs(title = complication_name, subtitle = sub_parts, y = "CUSUM Statistic") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold", size = 11, color = title_color),
      plot.subtitle = element_text(size = 7.5, color = "gray40", lineheight = 1.2),
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y   = element_text(size = 8),
      panel.grid.minor = element_blank(),
      plot.margin = margin(5, 10, 5, 5)
    )
  
  p
}


#' Create an O/E trend sparkline chart for a specialty-complication pair
#'
#' @param ot_data Over-time data from parse_over_time(), filtered to one pair
#' @return A ggplot or NULL if insufficient data
plot_oe_trend <- function(ot_data, complication_name) {
  
  if (nrow(ot_data) < 2) return(NULL)
  
  ot_data <- ot_data |>
    mutate(period_idx = row_number())
  
  p <- ggplot(ot_data, aes(x = period_idx, y = oe_ratio)) +
    geom_hline(yintercept = 1.0, linetype = "solid", color = "gray70", linewidth = 0.3) +
    geom_line(color = "#2171b5", linewidth = 0.5) +
    geom_point(aes(color = ifelse(is_outlier_high, "High", "Normal")),
               size = 1.5, show.legend = FALSE) +
    scale_color_manual(values = c("High" = "#cb181d", "Normal" = "#2171b5")) +
    scale_x_continuous(
      breaks = ot_data$period_idx,
      labels = ot_data$period
    ) +
    labs(
      title = paste0(complication_name, " — O/E Trend (SAR Periods)"),
      x = NULL, y = "O/E Ratio"
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(size = 9, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      panel.grid.minor = element_blank(),
      plot.margin = margin(2, 5, 2, 2)
    )
  
  p
}


#' Generate all CUSUM charts for a given specialty (and optionally division)
#'
#' @param data Processed case data
#' @param spec Specialty name
#' @param rates Benchmark rates tibble (from get_benchmark_rates)
#' @param odds_ratio Odds ratio for p1 (default 2)
#' @param div Optional division name (NULL for all cases in specialty)
#' @return A named list of ggplot objects
generate_specialty_charts <- function(data, spec, rates, odds_ratio = 2.0,
                                      div = NULL) {
  
  spec_data <- data |> filter(specialty == spec) |> arrange(op_date)
  if (!is.null(div) && nchar(div) > 0) {
    spec_data <- spec_data |> filter(division == div)
  }
  
  display_name <- if (!is.null(div) && nchar(div) > 0) {
    paste0(spec, " — ", div)
  } else {
    spec
  }
  
  if (nrow(spec_data) == 0) {
    warning("No cases for: ", display_name)
    return(list())
  }
  
  comp_var_to_label <- c(
    mortality = "Mortality", morbidity = "Morbidity",
    cardiac = "Cardiac", pneumonia = "Pneumonia",
    unplanned_intubation = "Unplanned Intubation",
    vent48 = "Ventilator > 48h", vte = "VTE",
    renal_failure = "Renal Failure", uti = "UTI",
    ssi = "SSI", sepsis = "Sepsis", cdiff = "C.diff Colitis",
    unplanned_reop = "Unplanned Reoperation",
    unplanned_readmit = "Unplanned Readmission"
  )
  
  charts <- list()
  
  for (var_name in names(comp_var_to_label)) {
    comp_label <- comp_var_to_label[var_name]
    
    rate_info <- rates |> filter(specialty == spec, complication == comp_label)
    if (nrow(rate_info) == 0 || is.na(rate_info$p0)) next
    
    p0 <- rate_info$p0
    if (p0 < 0.0001) {
      message("  Skipping ", comp_label, " for ", spec, " (rate < 0.01%)")
      next
    }
    
    outcomes <- spec_data[[var_name]]
    
    message("  CUSUM: ", display_name, " — ", comp_label,
            " (p0=", round(p0*100, 2), "%, events=", sum(outcomes),
            "/", length(outcomes), ")")
    
    cusum_result <- compute_cusum(
      outcomes   = outcomes,
      p0         = p0,
      odds_ratio = odds_ratio,
      h          = NULL,
      reset      = TRUE
    )
    
    # Extract SAR context
    assessment     <- if ("assessment" %in% names(rate_info)) rate_info$assessment else NA
    sar_oe         <- if ("odds_ratio_sar" %in% names(rate_info)) rate_info$odds_ratio_sar else NA
    sar_percentile <- if ("adj_percentile" %in% names(rate_info)) rate_info$adj_percentile else NA
    
    chart <- plot_cusum(
      cusum_data       = cusum_result,
      specialty_name   = display_name,
      complication_name = comp_label,
      dates            = spec_data$op_date,
      benchmark_source = rate_info$benchmark_source,
      assessment       = assessment,
      sar_oe           = sar_oe,
      sar_percentile   = sar_percentile
    )
    
    charts[[var_name]] <- chart
  }
  
  charts
}
