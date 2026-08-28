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
#' Supports a per-case p0 (risk-adjusted CUSUM, Steiner et al. 2000): when p0
#' is a vector, each case is scored against its own expected rate, so a chart
#' can mix procedure cohorts with different baseline risk.
#'
#' @param outcomes Integer vector of 0/1 outcomes in chronological order
#' @param p0 Acceptable (expected) event rate as a proportion. Either a single
#'   value applied to every case, or a vector of length(outcomes) giving each
#'   case its own expected rate.
#' @param odds_ratio The odds ratio for p1 relative to p0 (default 2.0)
#' @param h Decision boundary. If NULL, calibrated by simulation.
#' @param reset Logical: reset to 0 after crossing h?
#' @param target_arl Target in-control ARL used when h is calibrated
#' @return A tibble with columns: case_num, outcome, score, cusum, signal, h,
#'   p0, p1. Attributes `arl0` and `events_to_signal` carry the achieved
#'   in-control ARL and the minimum number of consecutive events that will
#'   drive the statistic from 0 across h.
compute_cusum <- function(outcomes, p0, odds_ratio = 2.0, h = NULL,
                          reset = TRUE, target_arl = 500) {

  n <- length(outcomes)
  if (n == 0) stop("compute_cusum(): `outcomes` is empty.")

  if (length(p0) == 1) p0 <- rep(p0, n)
  if (length(p0) != n) {
    stop("compute_cusum(): `p0` must be length 1 or length(outcomes) (",
         n, "), got ", length(p0), ".")
  }
  if (anyNA(p0)) stop("compute_cusum(): `p0` contains NA.")
  if (any(p0 <= 0 | p0 >= 1)) {
    stop("compute_cusum(): `p0` must be strictly between 0 and 1.")
  }

  or0 <- p0 / (1 - p0)
  or1 <- odds_ratio * or0
  p1  <- or1 / (1 + or1)

  s_event   <- log((p1 * (1 - p0)) / (p0 * (1 - p1)))
  s_noevent <- log((1 - p1) / (1 - p0))

  if (is.null(h)) {
    h <- calibrate_h(p0, odds_ratio = odds_ratio, target_arl = target_arl)
  }

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

  out <- tibble(
    case_num = seq_len(n), outcome = outcomes, score = scores,
    cusum = cusum_vals, signal = signals, h = h, p0 = p0, p1 = p1
  )

  attr(out, "arl0")             <- attr(h, "arl0")
  attr(out, "events_to_signal") <- ceiling(as.numeric(h) / mean(s_event))
  out
}


#' Calibrate the decision boundary h to a target in-control ARL
#'
#' Replaces the previous hard-coded lookup table, which was generated only for
#' OR = 2 and was badly miscalibrated for p0 below 4.5%: it returned h = 4.5
#' there, giving an in-control ARL of 4,000–29,000 rather than the documented
#' 500. Because most monitored complications sit in that range (mortality,
#' cardiac, VTE, pneumonia are all ~0.5–1.5%), those charts could not
#' realistically signal.
#'
#' Method: simulate in-control case series by drawing expected rates i.i.d.
#' from the empirical distribution of `p0` — i.e. future cases from the same
#' case mix, not this exact past order — accumulate the CUSUM, and record the
#' running maximum. Because the run length to a first signal is a monotone
#' step function of h over a fixed set of paths, every candidate h is
#' evaluated against the same simulated paths and the search is exact.
#' Resetting after a signal does not affect the first crossing, so it is
#' ignored here.
#'
#' A caution the caller should propagate: an ARL measured in *cases* is a weak
#' guarantee for a rare complication. At p0 = 0.6%, ARL0 = 500 cases means
#' signalling on roughly two events, because only three are expected in that
#' span. Check `attr(h, "events_to_signal")` before treating a low-rate chart
#' as a calibrated alarm rather than a description.
#'
#' @param p0 Expected rate(s) as proportions — scalar or per-case vector
#' @param odds_ratio Odds ratio defining p1 (default 2.0)
#' @param target_arl Target in-control ARL, in cases (default 500)
#' @param n_sim Number of simulated series (default 400)
#' @param seed RNG seed, so a given report is reproducible
#' @return Calibrated h, with attributes `arl0`, `events_to_signal` and
#'   `censored` (fraction of paths that never signalled)
calibrate_h <- function(p0, odds_ratio = 2.0, target_arl = 500,
                        n_sim = 400, seed = 20260101) {

  p0 <- p0[!is.na(p0)]
  if (length(p0) == 0) stop("calibrate_h(): no usable p0 values.")
  if (any(p0 <= 0 | p0 >= 1)) {
    stop("calibrate_h(): `p0` must be strictly between 0 and 1.")
  }

  key <- .h_cache_key(p0, odds_ratio, target_arl, n_sim)
  cached <- .h_cache[[key]]
  if (!is.null(cached)) return(cached)

  or1 <- odds_ratio * (p0 / (1 - p0))
  p1  <- or1 / (1 + or1)
  s_event   <- log((p1 * (1 - p0)) / (p0 * (1 - p1)))
  s_noevent <- log((1 - p1) / (1 - p0))

  # Long enough that few in-control paths fail to signal by the target ARL
  max_cases <- max(2000L, as.integer(20 * target_arl))

  old_seed <- if (exists(".Random.seed", .GlobalEnv)) {
    get(".Random.seed", .GlobalEnv)
  } else NULL
  set.seed(seed)
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, .GlobalEnv)
  }, add = TRUE)

  # For each path keep only the record levels of the running max and the case
  # index at which each was reached — enough to read off the first crossing
  # for any h, at a fraction of the memory of the full path.
  rec_val <- vector("list", n_sim)
  rec_idx <- vector("list", n_sim)

  for (k in seq_len(n_sim)) {
    j  <- sample.int(length(p0), max_cases, replace = TRUE)
    ev <- stats::runif(max_cases) < p0[j]
    x  <- ifelse(ev, s_event[j], s_noevent[j])

    S <- cumsum(x)
    C <- S - pmin(0, cummin(S))
    M <- cummax(C)

    up <- which(M > c(0, M[-max_cases]))
    rec_val[[k]] <- M[up]
    rec_idx[[k]] <- up
  }

  # Run length to first signal at boundary h, per path
  rl_at <- function(h) {
    vapply(seq_len(n_sim), function(k) {
      v <- rec_val[[k]]
      i <- which.max(v >= h)
      if (length(v) == 0 || v[i] < h) max_cases else rec_idx[[k]][i]
    }, numeric(1))
  }

  # ARL is monotone increasing in h, so bisect
  lo <- 1e-6
  hi <- max(vapply(rec_val, function(v) if (length(v)) max(v) else 0, numeric(1)))
  if (hi <= lo) stop("calibrate_h(): simulation produced no CUSUM excursions.")

  for (i in seq_len(40)) {
    mid <- (lo + hi) / 2
    if (mean(rl_at(mid)) < target_arl) lo <- mid else hi <- mid
  }
  h <- (lo + hi) / 2

  rl <- rl_at(h)
  achieved <- mean(rl)
  attr(h, "arl0")             <- achieved
  attr(h, "censored")         <- mean(rl >= max_cases)
  attr(h, "events_to_signal") <- ceiling(h / mean(s_event))

  # The CUSUM is a discrete process: at low p0 a single event moves the
  # statistic by ~log(OR), so ARLs below roughly 1/mean(p0) are unreachable at
  # any h. Say so rather than returning a boundary that silently misses the
  # target — that was the failure mode of the lookup table this replaces.
  if (abs(achieved - target_arl) / target_arl > 0.2) {
    warning(
      "calibrate_h(): closest achievable in-control ARL is ",
      round(achieved), ", not the requested ", target_arl,
      ". At mean p0 = ", round(mean(p0) * 100, 2),
      "% the statistic moves in steps of ~", round(mean(s_event), 2),
      ", so ", attr(h, "events_to_signal"),
      " event(s) already cross h = ", round(h, 3), ".",
      call. = FALSE
    )
  }
  if (attr(h, "censored") > 0.05) {
    warning(
      "calibrate_h(): ", round(attr(h, "censored") * 100),
      "% of simulated in-control series never signalled within ",
      max_cases, " cases; the reported ARL is a lower bound.",
      call. = FALSE
    )
  }

  .h_cache[[key]] <- h
  h
}

# Calibration is the slow step and is repeated across specialties, divisions
# and the report/slides pair, so memoise on the p0 *distribution* (which is
# what the simulation actually consumes) rather than the case order.
.h_cache <- new.env(hash = TRUE, parent = emptyenv())

.h_cache_key <- function(p0, odds_ratio, target_arl, n_sim) {
  tb <- table(sprintf("%.6f", p0))
  paste(
    paste(names(tb), as.integer(tb), sep = ":", collapse = "|"),
    odds_ratio, target_arl, n_sim, sep = "//"
  )
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
  n_events  <- sum(cusum_data$outcome)
  n_cases   <- nrow(cusum_data)
  obs_rate  <- round(n_events / n_cases * 100, 2)
  n_signals <- sum(cusum_data$signal)

  # p0/p1 vary by case when procedure-matched benchmarks are in play
  p0_vals  <- cusum_data$p0
  p0_mixed <- length(unique(round(p0_vals, 8))) > 1
  p0_pct   <- round(mean(p0_vals) * 100, 2)
  p1_pct   <- round(mean(cusum_data$p1) * 100, 2)
  
  plot_df <- cusum_data
  if (!is.null(dates)) plot_df$date <- dates
  
  # Build subtitle
  p0_txt <- if (p0_mixed) {
    paste0("p\u2080 = ", p0_pct, "% mean (",
           round(min(p0_vals) * 100, 2), "\u2013",
           round(max(p0_vals) * 100, 2), "%)")
  } else {
    paste0("p\u2080 = ", p0_pct, "%")
  }

  sub_parts <- paste0(
    p0_txt, "  |  p\u2081 = ", p1_pct,
    "%  |  Observed: ", obs_rate, "% (", n_events, "/", n_cases, ")"
  )
  
  # Benchmark type indicator
  if (grepl("Site expected", benchmark_source)) {
    sub_parts <- paste0(sub_parts, "  |  Benchmark: site risk-adjusted")
  } else if (grepl("ALLCASES", benchmark_source)) {
    sub_parts <- paste0(sub_parts, "  |  Benchmark: ALLCASES (no specialty model)")
  }
  
  # Calibration line: what this boundary actually delivers. An ARL in cases is
  # a weak guarantee for a rare complication, so state the event count that
  # trips the chart alongside it.
  arl0 <- attr(cusum_data, "arl0")
  ev2  <- attr(cusum_data, "events_to_signal")
  if (!is.null(arl0) && !is.null(ev2)) {
    sub_parts <- paste0(
      sub_parts, "\nh = ", round(h_val, 2),
      " (ARL\u2080 \u2248 ", format(round(arl0), big.mark = ","),
      " cases; signals on ", ev2, " event", ifelse(ev2 == 1, "", "s"),
      " in quick succession)"
    )
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
#' Display title for a chart returned by generate_specialty_charts()
#'
#' The returned list is keyed on the column name, which is what callers index
#' by; anything rendering a heading needs the label instead.
#'
#' @param var_name Name from `names(charts)`
#' @return The display label, or `var_name` unchanged if it is not a monitored
#'   complication
chart_frame_title <- function(var_name) {
  lbl <- unname(complication_labels[var_name])
  ifelse(is.na(lbl), var_name, lbl)
}


generate_specialty_charts <- function(data, spec, rates, odds_ratio = 2.0,
                                      div = NULL, targeted_rates = NULL,
                                      target_arl = 500) {
  
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
  
  # complication_labels (R/data_processing.R) is the single source. This used
  # to be a private duplicate of it, which is how the slide decks ended up
  # titling frames "unplanned_reop": the labels existed but were not reachable
  # from the returned list, and two copies could drift apart unnoticed.
  charts <- list()

  for (var_name in names(complication_labels)) {
    comp_label <- unname(complication_labels[var_name])
    
    rate_info <- rates |> filter(specialty == spec, complication == comp_label)
    if (nrow(rate_info) == 0 || is.na(rate_info$p0)) next
    
    # Per-case p0: the most specific SAR model that applies to each case,
    # falling back to the specialty model. Passing targeted_rates = NULL
    # reproduces the previous single-rate behaviour.
    case_p0 <- build_case_p0(
      case_data       = spec_data,
      comp_label      = comp_label,
      spec            = spec,
      benchmark_rates = rates,
      targeted_rates  = targeted_rates
    )

    if (all(is.na(case_p0$p0)) || mean(case_p0$p0, na.rm = TRUE) < 0.0001) {
      message("  Skipping ", comp_label, " for ", spec, " (rate < 0.01%)")
      next
    }
    # A case with no usable benchmark falls back to the mean rather than
    # dropping out, so the case series stays chronologically intact.
    case_p0$p0[is.na(case_p0$p0)] <- mean(case_p0$p0, na.rm = TRUE)

    outcomes <- spec_data[[var_name]]

    message("  CUSUM: ", display_name, " \u2014 ", comp_label,
            " (", describe_case_p0(case_p0), ", events=", sum(outcomes),
            "/", length(outcomes), ")")

    cusum_result <- compute_cusum(
      outcomes   = outcomes,
      p0         = case_p0$p0,
      odds_ratio = odds_ratio,
      h          = NULL,
      reset      = TRUE,
      target_arl = target_arl
    )
    attr(cusum_result, "cohort_mix") <- case_p0$mix
    
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
