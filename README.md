# NSQIP CUSUM Surgical Outcome Monitoring

Bernoulli CUSUM charts for near-real-time monitoring of surgical complications
using NSQIP Case Details Report data, benchmarked against risk-adjusted site
SAR expected rates or national observed rates.

## Quick Start

1. **Open** `nsqip_cusum.Rproj` in RStudio
2. **Install** required packages (see below)
3. **Place files** in the `data/` folder:
   - `Case_Details_Report.xlsx` — your latest NSQIP case download *(required)*
      --  NSQIP site -> Resource Portal -> Reports -> CASE DETAILS AND CUSTOM FIELDS REPORT
      --  Case Details Only + selected General / Vascular Procedures + Reporting Period + Filter By specialty + Case Status = Complete
   - `SAR_Site_Summary.xlsx` — your site SAR summary *(recommended)*
      -- NSQIP Site -> Resource Portal -> Current SAR/ISAR -> Site Summary Excel Report -> Rename to `SAR_Site_Summary.xlsx`
   - `surgeon_division_mapping.csv` - general surgery separated into divisions by surgeon name
4. **Run** `source("render_reports.R")`
5. **Find** PDFs in the `output/` folder

## Project Structure

```
nsqip_cusum/
├── nsqip_cusum.Rproj          # RStudio project file
├── render_reports.R            # Master script — run this to generate PDFs
├── nsqip_cusum_report.qmd     # Quarto report template (parameterized)
├── R/
│   ├── benchmarks.R            # SAR rates: national + site risk-adjusted
│   ├── data_processing.R       # Case Details ingestion, PATOS handling
│   └── cusum_functions.R       # CUSUM computation, charts, O/E trends
├── data/                       # Your data files go here (not tracked by git)
│   ├── Case_Details_Report.xlsx
│   └── SAR_Site_Summary.xlsx
├── output/                     # Generated PDFs
└── README.md
```

## Required R Packages

```r
install.packages(c(
  "quarto", "readxl", "dplyr", "tidyr", "lubridate",
  "ggplot2", "scales", "knitr", "kableExtra", "tibble"
))
```

Also need: **Quarto** (bundled with RStudio ≥ 2022.07) and a **LaTeX**
distribution (`quarto install tinytex`).

## Benchmark Options

The system supports two benchmark modes, controlled by `benchmark_type` in
`render_reports.R`:

| Mode | `benchmark_type` | p₀ source | Risk-adjusted? |
|------|-----------------|-----------|----------------|
| **Preferred** | `"site_expected"` | Site SAR expected rate | Yes — accounts for your case mix |
| Fallback | `"national_observed"` | National SAR observed rate | No — raw specialty rates |

When using `site_expected`, the system reads your `SAR_Site_Summary.xlsx` to
extract the expected rate from the hierarchical model for each
specialty-complication pair. This rate reflects what would be expected given
your patient population's risk profile. If a site rate is not available for
a particular complication, the national observed rate is used as fallback.

## Report Contents

Each specialty PDF includes:

1. **SAR Context** — prior SAR results table with O/E ratios, percentiles,
   and assessments (Exemplary / As Expected / Needs Improvement)
2. **Observed vs. Benchmark Table** — current period rates compared to
   benchmarks
3. **CUSUM Charts** — one per complication, with:
   - Chart title colored by SAR assessment (red = Needs Improvement)
   - Subtitle showing p₀, p₁, observed rate, benchmark source
   - SAR O/E ratio and percentile annotation
   - Decision boundary (h), signal markers (▲), event ticks (|)
4. **O/E Trend Charts** — historical O/E ratios across SAR periods for
   complications flagged as Needs Improvement or with outlier history
5. **Appendix** — ASA distribution and monthly case volume

## Updating for New Data

### New Case Details download:
1. Place the new `.xlsx` in `data/`
2. Update `data_file` path in `render_reports.R`
3. Run `source("render_reports.R")`

### New SAR release:
1. Place the new site SAR summary in `data/`
2. Update `site_sar_file` path in `render_reports.R`
3. Update national rates in `R/benchmarks.R` from the new SAR Summary Report
4. Re-render

## Methodology

### Bernoulli CUSUM

For each case (chronological order):

- **Score:** s = log[p₁(1-p₀) / (p₀(1-p₁))] if event; log[(1-p₁)/(1-p₀)] if not
- **Accumulation:** C_i = max(0, C_{i-1} + s_i)
- **Signal:** When C_i ≥ h
- **Reset:** CUSUM resets to 0 after each signal

### Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| p₀ | Site expected rate | Acceptable complication rate |
| p₁ | OR = 2× vs p₀ | Rate to detect |
| h | Auto (ARL₀≈500) | Decision boundary |

### PATOS Exclusions

Per SAR methodology, complications Present at Time of Surgery are excluded
from event counts for: Pneumonia, Ventilator >48h, UTI, SSI (all types),
and Sepsis. This aligns the raw case data with how the SAR calculates its
benchmark rates.

### Composite Outcomes

| Outcome | Components |
|---------|-----------|
| Morbidity | SSI + wound disruption + pneumonia + unplanned intubation + vent >48h + dialysis + renal insufficiency + UTI + stroke/CVA + cardiac arrest + MI + sepsis |
| Cardiac | Cardiac arrest + MI |
| VTE | PE + venous thrombosis requiring therapy |
| Renal Failure | Renal insufficiency + postop dialysis |
| SSI | Superficial + deep incisional + organ/space |

## Customization

### Change detection sensitivity
- `odds_ratio` (default 2.0): Lower → more sensitive; Higher → fewer false alarms
- `target_arl` (default 500): Lower → quicker detection but more false alarms

### Add specialties
Add to the `specialties` vector in `render_reports.R` and add corresponding
national rates in `R/benchmarks.R`. Site SAR rates are auto-parsed.

## Limitations

- **Benchmark timing:** SAR rates reflect a prior period; case mix may shift
- **No dynamic risk adjustment:** The CUSUM monitors raw event streams; only
  the p₀ benchmark incorporates risk adjustment
- **NSQIP sampling:** Not all surgical cases are captured
- **Intended for internal QI** — not a substitute for official SAR profiling
