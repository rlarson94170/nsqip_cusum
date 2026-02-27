# NSQIP CUSUM Surgical Outcome Monitoring

Bernoulli CUSUM charts for near-real-time monitoring of surgical complications
using NSQIP Case Details Report data, benchmarked against risk-adjusted site
SAR expected rates or national observed rates. Generates comprehensive PDF
reports and presentation slide decks for specialty and division-level quality
improvement review.

## Quick Start

1. **Open** `nsqip_cusum.Rproj` in RStudio
2. **Install** required packages (see below)
3. **Place files** in the `data/` folder:
   - `Case_Details_Report.xlsx` — your latest NSQIP case download *(required)*
   - `SAR_Site_Summary.xlsx` — your site SAR/ISAR summary *(recommended)*
   - `surgeon_division_mapping.csv` — surgeon-to-division mapping *(for division reports)*
4. **Run** `source("render_reports.R")`
5. **Find** PDFs in the `output/` folder

## Project Structure

```
nsqip_cusum/
├── nsqip_cusum.Rproj          # RStudio project file
├── render_reports.R            # Master script — run this to generate all PDFs
├── nsqip_cusum_report.qmd     # Quarto report template (parameterized)
├── R/
│   ├── benchmarks.R            # SAR rates: national + site risk-adjusted
│   ├── data_processing.R       # Case Details ingestion, PATOS, divisions
│   ├── cusum_functions.R       # CUSUM computation, charts, O/E trends
│   └── render_beamer_slides.R  # Direct R → LaTeX slide deck generator
├── data/                       # Your data files go here (not tracked by git)
│   ├── Case_Details_Report.xlsx
│   ├── SAR_Site_Summary.xlsx
│   └── surgeon_division_mapping.csv
├── output/                     # Generated PDFs and slide decks
└── README.md
```

## Required R Packages

```r
install.packages(c(
  "quarto", "readxl", "dplyr", "tidyr", "lubridate",
  "ggplot2", "scales", "knitr", "kableExtra", "tibble", "tinytex"
))
```

Also needed: **Quarto** (bundled with RStudio ≥ 2022.07) and a **LaTeX**
distribution (`quarto install tinytex`).


## Configuration

All settings are in the top section of `render_reports.R`:

| Setting | Default | Description |
|---------|---------|-------------|
| `data_file` | `"data/Case_Details_Report.xlsx"` | Path to NSQIP Case Details download |
| `site_sar_file` | `"data/SAR_Site_Summary.xlsx"` | Site SAR/ISAR summary (set `NULL` to skip) |
| `surgeon_mapping_file` | `"data/surgeon_division_mapping.csv"` | Surgeon-to-division CSV (set `""` to skip) |
| `benchmark_type` | `"site_expected"` | `"site_expected"` (risk-adjusted) or `"national_observed"` |
| `specialties` | General Surgery, Vascular, Thoracic, Plastics | Which specialties to report on |
| `division_specialties` | General Surgery | Which specialties get division-level breakouts |
| `min_division_cases` | `10` | Minimum cases required to generate a division report |
| `odds_ratio` | `2.0` | Detection target: p₁ = OR × p₀ |
| `target_arl` | `500` | Average run length before false alarm |
| `render_slides` | `TRUE` | Also produce beamer slide decks |


## Output

A single `source("render_reports.R")` call generates:

**For each specialty** (e.g., General Surgery, Vascular, Thoracic, Plastics):
- `NSQIP_CUSUM_{specialty}_{date}.pdf` — full PDF report
- `NSQIP_CUSUM_{specialty}_{date}_slides.pdf` — beamer slide deck

**For each division** within specialties listed in `division_specialties`:
- `NSQIP_CUSUM_{specialty}_{division}_{date}.pdf` — division PDF report
- `NSQIP_CUSUM_{specialty}_{division}_{date}_slides.pdf` — division slide deck

Divisions are auto-discovered from the surgeon mapping file. Only divisions
meeting the `min_division_cases` threshold are rendered.


## Benchmark Options

| Mode | `benchmark_type` | p₀ source | Risk-adjusted? |
|------|-----------------|-----------|----------------|
| **Preferred** | `"site_expected"` | Site SAR expected rate | Yes — accounts for your case mix |
| Fallback | `"national_observed"` | National SAR observed rate | No — raw specialty rates |

When using `site_expected`, the system reads your `SAR_Site_Summary.xlsx` to
extract the expected rate from the hierarchical model for each
specialty-complication pair. If a site rate is not available for a particular
complication, the national observed rate is used as fallback. The system
auto-detects SAR vs. ISAR format.


## PDF Report Contents

Each specialty/division PDF includes the following sections:

### SAR Context
Prior SAR results table with O/E ratios, percentiles, and assessments
(Exemplary / As Expected / Needs Improvement). Color-coded rows highlight
areas of concern.

### Observed Rates vs. Benchmarks
Current-period observed complication rates compared to benchmarks, with
observed/benchmark ratio for each complication.

### Monthly Complication Dashboard
Event counts by month in a grid layout, organized by complication category
(Infection, Respiratory, Renal, Cardiac, VTE/PE, Readm/ROR). Includes
granular sub-complications (e.g., Superficial/Deep/Organ-Space SSI, Renal
Insufficiency vs. Dialysis) with PATOS exclusions applied. Shows raw rates
and SAR benchmark rates for comparison.

### CUSUM Charts
One chart per monitored complication, with:
- Chart title colored by SAR assessment (red = Needs Improvement)
- Subtitle showing p₀, p₁, observed rate, benchmark source
- SAR O/E ratio and percentile annotation
- Decision boundary (h), signal markers (▲), event ticks (|)

### O/E Trend Charts
Historical O/E ratios across SAR periods for complications flagged as
Needs Improvement or with high outlier history. Red dots indicate periods
flagged as high outlier. Division reports note that O/E trends reflect the
specialty-level SAR model.

### Appendix: Procedure Mix Profile
Case distribution by procedure category (using NSQIP targeted module flags
and CPT code groupings), with complication rates and median length of stay
per category.

### Appendix: Targeted SAR Benchmarks
For specialties with targeted procedure data (e.g., Colectomy, Proctectomy,
Pancreatectomy, Hepatectomy, VHR), shows site-level SAR results alongside
division-level unadjusted observed rates for direct comparison. Includes
procedure-specific complications like anastomotic leak and prolonged NPO/NGT.

### Appendix: Complication Case List
Patient-level detail for the most recent quarter's complications, including
case ID, surgeon, CPT, ASA class, length of stay, and specific occurrences.
Excludes PATOS complications. Readmission entries indicate whether the
readmission was related to the index procedure.

### Appendix: Case Mix
ASA class distribution and monthly case volume chart.


## Slide Decks

Slide decks are generated directly from R as LaTeX beamer documents (not via
Quarto) to avoid pandoc's beamer frame-management limitations. Each deck
includes:

- Title slide with specialty/division, date range, and case count
- Report summary with key parameters
- Monthly case volume chart
- Prior SAR performance table
- Observed vs. benchmark comparison table
- Monthly complication dashboard
- CUSUM charts (one per slide)
- O/E trend charts for flagged complications
- Procedure mix profile
- Targeted SAR benchmark tables (when available)

Slides use a 16:9 aspect ratio with institutional color theming (VCU blue/gold).
Set `render_slides <- FALSE` in `render_reports.R` to skip slide generation.


## Surgeon-to-Division Mapping

The mapping file (`surgeon_division_mapping.csv`) links individual surgeons
to their division for division-level reporting. Format:

```csv
surgeon_name,specialty,division
"LastName, FirstName",General Surgery,Surgical Oncology
"LastName, FirstName",General Surgery,Colorectal
```

The `surgeon_name` column should match the surgeon name format in the NSQIP
Case Details Report. The system uses this to filter cases by division and
generate separate reports for each division within a specialty.


## Updating for New Data

### New Case Details download:
1. Place the new `.xlsx` in `data/`
2. Update `data_file` path in `render_reports.R`
3. Run `source("render_reports.R")`

### New SAR/ISAR release:
1. Place the new site summary in `data/`
2. Update `site_sar_file` path in `render_reports.R`
3. Update national rates in `R/benchmarks.R` from the new SAR Summary Report
4. Re-render

### New surgeons or division changes:
1. Update `surgeon_division_mapping.csv`
2. Re-render — divisions are auto-discovered from the mapping


## Methodology

### Bernoulli CUSUM

For each case in chronological order:

- **Score:** s = log[p₁(1-p₀) / (p₀(1-p₁))] if event; log[(1-p₁)/(1-p₀)] if not
- **Accumulation:** C_i = max(0, C_{i-1} + s_i)
- **Signal:** When C_i ≥ h
- **Reset:** CUSUM resets to 0 after each signal

### Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| p₀ | Site expected rate | Acceptable complication rate |
| p₁ | OR = 2× vs p₀ | Rate to detect |
| h | Auto (ARL₀≈500) | Decision boundary via Monte Carlo calibration |

The decision boundary h is calibrated so that, when the true rate equals p₀,
a false alarm occurs on average once every ~500 cases.

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

### Division-Level Benchmarking

Division reports use the **specialty-level** SAR expected rates as p₀ (since
NSQIP does not produce division-level risk-adjusted models). Targeted SAR
tables display site-level risk-adjusted benchmarks alongside division-level
unadjusted observed rates, clearly labeled to support meaningful comparison.


## Customization

### Change detection sensitivity
- `odds_ratio` (default 2.0): Lower → more sensitive; Higher → fewer false alarms
- `target_arl` (default 500): Lower → quicker detection but more false alarms

### Add specialties
Add to the `specialties` vector in `render_reports.R` and add corresponding
national rates in `R/benchmarks.R`. Site SAR rates are auto-parsed.

### Add division breakouts for another specialty
Add the specialty name to `division_specialties` in `render_reports.R` and
ensure the surgeon mapping CSV includes entries for that specialty.

### Skip slide generation
Set `render_slides <- FALSE` in `render_reports.R`.


## Limitations

- **Benchmark timing:** SAR rates reflect a prior period; case mix may shift
- **No dynamic risk adjustment:** The CUSUM monitors raw event streams; only
  the p₀ benchmark incorporates risk adjustment
- **Division benchmarks:** Division reports use specialty-level SAR expected
  rates since division-level risk models are not available from NSQIP
- **NSQIP sampling:** Not all surgical cases are captured
- **Intended for internal QI** — not a substitute for official SAR profiling
