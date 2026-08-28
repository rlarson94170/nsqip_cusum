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
   - `Case_Details_Report*.xlsx` — your latest NSQIP case download, under
     whatever name it arrived with *(required)*
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
│   ├── version.R               # Release version — the one place it is defined
│   ├── benchmarks.R            # SAR rates: national + site risk-adjusted
│   ├── data_processing.R       # Case Details ingestion, PATOS, divisions
│   ├── cusum_functions.R       # CUSUM computation, charts, O/E trends
│   ├── triage.R                # Chart-review tiering + carry-over tracking
│   ├── load_report_data.R      # Shared input loading + on-disk cache
│   └── render_beamer_slides.R  # Direct R → LaTeX slide deck generator
├── data/                       # Your data files go here (not tracked by git)
│   ├── Case_Details_Report.xlsx
│   ├── SAR_Site_Summary.xlsx
│   └── surgeon_division_mapping.csv
├── output/                     # Generated PDFs, slide decks, triage history
├── run_tests.R                 # Test runner — source() or Rscript
└── tests/testthat/             # Test suite (synthetic fixtures only)
└── README.md
```

## Tests

```bash
Rscript run_tests.R
```

Or `source("run_tests.R")` in RStudio. The suite covers the CUSUM scoring and
boundary calibration, case-level benchmark assignment, triage tiering, and
carry-over tracking. It uses only synthetic fixtures — it never reads `data/`,
so it runs on a fresh clone with no PHI present.

`process_case_details()` is split so the logic is testable without an xlsx:

| Function | Role |
|----------|------|
| `read_case_details()` | reads the `report_data` sheet — I/O only |
| `derive_case_indicators()` | filtering, PATOS exclusions, composites — pure |
| `process_case_details()` | orchestrates the two, unchanged signature |

`derive_case_indicators()` takes a data frame, so the PATOS exclusions and the
SAR composite definitions are covered directly against synthetic fixtures
(`raw_cases()` in `helper-setup.R` builds a raw frame with every required
column at a benign default; override any column to build a scenario).

`derive_case_indicators()` also validates its input against
`REQUIRED_CASE_COLUMNS`. NSQIP renames columns between releases, and a rename
now reports which columns are missing instead of failing obscurely deep in the
derivation.

Requires `testthat` (which brings `withr`, used for the temp-directory and
locale fixtures):

```r
install.packages("testthat")
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

## Caching

The Case Details workbook is read once per run, not once per output. Both the
Quarto report and the slide renderer call `load_report_data()`, which caches
the prepared data to `.cache/report_data.rds` keyed on the size and mtime of
every input file. Replace or touch any input and the next run reloads from
source. The cache holds data derived from `data/`, so it is git-ignored and
should be treated as PHI-bearing.

To force a reload: `clear_report_cache()`, or delete `.cache/`.


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
| `target_arl` | `1500` | CUSUM in-control ARL, in cases — see Triage below |
| `render_slides` | `TRUE` | Also produce beamer slide decks |


## Output

A single `source("render_reports.R")` call generates the outputs below.

The run ends with a count of what succeeded. If any render fails it names each
one, notes that the previous run's PDF for that report is now stale, and exits
non-zero — a partial set is never reported as a complete one. Specialties with
no matching cases are detected up front and skipped with a warning rather than
producing an empty PDF, which is how a typo in `specialties` used to pass
unnoticed.


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

### Chart Review Priorities
The worklist: which complications, if any, warrant pulling charts this period,
and whether each is new or carried over from the previous report. Where a
flagged composite is fully explained by another flag, a note says so, so a
division does not open two chart reviews on the same patients. Followed by
**Where the events sit** — for each flagged complication, the CPTs carrying
the most events with their rates, against a rollup row for the rest of the
service — and a detection-floor table showing what each complication would
have needed to flag, so that "nothing flagged" can be read correctly. See
**Triage** below.

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
- Chart review priorities (tiered worklist, or an explicit "nothing met the
  threshold" statement)
- Monthly case volume chart
- Prior SAR performance table
- Observed vs. benchmark comparison table
- Monthly complication dashboard
- CUSUM charts (one per slide)
- Procedure mix profile
- Targeted SAR benchmark tables (when available)

Decks carry no O/E trend charts. The PDF report still has them, and the
divisions already receive the same history in the SAR/ISAR reports from NSQIP,
so repeating it in the deck added length without adding information.

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
1. Drop the new `.xlsx` in `data/` — keep the name NSQIP gave it
2. Run `source("render_reports.R")`

No renaming and no config edit. `data_file` in `render_reports.R` is a prefix,
not an exact filename: any `Case_Details_Report*.xlsx` in the folder is
ingested, whatever NSQIP appended to it. The expected case is one download in
`data/` at a time, and a lone file is used regardless of its name — nothing in
the suffix has to parse. The run prints which file it picked and, when the
name carries a readable date, when it was downloaded.

If more than one is present, ranking is by the date in the filename, newest
first, with modification time breaking ties. Whatever follows the date is
ignored. A file with no date at all — a leftover `Case_Details_Report.xlsx`
from an earlier run — ranks below every dated one, so it can never silently
win over a fresh download. Excel's `~$` lock files are ignored, so a report
can be generated while the workbook is open.

To pin a run to a specific download rather than the newest, put its full
filename in `data_file`; a complete name is a prefix that matches only itself.

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
| p₀ | Per-case expected rate | Acceptable complication rate (see below) |
| p₁ | OR = 2× vs p₀ | Rate to detect |
| h | Auto | Decision boundary, calibrated by simulation to `target_arl` |

The decision boundary h is calibrated at render time so that, when the true
rate equals p₀, a false alarm occurs on average once every `target_arl` cases.
Calibration simulates in-control case series drawn from the chart's own p₀
distribution, so it respects both the procedure mix and the `odds_ratio` in
use. Each chart's subtitle reports the h it received, the in-control ARL that
h actually achieves, and the number of events in quick succession that will
trip it.

**Read the ARL alongside the event count.** An ARL measured in cases is a weak
guarantee for a rare complication: at p₀ = 0.6%, an ARL₀ of 500 cases means
signalling on about two events, because only three are expected in that span.
`calibrate_h()` warns when the requested ARL is unreachable — below roughly
1/p₀ the statistic's step size makes it unattainable at any h.

**Choosing `target_arl` with multiplicity in mind.** The ARL is per chart, and
a full run produces many charts (currently ~66 across General Surgery and its
divisions). Expected false alarms per reporting cycle is approximately the
total cases monitored divided by `target_arl`. At the default 500 that is
roughly 20 per cycle; at 1500 it is roughly 5. Set it for the number of charts
you actually review, not for a single chart in isolation.

### Case-Level (Procedure-Matched) Benchmarks

p₀ is assigned **per case**, not per report: each case is scored against the
most specific SAR model that applies to it, falling back to the specialty
model. This is the risk-adjusted CUSUM of Steiner et al. (*Biostatistics*,
2000).

This matters because NSQIP's targeted models are defined by procedure while
reports are organised by division, and the two do not line up. Colorectal
procedures at this site are performed by several divisions, and site expected
SSI is 3.90% under the General Surgery model but 5.88% for Colectomy and
13.95% for Proctectomy. Scoring every case in a division against one specialty
rate distorts any division whose case mix is not typical of the specialty.

Currently available for General Surgery: **Colectomy** and **Proctectomy**
(full complication sets). Hepatectomy, Pancreatectomy and Esophagectomy appear
in the targeted SAR with *Length of Stay only*, so those cases still fall back
to the specialty rate. Chart subtitles show the p₀ range and how many cases
were procedure-matched.

### Triage: from charts to a worklist

The CUSUM answers *when did events cluster*, which is what makes the case list
actionable. It does not answer *is this division's rate elevated* — it fires on
clustering even when the cumulative rate is at or below expected. In the
2026-H1 data it flagged Unplanned Readmission at 28 observed against 32.3
expected: a real run of events, no excess behind it, and a wasted chart review.

So each complication is scored on two independent gates:

| Gate | Test |
|------|------|
| **Magnitude** | >= 3 events **and** one-sided Poisson p < 0.10 vs summed expected |
| **Timing** | CUSUM signalled |

| Tier | Condition | Meaning |
|------|-----------|---------|
| **1 — Review now** | both gates | elevated rate, with a recent cluster to look at |
| **2 — Worth a look** | magnitude only | elevated over the window, no recent cluster |
| **3 — Watch** | timing only, obs >= exp | recent cluster, rate not yet elevated |
| *(not listed)* | timing only, obs < exp | clustering noise — deliberately suppressed |

The 3-event floor matters as much as the p-value: two events against one
expected is a "2x excess" and pure noise. Thresholds live at the top of
`R/triage.R` (`TRIAGE_ALPHA`, `TRIAGE_MIN_EVENTS`).

**Detection floor.** Each report states how many events every complication
would have needed in order to flag. Over a 6-month window this is roughly
1.2–1.8x expected for the common complications (SSI, morbidity, readmission,
reoperation) and 2.4–4.7x for rare ones at division volumes (mortality,
sepsis). A division reading "no flags" should read it as "nothing above the
floor", not as "nothing to see".

**Carry-over tracking.** Reports run quarterly over a trailing ~6 months, so
consecutive reports overlap by about three months and every event is seen
twice. Flags are matched against the previous report for the same scope and
marked **New** or **Carried over**, so a division does not investigate the same
cluster twice. State lives in `output/triage_history.csv` (git-ignored,
regenerated each run; writes are an upsert keyed on report date and scope, so
the PDF and slide renders of one report do not double-count).

**Where the events sit.** A flag names a complication, not a place to look.
For each flagged complication the report lists the CPTs carrying the most
events, with a rollup row for everything else. Cases are grouped on the raw
CPT rather than `procedure_category`, because `PROCEDURE_CPT_MAP` covers
General Surgery only and every case in the other specialties falls through to
"Other".

Three rules keep the table honest, and all three are load-bearing:

- **Ranked by event count, not rate.** Ranking by rate puts every 1-of-1
  procedure at the top and buries the concentration worth reviewing.
- **A CPT needs 2+ events to be listed separately.** One event is a case, not
  a concentration. Listing singletons also drains the rollup, which is the
  baseline the listed rows are read against — in the 2026-H1 Plastics data,
  promoting three singletons moved the rest-of-service rate from 4.3% to 0.0%.
- **Denominators are shown, never filtered on.** The concentrations that
  motivated this sat at n = 15 and n = 18, so any floor high enough to make a
  rate "reliable" would discard the finding.

A complication whose events are all one-offs gets no table, rather than a
table implying a pattern that is not there. Settings are at the top of
`R/triage.R` (`FLAG_PROCEDURE_TOP_N`, `FLAG_PROCEDURE_MIN_EV`).

**Composite overlap.** Morbidity is an OR over the individual complications,
so when SSI is elevated morbidity is elevated too and the worklist shows two
flags for one problem. Whether that is actually the case is checked rather
than assumed, and the note is only shown when it is true: in the 2026-H1 data
Plastics morbidity was entirely SSI ("13 events, all also counted under SSI —
one chart review, not two"), while General Surgery was about half, leaving a
block of morbidity events no other flag accounted for. Overlap is measured
only against complications that *themselves flagged* — the question is whether
one flag on the worklist subsumes another, not whether the composite is
definitionally a union, which it always is. Where a composite adds no patients
at all, its procedure breakdown is suppressed as well, since it would repeat
the explaining flag's table verbatim.

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

NSQIP does not produce division-level risk-adjusted models, so a division's
p₀ is built up from the case level instead: procedure-matched expected rates
where a targeted model exists, and the specialty expected rate otherwise. A
division doing mostly colorectal work is therefore benchmarked mostly against
colorectal models, without needing a division-level model to exist.

Targeted SAR tables display site-level risk-adjusted benchmarks alongside
division-level unadjusted observed rates, clearly labeled to support
meaningful comparison.


## Customization

### Change detection sensitivity
- `odds_ratio` (default 2.0): Lower → more sensitive; Higher → fewer false alarms
- `target_arl` (default 500): Lower → quicker detection but more false alarms.
  Now genuinely honoured — h is re-simulated for the value you set.

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
- **Division benchmarks:** Division reports use procedure-matched expected
  rates where a targeted SAR model exists (Colectomy, Proctectomy) and the
  specialty expected rate otherwise; NSQIP publishes no division-level model
- **Multiplicity:** Each chart's ARL is its own. A full run produces dozens of
  charts, so some signals are expected by chance — see `target_arl`
- **NSQIP sampling:** Not all surgical cases are captured
- **Blank PATOS fields:** treated as "not present at surgery", so an
  occurrence with a blank PATOS value still counts
- **Dashboard months:** Month columns are keyed on the month's start date and
  labelled with the year whenever the window crosses a calendar year, so a
  window longer than twelve months does not merge same-named months
- **Intended for internal QI** — not a substitute for official SAR profiling


## Version History

### v1.5.1 — August 2026

Two additions to the worklist, both aimed at the same thing: making a flag
tell a division where to look, not just what to look for. No change to which
complications flag — tiering, gates and boundaries are untouched, so reports
remain comparable to v1.5.0.

- **Per-flag procedure breakdown.** Each flagged complication now lists the
  CPTs carrying its events, with rates and a rest-of-service rollup. On the
  2026-H1 Plastics data this puts 10 of the 13 SSIs into two procedures —
  breast reduction (5/18, 27.8%) and free-flap breast reconstruction (5/15,
  33.3%) — against 4.3% for everything else, which was previously invisible
  in the PDF and could not be surfaced by the Procedure Mix appendix, since
  `PROCEDURE_CPT_MAP` is General Surgery-only and every Plastics case falls
  through to "Other". PDF only for now; the slide decks are unchanged.
- **Composite-overlap note.** When a flagged composite is explained by another
  flag, the report says so rather than leaving it to read as a second problem
  — "Morbidity: 13 events, all also counted under SSI — one chart review, not
  two." The note is conditional on the overlap actually existing and is
  measured only against complications that themselves flagged.

See **Triage** under Methodology for the rules that keep the procedure table
honest at these volumes.

The Case Details download no longer has to be renamed. `data_file` is treated
as a prefix, so the file NSQIP produces —
`Case_Details_Report-17-Aug-2026-1503.xlsx` — can be dropped into `data/` as
is, and the newest matching download is used. Old downloads can stay in the
folder; the newest by filename date is used, and whatever follows the date is
ignored. An undated leftover ranks below every dated file so it cannot
silently win, and Excel `~$` lock files are skipped.

Also in this release: the slide decks no longer carry O/E trend charts. That
history is already in the PDF report and in the SAR/ISAR reports NSQIP shares
with the divisions, so the deck slides were duplication. The PDF is unchanged.

Two long-standing cosmetic bugs are fixed alongside it. Deck CUSUM frames were
titled with column names — "unplanned\_reop", "cdiff" — because
`generate_specialty_charts()` keys its list on the column and the deck used
that key as the heading; the labels existed all along in a private duplicate
of `complication_labels` inside `cusum_functions.R`, which is now collapsed
into the one definition. And the version string in the report footer and the
deck's closing frame was hard-coded separately in each, and had read v1.4
since before the v1.5.0 release; both now read `R/version.R`, which a test
enforces.

### v1.5.0 — August 2026

**Reports from this version are not directly comparable to earlier ones.** Two
changes alter which complications signal, so a chart that was quiet in a prior
month may signal now, and vice versa — 8 of 66 General Surgery
chart-complication pairs changed state on the same data. Read a change in
signalling across this boundary as a change in method, not in performance.

- **CUSUM boundaries recalibrated.** The previous hard-coded lookup table
  returned h = 4.5 for every p₀ below 4.5%, giving in-control ARLs of
  4,000–29,000 rather than the documented 500. Most monitored complications
  sit in that range, so those charts could not realistically signal.
  Boundaries are now simulated per chart, and `target_arl` and `odds_ratio`
  genuinely affect them. Effect: rare complications became able to signal.
- **Benchmarks assigned per case.** Each case is now scored against the most
  specific SAR model that applies (Colectomy and Proctectomy targeted models
  where available, specialty model otherwise) rather than one specialty rate
  for every case in a report. Effect: divisions whose case mix differs from
  the specialty average — colorectal work especially — shifted most.
- **Triage replaces the bare CUSUM signal** as the review trigger, and
  `target_arl` moved from 500 to 1500. Effect: fewer, better-founded flags.

Also fixed in this release: dashboard months no longer merge across years in
windows over twelve months; blank PATOS fields no longer discard the
occurrence; MRNs keep their leading zeros; slide-deck charts no longer mangle
the p₀/p₁ subscripts; and failed renders are reported and exit non-zero
instead of a partial set being announced as complete.

The two data fixes (PATOS, MRN) changed no values in the August 2026 data —
no case had a blank PATOS field alongside an occurrence, and no MRN had a
leading zero. They are correctness fixes for future downloads.

Added: a 265-test suite (`run_tests.R`), shared input loading with an on-disk
cache, and carry-over tracking so quarterly reports do not prompt duplicate
chart reviews.
