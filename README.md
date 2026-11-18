<!--
README draft updated 18 November 2026. Not committed to the repository.
Publication/reproducibility checks still pending:
  [A] Script 17 S2 Monte Carlo figures below are from the author's v2 local run
      (200 replications, output/tables/phase3/audit17/). If adopting Script 17 v3,
      re-run it and update estimates, output paths and run metadata accordingly.
  [B] Script 17 v3-only revision-sign counts (58/22/28) and S4 smoothed-series
      benchmark (0.650/0.429/0.028) are NOT reported below: the author has not
      rerun those v3 diagnostics.
  [C] Choose and archive one official Script 18 version, then verify the exact
      scripts, source datasets and outputs in a clean run. The revised Script 19
      reads data/processed/quarterly_series_2010_2025.csv and
      data/processed/sia_regional_3mma_vintage12m_2015_2025.csv, and writes to
      output/tables/phase3/audit19_revised/.
  [D] Do not claim full clean-checkout reproducibility until these checks pass.
-->

# Semiconductors II

**Trade War, Technology Policy, and the Global Semiconductor Market**

An open, reproducible econometric research project examining whether U.S.–China trade and technology rivalry is associated with statistically detectable changes in global and regional semiconductor-market dynamics.

Semiconductors II builds on the descriptive work of Semiconductors I. It combines data construction, time-series diagnostics, endogenous structural-break analysis, a sourced policy chronology, and—once identification and power have been addressed—policy-intervention modelling.

> **Research status — 18 November 2026:** Phases 1 and 2 are complete. The Phase 3A methodological reassessment (Scripts 17–19) is **complete**: the original late-2018 trend breakpoints are not corroborated by the more robust quarterly growth-shift tests and are retained only as exploratory findings, not as established structural-change evidence. The Phase 3B policy chronology is documented. **Phase 3C has not begun** and requires a frozen event-selection protocol and a power assessment. No causal effect of U.S.–China trade or technology policy has been identified.

## Research question

**Did trade and technology policies associated with U.S.–China strategic rivalry significantly alter the dynamics of the global semiconductor market?**

The project does not presume an effect. A statistical change near a policy date is not, by itself, evidence that the policy caused that change. Policy interventions must be defined from documented events and evaluated against alternative economic explanations.

## Data and empirical architecture

| Layer | Source | Coverage | Intended use | Key caution |
| --- | --- | --- | --- | --- |
| Monthly market sales | WSTS Historical Billings | 2010–2025; 192 months per region | Global/regional cycles, growth, seasonality | Strong quarter-end calendar pattern and serial dependence |
| Rolling 3-month average (3MMA) | WSTS | 2010–2025 | Descriptive comparison with raw monthly flows | Overlapping windows induce mechanical autocorrelation |
| China and Asia-Pacific Other 3MMA | SIA/WSTS official monthly releases | 2015–2025; 132 months | China decomposition | Processed series mixes vintages; see single-vintage layer |
| Single-vintage China / regional 3MMA | SIA releases, "Last Year" columns (+12-month vintage) | 2015–2024 (2025 first release, flagged) | Primary SIA series for inference | 2025 awaits the 2026 releases |
| Non-overlapping quarterly series | Derived from WSTS (sums of monthly flows) and SIA (3 × quarter-end 3MMA) | WSTS 2010Q1–2025Q4; SIA 2015Q1–2024Q4 primary | Diagnostics and break inference without 3MMA overlap | Short samples (40–64 quarters); aggregation induces some autocorrelation |
| Policy-event registry | Official institutional documentation and academic sources | Mainly 2014–2026 | Historical context and prospective intervention dates | Events in 2026 fall outside the 2010–2025 empirical window |
| International trade (optional extension) | UN Comtrade, U.S. Census: HS 8542; HS 8541 and HS 8486 under evaluation; SEMI regional statistics if feasible | To be defined | Investigate bilateral trade diversion, production and investment mechanisms in later work | Not yet assembled; not required to start exploratory Phase 3C |

**Regional convention:** `America` is the normalized name of the WSTS *Americas* market region; it is **not** a United States-only series. `Worldwide` includes the regional components, so it must not be inserted alongside all those components in a multivariate system as if it were independent.

### Main datasets

- `data/processed/wsts_monthly_2010_2025.csv`
- `data/processed/wsts_3mma_2010_2025.csv`
- `data/processed/sia_regional_3mma_2015_2025.csv` (processed, mixed vintage)
- `data/processed/sia_china_ap_other_3mma_2015_2025.csv`
- `data/processed/sia_regional_3mma_vintage12m_2015_2025.csv` (single +12-month vintage; Script 18)
- `data/processed/wsts_quarterly_2010_2025.csv`, `data/processed/sia_quarterly_implied_vintage12m_2015_2025.csv`, `data/processed/sia_quarterly_implied_2015_2025.csv` (quarterly series; Script 18)
- `output/tables/phase1/sia_regional_pdf_parse_audit.csv` (source parsing; "Current" and "Last Year" values per release)

## Project phases

### Phase 1 — Data construction and source checks: completed

- Processed WSTS regional monthly data and recorded 3MMA series for 2010–2025.
- Reconstructed the SIA/WSTS regional and China–Asia-Pacific Other series for 2015–2025 using 120 official monthly releases.
- Implemented parsing, completeness and within-source consistency checks.
- **Qualification (resolved in Phase 3A reassessment):** the SIA–WSTS Asia-Pacific gap is mostly a publication-revision effect, not a source discrepancy. A single-vintage SIA series built in Script 18 matches WSTS closely (see Phase 3A).

### Phase 2 — Descriptive and time-series diagnostics: completed

- Investigated regional sales, growth, shares, volatility and seasonal patterns.
- Applied ADF, KPSS, Phillips–Perron, ACF/PACF and Ljung–Box diagnostics.
- Compared raw monthly sales with rolling 3MMA series.
- First log differences are more consistent with stationarity than log levels, but serial dependence persists.
- **Qualification:** the quarter-end calendar pattern and 3MMA overlap affect monthly dependence measures. Script 18 repeats the diagnostics on non-overlapping quarterly series.

### Phase 3A — Endogenous structural-break analysis: closed after methodological reassessment

The original analysis (`R/14_structural_breaks.R`, `R/15_china_structural_breaks.R`, `R/16_hac_diagnostics.R`) estimated Bai–Perron breakpoints by BIC in two specifications: mean monthly log growth and log-level trends. It reported a cluster of trend breaks in late 2018 (America, Asia Pacific and Worldwide in November 2018; Japan in December 2018; China 3MMA in December 2018).

A three-step reassessment (Scripts 17–19) identified substantial false-break risks under the original modelling assumptions and found **no corroborating evidence of a single shift in quarterly mean growth**. The original trend-level and revised growth-shift models test different hypotheses; this reassessment does **not** prove that every original break date was spurious. **The late-2018 dates are therefore retained as exploratory outputs, not as established evidence of structural change.**

#### Step 1 — Audit replication (`R/17_reproduce_audit_checks.R`)

| Check | Finding |
| --- | --- |
| **S1** Calendar pattern | In 59 of 64 quarters the third month has the highest raw WSTS sales. The quarter-end month exceeds the other two by about 0.107 log points; the pattern is stable across subperiods (0.08–0.14). Monthly log-growth ACF(3) is 0.766 and falls to 0.27 after removing quarter-position means. This is about half of what a pure 4-4-5 week calendar would imply (log 5/4 = 0.223); a 4-4-5 convention is **not** established. |
| **S2** BIC for trend breaks | In the author's recorded Script 17 v2 Monte Carlo run, simulated AR(1) series **with no true break** (n = 192, h = 24, 200 replications), `strucchange` BIC selects at least one break with probability 0.02 (φ = 0), 0.70 (φ = 0.5), 1.00 (φ = 0.8) and 1.00 (φ = 0.95); the mean number of selected breaks is 0.02, 1.34, 3.90 and 4.72 (MC). In this design selection does not depend on intercept, slope or noise scale. The three trend breaks selected for observed Worldwide data are within the range produced by persistent processes without breaks. |
| **S2** BIC for growth-mean breaks | With the calendar pattern left in, raw monthly growth has essentially no power (detection ≤ 0.005 even for a 0.02 monthly shift). After removing quarter-position means, false detection is 0.035 and power is 0.135 for a 0.01 shift and 0.58 for a 0.02 shift. On quarterly growth BIC over-selects (0.19 false detection) (MC). The original "no break in mean growth" result was therefore uninformative. |
| **S4** 3MMA overlap | The recorded 3MMA growth ACF is 0.635, 0.433 and 0.017 at lags 1–3. Overlapping trailing averages mechanically induce short-run dependence, while underlying monthly dynamics may also contribute. The comparison with a calendar-adjusted smoothing benchmark is pending the author's re-execution of Script 17 v3 and is not numerically reported here. |
| **D1/D2** SIA vs WSTS and vintages | The SIA–WSTS Asia-Pacific gap is mostly a revision effect. Months within 0.02 billion of WSTS rise from 39/108 (first release) to 104/108 (same month as published 12 months later). The recorded v2 analysis found predominantly upward revisions; the exact positive/negative/zero counts await the author's re-execution of the tolerance-corrected v3 check. The published SIA Total matches the sum of the five regions within rounding. |

#### Step 2 — Single vintage and quarterly diagnostics (`R/18_quarterly_series_diagnostics.R`)

- **Single-vintage SIA series.** Each month of 2015–2024 is taken from the "Last Year" column of the SIA release published 12 months later. It matches WSTS Asia Pacific within 0.02 billion in 116 of 120 months, against 51 of 120 for the mixed-vintage processed series. 2025 remains first release and is used only as a sensitivity case until the 2026 releases are incorporated (optional input `data/manual/sia_2025_vintage12m.csv`).
- **Vintage matters little for China.** Between vintages, China's share of Asia Pacific differs by at most 0.61 percentage points, quarterly changes of log(China / Asia Pacific Other) correlate at 0.99, and both versions peak in 2018Q3.
- **Non-overlapping quarterly series.** WSTS: sums of monthly flows (2010Q1–2025Q4), validated against 3 × quarter-end 3MMA and against the sum of the four regions. SIA: 3 × the quarter-end trailing 3MMA (primary window 2015Q1–2024Q4).
- **Levels behave as integrated processes.** In most series ADF and PP do not reject a unit root and KPSS rejects stationarity. Tests have low power with 40–64 quarters.
- **Regional and China growth remain persistent.** After quarter-of-year demeaning, ACF(1) is 0.35–0.59, above the 0.21 that temporal aggregation alone would produce (Working, 1960). BIC selects AR(1).
- **The relative China series is not persistent.** For log(China / Asia Pacific Other) and China's share of Asia Pacific, ACF(1) after demeaning is about −0.10, Ljung–Box(8) p ≈ 0.89 and BIC selects AR(0). Quarter-of-year seasonality is significant (HAC p ≈ 2 × 10⁻⁵). The common global cycle largely cancels in the ratio, which behaves approximately as a random walk with seasonality.

#### Step 3 — Break inference robust to dynamics and date search (`R/19_break_robustness_quarterly.R`)

**Test.** One unknown shift in the conditional mean of quarterly growth (or of the change in the relative indicator), with quarter dummies and an AR order declared before estimation. The break date is searched endogenously; no policy date is imposed. p-values come from a recursive residual bootstrap of the no-break model (399 replications) that repeats the date search in every replication, so they account for the search. Primary cases use both iid and wild (Rademacher) bootstraps, with a Holm correction across the three prespecified primary hypotheses.

| Primary series | AR order | sup-F | p (iid) | p (wild) | Holm p (iid / wild) | Maximizing date (descriptive) |
| --- | --- | --- | --- | --- | --- | --- |
| WSTS Worldwide, 2010–2025 | 1 | 6.11 | 0.210 | 0.258 | 0.63 / 0.77 | 2023Q1 → 2023Q2 |
| SIA China, +12-month vintage, 2015–2024 | 1 | 1.48 | 0.870 | 0.910 | 0.87 / 0.91 | 2018Q2 → 2018Q3 |
| SIA log(China / AP Other), +12-month vintage, 2015–2024 | 0 | 3.62 | 0.395 | 0.350 | 0.79 / 0.77 | 2018Q3 → 2018Q4 |

- **Sensitivity.** None of the 15 additional specifications rejects even without multiplicity adjustment (smallest p = 0.12). They cover AR(2), AR(1) for the ratio, the mixed vintage, trimming h = 8 or 10, the four WSTS regions, Asia Pacific Other and the 2025 extension.
- **Model diagnostics.** Null-model residuals show no autocorrelation (Ljung–Box(8) p > 0.30 except Asia Pacific Other, p = 0.077), so non-rejection is not explained by an underfitted model.
- **Descriptive dates.** 2023Q1–Q2 (Worldwide, America, Asia Pacific) coincides with the AI-driven upswing; 2020Q2 (Europe, AP Other) with COVID-19. The 2018Q2–Q3 location for China and the ratio is stable across vintage, trimming and AR order, but **not across sample end**: adding the first-release 2025 data moves the ratio's maximizing date to 2024Q1.

#### Conclusions of Phase 3A

1. The late-2018 breakpoint cluster was selected by BIC in trend-level specifications under conditions associated with high false-break selection risk. The revised quarterly growth-shift tests do not corroborate a single permanent change in mean growth. Because the specifications differ, the audit cannot establish that all the original trend-level breaks were false; the cluster should remain exploratory rather than be presented as established structural-change evidence.
2. With inference that accounts for seasonality, short-run dynamics and endogenous date search, there is **no detectable single shift** in quarterly growth of worldwide sales, Chinese sales, or China relative to the rest of Asia Pacific.
3. **This does not establish the absence of an effect.** At the endogenously selected candidate date after 2018Q3, the descriptive estimate for the relative China series is about −0.030 log points per quarter (approximately −11.5% when annualized as a sustained relative-growth difference). It is not statistically distinguishable from no shift with 39 quarterly changes, and is **not** an estimated policy effect. The tests may have limited power; no formal minimum-detectable-effect or power calculation has yet been completed.
4. In these data, the period around 2018Q3 coincides with the peak of China's share of Asia Pacific, the first reciprocal tariff tranche and the peak of the memory-price cycle; the series cannot separate these explanations. Because 2018Q3 emerged here as the maximizing date, **it cannot be used to choose intervention dates in Phase 3C**.

### Phase 3B — Evidence-based historical chronology: documented

A dated chronology of trade, technology and industrial-policy events has been assembled and sourced, including a distinction between policy announcements, enactment and effective dates where applicable.

- [`docs/phase3b_timeline_guide.md`](docs/phase3b_timeline_guide.md)
- [`data/manual/phase3b_source_registry.csv`](data/manual/phase3b_source_registry.csv)
- [`docs/research_design.md`](docs/research_design.md)

The chronology was assembled **after** the first endogenous breakpoint estimates. This sequencing creates a risk of choosing policy dates because they appear close to discovered breaks. Before estimating intervention effects, the team must define, justify and version its event-selection protocol and distinguish exploratory alignment from prespecified tests. Historical events in 2026 are contextual unless the data window is explicitly extended.

### Phase 3C — Policy-intervention models: not started

**Decision gate.** Phase 3C may begin only after the following are documented and versioned:

- an **event-selection protocol** that derives intervention dates from the Phase 3B documentary chronology (announcement / enactment / effective dates), written before any intervention model is estimated;
- the **relative China / Asia Pacific Other series** (single vintage) as the primary outcome, with global series as context;
- an explicit **power assessment** for each planned test;
- an explicit record of the identification limitations and the scope of the proposed exploratory intervention estimands.

Planned work then includes exploratory intervention models with seasonal and dynamic treatment, transparent event dates, controls and alternative explanations (semiconductor and memory-price cycles, macroeconomic conditions, COVID-19, shortages and AI demand), and explicit limits on causal interpretation. **The international-trade layer is a prospective extension, not a prerequisite for starting Phase 3C.** Later UN Comtrade / U.S. Census data (HS 8542, with HS 8541 and HS 8486 under evaluation) and, if feasible, SEMI statistics could help study trade-diversion and production mechanisms; their availability, comparability and identifying value must be assessed rather than assumed.

A working statistical null may be written as:

$$
H_0: \beta_{\mathrm{policy}} = 0
\qquad\text{versus}\qquad
H_1: \beta_{\mathrm{policy}} \ne 0.
$$

A statistically significant intervention coefficient **would not, on its own, establish causality**. The specific estimand and identifying assumptions must be stated before claims about policy impacts.

### Phase 4 — Cointegration and multivariate dynamics: planned

Candidate methods include rechecking integration orders; Engle–Granger/Phillips–Ouliaris and Johansen approaches where suitable; VECM where supported; China versus Asia-Pacific Other comparisons; and sensitivity to structural breaks. Accounting identities must be excluded from systems that assume distinct variables.

### Phase 5 — Broader robustness and identification: planned

Assess dependence on samples, intervention windows, controls, alternative models and competing explanations, subject to the data actually available. Causal identification remains an open design problem, not an established project result.

## Reproducibility

Run scripts from the repository root (open the `.Rproj` in RStudio). Required packages: `strucchange` (Scripts 17 and 19) and `tseries` (Script 18).

```r
# Script 17 — audit replication (Monte Carlo replications set by environment variable)
Sys.setenv(SEMICON_AUDIT_REPS = "200")
source("R/17_reproduce_audit_checks.R", encoding = "UTF-8")

# Script 18 — single vintage and quarterly diagnostics
# Confirm that this is the selected official version before a clean rerun.
source("R/18_quarterly_series_diagnostics.R", encoding = "UTF-8")

# Script 19 — quarterly break inference (bootstrap replications per case)
Sys.setenv(SEMICON_S19_BOOT_REPS = "399")
source("R/19_break_robustness_quarterly.R", encoding = "UTF-8")
```

In the recorded runs, outputs were organized under `output/tables/phase3/audit17/`, a Script 18 audit directory (local variants used different folder names), and `output/tables/phase3/audit19_revised/`. The revised Script 19 reads `data/processed/quarterly_series_2010_2025.csv` and `data/processed/sia_regional_3mma_vintage12m_2015_2025.csv`. The exact **official Script 18 version, matching output location and clean-run dependency chain are not yet frozen**. The code above documents the intended repository commands, not a claim that the full pipeline has been reproduced from a fresh checkout. Scripts 17–19 are intended not to overwrite Phase 3A outputs from Scripts 14–16. Reproducibility requires preserving source data, script versions, R/package versions, seeds and output files.

The scripts and project documentation are maintained primarily in English; preliminary reports are available in English and Spanish.

## Preliminary findings and interpretation

- Global sales exhibit strong long-term growth, cycles and a marked quarter-end calendar structure.
- Regional market shares have changed considerably; `America` denotes a region, not U.S. domestic sales.
- China's share within Asia Pacific reached approximately **57.9% in September 2018** and was approximately **47.7% in December 2025** in the processed series (single-vintage quarterly peak: 2018Q3).
- The late-2018 trend-break cluster from the original Phase 3A is **not corroborated by the revised quarterly growth-shift tests**. Those tests find no statistically significant single permanent shift in the mean growth of worldwide, Chinese or relative China/Asia-Pacific sales; they do not adjudicate every possible trend-level change or establish that policy had no effect.
- No causal effect of U.S.–China trade or technology policy has been identified.

## Research material and repository structure

```text
R/                    Reproducible data-construction and econometric scripts
data/processed/       Analysis-ready market series
data/manual/          Evidence-based historical policy source registry
docs/                 Research design, historical timeline and preliminary reports
output/figures/       EDA, diagnostic and structural-break figures
output/tables/        Validation outputs, parsing audits and model results
```

Preliminary reports:

- [English — Preliminary Report: Phases 1 & 2](docs/Semiconductors_II_Preliminary_Report_Phases_1_and_2.pdf)
- [Español — Informe preliminar: Fases 1 y 2](docs/Semiconductores_II_Informe_Preliminar_Fases_1_y_2.pdf)

## Research integrity and AI-assisted review

The project uses AI assistance for code review, methodological challenge and documentation. AI-assisted findings are treated as **provisional until reproduced** from versioned data and code. Numerical findings here are drawn from recorded local runs; Script 17 S2 Monte Carlo estimates specifically refer to the author's v2 run (200 replications). Values available only from an AI-run Script 17 v3 review have been omitted pending the author's own reproduction. A claim of full end-to-end reproducibility from a clean repository checkout remains **pending** the selection of one canonical Script 18 version, alignment of Script 19 inputs and preservation of run metadata. Tools are not represented as human researchers or independent empirical validators. Responsibility for research design, execution, verification and scientific claims remains with the project author.

---

**Status (18 November 2026):** Phases 1–2 completed · Phase 3A closed after methodological reassessment (Scripts 17–19) · Phase 3B chronology documented · Phase 3C **not started** (pending event-selection protocol and power assessment) · Phases 4–5 planned.
