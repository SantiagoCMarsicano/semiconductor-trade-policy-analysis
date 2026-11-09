Semiconductors II

Trade War, Technology Policy, and the Global Semiconductor Market

Econometric research project on whether growing US–China trade and technology rivalry was associated with statistically significant changes in global and regional semiconductor market dynamics.

It continues Semiconductors I, moving from descriptive industry analysis toward reproducible time-series and econometric research.

Research question

Did trade and technology policies associated with US–China strategic rivalry significantly alter the dynamics of the global semiconductor market?

The project does not assume a policy effect in advance. Historical policy periods are treated as working hypotheses to be compared with statistically detected changes in the data.

Empirical architecture

Layer

Source

Period

Main use

Monthly

WSTS Historical Billings

2010–2025

Global/regional dynamics, seasonality and structural breaks

3-month moving average

SIA/WSTS

2015–2025

China vs. Asia-Pacific Other decomposition

The project uses America as its normalized label for the WSTS Americas region. It represents the regional market and should not be interpreted as the United States alone.

Current status

Phase 1 — Data construction and validation

WSTS monthly and 3-month moving-average series processed for 2010–2025.

192 monthly observations per regional series, with no missing values.

120 official SIA/WSTS monthly PDFs parsed and validated.

China / Asia-Pacific Other series reconstructed for 2015–2025.

Regional totals and source consistency checks completed.

Phase 2 — Exploratory analysis and time-series diagnostics

Regional sales, growth, market shares and seasonality analyzed.

ADF, KPSS and Phillips–Perron stationarity tests completed.

Ljung–Box, ACF and PACF diagnostics completed.

Monthly data compared directly with the 3-month moving average.

Preliminary reports

English: Preliminary Report — Phases 1 & 2

Español: Informe preliminar — Fases 1 y 2

Preliminary findings

Global semiconductor sales show strong long-run growth together with pronounced cycles and seasonality.

America gains substantial global market share, while Europe and Japan lose relative weight.

China grows strongly in absolute terms, but its share within Asia-Pacific peaks around 57.9% in September 2018 and ends 2025 near 47.7%.

First log differences are substantially more compatible with stationarity than levels.

Strong serial dependence remains after differencing, requiring explicit time-series modelling.

The 3-month moving average preserves long-run levels well but materially changes short-run volatility and autocorrelation.



Monthly WSTS semiconductor sales by region, 2010–2025.

Econometric direction

The next stage will test whether statistically detected structural changes align with the historical policy chronology.

Planned methods include:

Bai–Perron structural-break tests;

segmented / intervention time-series models;

explicit treatment of seasonality and autocorrelation;

robustness checks against major semiconductor-cycle and macroeconomic shocks.

The central policy hypothesis remains:

[
H_0: \beta_{policy}=0
]

against

[
H_1: \beta_{policy}\neq0
]

No causal policy effect has been established at the current stage.

Repository

R/                 Reproducible data and econometric scripts
data/processed/    Analysis-ready datasets
docs/              Research design and preliminary reports
output/figures/    Exploratory and diagnostic figures
output/tables/     Validation and econometric results

The analytical code and repository documentation are maintained in English. Preliminary academic reports are available in both English and Spanish.

Status: Phases 1 and 2 completed · Structural-break analysis next