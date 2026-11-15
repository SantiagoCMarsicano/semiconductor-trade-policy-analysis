Semiconductors II
Trade War, Technology Policy, and the Global Semiconductor Market
Econometric research project on whether growing US–China trade and technology rivalry was associated with statistically significant changes in global and regional semiconductor market dynamics.

It continues Semiconductors I, moving from descriptive industry analysis toward reproducible time-series and econometric research.
Research question
Did trade and technology policies associated with US–China strategic rivalry significantly alter the dynamics of the global semiconductor market?

The project does not assume a policy effect in advance. Historical policy periods are treated as working hypotheses to be compared with statistically detected changes in the data.
Empirical architecture
Layer	Source	Period	Main use
Monthly	WSTS Historical Billings	2010–2025	Global/regional dynamics, seasonality, structural breaks and policy-intervention models
3-month moving average	SIA/WSTS	2015–2025	China vs. Asia-Pacific Other decomposition and China-specific structural-break analysis


The project uses America as its normalized label for the WSTS Americas region. It represents the regional market and should not be interpreted as the United States alone.
Current status
Phase 1 — Data construction and validation — Completed
- WSTS monthly and 3-month moving-average series processed for 2010–2025.
- 192 monthly observations per WSTS regional series, with no missing values.
- 120 official SIA/WSTS monthly PDFs parsed and validated.
- China / Asia-Pacific Other series reconstructed for 2015–2025.
- Regional totals and source-consistency checks completed.
Phase 2 — Exploratory analysis and time-series diagnostics — Completed
- Regional sales, growth, market shares and seasonality analyzed.
- ADF, KPSS and Phillips–Perron stationarity tests completed.
- Ljung–Box, ACF and PACF diagnostics completed.
- Monthly data compared directly with the 3-month moving average.
- First log differences are substantially more compatible with stationarity than levels.
- Strong serial dependence remains after differencing, requiring explicit time-series modelling.
- The 3-month moving average preserves long-run levels well but materially changes short-run volatility and autocorrelation.
Phase 3 — Structural breaks and policy intervention — In progress
Phase 3A — Endogenous structural-break detection — Initial analysis completed; robustness reassessment pending
The project first estimates structural changes without imposing policy dates.

**Robustness review — 14 November 2026:**
A second methodological audit identified a strong quarterly calendar pattern, substantial residual serial dependence, mechanical dependence induced by the 3-month moving average, and potential over-selection of trend breaks under persistent errors.

The existing Phase 3A breakpoints are therefore retained as descriptive candidate structural changes pending replication with quarterly or seasonally adjusted data and explicit dynamic treatment.

Main results from WSTS monthly data:
- SupF tests do not reject structural stability in mean monthly log growth for America, Europe, Japan, Asia Pacific or Worldwide at either the 5% or 10% level when HAC inference is used.
- Log-level trend models show strong structural instability across all major regions.
- Bai–Perron break dates are particularly robust for America, Asia Pacific and Worldwide.
- A highly synchronized break appears around late 2018:
  - America: November 2018;
  - Asia Pacific: November 2018;
  - Worldwide: November 2018;
  - Japan: December 2018.
China extension using the SIA/WSTS 3-month moving-average series:
- China does not show robust evidence of a break in mean smoothed sales growth.
- China does show strong instability in its long-run smoothed trajectory.
- A China break is estimated in December 2018, only one month from the November 2018 WSTS Asia-Pacific and Worldwide breaks.
- China and Asia-Pacific Other do not display identical breakpoint patterns, supporting the value of decomposing the region.
- China’s share within Asia Pacific also shows structural changes in trend, although the exact first breakpoint within 2018 is more specification-sensitive.
HAC robustness checks:
- The unusually large default-HAC SupF statistics for China and China share are not driven by near-singular covariance matrices.
- Structural-instability conclusions remain significant under Newey–West HAC estimators with 2, 6 and 12 lags and under a Quadratic Spectral estimator with prewhitening.
- Residual serial dependence remains strong after segmentation; intervention models therefore need explicit dynamic and seasonal treatment.
- Break dates are statistical, endogenous dates and must not be interpreted as causal policy effects.
Scripts:
- R/14_structural_breaks.R — WSTS monthly structural-break analysis.
- R/15_china_structural_breaks.R — China / Asia-Pacific Other structural-break extension using smoothed monthly data.
- R/16_hac_diagnostics.R — HAC numerical and inferential robustness checks.
Phase 3B — Historical policy chronology — Completed
A dated and documented chronology of major trade, technology and industrial-policy events has been assembled after the initial endogenous break dates were estimated.

The chronology will be used to assess whether detected structural changes align temporally with major events such as:
- US–China trade-war escalation;
- semiconductor and technology export restrictions;
- COVID-19 and the global semiconductor shortage;
- US industrial-policy measures;
- later export-control regimes and technology restrictions.
Temporal alignment will be treated as evidence for further modelling, not as causal identification.
Phase 3C — Policy-intervention models — Planned
Planned work includes:
- segmented / intervention time-series models;
- explicit treatment of seasonality and residual autocorrelation;
- comparison of alternative intervention dates and specifications;
- robustness checks against major semiconductor-cycle and macroeconomic shocks.
The central policy hypothesis remains:
\[
H_0: \beta_{policy}=0
\]
against
\[
H_1: \beta_{policy}\neq0
\]
No causal policy effect has been established at the current stage.
Phase 4 — Cointegration and multivariate dynamics — Planned
Phase 4 will study whether major semiconductor markets share stable long-run equilibrium relationships.

Planned methods include:
- formal re-verification of integration orders for the exact variables entering each system;
- bivariate Engle–Granger / Phillips–Ouliaris cointegration analysis;
- Johansen cointegration analysis for the WSTS regional system;
- exclusion of accounting identities such as Worldwide together with all of its regional components;
- VECM estimation if the cointegration rank is greater than zero;
- China vs. Asia-Pacific Other long-run relationship analysis;
- robustness of cointegrating relationships to structural breaks.
This phase will distinguish between:
- Phase 3: when market dynamics changed;
- Phase 4: whether regional markets remained linked by long-run equilibrium relationships.
Phase 5 — Robustness and causal identification — Planned
A later phase will assess how strongly the policy interpretation survives alternative specifications, windows, controls and competing explanations.

Preliminary reports:
- [English — Preliminary Report: Phases 1 & 2](docs/Semiconductors_II_Preliminary_Report_Phases_1_and_2.pdf)
- [Español — Informe preliminar: Fases 1 y 2](docs/Semiconductores_II_Informe_Preliminar_Fases_1_y_2.pdf)

Preliminary findings
- Global semiconductor sales show strong long-run growth together with pronounced cycles and seasonality.
- America gains substantial global market share, while Europe and Japan lose relative weight.
- China grows strongly in absolute terms, but its share within Asia Pacific peaks around 57.9% in September 2018 and ends 2025 near 47.7%.
- First log differences are substantially more compatible with stationarity than levels.
- Structural instability is much stronger in long-run trajectories than in mean monthly growth.
- Late 2018 emerges independently as an important structural-break window across WSTS monthly data and the China 3-month moving-average series.
- No causal interpretation is assigned to these break dates prior to policy-intervention modelling.
Repository
R/                  Reproducible data and econometric scripts
data/processed/     Analysis-ready datasets
docs/               Research design and preliminary reports
output/figures/     Exploratory, diagnostic and structural-break figures
output/tables/      Validation and econometric results
The analytical code and repository documentation are maintained in English. Preliminary academic reports are available in both English and Spanish.

Status: Phases 1 and 2 completed · Phase 3A initial analysis completed, robustness reassessment pending · Phase 3B completed · Phase 3C pending
