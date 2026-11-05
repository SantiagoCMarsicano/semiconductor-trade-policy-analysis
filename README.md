# Semiconductors II
## Trade War, Technology Policy, and the Global Semiconductor Market

Econometric research project developed as a continuation of **Semiconductors I — The Global Semiconductor Industry (2010–2025): Growth, Transformation, and Technological Leadership**.

While Semiconductors I followed a mainly descriptive and exploratory approach, Semiconductors II moves toward inferential and econometric analysis of the structural changes observed in the industry.

The project studies whether trade, industrial, and technology policies implemented in the context of growing strategic rivalry between the United States and China were associated with statistically significant changes in global and regional semiconductor sales dynamics.

---

# 1. Research Question

The central research question is:

> **Did trade and technology policies implemented by the United States and China in the context of the so-called trade war significantly alter the dynamics of the global semiconductor market?**

The analysis will focus particularly on potential changes in:

- semiconductor sales in China;
- growth of the Chinese market;
- China's share of global semiconductor sales;
- the evolution of Asia-Pacific excluding China;
- the relative evolution of the Americas;
- the regional distribution of the global market;
- trends and volatility in the main semiconductor sales series.

The objective is not to assume in advance that these policies had an effect, but to test whether the available evidence is statistically consistent with that hypothesis.

---

# 2. General Hypothesis

## Null hypothesis

\[
H_0:
\text{US-China trade and technology policies did not produce statistically significant changes in semiconductor sales dynamics.}
\]

## Alternative hypothesis

\[
H_1:
\text{US-China trade and technology policies produced statistically significant changes in semiconductor sales dynamics.}
\]

The general hypothesis will later be divided into specific hypotheses related to growth, regional market shares, trend changes, volatility, and structural breaks.

---

# 3. Initial Historical Periodization

The analysis starts from the following preliminary historical periodization:

\[
\underbrace{2010-2013}_{Pre}
\rightarrow
\underbrace{2014-2016}_{Industrial\ Policy / Early\ Rivalry}
\rightarrow
\underbrace{2017-2020}_{Trade-Tech\ War\ I}
\rightarrow
\underbrace{2021-2024}_{Biden}
\rightarrow
\underbrace{2025}_{Trump\ II}
\]

### 2010–2013 — Pre / Context

Reference period before the intensification of technological and commercial rivalry.

China is still included within the broader Asia-Pacific category in part of the historical data available for this stage.

### 2014–2016 — Industrial Policy / Early Rivalry

Period characterized by the growing importance of industrial policy, technological security, and strategic competition in advanced manufacturing.

From 2014 onward, available WSTS/SIA series make it possible to identify China separately from the rest of Asia-Pacific.

### 2017–2020 — Trade-Tech War I

First Donald Trump administration.

During this period, US-China trade disputes intensified through tariffs, technology restrictions, and measures involving strategically important firms and technologies.

### 2021–2024 — Biden

The change in administration provides an opportunity to test whether the previous market dynamics changed.

While several earlier trade measures remained in place, additional policy instruments became increasingly important, including:

- industrial policy;
- subsidies;
- economic security measures;
- restrictions on advanced semiconductors;
- export controls;
- domestic manufacturing incentives.

A specific research question will be:

> **Was semiconductor market dynamics under the Biden administration statistically different from those observed during Trump I?**

### 2025 — Trump II

The year 2025 will initially be treated mainly as an extension of the historical series.

Because there are too few post-treatment observations, Trump II will not initially be treated as an independent period for strong causal inference.

---

# 4. Methodological Principle

The historical periodization is a **working hypothesis**, not an econometric conclusion.

The project will therefore compare:

### Historically expected breaks

with

### Statistically detected breaks

This makes it possible to evaluate whether the observed changes in semiconductor sales actually coincide with the proposed political and policy chronology.

A result in which the statistically detected breaks do not match the expected dates will also be considered informative.

---

# 5. Main Variables

The initial master dataset will seek to include the following regional sales series:

```text
world
china
asia_pacific_other
americas
europe
japan
```

Derived variables will include:

```text
china_growth
americas_growth
asia_pacific_other_growth

china_share
americas_share
asia_pacific_other_share

china_log_sales
americas_log_sales
asia_pacific_other_log_sales
```

Policy and intervention variables may include:

```text
policy_period
post_2018
post_2022
trump1_period
biden_period
trump2_period
covid_period
ai_period
```

The exact definition of each variable will be documented before estimation.

---

# 6. Interpretation of WSTS Data

Regional WSTS data must be interpreted carefully.

The China series primarily represents semiconductor sales or shipments to the Chinese market.

Therefore:

```text
WSTS China
```

should not automatically be interpreted as:

```text
Semiconductor production in China
```

The initial project focuses on **semiconductor market dynamics**, not exclusively on the physical location of semiconductor production.

---

# 7. Initial Specific Hypotheses

## H1 — Change during Trade-Tech War I

\[
H_0:\beta_{Post2018}=0
\]

\[
H_1:\beta_{Post2018}\neq0
\]

The project will test whether a statistically significant change in level, growth, or trend appears around 2017–2018.

## H2 — China versus Asia-Pacific Other

The project will test whether the trajectory of China differs from the rest of Asia-Pacific after the intensification of the trade conflict.

This comparison may help distinguish between:

- broader regional shocks affecting Asia;
- changes more specific to the Chinese market.

## H3 — Trump I versus Biden

\[
H_0:
\beta_{TrumpI}=\beta_{Biden}
\]

\[
H_1:
\beta_{TrumpI}\neq\beta_{Biden}
\]

This hypothesis evaluates whether the change in administration and policy instruments coincided with a statistically significant change in market dynamics.

## H4 — Regional Rebalancing

Semiconductors I descriptively identified particularly strong relative growth in the Americas during the final years of the sample.

Semiconductors II will test whether this process reflects:

- normal semiconductor industry cycles;
- a structural change;
- or a phenomenon temporally associated with geoeconomic fragmentation and new industrial policies.

## H5 — Structural Breaks

The project will test whether the data identify structural breaks close to major policy events.

Historically imposed break dates will be compared with methods that detect changes endogenously.

---

# 8. Initial Econometric Strategy

The analysis will proceed gradually.

## Stage 1 — Descriptive Analysis

Reconstruction and extension of the main regional series:

- levels;
- growth rates;
- regional market shares;
- indices;
- volatility.

## Stage 2 — Time-Series Diagnostics

Before estimating econometric models, the statistical properties of the series will be examined using tools such as:

```text
ADF test
KPSS test
ACF
PACF
seasonality analysis
autocorrelation tests
```

## Stage 3 — Hypothesis Testing

Comparisons across policy periods will be used to evaluate changes in:

- average growth;
- market share;
- trend;
- volatility.

These tests will be interpreted as preliminary statistical evidence rather than definitive causal identification.

## Stage 4 — Structural Breaks

Methods may include:

```text
Chow test
Bai-Perron multiple structural breaks
```

The objective is to compare statistically estimated breaks with the historical policy chronology.

## Stage 5 — Intervention Models

The project will estimate intervention or segmented time-series models.

Example:

\[
Y_t =
\beta_0
+
\beta_1 Trend_t
+
\beta_2 Post2018_t
+
\beta_3 TrendAfter2018_t
+
\epsilon_t
\]

This framework allows the analysis to distinguish between:

- immediate level changes;
- changes in the post-intervention trend.

## Stage 6 — Extensions

Additional methods will only be incorporated if they are justified by the data and research question.

Potential extensions include:

```text
ARIMA / ARIMAX
VAR
VECM
Granger causality
Difference-in-Differences
Event Study
Synthetic Control
```

The objective is not to use every available econometric technique.

Model selection will depend on the properties of the data and the economic question being addressed.

---

# 9. Confounding Factors

A statistically significant change after a policy intervention does not automatically imply causality.

The semiconductor market was simultaneously affected by several major shocks, including:

- the global semiconductor cycle;
- cryptocurrency demand;
- the COVID-19 pandemic;
- the global semiconductor shortage;
- smartphone demand;
- the PC market;
- automotive demand;
- artificial intelligence;
- data-center investment;
- interest rates;
- global economic growth.

These factors must be considered when interpreting the results.

---

# 10. Complementary Data

The first stage of the project will rely primarily on WSTS/SIA data.

Large additional datasets will not be incorporated unless a specific research question cannot be answered with the initial data.

Potential extensions include:

```text
UN Comtrade
BACI - CEPII
USITC
USTR
BIS
MOFCOM
World Bank
IMF
OECD
```

For example, bilateral trade data would be required to investigate **trade diversion**.

This could allow the project to examine whether lower US-China semiconductor trade was accompanied by higher imports from:

- Taiwan;
- South Korea;
- Japan;
- Malaysia;
- Vietnam;
- other economies.

---

# 11. Repository Structure

```text
semiconductor-trade-policy-analysis/
│
├── R/
│   ├── 01_data_import.R
│   ├── 02_data_cleaning.R
│   ├── 03_descriptive_analysis.R
│   ├── 04_hypothesis_tests.R
│   ├── 05_structural_breaks.R
│   └── 06_intervention_models.R
│
├── data/
│   ├── raw/
│   └── processed/
│
├── docs/
│   └── research_design.md
│
├── output/
│   ├── figures/
│   └── tables/
│
├── references/
│   ├── policies/
│   ├── papers/
│   └── industry_reports/
│
├── .gitignore
├── LICENSE
└── README.md
```

Technical code, variable names, function names, comments, and repository documentation will be maintained in English.

The final academic report will be written in Spanish.

---

# 12. Relationship with Semiconductors I

Semiconductors I followed a mainly exploratory and descriptive approach.

Its main question was:

> **How did the global semiconductor industry evolve between 2010 and 2025?**

Semiconductors II builds on those descriptive findings and asks:

> **Is there statistical evidence that growing US-China trade and technology rivalry altered that trajectory?**

Conceptually:

```text
Semiconductors I
What happened?
```

↓

```text
Semiconductors II
Did trade and technology policy change the dynamics?
```

---

# 13. First Milestone

The first milestone is to build a validated **master dataset for 2010–2025** that reproduces the main regional sales series and separates China whenever the data allow it.

The initial dataset should include:

```text
date
world
china
asia_pacific_other
americas
europe
japan
policy_period
```

as well as reproducibly generated derived variables.

Once the dataset is validated, the first analytical visualization will compare:

> **China vs Asia-Pacific Other vs Americas**

with the main trade and technology policy periods marked on the time axis.

This figure will serve as the starting point for the econometric analysis.

---

# 14. Final Objective

Semiconductors II aims to move from descriptive analysis toward applied economic research following the logic:

```text
Historical hypothesis
        ↓
Data
        ↓
Statistical testing
        ↓
Structural breaks
        ↓
Econometric modelling
        ↓
Economic interpretation
```

The purpose of the project is not to prove in advance that the trade war affected the semiconductor industry.

The objective is to determine **whether there is sufficient statistical evidence to reject the hypothesis that US-China trade and technology policies did not significantly alter semiconductor market dynamics**, and to evaluate how far any observed changes can be distinguished from other major shocks affecting the industry.
