# Semiconductors II — Economic framework for Phase 3C

**Approved: 18 November 2026.**
This document defines the economic question, variables, hypotheses, limits and general empirical approach of Phase 3C. The econometric protocol (exact specifications, statistical hypotheses, inference, power and sensitivity analyses) is a separate document: [`docs/phase3c_protocol.md`](phase3c_protocol.md).

**Guiding principle:** this framework does not anticipate any result. The investigation must be able to conclude with a negative, positive, transitory or statistically indistinguishable-from-zero association, and any of these outcomes is informative.

---

## 1. Research question

**Overall project question:** did the trade and technology policies associated with the U.S.–China rivalry alter the dynamics of the global semiconductor market?

**Main Phase 3C question:**

> **How did China's share of semiconductor shipments destined for Asia-Pacific evolve between 2015 and 2024, and what changes are observed around the main trade and technology policies of the U.S.–China rivalry?**

**Secondary questions:**

- Are the observed changes persistent or transitory?
- Are the 2018 wave (tariffs) and the 2022 wave (technology controls) associated with different patterns?

**Relation to the literature.** The reorganisation of global value chains after 2018 has been documented with disaggregated international trade data (Fajgelbaum et al., 2024; Freund et al., 2024; Alfaro and Chor, 2023). Our contribution is not to demonstrate that relocation again, but to study **whether that transformation is also reflected in the regional destinations of semiconductor sales**. Production relocation is a **mechanism to be investigated**, not something our indicator measures directly.

## 2. What our variables measure

### 2.1 Source definitions

- **Regional allocation.** WSTS assigns each sale according to the **ship-to destination of the customer, distributor or consignee**, regardless of where the semiconductor was manufactured. The destination may be a factory, but also a distributor.
- **Coverage.** WSTS incorporates **estimates for non-participating companies** to represent the total market. It cannot be assumed that Chinese manufacturers are automatically excluded from the statistics.
- **Definition of China.** According to the *WSTS Product Classification 2021* (p. 25, section 4):
  - **Asia Pacific/China:** People's Republic of China, **including Hong Kong and Macau**.
  - **Asia Pacific/All Other:** the rest of Asia-Pacific, including Taiwan, Korea, Singapore, Malaysia, Vietnam and India, among others.
- **Stability of the definition.** An earlier WSTS classification, available in the 2018 SIA archive, uses the same territorial grouping (China with Hong Kong and Macau; "All Other" with Taiwan, Korea, Singapore, Malaysia, India, Australia and New Zealand, among others) and the same ship-to criterion, including when the destination is a distributor. This reduces uncertainty about the stability of the definitions, although it does not formally demonstrate that they were unchanged in every year of 2015–2024.
- **Units.** Current U.S. dollars.

### 2.2 Outcome variable

**y = log(China / Asia Pacific Other)**, quarterly series with a **homogeneous 12-month revision horizon**, 2015Q1–2024Q4: each observation comes from a different release, but all with the same relative age (the value published 12 months after the reference month).

It measures the **regional distribution of semiconductor shipment destinations within Asia-Pacific**. It is the outcome of interest in its own right: if destinations were reallocated between China and the rest of Asia, the ratio is precisely the measure of that reallocation. It is **not** presented as "treated group minus control group".

### 2.3 What the indicator observes and what it does not

| Phenomenon | Directly observed? |
| --- | --- |
| Change in the regional destination of chip shipments | **Yes**, within WSTS statistical coverage |
| Relocation of assembly plants | **No**; only possible indirect signals |
| Substitution of foreign semiconductors by domestic ones | **No**, not directly |
| Changes in logistics or intermediation (e.g. via Hong Kong) | **Not separable**: part of "China" |

### 2.4 Measurement limitations

1. **Prices:** a fall in memory prices reduces dollar sales even if the same chips are shipped.
2. **Hong Kong and Macau:** a change in the ratio may reflect changes in distribution or intermediation, not only production changes. We do not know what share of the variation corresponds to Hong Kong; the aggregate series does not allow it to be separated.
3. **Stability of the definition:** the 2018 and 2021 classifications coincide (section 2.1), but it has not been formally demonstrated that the definition was identical in every year of 2015–2024.
4. **Sample size:** 40 quarters in the main sample; 2025 is still first-release data.

## 3. Hypotheses: compatible patterns, not identification criteria

Several hypotheses produce similar movements in the indicator. Each is therefore stated as a **compatible pattern**, not as a test able to identify it on its own.

| | Hypothesis | Proposed mechanism | Compatible pattern | Also compatible with |
| --- | --- | --- | --- | --- |
| **H1** | U.S. tariffs (Section 301, from July 2018) | Supply-chain reorganisation → part of assembly or purchasing moves out of China → chip destinations change | Gradual, persistent decline in China's share | H3, H4 |
| **H2** | Technology controls (Huawei 2019–2020; October 2022 and extensions) | Restrictions on selling certain chips and equipment to Chinese firms | Decline for China, possibly preceded by anticipatory stockpiling | H3, H4; AI-driven demand |
| **H3** | Chinese industrial substitution (Big Fund, Made in China 2025) | Substitution of foreign chips by domestic ones | Gradual decline, with no precise date | H1, H4 |
| **H4** | Changes in demand, products or relative prices | Saturation of the Chinese smartphone market, product composition, price cycles | Any pattern, including persistent declines | H1–H3 |

**Persistence helps interpretation but is not a sufficient criterion.** A persistent change may be due to relocation, but also to structural demand, technological change, product composition or relative prices; and a corporate reorganisation need not be irreversible.

### 3.1 Actual scope of the tariffs (H1)

The exposure of electronics to the tariffs was **not uniform**:

- **List 1** (in force from 6 July 2018): 25% on 818 tariff lines, about USD 34 billion.
- **Lists 2 and 3** (August and September 2018): escalation to further products.
- **List 4B**: included **mobile phones and laptop computers**; it was postponed to 15 December 2019 and **suspended before taking effect** in the context of the Phase One agreement.

Consequence: the products that consume the most semiconductors did not face the tariff directly. This **weakens** the interpretation of a generalised relocation of assembly induced by the 2018 tariffs. It does not rule out other channels—tariffs on other products, trade-policy uncertainty and anticipation of future restrictions—but it requires studying the **effective exposure** of each sector rather than assuming a uniform incentive.

### 3.2 Scope of the technology controls (H2)

The October 2022 controls have a **much narrower** technological coverage than total WSTS-recorded sales. Their transmission to the aggregate must be justified, not assumed.

## 4. Competing explanations

| Factor | Why it may look like a policy effect | Treatment |
| --- | --- | --- |
| **Memory cycle 2017–2019** | Peak in 2018, downturn in 2019 | The ratio removes only components **strictly proportional** to both regions; it does not remove differences in product composition (Korea and Taiwan are in "Asia Pacific Other"). Discuss explicitly when interpreting 2018 |
| **COVID-19 and chip shortage 2020–2022** | Large supply and demand shocks | Sensitivity with indicators for 2020Q1–Q2 |
| **AI-driven demand from 2023** | AI servers assembled in Taiwan raise "Asia Pacific Other" | Almost coincides with the October 2022 controls. Separating them is very difficult with these data; this must be stated explicitly |
| **Logistics via Hong Kong** | Changes in intermediation alter "China" | Not separable with the aggregate series; declared limitation |
| **Chinese demand and product composition** | Gradual declines unrelated to policy | H4; declared limitation |
| **Exchange rates** | All values in dollars | Probably minor effect on the ratio |

## 5. Econometric design

### 5.1 Type of design

**Interrupted time series on a relative regional indicator.**

- **It is not difference-in-differences.** It is a single aggregate series, not a panel. Moreover, if reallocation occurred, Asia Pacific Other was also affected: what China loses, another country gains.
- **What it allows:** estimating changes in trajectory around events defined in advance, removing shocks that affect both regions proportionally.
- **What it does not allow:** automatically constructing a credible no-policy counterfactual.

### 5.2 Main model

Δy_t = α + γ₂·Q2_t + γ₃·Q3_t + γ₄·Q4_t + β·D_t + ε_t

- **Δy_t:** quarterly change in log(China / Asia Pacific Other).
- **α:** intercept of the **reference quarter (Q1) before the event**, conditional on the seasonal dummies. It is not exactly the raw mean growth of the pre-event period. With only 13 pre-event quarterly changes (2015Q2–2018Q2), it is imprecisely estimated.
- **Q2–Q4:** quarterly seasonality; control, not interpreted.
- **D_t:** equals 1 from the event quarter onwards. It is a **step in Δy**, i.e. a **persistent change in relative growth** after the event, not simply a fall in level.

This model is consistent with the Phase 3A diagnostics: for the ratio, after controlling for seasonality, quarterly changes show no significant autocorrelation.

### 5.3 How to read β

| Expression | Meaning |
| --- | --- |
| β | Change in the quarterly growth of the ratio, in log points |
| 4β | Approximation in annual log points |
| 100·(exp(4β) − 1) | Annual % change in the China / Asia Pacific Other ratio |
| Share points | Require a further conversion: China's share is s = R / (1 + R), with R the ratio |

- If autoregressive terms are added, the meaning changes: the long-run effect becomes β / (1 − φ).
- A **pulse** in Δy (a one-time level shift) will be included as an alternative specification only if it is predefined in the protocol with economic and documentary justification.

**What β estimates, and what it does not.** D_t remains active from 2018Q3 to 2024Q4. The post-event period simultaneously includes the 2018–2019 trade escalation, the Huawei restrictions, the pandemic and chip shortage, the 2022 technology controls and the expansion of AI-related demand. β therefore estimates a **difference in relative growth between two historical periods** (2015Q2–2018Q2 versus 2018Q3–2024Q4), conditional on the model and seasonality. **It does not identify the effect of the tariffs alone.**

### 5.4 Date of the 2018 event

| Specification | Rule | Justification |
| --- | --- | --- |
| **Main** | **2018Q3** | Contains 6 July 2018, entry into force of the first tranche |
| Sensitivity | 2018Q4 | First quarter entirely after that entry into force |

- Q3 also captures the August and September escalations. β is therefore interpreted as an **association with the onset of the tariff-escalation regime**, not as the isolated effect of the 6 July measure.
- The Q4 sensitivity is justified by the **temporal exposure of the quarters**.

**How this date was reached (transparency statement).** The Q3 rule has its own institutional justification, but the project had already observed the data and the breakpoints near 2018 (Phase 3A, Scripts 14–19) before this framework was formulated. The analysis is therefore characterised as **retrospective, with specifications fixed before running the new models**:

| Statement | Defensible? |
| --- | --- |
| We chose Q3 because it contains 6 July 2018 | Yes |
| We will fix Q3 before running the intervention model | Yes |
| We never used information from earlier results to orient the study towards 2018 | **No** |
| The future test will be an independent confirmation of the Phase 3A findings | **No** |

This does not invalidate the investigation; it characterises it correctly.

### 5.5 The 2022 event: exploratory

For October 2022 only **9 quarters** remain (2022Q4–2024Q4), and the event coincides with the rise of AI-related demand. It is analysed as a **second, exploratory investigation**, not as the main test.

### 5.6 To be defined in the protocol, before estimation

- Treatment of the pre-event trend and an alternative counterfactual scenario (for example, zero pre-event relative growth).
- Inference method and residual diagnostics.
- Multiple-testing correction, if more than one event is tested.
- **Power assessment** for the main test.
- Placebo dates: position of the event date relative to other possible dates.

Many events, anticipation terms, lags and controls will not be estimated simultaneously: the sample does not allow it.

## 6. Organisation of Phase 3C

| Stage | Question | Output |
| --- | --- | --- |
| **A. Economic evolution** | When and by how much did China's share change? | Figures, magnitudes and historical description |
| **B. 2018 intervention** | Was there a change in trajectory around the onset of the tariffs? | Main model with a predefined date |
| **C. Sensitivity and competing explanations** | What happens when assumptions or windows change, and with 2022? | Robustness and limits |
| **D. Economic interpretation** | Which mechanisms are compatible with the evidence? | Discussion grounded in the literature and the results |

2019, Chinese industrial policy and AI-related demand are not ignored: they are essential for interpreting the results and avoiding undue attribution.

## 7. Three levels of conclusion, always kept separate

| Level | Question | Example wording |
| --- | --- | --- |
| **Statistical evidence** | Is there a change in trajectory distinguishable from zero around the event? | "Between 2018Q3 and 2024Q4, China's relative growth was X points lower than in 2015Q2–2018Q2 (CI: …)", without attributing it to a single policy |
| **Economic interpretation** | Which mechanisms are compatible with that pattern? | "The pattern is compatible with a reallocation of destinations; it does not distinguish between relocation, substitution or demand changes" |
| **Causal attribution** | Can the change be attributed to the policy? | Outside the current scope, except under explicit assumptions and additional data |

**Outside the scope of the current data:** firm causality; separating relocation, substitution, demand and logistics; effects on production, employment or welfare; conclusions about the United States as a country ("America" is a region).

**Possible extension, not a requirement:** trade data (UN Comtrade, U.S. Census) to assess whether regional signals relate to shifts in assembly or imports (HS 8542 for chips; HS 8471/8517 for computers and phones; HS 8486 for equipment).

## 8. What the work so far contributes

- **Phases 1–2:** a validated database and an understanding of what each series measures.
- **Phase 3A:** quarterly growth models **did not corroborate** a permanent change in mean growth around 2018. This does not invalidate all previously estimated trend breaks, because different hypotheses were being tested. It also showed that in the China / Asia Pacific Other ratio, after controlling for seasonality, quarterly changes show no significant autocorrelation, which makes the model in section 5 viable.
- **Phase 3B:** the documented chronology from which event dates are taken.

## 9. Next steps

1. Pending verifications:
   - stability of the WSTS regional definition: **favourable evidence** (the 2018 and 2021 classifications coincide); year-by-year formal verification remains open;
   - add to the Phase 3B registry the 2018 and 2021 WSTS classifications and the USITC documentation on the suspension of List 4B.
2. Draft and freeze the **Phase 3C econometric protocol** (`docs/phase3c_protocol.md`): exact estimands, power assessment, inference method and comparisons fixed before running the new models.
3. Only then, write and run Script 20.

---

## References

- Alfaro, L. and Chor, D. (2023). *Global Supply Chains: The Looming "Great Reallocation"*. NBER Working Paper 31661.
- Bown, C. P. (2020). How the United States marched the semiconductor industry into its trade war with China. *East Asian Economic Review*, 24(4), 349–388. doi:10.11644/KIEP.EAER.2020.24.4.384.
- Fajgelbaum, P., Goldberg, P., Kennedy, P. and Khandelwal, A. (2020). The Return to Protectionism. *Quarterly Journal of Economics*, 135(1), 1–55.
- Fajgelbaum, P., Goldberg, P., Kennedy, P., Khandelwal, A. and Taglioni, D. (2024). The US-China Trade War and Global Reallocations. *American Economic Review: Insights*, 6(2), 295–312.
- Freund, C., Mattoo, A., Mulabdic, A. and Ruta, M. (2024). Is US trade policy reshaping global supply chains? *Journal of International Economics*, 152, 104011.
- USTR (2018). Section 301 List 1 notice (83 FR 28710). In the Phase 3B registry.
- USITC. Documentation on the postponement to 15 December 2019 and suspension of List 4B (exact reference to be added to the Phase 3B registry).
- WSTS (2018). WSTS product and regional classification, available in the SIA archive (exact reference to be added to the Phase 3B registry).
- WSTS (2021). *WSTS Product Classification 2021*, p. 25, section 4 (regional definitions).
