# Potential external trade-data sources (backlog)

**Status:** Documented for future feasibility review; **not yet ingested, processed, or used in any estimate**.
**Recorded:** 17 November 2026.

The current empirical analysis continues to use WSTS Historical Billings and SIA 3MMA data only. This note records two possible complementary data sources without starting a new data pipeline or changing the Phase 3A/3C scope.

## 1. UN Comtrade — United Nations Statistics Division

- **Official portal:** https://comtradeplus.un.org/en/
- **API documentation:** https://comtradeplus.un.org/en/api-docs
- **Provider:** United Nations Statistics Division (UNSD); national reporting authorities supply the underlying merchandise trade records.
- **Potential variables:** reporter and partner economy, trade flow (import/export), HS classification and commodity code, reference period, trade value (USD), quantity when available, and relevant metadata.
- **Candidate products:** HS 8542 (electronic integrated circuits); HS 8541 and HS 8486 require separate product-scope checks.
- **Proposed future use:** bilateral semiconductor trade dynamics; trade-network edges; market shares; trade diversion hypotheses.
- **Verification required:** coverage for 2010–2025, HS revisions/harmonization, reporter-partner discrepancies, re-exports, CIF/FOB valuation, quantity units, missingness and API limits.

## 2. U.S. Census Bureau — International Trade Time Series

- **Official overview:** https://www.census.gov/data/developers/data-sets/international-trade.html
- **Exports HS API:** https://api.census.gov/data/timeseries/intltrade/exports/hs.html
- **Imports HS API:** https://api.census.gov/data/timeseries/intltrade/imports/hs.html
- **Provider:** United States Census Bureau, international merchandise trade statistics.
- **Potential variables:** monthly U.S. imports and exports by partner economy and HS product, value and additional available measures.
- **Proposed future use:** U.S.-reported bilateral cross-checks and monthly trade-policy timing.
- **Verification required:** import/export measurement conventions, product concordances, applicable monthly start dates, missingness and reporting changes.

## Measurement and interpretation rules

1. **Do not treat customs flows and WSTS sales as equivalent totals.** WSTS regional semiconductor billings/sales and international border crossings use different geographic, product, valuation and counting concepts.
2. Correlation in growth rates, cyclical turning points or regional shares is a legitimate **testable hypothesis**, not a maintained assumption or an identity.
3. Summing reported exports/imports across bilateral records can double-count movements and re-exports.
4. HS 8542 is a practical baseline, but is not the entire WSTS semiconductor product universe. HS 8541 and 8486 cover distinct scopes.
5. No graph/network evidence, bilateral policy effect, or data agreement is claimed until data acquisition, auditing and model specification.

## Gating decision

No API calls, downloads, extraction scripts, regressions or network analyses are part of the present methodological review. Reassess whether these sources should enter the project **after scripts 17–19** and before locking the Phase 3C design. SEMI and additional providers are not opened by this note.
