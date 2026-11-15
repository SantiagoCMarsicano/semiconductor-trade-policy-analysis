# Research Design — Semiconductors II

## Core question

¿Las políticas comerciales y tecnológicas implementadas por Estados Unidos y China en el marco de la denominada guerra comercial modificaron significativamente la dinámica del mercado global de semiconductores?

## General hypothesis

**H0:** las políticas comerciales y tecnológicas de Estados Unidos y China no produjeron cambios estadísticamente significativos en la dinámica de las ventas de semiconductores.

**H1:** las políticas comerciales y tecnológicas de Estados Unidos y China produjeron cambios estadísticamente significativos en la dinámica de las ventas de semiconductores.

## Historical policy chronology

The initial presidency-based working periods are no longer used as empirical intervention windows.

Policy dates and historical stages are documented in:

- docs/phase3b_timeline_guide.md
- data/manual/phase3b_source_registry.csv

The chronology was constructed after the initial endogenous structural-break analysis. Policy dates must therefore be selected from documented institutional events rather than chosen to match estimated breakpoints.

Announcement, enactment and effective dates should be distinguished whenever relevant.

Temporal proximity between a policy event and a statistical breakpoint is not, by itself, evidence of causality.

## Initial empirical workflow

1. Build and validate the master dataset.
2. Reconstruct regional sales dynamics with China separated where available.
3. Study levels, growth, shares, volatility, and time-series properties.
4. Run pre-specified hypothesis tests.
5. Test historically motivated structural breaks.
6. Detect endogenous structural breaks.
7. Estimate intervention models.
8. Add bilateral trade data only if the initial results require analysis of trade diversion or mechanisms.

## Interpretation rule

Rejecting H0 around a policy date is not, by itself, sufficient evidence of causality. Results must be evaluated against competing shocks such as COVID-19, semiconductor cycles, crypto, supply shortages, AI demand, and broader macroeconomic conditions.
