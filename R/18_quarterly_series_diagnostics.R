# Semiconductors II
# 18_quarterly_series_diagnostics.R
#
# Phase 3A methodological reassessment, step 2 of 3 (NOT Phase 3C).
#
# Purpose:
#   A. Build an SIA regional 3MMA series at a FIXED +12-month revision horizon:
#      every month 2015-2024 is taken from the "Last Year" column of the SIA
#      release published 12 months later. This removes the mixed-vintage
#      problem documented in Script 17 (D2): the current processed series uses
#      a +12-month vintage for 2015 but first releases for 2016-2025.
#      2025 has no +12-month value until the 2026 releases are parsed; it is
#      kept as first release and flagged (optional manual file, see below).
#   B. Build non-overlapping quarterly series:
#      - WSTS: sum of the three monthly flows (2010Q1-2025Q4, 64 quarters).
#        Calendar quarters absorb the within-quarter pattern documented in S1.
#      - SIA: the trailing 3MMA at Mar/Jun/Sep/Dec is exactly the mean monthly
#        sales of that calendar quarter (verified for WSTS in Script 17, S4).
#      - Relative China series: log(China / Asia-Pacific Other) and China's
#        share of Asia Pacific. These remove a strictly common proportional
#        shock, but memory-price cycles and global demand may affect both
#        regions UNEQUALLY. Ratios are comparisons, not causal controls.
#   C. Repeat Phase 2 diagnostics at quarterly frequency: ADF, Phillips-Perron,
#      KPSS (level and trend), descriptive seasonality F-test supplemented
#      with HAC-robust Wald inference, ACF, Ljung-Box, and AR(p) BIC selection.
#   D. Vintage sensitivity: first-release vs +12-month vintage for the China
#      series over the common 2015-2024 window.
#
# This script imposes no policy dates and estimates no policy effects.
# It does not overwrite processed datasets or outputs of Scripts 01-17.
# Revised audit tables use a separate audit18_claudio_revised directory to
# avoid mixing with outputs from any other local Script 18 implementation.
#
# Run from repository root:
#   Rscript R/18_quarterly_series_diagnostics.R
#   source("R/18_quarterly_series_diagnostics.R", encoding = "UTF-8")
#
# Optional input (not required): data/manual/sia_2025_vintage12m.csv
#   One row per month 2025-01..2025-12 with columns
#   date, america, europe, japan, china, asia_pacific_other, total
#   holding the "Last Year" values transcribed or parsed from the SIA releases
#   for January-December 2026. If present and complete, 2025 joins the
#   single-vintage series; otherwise 2025 stays first release and is flagged.
#
# Interpretation limits:
#   - Quarterly samples are short (64 WSTS quarters; 40 single-vintage SIA
#     quarters). Unit-root and stationarity tests have low power here.
#   - tseries ADF/PP p-values use tables bounded to [0.01, 0.99], while
#     KPSS uses [0.01, 0.10]. Truncated p-values are flagged accurately.
#   - Classical F seasonality tests assume uncorrelated errors. HAC-robust
#     Wald p-values are also reported, but are approximate in short samples.
#   - AR(p) selected by BIC is only a diagnostic of residual dependence;
#     the selected order does not establish a structural economic mechanism.
#   - Temporal aggregation of flows induces autocorrelation even without
#     economic dynamics: if monthly log levels were a random walk, quarterly
#     growth would have lag-1 autocorrelation 8/38 = 0.21 (Working, 1960).

required_packages <- c("tseries", "sandwich")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  stop("Install missing package(s): ", paste(missing_packages, collapse = ", "))
}

if (!file.exists("data/processed/wsts_monthly_2010_2025.csv")) {
  stop("Run from the repository root. Missing WSTS processed CSV.")
}

out_dir <- file.path("output", "tables", "phase3", "audit18_claudio_revised")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
processed_dir <- file.path("data", "processed")

write_table <- function(x, name, dir = out_dir) {
  path <- file.path(dir, paste0(name, ".csv"))
  utils::write.csv(x, path, row.names = FALSE, na = "")
  message("[SAVE] ", path)
  invisible(path)
}

read_data <- function(path) {
  if (!file.exists(path)) stop("Missing input: ", path)
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}

require_cols <- function(x, columns, name) {
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0L) {
    stop(name, ": missing columns: ", paste(missing, collapse = ", "))
  }
}

check_dates <- function(dates, start_date, end_date, label) {
  expected <- seq(as.Date(start_date), as.Date(end_date), by = "month")
  if (!identical(dates, expected)) {
    stop(label, ": dates do not match the complete expected monthly sequence.")
  }
}

r6 <- function(x) round(x, 6L)
month_num <- function(dates) as.integer(format(as.Date(dates), "%m"))
year_num <- function(dates) as.integer(format(as.Date(dates), "%Y"))
quarter_num <- function(dates) (month_num(dates) - 1L) %/% 3L + 1L
quarter_label <- function(dates) paste0(year_num(dates), "Q", quarter_num(dates))

sia_regions <- c("america", "europe", "japan", "china",
                 "asia_pacific_other", "total")

# ---------------------------------------------------------------------------
# A. Fixed revision horizon (+12 months) SIA regional 3MMA series
# ---------------------------------------------------------------------------
message("[A] Building SIA series at a +12-month revision horizon")

sia <- read_data(file.path(processed_dir, "sia_regional_3mma_2015_2025.csv"))
require_cols(sia, c("date", paste0(sia_regions, "_3mma_usd_billions"),
                    "source_method"), "SIA regional 3MMA")
sia$date <- as.Date(sia$date)
sia <- sia[order(sia$date), , drop = FALSE]
check_dates(sia$date, "2015-01-01", "2025-12-01", "SIA regional 3MMA")

parse_audit <- read_data("output/tables/phase1/sia_regional_pdf_parse_audit.csv")
require_cols(parse_audit, c("reference_month", "parse_status",
                            paste0(sia_regions, "_current"),
                            paste0(sia_regions, "_last_year")),
             "SIA PDF parse audit")
parse_audit$reference_month <- as.Date(parse_audit$reference_month)
parse_audit <- parse_audit[parse_audit$parse_status == "ok", , drop = FALSE]
parse_audit <- parse_audit[order(parse_audit$reference_month), , drop = FALSE]
check_dates(parse_audit$reference_month, "2016-01-01", "2025-12-01",
            "SIA PDF parse audit")

# The "Last Year" column of the release for month t refers to month t - 12.
vintage12 <- data.frame(
  date = as.Date(sprintf("%04d-%02d-01",
                         year_num(parse_audit$reference_month) - 1L,
                         month_num(parse_audit$reference_month))),
  vintage = "plus_12_months",
  source_reference_month = as.character(parse_audit$reference_month),
  stringsAsFactors = FALSE
)
for (r in sia_regions) {
  vintage12[[paste0(r, "_3mma_usd_billions")]] <-
    parse_audit[[paste0(r, "_last_year")]]
}
check_dates(vintage12$date, "2015-01-01", "2024-12-01", "+12-month vintage")

# Consistency: 2015 in the processed series was already built from the same
# "Last Year" columns of the 2016 releases, so it must match exactly.
sia_2015 <- sia[year_num(sia$date) == 2015L, , drop = FALSE]
v12_2015 <- vintage12[year_num(vintage12$date) == 2015L, , drop = FALSE]
max_gap_2015 <- max(abs(r6(
  as.matrix(sia_2015[, paste0(sia_regions, "_3mma_usd_billions")]) -
  as.matrix(v12_2015[, paste0(sia_regions, "_3mma_usd_billions")]))))
if (max_gap_2015 > 0) {
  stop("2015 +12-month values do not reproduce the processed 2015 series (max gap ",
       max_gap_2015, ").")
}

# 2025: optional manual +12-month values from the 2026 releases.
manual_2025_path <- file.path("data", "manual", "sia_2025_vintage12m.csv")
sia_2025 <- sia[year_num(sia$date) == 2025L, , drop = FALSE]
block_2025 <- data.frame(
  date = sia_2025$date,
  vintage = "first_release",
  source_reference_month = as.character(sia_2025$date),
  stringsAsFactors = FALSE
)
for (r in sia_regions) {
  block_2025[[paste0(r, "_3mma_usd_billions")]] <-
    sia_2025[[paste0(r, "_3mma_usd_billions")]]
}
if (file.exists(manual_2025_path)) {
  manual <- read_data(manual_2025_path)
  require_cols(manual, c("date", sia_regions), "Manual 2025 +12-month file")
  manual$date <- as.Date(manual$date)
  manual <- manual[order(manual$date), , drop = FALSE]
  if (identical(manual$date, block_2025$date) &&
      all(is.finite(as.matrix(manual[, sia_regions])))) {
    for (r in sia_regions) {
      block_2025[[paste0(r, "_3mma_usd_billions")]] <- manual[[r]]
    }
    block_2025$vintage <- "plus_12_months"
    block_2025$source_reference_month <- "manual_2026_releases"
    message("[A] 2025 +12-month values loaded from ", manual_2025_path)
  } else {
    warning("Manual 2025 file incomplete; 2025 kept as first release.")
  }
} else {
  message("[A] No manual 2025 +12-month file; 2025 kept as first release (flagged).")
}

sia_v12 <- rbind(vintage12, block_2025)
sia_v12 <- sia_v12[order(sia_v12$date), , drop = FALSE]
row.names(sia_v12) <- NULL
check_dates(sia_v12$date, "2015-01-01", "2025-12-01", "+12-month SIA series")
sia_v12$asia_pacific_total_3mma_usd_billions <-
  sia_v12$china_3mma_usd_billions + sia_v12$asia_pacific_other_3mma_usd_billions
sia_v12$china_share_asia_pacific <-
  sia_v12$china_3mma_usd_billions / sia_v12$asia_pacific_total_3mma_usd_billions

if (any(as.matrix(sia_v12[, paste0(sia_regions, "_3mma_usd_billions")]) <= 0)) {
  stop("Non-positive values in +12-month SIA series.")
}

write_table(sia_v12, "sia_regional_3mma_vintage12m_2015_2025", dir = processed_dir)

# Validation of the single-vintage series against the WSTS historical series.
wsts_m <- read_data(file.path(processed_dir, "wsts_monthly_2010_2025.csv"))
wsts_3m <- read_data(file.path(processed_dir, "wsts_3mma_2010_2025.csv"))
require_cols(wsts_m, c("date", "region", "sales_usd_billions"), "WSTS monthly")
require_cols(wsts_3m, c("date", "region", "sales_usd_billions"), "WSTS 3MMA")
wsts_m$date <- as.Date(wsts_m$date)
wsts_3m$date <- as.Date(wsts_3m$date)

wsts_regions <- c("America", "Europe", "Japan", "Asia Pacific", "Worldwide")
extract_region <- function(data, region) {
  d <- data[data$region == region, , drop = FALSE]
  d[order(d$date), , drop = FALSE]
}
for (r in wsts_regions) {
  check_dates(extract_region(wsts_m, r)$date, "2010-01-01", "2025-12-01",
              paste("WSTS monthly", r))
  check_dates(extract_region(wsts_3m, r)$date, "2010-01-01", "2025-12-01",
              paste("WSTS 3MMA", r))
}

wsts_ap_3m <- extract_region(wsts_3m, "Asia Pacific")
wsts_ap_3m <- wsts_ap_3m[wsts_ap_3m$date >= as.Date("2015-01-01"), , drop = FALSE]
v12_check <- data.frame(
  date = sia_v12$date,
  vintage = sia_v12$vintage,
  sia_v12_ap = sia_v12$asia_pacific_total_3mma_usd_billions,
  sia_mixed_ap = sia$china_3mma_usd_billions + sia$asia_pacific_other_3mma_usd_billions,
  wsts_ap = wsts_ap_3m$sales_usd_billions
)
v12_check$gap_v12_vs_wsts <- r6(v12_check$sia_v12_ap - v12_check$wsts_ap)
v12_check$gap_mixed_vs_wsts <- r6(v12_check$sia_mixed_ap - v12_check$wsts_ap)

v12_check_summary <- do.call(rbind, lapply(
  split(v12_check, v12_check$vintage), function(d) {
    data.frame(
      vintage = d$vintage[1L],
      n_months = nrow(d),
      first_date = as.character(min(d$date)),
      last_date = as.character(max(d$date)),
      n_within_0_02_v12 = sum(abs(d$gap_v12_vs_wsts) <= 0.02),
      n_within_0_02_mixed = sum(abs(d$gap_mixed_vs_wsts) <= 0.02),
      mean_abs_gap_v12 = mean(abs(d$gap_v12_vs_wsts)),
      mean_abs_gap_mixed = mean(abs(d$gap_mixed_vs_wsts)),
      max_abs_gap_v12 = max(abs(d$gap_v12_vs_wsts)),
      mean_abs_rel_gap_v12_percent = 100 * mean(abs(d$gap_v12_vs_wsts) / d$wsts_ap)
    )
  }))
row.names(v12_check_summary) <- NULL
write_table(v12_check, "a_vintage12m_vs_wsts_ap_monthly")
write_table(v12_check_summary, "a_vintage12m_vs_wsts_ap_summary")

# ---------------------------------------------------------------------------
# B. Non-overlapping quarterly series
# ---------------------------------------------------------------------------
message("[B] Building non-overlapping quarterly series")

quarter_end <- function(dates) month_num(dates) %in% c(3L, 6L, 9L, 12L)

# WSTS: quarterly sum of monthly flows; cross-check against 3 x 3MMA at quarter end.
wsts_q_list <- lapply(wsts_regions, function(r) {
  m <- extract_region(wsts_m, r)
  a <- extract_region(wsts_3m, r)
  q <- quarter_label(m$date)
  sums <- tapply(m$sales_usd_billions, q, sum)
  counts <- tapply(m$sales_usd_billions, q, length)
  if (any(counts != 3L)) stop("Incomplete WSTS quarter for ", r)
  ends <- a[quarter_end(a$date), , drop = FALSE]
  d <- data.frame(
    quarter = names(sums),
    quarter_end_date = ends$date[match(names(sums), quarter_label(ends$date))],
    series = r,
    source = "WSTS",
    vintage = "historical",
    quarterly_sales_usd_billions = as.numeric(sums),
    three_x_3mma_quarter_end = 3 * ends$sales_usd_billions[
      match(names(sums), quarter_label(ends$date))],
    stringsAsFactors = FALSE
  )
  d[order(d$quarter_end_date), , drop = FALSE]
})
wsts_q <- do.call(rbind, wsts_q_list)
row.names(wsts_q) <- NULL
wsts_q_check_gap <- max(abs(wsts_q$quarterly_sales_usd_billions -
                              wsts_q$three_x_3mma_quarter_end))
if (wsts_q_check_gap > 1e-6) {
  stop("WSTS quarterly sums do not match 3 x 3MMA at quarter end (gap ",
       wsts_q_check_gap, ").")
}

# SIA: quarterly totals = 3 x 3MMA at quarter-end months.
sia_quarterly <- function(data, label) {
  d <- data[quarter_end(data$date), , drop = FALSE]
  base <- data.frame(
    quarter = quarter_label(d$date),
    quarter_end_date = d$date,
    vintage = if ("vintage" %in% names(d)) d$vintage else d$source_method,
    stringsAsFactors = FALSE
  )
  out <- lapply(c("china", "asia_pacific_other", "total"), function(r) {
    x <- base
    x$series <- c(china = "China", asia_pacific_other = "Asia Pacific Other",
                  total = "Total")[[r]]
    x$source <- label
    x$quarterly_sales_usd_billions <- 3 * d[[paste0(r, "_3mma_usd_billions")]]
    x
  })
  do.call(rbind, out)
}
sia_q_v12 <- sia_quarterly(sia_v12, "SIA_vintage12m")
sia_q_mixed <- sia_quarterly(sia, "SIA_processed_mixed_vintage")

quarterly_long <- rbind(
  wsts_q[, c("quarter", "quarter_end_date", "series", "source", "vintage",
             "quarterly_sales_usd_billions")],
  sia_q_v12[, c("quarter", "quarter_end_date", "series", "source", "vintage",
                "quarterly_sales_usd_billions")],
  sia_q_mixed[, c("quarter", "quarter_end_date", "series", "source", "vintage",
                  "quarterly_sales_usd_billions")]
)
row.names(quarterly_long) <- NULL
write_table(quarterly_long, "quarterly_series_2010_2025", dir = processed_dir)

# Relative China series from a quarterly SIA block.
relative_series <- function(q) {
  ch <- q[q$series == "China", , drop = FALSE]
  ot <- q[q$series == "Asia Pacific Other", , drop = FALSE]
  ch <- ch[order(ch$quarter_end_date), , drop = FALSE]
  ot <- ot[order(ot$quarter_end_date), , drop = FALSE]
  if (!identical(ch$quarter, ot$quarter)) stop("China/AP Other quarters misaligned")
  data.frame(
    quarter = ch$quarter,
    quarter_end_date = ch$quarter_end_date,
    vintage = ch$vintage,
    log_china_over_ap_other = log(ch$quarterly_sales_usd_billions /
                                    ot$quarterly_sales_usd_billions),
    china_share_asia_pacific = ch$quarterly_sales_usd_billions /
      (ch$quarterly_sales_usd_billions + ot$quarterly_sales_usd_billions),
    stringsAsFactors = FALSE
  )
}
rel_v12 <- relative_series(sia_q_v12)
rel_mixed <- relative_series(sia_q_mixed)
write_table(rel_v12, "b_quarterly_relative_china_vintage12m")

# ---------------------------------------------------------------------------
# C. Quarterly diagnostics
# ---------------------------------------------------------------------------
message("[C] Quarterly diagnostics")

working_acf_1 <- 8 / 38

# tseries reference-table limits differ: ADF/PP [0.01, 0.99],
# KPSS [0.01, 0.10]. We do not report a boundary as an exact p-value.
# Save unexpected warnings/errors instead of suppressing them silently.
run_tseries_test <- function(fun, x, test_name, ...) {
  upper_bound <- if (test_name %in% c("ADF", "PP")) 0.99 else
    if (test_name == "KPSS") 0.10 else stop("Unknown test: ", test_name)
  truncated <- NA_character_
  other_warnings <- character()
  failure <- NA_character_
  res <- tryCatch(
    withCallingHandlers(fun(x, ...), warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("p-value smaller than printed p-value", msg, fixed = TRUE)) {
        truncated <<- "p<=0.01"
      } else if (grepl("p-value greater than printed p-value", msg,
                       fixed = TRUE)) {
        truncated <<- paste0("p>=", sprintf("%.2f", upper_bound))
      } else {
        other_warnings <<- unique(c(other_warnings, msg))
      }
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      failure <<- conditionMessage(e)
      NULL
    })
  if (is.null(res)) {
    return(data.frame(statistic = NA_real_, p_value = NA_real_,
                      lag_parameter = NA_real_, p_value_bound = NA_character_,
                      diagnostic_message = paste("ERROR:", failure)))
  }
  param <- if (length(res$parameter)) as.numeric(res$parameter[1L]) else NA_real_
  data.frame(statistic = as.numeric(res$statistic), p_value = as.numeric(res$p.value),
             lag_parameter = param, p_value_bound = truncated,
             diagnostic_message = paste(other_warnings, collapse = " | "))
}

lag_acf <- function(x, lag) {
  x <- x[is.finite(x)]
  if (length(x) <= lag + 1L) return(NA_real_)
  as.numeric(stats::acf(x, lag.max = lag, plot = FALSE)$acf[lag + 1L])
}

quarter_of_year <- function(qlabel) as.integer(sub("^[0-9]{4}Q", "", qlabel))

# AR(p), p=0..4, selected by BIC on quarter-of-year demeaned growth.
# This is a provisional diagnostic, NOT a final time-series model.
# Failed or non-converged candidate fits are excluded from BIC comparison.
ar_by_bic <- function(x, max_p = 4L) {
  x <- x[is.finite(x)]
  fits <- lapply(0:max_p, function(p) {
    tryCatch(suppressWarnings(stats::arima(
      x, order = c(p, 0L, 0L), include.mean = TRUE, method = "ML")),
      error = function(e) NULL)
  })
  bic <- vapply(fits, function(f) {
    if (is.null(f) || (!is.null(f$code) && f$code != 0)) return(NA_real_)
    tryCatch(as.numeric(stats::AIC(f, k = log(length(x)))),
             error = function(e) NA_real_)
  }, numeric(1L))
  if (all(!is.finite(bic))) {
    return(data.frame(ar_order_bic = NA_integer_,
                      ar1_coefficient = NA_real_,
                      sum_ar_coefficients = NA_real_,
                      residual_lb8_p = NA_real_,
                      ar_bic_gap_to_second = NA_real_,
                      ar_fit_status = "no_converged_candidate"))
  }
  best <- which.min(replace(bic, !is.finite(bic), Inf))
  p <- best - 1L
  f <- fits[[best]]
  valid <- sort(bic[is.finite(bic)])
  gap <- if (length(valid) > 1L) valid[2L] - valid[1L] else NA_real_
  lb <- stats::Box.test(stats::residuals(f), lag = 8L,
                        type = "Ljung-Box", fitdf = p)
  data.frame(ar_order_bic = p,
             ar1_coefficient = if (p >= 1L) unname(stats::coef(f)["ar1"]) else NA_real_,
             sum_ar_coefficients = if (p >= 1L)
               sum(stats::coef(f)[paste0("ar", seq_len(p))]) else 0,
             residual_lb8_p = lb$p.value,
             ar_bic_gap_to_second = gap,
             ar_fit_status = "converged")
}

# Joint seasonality test with HAC (Newey-West) covariance. The null is that
# Q2/Q3/Q4 seasonal dummy coefficients all equal zero, relative to Q1.
# A chi-squared approximation is used; with ~39 or ~63 growth observations,
# neither this nor the classical F is definitive.
seasonality_hac_test <- function(fitted_model, lag = 4L) {
  result <- tryCatch({
    covariance <- sandwich::NeweyWest(fitted_model, lag = lag,
                                      prewhite = FALSE, adjust = TRUE)
    coefficients <- stats::coef(fitted_model)
    idx <- seq.int(2L, length(coefficients))
    b <- coefficients[idx]
    v <- covariance[idx, idx, drop = FALSE]
    chi_sq <- as.numeric(crossprod(b, solve(v, b)))
    data.frame(seasonality_hac_chisq = chi_sq,
               seasonality_hac_df = length(idx),
               seasonality_hac_p = stats::pchisq(chi_sq, df = length(idx),
                                                  lower.tail = FALSE),
               seasonality_hac_lag = lag,
               seasonality_hac_status = "ok")
  }, error = function(e) {
    data.frame(seasonality_hac_chisq = NA_real_,
               seasonality_hac_df = NA_integer_,
               seasonality_hac_p = NA_real_,
               seasonality_hac_lag = lag,
               seasonality_hac_status = paste("ERROR:", conditionMessage(e)))
  })
  result
}

diagnose <- function(level, growth, quarters, series, source, vintage,
                     level_kind) {
  qy <- quarter_of_year(quarters[-1L])
  demeaned <- growth - stats::ave(growth, qy, FUN = mean)
  seas_fit <- stats::lm(growth ~ factor(qy))
  seas_f <- stats::anova(stats::lm(growth ~ 1), seas_fit)
  seas_hac <- seasonality_hac_test(seas_fit, lag = 4L)

  unit_root <- do.call(rbind, list(
    cbind(transformation = level_kind, test = "ADF (H0: unit root)",
          run_tseries_test(tseries::adf.test, level, "ADF", alternative = "stationary")),
    cbind(transformation = level_kind, test = "PP (H0: unit root)",
          run_tseries_test(tseries::pp.test, level, "PP", alternative = "stationary")),
    cbind(transformation = level_kind, test = "KPSS trend (H0: trend-stationary)",
          run_tseries_test(tseries::kpss.test, level, "KPSS", null = "Trend")),
    cbind(transformation = "q/q change", test = "ADF (H0: unit root)",
          run_tseries_test(tseries::adf.test, growth, "ADF", alternative = "stationary")),
    cbind(transformation = "q/q change", test = "PP (H0: unit root)",
          run_tseries_test(tseries::pp.test, growth, "PP", alternative = "stationary")),
    cbind(transformation = "q/q change", test = "KPSS level (H0: level-stationary)",
          run_tseries_test(tseries::kpss.test, growth, "KPSS", null = "Level"))
  ))
  unit_root <- cbind(series = series, source = source, vintage = vintage,
                     n_level = length(level), unit_root)

  dyn <- cbind(
    data.frame(series = series, source = source, vintage = vintage,
               n_growth = length(growth),
               sd_growth = stats::sd(growth),
               sd_seasonally_demeaned_growth = stats::sd(demeaned),
               seasonality_F = seas_f$F[2L],
               seasonality_F_p = seas_f$`Pr(>F)`[2L],
               seasonality_hac_chisq = seas_hac$seasonality_hac_chisq,
               seasonality_hac_df = seas_hac$seasonality_hac_df,
               seasonality_hac_p = seas_hac$seasonality_hac_p,
               seasonality_hac_lag = seas_hac$seasonality_hac_lag,
               seasonality_hac_status = seas_hac$seasonality_hac_status,
               acf1_growth = lag_acf(growth, 1L),
               acf2_growth = lag_acf(growth, 2L),
               acf4_growth = lag_acf(growth, 4L),
               acf1_demeaned = lag_acf(demeaned, 1L),
               acf2_demeaned = lag_acf(demeaned, 2L),
               acf4_demeaned = lag_acf(demeaned, 4L),
               working_benchmark_acf1 = working_acf_1,
               lb4_growth_p = stats::Box.test(growth, 4L, "Ljung-Box")$p.value,
               lb8_growth_p = stats::Box.test(growth, 8L, "Ljung-Box")$p.value,
               lb4_demeaned_p = stats::Box.test(demeaned, 4L, "Ljung-Box")$p.value,
               lb8_demeaned_p = stats::Box.test(demeaned, 8L, "Ljung-Box")$p.value),
    ar_by_bic(demeaned)
  )
  list(unit_root = unit_root, dynamics = dyn)
}

# Window labels for SIA series.
core_window <- function(q) year_num(q$quarter_end_date) <= 2024L

specs <- list()
for (r in wsts_regions) {
  d <- wsts_q[wsts_q$series == r, , drop = FALSE]
  lv <- log(d$quarterly_sales_usd_billions)
  specs[[length(specs) + 1L]] <- list(level = lv, growth = diff(lv),
    quarters = d$quarter, series = r, source = "WSTS",
    vintage = "historical_2010Q1_2025Q4", level_kind = "log level")
}
add_sia_specs <- function(q, rel, source, vintage_label, keep) {
  out <- list()
  for (r in c("China", "Asia Pacific Other", "Total")) {
    d <- q[q$series == r, , drop = FALSE]
    d <- d[order(d$quarter_end_date), , drop = FALSE]
    d <- d[keep(d), , drop = FALSE]
    lv <- log(d$quarterly_sales_usd_billions)
    out[[length(out) + 1L]] <- list(level = lv, growth = diff(lv),
      quarters = d$quarter, series = r, source = source,
      vintage = vintage_label, level_kind = "log level")
  }
  rr <- rel[keep(rel), , drop = FALSE]
  out[[length(out) + 1L]] <- list(level = rr$log_china_over_ap_other,
    growth = diff(rr$log_china_over_ap_other), quarters = rr$quarter,
    series = "log(China / AP Other)", source = source,
    vintage = vintage_label, level_kind = "log ratio")
  out[[length(out) + 1L]] <- list(level = rr$china_share_asia_pacific,
    growth = diff(rr$china_share_asia_pacific), quarters = rr$quarter,
    series = "China share of Asia Pacific", source = source,
    vintage = vintage_label, level_kind = "share level")
  out
}
specs <- c(specs,
  add_sia_specs(sia_q_v12, rel_v12, "SIA", "vintage12m_2015Q1_2024Q4",
                core_window),
  add_sia_specs(sia_q_v12, rel_v12, "SIA",
                if (all(sia_v12$vintage[year_num(sia_v12$date) == 2025L] ==
                        "plus_12_months")) {
                  "vintage12m_2015Q1_2025Q4"
                } else {
                  "vintage12m_plus_2025_first_release_2015Q1_2025Q4"
                },
                function(q) rep(TRUE, nrow(q))),
  add_sia_specs(sia_q_mixed, rel_mixed, "SIA",
                "processed_mixed_vintage_2015Q1_2024Q4", core_window))

results <- lapply(specs, function(s) diagnose(s$level, s$growth, s$quarters,
  s$series, s$source, s$vintage, s$level_kind))
unit_root_table <- do.call(rbind, lapply(results, `[[`, "unit_root"))
dynamics_table <- do.call(rbind, lapply(results, `[[`, "dynamics"))
row.names(unit_root_table) <- NULL
row.names(dynamics_table) <- NULL
write_table(unit_root_table, "c_quarterly_unit_root_tests")
write_table(dynamics_table, "c_quarterly_dynamics_and_seasonality")

# ---------------------------------------------------------------------------
# D. Vintage sensitivity for the China series (common window 2015Q1-2024Q4)
# ---------------------------------------------------------------------------
message("[D] Vintage sensitivity (first release vs +12-month vintage)")
v12_core <- rel_v12[core_window(rel_v12), , drop = FALSE]
mx_core <- rel_mixed[core_window(rel_mixed), , drop = FALSE]
if (!identical(v12_core$quarter, mx_core$quarter)) stop("Vintage windows misaligned")

vintage_sensitivity <- data.frame(
  n_quarters = nrow(v12_core),
  max_abs_share_difference_pp = 100 * max(abs(v12_core$china_share_asia_pacific -
                                                 mx_core$china_share_asia_pacific)),
  mean_share_difference_pp = 100 * mean(v12_core$china_share_asia_pacific -
                                          mx_core$china_share_asia_pacific),
  max_abs_log_ratio_difference = max(abs(v12_core$log_china_over_ap_other -
                                           mx_core$log_china_over_ap_other)),
  corr_log_ratio_changes = stats::cor(diff(v12_core$log_china_over_ap_other),
                                      diff(mx_core$log_china_over_ap_other)),
  sd_log_ratio_changes_v12 = stats::sd(diff(v12_core$log_china_over_ap_other)),
  sd_log_ratio_change_difference = stats::sd(
    diff(v12_core$log_china_over_ap_other) - diff(mx_core$log_china_over_ap_other)),
  quarter_of_max_share_v12 = v12_core$quarter[which.max(v12_core$china_share_asia_pacific)],
  quarter_of_max_share_mixed = mx_core$quarter[which.max(mx_core$china_share_asia_pacific)]
)
write_table(vintage_sensitivity, "d_china_vintage_sensitivity")

# ---------------------------------------------------------------------------
# Notes
# ---------------------------------------------------------------------------
notes <- c(
  "Semiconductors II — Script 18 quarterly series diagnostics",
  paste0("Date: ", Sys.Date()),
  paste0("R version: ", getRversion(), "; tseries ",
         as.character(utils::packageVersion("tseries")), "; sandwich ",
         as.character(utils::packageVersion("sandwich"))),
  paste0("2025 source vintage in fixed-horizon series: ",
         paste(unique(sia_v12$vintage[year_num(sia_v12$date) == 2025L]), collapse = ", ")),
  paste0("WSTS quarterly sum vs 3 x 3MMA quarter-end max gap: ", signif(wsts_q_check_gap, 3)),
  paste0("2015 +12-month vintage reproduces processed 2015 values: max gap ", max_gap_2015),
  "",
  "A: +12-month vintage = 'Last Year' column of the SIA release for the same month one year later.",
  "A: 2015-2024 fixed +12-month REVISION HORIZON (not a single publication vintage).",
  "A: 2025 is first release unless data/manual/sia_2025_vintage12m.csv is supplied.",
  "B: WSTS quarterly = sum of monthly flows. SIA quarterly = 3 x trailing 3MMA at quarter-end months.",
  "B: Relative China series may remove strictly common proportional shocks; unequal regional exposure to memory pricing and demand can remain.",
  "B: Ratios and market shares are relative outcomes, NOT automatically valid causal controls.",
  "C: ADF/PP interpolated p-value range [0.01, 0.99]; KPSS range [0.01, 0.10]. See p_value_bound.",
  "C: ADF lag order follows the tseries default trunc((n - 1)^(1/3)); n is small (40-64 quarters), so power is low.",
  "C: Classical seasonality F-test assumes uncorrelated errors and is descriptive.",
  "C: Added HAC-robust joint seasonality Wald test (Newey-West, fixed 4 quarter lags, prewhite=FALSE).",
  "C: HAC Wald p-values use asymptotic chi-square; short quarterly samples require caution.",
  "C: Working (1960) benchmark: quarterly aggregation of a monthly random walk alone gives growth ACF(1) = 8/38 = 0.21.",
  "C: AR order chosen by BIC (p = 0..4) on quarter-of-year demeaned growth; not a final dynamic model.",
  "C: Models that fail to converge are excluded; BIC gap to runner-up and residual Ljung-Box(8) (fitdf=p) are reported.",
  "C: BIC-selected AR order and Ljung-Box are diagnostics, not proof of stationarity or valid causal inference.",
  "D: compares first-release (processed) and +12-month vintages over 2015Q1-2024Q4.",
  "No policy dates were imposed, and no causal effect is identified.",
  "",
  "sessionInfo():",
  utils::capture.output(utils::sessionInfo())
)
writeLines(notes, file.path(out_dir, "audit18_notes.txt"), useBytes = TRUE)

message("[DONE] Wrote Script 18 outputs to ", out_dir)
print(v12_check_summary)
print(dynamics_table[, c("series", "vintage", "n_growth", "seasonality_F_p",
                         "seasonality_hac_p", "acf1_demeaned", "acf4_demeaned",
                         "lb8_demeaned_p", "ar_order_bic",
                         "ar_bic_gap_to_second", "residual_lb8_p")])
print(vintage_sensitivity)
