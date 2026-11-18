# Semiconductors II
# 17_reproduce_audit_checks.R
#
# Phase 3A methodological audit replication, v3 (NOT Phase 3C).
# S1: Quarterly calendar pattern in raw WSTS monthly sales.
# S2: Bai-Perron/BIC sensitivity under serial correlation and calendar effects.
# S4: Three-month moving-average (3MMA) overlap and dynamics.
# D1: SIA vs WSTS Asia-Pacific reconciliation (between-document comparison).
# D2: Paired 12-month-later revisions within SIA (actual vintage revisions).
#
# This script never imposes policy intervention dates and never estimates
# causal policy effects. It does not overwrite original Phase 3A outputs.
#
# Run from repository root:
#   Rscript R/17_reproduce_audit_checks.R
#
# Monte Carlo settings (set environment variable before running):
#   Sys.setenv(SEMICON_AUDIT_REPS = "0")    # deterministic checks only, no Monte Carlo
#   Sys.setenv(SEMICON_AUDIT_REPS = "5")    # smoke test (default)
#   Sys.setenv(SEMICON_AUDIT_REPS = "200")  # full run
#   source("R/17_reproduce_audit_checks.R", encoding = "UTF-8")
#
# Outputs go to output/tables/phase3/audit17/reps_<N>/ so that smoke and full
# runs do not overwrite each other.
#
# Changes from v2 (Claude audit, 18 Nov 2026):
#   1. Revisions and gaps are rounded to 1e-6 before sign/threshold
#      comparisons (v2 misclassified 2 floating-point residuals as revisions).
#   2. D2 paired revisions now cover China and China's share of Asia Pacific,
#      not only the Asia-Pacific total.
#   3. SIA-vs-WSTS dynamics outputs renamed from d2_* to d1_*; the duplicated
#      d2_vintage_gap_by_year file is removed.
#   4. The tautological AP "internal aggregation" check is relabelled as a
#      construction check; a genuine internal check (published SIA Total vs
#      sum of the five published regions) is added.
#   5. S4 adds an empirical overlap benchmark (3-term MA of calendar-demeaned
#      monthly growth) and the Working (1960) temporal-aggregation benchmark.
#   6. sessionInfo(), RNGkind() and per-block seeds are archived; ties are
#      reported for the "closer after 12 months" comparison.
#
# The original independent audit used 200 replications with a different
# implementation and simulation design. Identical results are NOT expected.

required_packages <- c("strucchange")
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  stop("Install missing package(s): ", paste(missing_packages, collapse = ", "))
}

if (!file.exists("data/processed/wsts_monthly_2010_2025.csv")) {
  stop("Run from the repository root. Missing WSTS processed CSV.")
}

base_seed <- 17112026L
set.seed(base_seed)

reps <- suppressWarnings(as.integer(Sys.getenv("SEMICON_AUDIT_REPS", "5")))
if (is.na(reps) || reps < 0L) {
  stop("SEMICON_AUDIT_REPS must be a non-negative integer (0 = skip Monte Carlo).")
}

out_dir <- file.path("output", "tables", "phase3", "audit17", paste0("reps_", reps))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Values in the SIA sources have two decimals; sums of such values carry
# floating-point residuals (~1e-15). Round before any sign or threshold test.
r6 <- function(x) round(x, 6L)

write_table <- function(x, name) {
  path <- file.path(out_dir, paste0(name, ".csv"))
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

lag_acf <- function(x, lag) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) <= lag + 1L) return(NA_real_)
  as.numeric(stats::acf(x, lag.max = lag, plot = FALSE, na.action = na.fail)$acf[lag + 1L])
}

sample_sd <- function(x) stats::sd(as.numeric(x), na.rm = TRUE)

month_num <- function(dates) as.integer(format(as.Date(dates), "%m"))
year_num <- function(dates) as.integer(format(as.Date(dates), "%Y"))
quarter_num <- function(dates) (month_num(dates) - 1L) %/% 3L + 1L
position_in_quarter <- function(dates) (month_num(dates) - 1L) %% 3L + 1L

mean_by_position <- function(values, dates) {
  positions <- position_in_quarter(dates)
  means <- tapply(values, positions, mean, na.rm = TRUE)
  as.numeric(means[as.character(positions)])
}

# Includes zero-break model. strucchange::AIC(..., k=log(n)) provides
# the BIC-style penalty with a common convention across all simulated models.
# The original audit's custom dynamic-programming implementation used its
# own penalty; this is a genuine R/strucchange replication, not identical code.
choose_bic_breaks <- function(response, trend = FALSE, h = 24L, max_breaks = 6L) {
  response <- as.numeric(response)
  if (any(!is.finite(response))) stop("Non-finite response in breakpoint model")
  n <- length(response)
  cap <- min(as.integer(max_breaks), floor(n / h) - 1L)
  if (cap < 1L) stop("Not enough observations for stated segment size")
  df <- data.frame(y = response, trend_index = seq_len(n))
  formula <- if (trend) y ~ trend_index else y ~ 1
  bp <- strucchange::breakpoints(formula, data = df, h = h, breaks = cap)
  nbreaks <- 0:cap
  bic <- as.numeric(stats::AIC(bp, breaks = nbreaks, k = log(n)))
  if (length(bic) != length(nbreaks) || any(!is.finite(bic))) {
    stop("Unexpected/non-finite BIC values returned by strucchange")
  }
  selected <- nbreaks[which.min(bic)]
  list(nbreaks = as.integer(selected), bic = bic, all_nbreaks = nbreaks)
}

message("[LOAD] Reading repository datasets")
wsts_m <- read_data("data/processed/wsts_monthly_2010_2025.csv")
wsts_3m <- read_data("data/processed/wsts_3mma_2010_2025.csv")
sia <- read_data("data/processed/sia_regional_3mma_2015_2025.csv")

require_cols(wsts_m, c("date", "region", "sales_usd_billions"), "WSTS monthly")
require_cols(wsts_3m, c("date", "region", "sales_usd_billions"), "WSTS 3MMA")
require_cols(sia, c("date", "china_3mma_usd_billions",
                    "asia_pacific_other_3mma_usd_billions",
                    "asia_pacific_total_3mma_usd_billions",
                    "source_method", "source_reference_month", "publication_date"), "SIA 3MMA")

wsts_m$date <- as.Date(wsts_m$date)
wsts_3m$date <- as.Date(wsts_3m$date)
sia$date <- as.Date(sia$date)
sia <- sia[order(sia$date), , drop = FALSE]

regions <- c("America", "Europe", "Japan", "Asia Pacific", "Worldwide")
for (r in regions) {
  idx_m <- which(wsts_m$region == r)
  idx_3m <- which(wsts_3m$region == r)
  idx_m <- idx_m[order(wsts_m$date[idx_m])]
  idx_3m <- idx_3m[order(wsts_3m$date[idx_3m])]
  check_dates(wsts_m$date[idx_m], "2010-01-01", "2025-12-01", paste("Monthly", r))
  check_dates(wsts_3m$date[idx_3m], "2010-01-01", "2025-12-01", paste("3MMA", r))
}
check_dates(sia$date, "2015-01-01", "2025-12-01", "SIA")

if (any(wsts_m$sales_usd_billions <= 0) ||
    any(wsts_3m$sales_usd_billions <= 0) ||
    any(sia$china_3mma_usd_billions <= 0) ||
    any(sia$asia_pacific_other_3mma_usd_billions <= 0)) {
  stop("Sales must be positive to form logarithms.")
}

extract_region <- function(data, region) {
  d <- data[data$region == region, , drop = FALSE]
  d[order(d$date), , drop = FALSE]
}

world_m <- extract_region(wsts_m, "Worldwide")
world_3m <- extract_region(wsts_3m, "Worldwide")
world_log <- log(world_m$sales_usd_billions)
world_growth <- diff(world_log)
world_growth_dates <- world_m$date[-1L]

# ---------------------------------------------------------------------------
# S1: Quarterly calendar pattern
# ---------------------------------------------------------------------------
message("[S1] Quarterly calendar checks")
q_groups <- split(seq_len(nrow(world_m)),
                  paste(year_num(world_m$date), quarter_num(world_m$date), sep = "Q"))
quarter_rows <- lapply(q_groups, function(ii) {
  if (length(ii) != 3L) stop("Incomplete calendar quarter in WSTS monthly")
  d <- world_m[ii, , drop = FALSE]
  v <- d$sales_usd_billions
  data.frame(
    year = year_num(d$date[1L]),
    quarter = quarter_num(d$date[1L]),
    end_month = as.character(d$date[3L]),
    end_month_largest = as.integer(which.max(v) == 3L),
    end_month_share = v[3L] / sum(v),
    # Explicit geometric-log definition to avoid ambiguity about "adjacent mean".
    log_end_excess_geometric = log(v[3L]) - mean(log(v[1:2])),
    # Separate arithmetic-level comparator; does NOT equal the above in general.
    log_end_excess_arithmetic = log(v[3L]) - log(mean(v[1:2])),
    stringsAsFactors = FALSE
  )
})
quarters <- do.call(rbind, quarter_rows)
quarters <- quarters[order(quarters$year, quarters$quarter), , drop = FALSE]
row.names(quarters) <- NULL

# Auditor's original S1 comparator: the quarter-end month against the
# immediately preceding and following months. 2010Q1..2025Q3: 63 cases.
# This differs from comparison to the other TWO months inside the quarter.
quarter_end_indices <- seq.int(3L, length(world_log) - 1L, by = 3L)
adjacent_excess <- world_log[quarter_end_indices] -
  0.5 * (world_log[quarter_end_indices - 1L] +
         world_log[quarter_end_indices + 1L])
adjacent_table <- data.frame(
  quarter_end_date = world_m$date[quarter_end_indices],
  log_end_excess_adjacent_months = adjacent_excess
)

world_growth_adjusted <- world_growth -
  mean_by_position(world_growth, world_growth_dates)

s1 <- data.frame(
  n_quarters = nrow(quarters),
  end_month_largest_count = sum(quarters$end_month_largest),
  end_month_largest_share = mean(quarters$end_month_largest),
  mean_end_month_share = mean(quarters$end_month_share),
  mean_log_end_excess_geometric = mean(quarters$log_end_excess_geometric),
  mean_log_end_excess_arithmetic = mean(quarters$log_end_excess_arithmetic),
  n_adjacent_comparisons = length(adjacent_excess),
  mean_log_end_excess_adjacent = mean(adjacent_excess),
  sd_log_end_excess_adjacent = sample_sd(adjacent_excess),
  se_log_end_excess_adjacent = sample_sd(adjacent_excess) /
    sqrt(length(adjacent_excess)),
  benchmark_log_5_over_4 = log(5 / 4),
  sd_raw_monthly_log_growth = sample_sd(world_growth),
  sd_position_demeaned_log_growth = sample_sd(world_growth_adjusted),
  acf_raw_lag_1 = lag_acf(world_growth, 1),
  acf_raw_lag_2 = lag_acf(world_growth, 2),
  acf_raw_lag_3 = lag_acf(world_growth, 3),
  acf_raw_lag_6 = lag_acf(world_growth, 6),
  acf_raw_lag_12 = lag_acf(world_growth, 12),
  acf_demeaned_lag_3 = lag_acf(world_growth_adjusted, 3),
  acf_demeaned_lag_6 = lag_acf(world_growth_adjusted, 6)
)

q_year_groups <- cut(
  quarters$year, breaks = c(2009, 2013, 2017, 2021, 2025),
  labels = c("2010-2013", "2014-2017", "2018-2021", "2022-2025")
)
s1_subperiod <- do.call(rbind, lapply(split(quarters, q_year_groups), function(d) {
  data.frame(period = as.character(q_year_groups[match(d$year[1], quarters$year)]),
             n_quarters = nrow(d),
             mean_log_end_excess_geometric = mean(d$log_end_excess_geometric))
}))
row.names(s1_subperiod) <- NULL
write_table(quarters, "s1_quarterly_calendar_observations")
write_table(adjacent_table, "s1_quarter_end_adjacent_month_comparison")
write_table(s1, "s1_calendar_summary")
write_table(s1_subperiod, "s1_calendar_subperiods")

# ---------------------------------------------------------------------------
# S4: Overlapping 3MMA and residual dependence
# ---------------------------------------------------------------------------
message("[S4] 3MMA overlap diagnostics")
# Reconstruct trailing 3-month arithmetic MA from raw WSTS monthly values.
# The workbook also contains an independently recorded 3MMA worksheet.
ma3_from_raw <- vapply(seq_along(world_m$sales_usd_billions), function(i) {
  if (i < 3L) return(NA_real_)
  mean(world_m$sales_usd_billions[(i - 2L):i])
}, numeric(1L))

wsts_3mma_gap <- data.frame(
  date = world_m$date,
  wsts_workbook_3mma = world_3m$sales_usd_billions,
  reconstructed_trailing_3mma = ma3_from_raw,
  gap = world_3m$sales_usd_billions - ma3_from_raw
)

world_3mma_growth <- diff(log(world_3m$sales_usd_billions))

# Stylized benchmark: if underlying monthly log growth innovations are iid,
# a THREE-term equal-weight average has ACF(1) = 2/3 and ACF(2) = 1/3.
# This is a model benchmark, NOT a reconstruction of the actual WSTS process.
iid_growth <- stats::rnorm(100000L)
iid_ma3 <- stats::filter(iid_growth, rep(1 / 3, 3), sides = 1)
iid_ma3 <- as.numeric(iid_ma3)[3:100000]

# Quarterly aggregation means the sum of the 3 NON-overlapping monthly flows,
# not the average of three rolling observations or an extra 3MMA filter.
quarterly_sales <- vapply(q_groups, function(ii) {
  sum(world_m$sales_usd_billions[ii])
}, numeric(1L))
q_order <- order(quarters$year, quarters$quarter)
quarterly_growth <- diff(log(quarterly_sales[q_order]))

# Empirical overlap benchmark: apply the same trailing 3-term equal-weight
# filter to calendar-demeaned monthly growth. A trailing 3-month MA removes any
# fixed within-quarter (period-3) pattern exactly, so the iid benchmark above
# is not the right comparator for raw monthly growth.
ma3_demeaned_growth <- as.numeric(stats::filter(world_growth_adjusted,
                                                rep(1 / 3, 3), sides = 1))

# Working (1960): if monthly log levels follow a random walk, growth of the
# 3-month temporal aggregate has lag-1 autocorrelation (m^2 - 1)/(2(2m^2 + 1)).
working_m <- 3
working_acf_1 <- (working_m^2 - 1) / (2 * (2 * working_m^2 + 1))

s4 <- data.frame(
  n_monthly_growth = length(world_growth),
  n_3mma_growth = length(world_3mma_growth),
  n_quarterly_growth = length(quarterly_growth),
  ma3_reconstruction_max_abs_gap = max(abs(wsts_3mma_gap$gap), na.rm = TRUE),
  sd_monthly_growth = sample_sd(world_growth),
  sd_3mma_growth = sample_sd(world_3mma_growth),
  sd_ratio_3mma_over_monthly = sample_sd(world_3mma_growth) / sample_sd(world_growth),
  acf_monthly_lag_1 = lag_acf(world_growth, 1),
  acf_monthly_lag_2 = lag_acf(world_growth, 2),
  acf_monthly_lag_3 = lag_acf(world_growth, 3),
  acf_monthly_lag_12 = lag_acf(world_growth, 12),
  acf_3mma_lag_1 = lag_acf(world_3mma_growth, 1),
  acf_3mma_lag_2 = lag_acf(world_3mma_growth, 2),
  acf_3mma_lag_3 = lag_acf(world_3mma_growth, 3),
  acf_3mma_lag_12 = lag_acf(world_3mma_growth, 12),
  iid_MA2_theoretical_acf_1 = 2 / 3,
  iid_MA2_theoretical_acf_2 = 1 / 3,
  iid_MA2_simulated_acf_1 = lag_acf(iid_ma3, 1),
  iid_MA2_simulated_acf_2 = lag_acf(iid_ma3, 2),
  empirical_ma3_demeaned_acf_1 = lag_acf(ma3_demeaned_growth, 1),
  empirical_ma3_demeaned_acf_2 = lag_acf(ma3_demeaned_growth, 2),
  empirical_ma3_demeaned_acf_3 = lag_acf(ma3_demeaned_growth, 3),
  empirical_ma3_demeaned_acf_12 = lag_acf(ma3_demeaned_growth, 12),
  working_aggregation_benchmark_acf_1 = working_acf_1,
  acf_quarterly_lag_1 = lag_acf(quarterly_growth, 1),
  acf_quarterly_lag_2 = lag_acf(quarterly_growth, 2),
  acf_quarterly_lag_4 = lag_acf(quarterly_growth, 4),
  lb12_monthly_p = stats::Box.test(world_growth, lag = 12, type = "Ljung-Box")$p.value,
  lb12_3mma_p = stats::Box.test(world_3mma_growth, lag = 12, type = "Ljung-Box")$p.value,
  lb4_quarterly_p = stats::Box.test(quarterly_growth, lag = 4, type = "Ljung-Box")$p.value
)
write_table(wsts_3mma_gap, "s4_wsts_3mma_reconstruction")
write_table(s4, "s4_smoothing_summary")

# ---------------------------------------------------------------------------
# D1: SIA / WSTS Asia-Pacific gap distribution
# ---------------------------------------------------------------------------
message("[D1] SIA vs WSTS Asia-Pacific reconciliation")
ap_3m <- extract_region(wsts_3m, "Asia Pacific")
wsts_ap_window <- ap_3m[ap_3m$date %in% sia$date,
                          c("date", "sales_usd_billions"), drop = FALSE]
names(wsts_ap_window)[2L] <- "wsts_ap_3mma_billions"

ap <- merge(sia[, c("date", "china_3mma_usd_billions",
                     "asia_pacific_other_3mma_usd_billions",
                     "asia_pacific_total_3mma_usd_billions", "source_method")],
            wsts_ap_window, by = "date", sort = TRUE)
if (nrow(ap) != 132L) stop("Unexpected AP joined sample size")

ap$sia_ap_3mma_billions <- ap$china_3mma_usd_billions +
  ap$asia_pacific_other_3mma_usd_billions
# Construction check only: R/05 defines asia_pacific_total as china + ap_other,
# so this is zero by construction and is NOT a validation of the sources.
ap$construction_check_gap <- r6(ap$sia_ap_3mma_billions -
  ap$asia_pacific_total_3mma_usd_billions)
ap$gap_billions <- r6(ap$sia_ap_3mma_billions - ap$wsts_ap_3mma_billions)
ap$abs_gap_billions <- abs(ap$gap_billions)
ap$abs_rel_gap_percent <- 100 * ap$abs_gap_billions / ap$wsts_ap_3mma_billions
ap$signed_rel_gap_percent <- 100 * ap$gap_billions / ap$wsts_ap_3mma_billions
ap$year <- year_num(ap$date)

d1 <- data.frame(
  n_months = nrow(ap),
  n_gap_over_0_02 = sum(ap$abs_gap_billions > 0.02),
  frac_gap_over_0_02 = mean(ap$abs_gap_billions > 0.02),
  n_gap_over_0_03 = sum(ap$abs_gap_billions > 0.03),
  n_gap_over_0_05 = sum(ap$abs_gap_billions > 0.05),
  n_gap_over_0_10 = sum(ap$abs_gap_billions > 0.10),
  n_gap_over_0_30 = sum(ap$abs_gap_billions > 0.30),
  mean_abs_gap_billions = mean(ap$abs_gap_billions),
  median_abs_gap_billions = median(ap$abs_gap_billions),
  p90_abs_gap_billions = as.numeric(quantile(ap$abs_gap_billions, 0.90)),
  max_abs_gap_billions = max(ap$abs_gap_billions),
  mean_abs_rel_gap_percent = mean(ap$abs_rel_gap_percent),
  mean_signed_gap_billions = mean(ap$gap_billions),
  max_abs_construction_check_gap = max(abs(ap$construction_check_gap))
)

# Genuine internal (within-document) check: published SIA Total versus the
# sum of the five published regions. Independent of the WSTS comparison.
sia_region_cols <- c("america_3mma_usd_billions", "europe_3mma_usd_billions",
                     "japan_3mma_usd_billions", "china_3mma_usd_billions",
                     "asia_pacific_other_3mma_usd_billions")
require_cols(sia, c(sia_region_cols, "total_3mma_usd_billions"), "SIA 3MMA")
internal_total <- data.frame(
  date = sia$date,
  source_method = sia$source_method,
  sum_of_regions = rowSums(sia[, sia_region_cols]),
  published_total = sia$total_3mma_usd_billions
)
internal_total$gap_billions <- r6(internal_total$sum_of_regions -
                                  internal_total$published_total)
d1_internal <- data.frame(
  n_months = nrow(internal_total),
  n_exact = sum(internal_total$gap_billions == 0),
  n_abs_gap_le_0_01 = sum(abs(internal_total$gap_billions) <= 0.01),
  n_abs_gap_gt_0_01 = sum(abs(internal_total$gap_billions) > 0.01),
  max_abs_gap_billions = max(abs(internal_total$gap_billions))
)
write_table(internal_total, "d1_sia_internal_total_check_monthly")
write_table(d1_internal, "d1_sia_internal_total_check_summary")

by_year <- split(ap, ap$year)
d1_yearly <- do.call(rbind, lapply(by_year, function(d) {
  data.frame(year = d$year[1L],
             n_months = nrow(d),
             n_over_0_02 = sum(d$abs_gap_billions > 0.02),
             mean_abs_gap_billions = mean(d$abs_gap_billions),
             max_abs_gap_billions = max(d$abs_gap_billions),
             mean_signed_gap_billions = mean(d$gap_billions),
             mean_signed_rel_gap_percent = mean(d$signed_rel_gap_percent))
}))
row.names(d1_yearly) <- NULL
write_table(ap, "d1_ap_monthly_reconciliation")
write_table(d1, "d1_ap_gap_summary")
write_table(d1_yearly, "d1_ap_gap_by_year")

# ---------------------------------------------------------------------------
# D2: Mixed vintage audit and difference dynamics
# ---------------------------------------------------------------------------
message("[D2] Source-method and vintage differences")
source_group <- split(sia, sia$source_method)
d2_methods <- do.call(rbind, lapply(names(source_group), function(label) {
  d <- source_group[[label]]
  data.frame(source_method = label,
             n_months = nrow(d),
             first_date = as.character(min(d$date)),
             last_date = as.character(max(d$date)))
}))
row.names(d2_methods) <- NULL

# SIA first release vs WSTS historical, 2016-2025. This mixes source and
# vintage differences, so it is a D1 (between-document) output, not D2.
ap_2016 <- ap[ap$date >= as.Date("2016-01-01"), , drop = FALSE]
d1_dynamics <- data.frame(
  n_months_2016_2025 = nrow(ap_2016),
  negative_gap_months = sum(ap_2016$gap_billions < 0),
  positive_gap_months = sum(ap_2016$gap_billions > 0),
  zero_gap_months = sum(ap_2016$gap_billions == 0),
  negative_gap_share = mean(ap_2016$gap_billions < 0),
  mean_signed_gap_billions = mean(ap_2016$gap_billions),
  mean_signed_rel_gap_percent = mean(ap_2016$signed_rel_gap_percent),
  correlation_monthly_log_changes = cor(
    diff(log(ap_2016$sia_ap_3mma_billions)),
    diff(log(ap_2016$wsts_ap_3mma_billions))
  ),
  sd_gap_in_log_changes = sd(
    diff(log(ap_2016$sia_ap_3mma_billions)) -
      diff(log(ap_2016$wsts_ap_3mma_billions))
  ),
  sd_wsts_log_changes = sd(diff(log(ap_2016$wsts_ap_3mma_billions))),
  sd_gap_as_fraction_of_wsts_signal = sd(
    diff(log(ap_2016$sia_ap_3mma_billions)) -
      diff(log(ap_2016$wsts_ap_3mma_billions))
  ) / sd(diff(log(ap_2016$wsts_ap_3mma_billions))),
  sia_log_growth_2016_2025 = log(
    tail(ap_2016$sia_ap_3mma_billions, 1L) / ap_2016$sia_ap_3mma_billions[1L]
  ),
  wsts_log_growth_2016_2025 = log(
    tail(ap_2016$wsts_ap_3mma_billions, 1L) / ap_2016$wsts_ap_3mma_billions[1L]
  )
)

stitch <- ap[ap$date %in% as.Date(c("2015-12-01", "2016-01-01")),
             c("date", "source_method", "gap_billions", "signed_rel_gap_percent")]
if (nrow(stitch) != 2L) stop("Cannot locate 2015/2016 source vintage boundary")

write_table(d2_methods, "d2_source_method_coverage")
write_table(d1_dynamics, "d1_sia_vs_wsts_dynamics_2016_2025")
write_table(stitch, "d2_2015_2016_vintage_boundary")

# Compare actual first-publication SIA values with the same reference month
# appearing as "Last Year" in the chart published one year later.
# The resulting 2016-2024 3MMA pairs are a direct within-SIA vintage check.
parse_audit_path <- "output/tables/phase1/sia_regional_pdf_parse_audit.csv"
parse_audit <- read_data(parse_audit_path)
require_cols(parse_audit, c("reference_month", "parse_status",
  "china_current", "asia_pacific_other_current",
  "china_last_year", "asia_pacific_other_last_year"), "SIA PDF audit")
parse_audit$reference_month <- as.Date(parse_audit$reference_month)
parse_audit <- parse_audit[parse_audit$parse_status == "ok", , drop = FALSE]
if (nrow(parse_audit) != 120L) {
  stop("SIA PDF audit must contain 120 successfully parsed months (2016-2025)")
}

first_publication <- data.frame(
  date = parse_audit$reference_month,
  sia_first_publication = parse_audit$china_current +
    parse_audit$asia_pacific_other_current,
  china_first_publication = parse_audit$china_current
)
next_year_retrospective <- data.frame(
  # Avoid date subtraction approximations; map the year directly.
  date = as.Date(sprintf("%04d-%02d-01",
    year_num(parse_audit$reference_month) - 1L,
    month_num(parse_audit$reference_month))),
  sia_12_month_later = parse_audit$china_last_year +
    parse_audit$asia_pacific_other_last_year,
  china_12_month_later = parse_audit$china_last_year
)

revision_pairs <- merge(first_publication, next_year_retrospective,
                        by = "date", sort = TRUE)
revision_pairs <- merge(revision_pairs,
  wsts_ap_window, by = "date", sort = TRUE)
if (nrow(revision_pairs) != 108L ||
    min(revision_pairs$date) != as.Date("2016-01-01") ||
    max(revision_pairs$date) != as.Date("2024-12-01")) {
  stop("Unexpected paired-vintage coverage: expected 2016-2024 (108 months)")
}
revision_pairs$revision_billions <- r6(
  revision_pairs$sia_12_month_later - revision_pairs$sia_first_publication)
revision_pairs$gap_first_vs_wsts <- r6(
  revision_pairs$sia_first_publication - revision_pairs$wsts_ap_3mma_billions)
revision_pairs$gap_after12_vs_wsts <- r6(
  revision_pairs$sia_12_month_later - revision_pairs$wsts_ap_3mma_billions)
revision_pairs$abs_gap_first <- abs(revision_pairs$gap_first_vs_wsts)
revision_pairs$abs_gap_after12 <- abs(revision_pairs$gap_after12_vs_wsts)
revision_pairs$year <- year_num(revision_pairs$date)

# China and China's share of Asia Pacific: the series actually used in the
# Phase 3A China break analysis (R/15).
revision_pairs$china_revision_billions <- r6(
  revision_pairs$china_12_month_later - revision_pairs$china_first_publication)
revision_pairs$china_revision_percent <- 100 *
  revision_pairs$china_revision_billions / revision_pairs$china_first_publication
revision_pairs$china_share_first <- revision_pairs$china_first_publication /
  revision_pairs$sia_first_publication
revision_pairs$china_share_after12 <- revision_pairs$china_12_month_later /
  revision_pairs$sia_12_month_later
revision_pairs$china_share_revision_pp <- r6(100 *
  (revision_pairs$china_share_after12 - revision_pairs$china_share_first))

revision_summary <- data.frame(
  n_months = nrow(revision_pairs),
  mean_revision_billions = mean(revision_pairs$revision_billions),
  median_revision_billions = median(revision_pairs$revision_billions),
  sd_revision_billions = sample_sd(revision_pairs$revision_billions),
  n_revision_positive = sum(revision_pairs$revision_billions > 0),
  n_revision_negative = sum(revision_pairs$revision_billions < 0),
  n_revision_zero = sum(revision_pairs$revision_billions == 0),
  mean_abs_gap_first_vs_wsts = mean(revision_pairs$abs_gap_first),
  mean_abs_gap_after12_vs_wsts = mean(revision_pairs$abs_gap_after12),
  max_abs_gap_first_vs_wsts = max(revision_pairs$abs_gap_first),
  max_abs_gap_after12_vs_wsts = max(revision_pairs$abs_gap_after12),
  n_within_002_first = sum(revision_pairs$abs_gap_first <= 0.02),
  n_within_002_after12 = sum(revision_pairs$abs_gap_after12 <= 0.02),
  n_closer_after12 = sum(revision_pairs$abs_gap_after12 <
                           revision_pairs$abs_gap_first),
  n_ties_after12 = sum(revision_pairs$abs_gap_after12 ==
                         revision_pairs$abs_gap_first),
  n_farther_after12 = sum(revision_pairs$abs_gap_after12 >
                            revision_pairs$abs_gap_first),
  fraction_closer_after12 = mean(revision_pairs$abs_gap_after12 <
                                   revision_pairs$abs_gap_first)
)

china_revision_summary <- data.frame(
  n_months = nrow(revision_pairs),
  n_revision_positive = sum(revision_pairs$china_revision_billions > 0),
  n_revision_negative = sum(revision_pairs$china_revision_billions < 0),
  n_revision_zero = sum(revision_pairs$china_revision_billions == 0),
  mean_revision_percent = mean(revision_pairs$china_revision_percent),
  median_revision_percent = median(revision_pairs$china_revision_percent),
  sd_revision_percent = sample_sd(revision_pairs$china_revision_percent),
  sd_china_first_release_log_change_percent =
    100 * sample_sd(diff(log(revision_pairs$china_first_publication))),
  mean_share_revision_pp = mean(revision_pairs$china_share_revision_pp),
  sd_share_revision_pp = sample_sd(revision_pairs$china_share_revision_pp),
  max_abs_share_revision_pp = max(abs(revision_pairs$china_share_revision_pp))
)
china_revision_by_year <- do.call(rbind, lapply(
  split(revision_pairs, revision_pairs$year), function(d) {
    data.frame(year = d$year[1L], n_months = nrow(d),
               mean_china_revision_percent = mean(d$china_revision_percent),
               mean_china_share_revision_pp = mean(d$china_share_revision_pp))
  }))
row.names(china_revision_by_year) <- NULL

write_table(revision_pairs, "d2_paired_vintage_revision_monthly")
write_table(revision_summary, "d2_paired_vintage_revision_summary")
write_table(china_revision_summary, "d2_china_paired_revision_summary")
write_table(china_revision_by_year, "d2_china_paired_revision_by_year")

# ---------------------------------------------------------------------------
# S2: Observed-data BIC sensitivity and stylized Monte Carlo
# ---------------------------------------------------------------------------
message("[S2] Observed BIC models using strucchange")
observed_specs <- list(
  list(name = "raw_monthly_growth_mean", response = world_growth,
       trend = FALSE, h = 24L),
  list(name = "position_demeaned_monthly_growth_mean",
       response = world_growth_adjusted, trend = FALSE, h = 24L),
  list(name = "raw_monthly_log_trend", response = world_log,
       trend = TRUE, h = 24L),
  list(name = "quarterly_log_growth_mean", response = quarterly_growth,
       trend = FALSE, h = 8L)
)
observed_bic <- do.call(rbind, lapply(observed_specs, function(s) {
  ans <- choose_bic_breaks(s$response, trend = s$trend, h = s$h)
  data.frame(specification = s$name,
             observations = length(s$response),
             min_segment_observations = s$h,
             chosen_breaks = ans$nbreaks,
             BIC_0_breaks = ans$bic[1L],
             minimum_BIC = min(ans$bic))
}))
write_table(observed_bic, "s2_observed_bic_models")

if (reps > 0L) {
  message("[S2] Stylized AR(1) trend Monte Carlo (", reps, " reps/case)")
  phi_values <- c(0, 0.5, 0.8, 0.95)
  # Per-block seed: results do not depend on random draws consumed earlier.
  set.seed(base_seed + 1L)
  ar_results <- vector("list", length(phi_values) * reps)
  k <- 0L
  n_months <- nrow(world_m)

  for (phi in phi_values) {
    for (iter in seq_len(reps)) {
      # Stationary AR(1) with unit unconditional variance.
      u <- numeric(n_months)
      u[1L] <- rnorm(1)
      innovations <- rnorm(n_months, sd = sqrt(1 - phi^2))
      for (i in 2:n_months) u[i] <- phi * u[i - 1L] + innovations[i]
      series <- 3 + 0.006 * seq_len(n_months) + 0.04 * u
      b <- choose_bic_breaks(series, trend = TRUE, h = 24L)$nbreaks
      k <- k + 1L
      ar_results[[k]] <- data.frame(phi = phi, iteration = iter,
                                     selected_breaks = b)
    }
    message("  AR(1) phi=", phi, " completed")
  }

  ar_reps <- do.call(rbind, ar_results)
  ar_summary <- do.call(rbind, lapply(split(ar_reps, ar_reps$phi), function(d) {
    data.frame(phi = d$phi[1L], reps = nrow(d),
               Pr_at_least_one = mean(d$selected_breaks >= 1),
               Pr_at_least_two = mean(d$selected_breaks >= 2),
               mean_breaks = mean(d$selected_breaks))
  }))
  row.names(ar_summary) <- NULL
  write_table(ar_reps, "s2_trend_ar1_simulation_draws")
  write_table(ar_summary, "s2_trend_ar1_simulation_summary")

  message("[S2] Stylized calendar + mean shift Monte Carlo (", reps, " reps/case)")
  seasonality <- tapply(world_growth, position_in_quarter(world_growth_dates), mean)
  seasonality <- seasonality - mean(seasonality)
  seasonality <- as.numeric(seasonality)

  delta_values <- c(0, 0.005, 0.01, 0.02)
  set.seed(base_seed + 2L)
  growth_reps <- vector("list", length(delta_values) * reps * 3L)
  k <- 0L

  for (delta in delta_values) {
    for (iter in seq_len(reps)) {
      idx <- seq_along(world_growth)
      positions <- position_in_quarter(world_growth_dates)
      # Fixed calendar effects calibrated to the observed three-position pattern.
      # No break for delta=0; for delta>0, the growth mean shifts mid-sample.
      growth <- 0.003 + seasonality[positions] +
        delta * as.numeric(idx > floor(length(idx) / 2L)) +
        rnorm(length(idx), sd = 0.045)
      adjusted <- growth - mean_by_position(growth, world_growth_dates)
      sim_levels <- exp(c(log(30), log(30) + cumsum(growth)))
      sim_quarters <- vapply(q_groups, function(ii) sum(sim_levels[ii]), numeric(1L))
      sim_qgrowth <- diff(log(sim_quarters[q_order]))

      cases <- list(
        raw_growth = list(x = growth, h = 24L),
        position_demeaned_growth = list(x = adjusted, h = 24L),
        quarterly_growth = list(x = sim_qgrowth, h = 8L)
      )

      for (name in names(cases)) {
        a <- cases[[name]]
        b <- choose_bic_breaks(a$x, trend = FALSE, h = a$h)$nbreaks
        k <- k + 1L
        growth_reps[[k]] <- data.frame(delta = delta, iteration = iter,
                                       specification = name, selected_breaks = b)
      }
    }
    message("  Growth delta=", delta, " completed")
  }

  growth_reps <- do.call(rbind, growth_reps)
  growth_summary <- do.call(rbind, lapply(
    split(growth_reps, interaction(growth_reps$delta, growth_reps$specification,
                                   drop = TRUE)), function(d) {
      data.frame(delta = d$delta[1L], specification = d$specification[1L],
                 reps = nrow(d),
                 Pr_at_least_one = mean(d$selected_breaks >= 1),
                 Pr_at_least_two = mean(d$selected_breaks >= 2),
                 mean_breaks = mean(d$selected_breaks))
    }
  ))
  row.names(growth_summary) <- NULL
  write_table(growth_reps, "s2_growth_calendar_simulation_draws")
  write_table(growth_summary, "s2_growth_calendar_simulation_summary")
} else {
  message("[S2] SEMICON_AUDIT_REPS = 0: Monte Carlo skipped (deterministic checks only)")
}
# ---------------------------------------------------------------------------
# Reproducibility / caveats
# ---------------------------------------------------------------------------
notes <- c(
  "Semiconductors II — Script 17 methodological replication (v3)",
  paste0("Date: ", Sys.Date()),
  paste0("strucchange version: ", as.character(utils::packageVersion("strucchange"))),
  paste0("R version: ", getRversion()),
  paste0("RNGkind: ", paste(RNGkind(), collapse = " / ")),
  paste0("Base seed: ", base_seed, " (AR(1) block: +1, calendar block: +2); ",
         "replications per Monte Carlo case: ", reps,
         if (reps == 0L) " (Monte Carlo skipped)" else ""),
  "",
  "S1: quarter-end calendar effect is an observed pattern, not proof of a 4-4-5 reporting calendar.",
  "S1: within-quarter and adjacent-month definitions of quarter-end excess are reported separately (64 vs 63 cases).",
  "S2: Monte Carlo results use strucchange BIC (AIC with k = log(n), zero breaks included), identical to summary()$RSS['BIC', ] used in R/14.",
  "S2: in the trend design, BIC selection is invariant to intercept, slope and noise scale (each segment has its own linear trend; scaling noise scales all RSS equally). Only phi, n and h matter.",
  "S2: Monte Carlo model is stylized; results are not estimates of actual market break size/power.",
  "S2: BIC model selection is not a calibrated supF significance test. SupF inference belongs in Script 19.",
  "S2: strucchange may emit 'sorting not possible' warnings from summary.breakpointsfull; these only affect display ordering of break dates, not RSS/BIC or selected break counts.",
  "S4: 3MMA autocorrelation includes a mechanical overlap component but may also reflect economic dynamics.",
  "S4: theoretical 2/3 and 1/3 lag correlations require an equal-weight MA of iid innovations; a trailing 3MMA also removes any fixed within-quarter pattern exactly.",
  "S4: empirical benchmark = trailing 3-term MA of calendar-demeaned monthly growth; Working (1960) benchmark for quarterly aggregation of a random walk: ACF(1) = 8/38.",
  "D1: SIA first release vs WSTS historical mixes source and vintage differences (d1_* files).",
  "D1: the AP construction check is zero by construction (R/05 defines AP total as China + AP Other); the genuine internal check is published Total vs sum of five regions.",
  "D1: a nominal 0.02-billion tolerance is descriptive and may be too strict for multi-vintage data.",
  "D2: actual revisions = same reference month, first publication vs 'Last Year' column 12 months later (2016-2024), for AP total, China and China share.",
  "D2: source_method documents a 2015 retrospective (+12-month vintage) / 2016-2025 first-publication boundary.",
  "D2: the +12-month retrospective is not necessarily the final WSTS vintage.",
  "Comparisons of revisions and gaps are made after rounding to 1e-6 (two-decimal source values).",
  "All Phase 3A break dates remain provisional until Scripts 18 and 19.",
  "No policy dates were imposed, and no causal effect is identified.",
  "",
  "sessionInfo():",
  utils::capture.output(utils::sessionInfo())
)
writeLines(notes, file.path(out_dir, "audit17_notes.txt"), useBytes = TRUE)

message("[DONE] Wrote audit results to ", out_dir)
print(s1)
print(s4)
print(d1)
print(d1_internal)
print(d1_dynamics)
print(revision_summary)
print(china_revision_summary)
print(observed_bic)
if (reps > 0L) {
  print(ar_summary)
  print(growth_summary)
}