# Semiconductors II
# R/19_break_robustness_quarterly.R
# Phase 3A - Methodological reassessment, step 3 of 3 (NOT Phase 3C)
#
# Scientific question:
# Do apparent breaks in quarterly semiconductor sales survive controls for
# quarter-of-year patterns and short-run dependence, without imposing policy dates?
#
# This script is intended to be RUN BY THE RESEARCHER in RStudio.
# It does not run Script 17/18, invoke Git/GitHub, simulate policy effects, or
# identify causal impacts. Nothing outside output/tables/phase3/audit19_revised/ is written.
#
# Data source: data/processed/quarterly_series_2010_2025.csv, WRITTEN BY
# THE CLAUDIO-BASED REVISED SCRIPT 18 (verified in the script source).
# Columns: quarter, quarter_end_date, series, source, vintage,
# quarterly_sales_usd_billions.
# Sources: WSTS, SIA_vintage12m, SIA_processed_mixed_vintage.
# The SIA +12m primary block is cross-checked against Script 18's monthly
# vintage file to avoid accidentally using a stale quarterly output.
# Fixed +12-month revision horizon: 2015Q1-2024Q4 is PRIMARY for SIA.
# 2025 retains first-release values unless completed separately: SENSITIVITY.
#
# CORE INFERENCE: sup-F for ONE unknown break in the conditional mean of
# quarterly log growth (or change in a relative indicator):
#   g_t = c + sum_{j=1}^p phi_j*g_{t-j} + quarter_dummies_t
#         + delta * I(t > b) + error_t
# The maximizing candidate b is searched ENDOGENOUSLY (no 2018 intervention).
# The p-value comes from a fixed-initial-conditions bootstrap of the
# no-break AR(p)+quarter-dummy model. Each bootstrap draw RE-SEARCHES all dates.
# Primary null inference is reported with both iid residual resampling and
# a Rademacher wild bootstrap as a sensitivity to heteroskedasticity.
# Both remain CONDITIONAL on stable AR dynamics, stable seasonality, and a
# correctly specified innovation process. Neither is a universal HAC-robust test.
# No renewed BIC analysis of log-level trend is conducted: see Script 17.
#
# Reproducibility:
#   Sys.setenv(SEMICON_S19_BOOT_REPS = "399")  # optional; e.g. 999 for review
#   source("R/19_break_robustness_quarterly_revisado.R", encoding = "UTF-8")
# Uses base R only; no packages need installation for this script.
# Note: this is a revised audit script, not a replacement for Script 17/18.
input_file <- "data/processed/quarterly_series_2010_2025.csv"
v12_file <- "data/processed/sia_regional_3mma_vintage12m_2015_2025.csv"
if (!file.exists(input_file) || !file.exists(v12_file)) {
  stop("Missing quarterly or +12-month vintage input from revised Script 18. ",
       "Expected: ", input_file, " and ", v12_file)
}
reps <- suppressWarnings(as.integer(Sys.getenv("SEMICON_S19_BOOT_REPS", "399")))
if (is.na(reps) || reps < 99L) stop("SEMICON_S19_BOOT_REPS must be >= 99.")
alpha <- 0.05
seed <- 19112026L
out_dir <- "output/tables/phase3/audit19_revised"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

save_table <- function(x, stem) {
  path <- file.path(out_dir, paste0(stem, ".csv"))
  utils::write.csv(x, path, row.names = FALSE, na = "")
  message("[SAVE] ", path)
}

message("[LOAD] Reading Script 18 quarterly series (read-only)")
qdata <- utils::read.csv(input_file, stringsAsFactors = FALSE)
needed <- c("quarter", "quarter_end_date", "series", "source", "vintage",
            "quarterly_sales_usd_billions")
missing <- setdiff(needed, names(qdata))
if (length(missing)) stop("Wrong Script 18 schema; missing: ",
                         paste(missing, collapse = ", "))
qdata$quarter_end_date <- as.Date(qdata$quarter_end_date)
if (anyNA(qdata$quarter_end_date) ||
    any(!is.finite(qdata$quarterly_sales_usd_billions)) ||
    any(qdata$quarterly_sales_usd_billions <= 0)) {
  stop("Invalid dates, sales values or non-positive sales in Script 18 input.")
}
# Verify the PRIMARY SIA quarterly entries against the +12-month monthly
# vintage that the revised Script 18 actually generated. A schema-compatible
# old CSV is not enough: the values must match 3 x quarter-end 3MMA.
v12_monthly <- utils::read.csv(v12_file, stringsAsFactors = FALSE)
required_v12 <- c("date", "vintage", "china_3mma_usd_billions",
                  "asia_pacific_other_3mma_usd_billions",
                  "total_3mma_usd_billions")
if (length(setdiff(required_v12, names(v12_monthly)))) {
  stop("Revised Script 18 monthly vintage CSV has unexpected schema.")
}
v12_monthly$date <- as.Date(v12_monthly$date)
v12_end <- v12_monthly[
  as.integer(format(v12_monthly$date, "%m")) %% 3L == 0L &
    as.integer(format(v12_monthly$date, "%Y")) <= 2024L, , drop = FALSE]
v12_end <- v12_end[order(v12_end$date), , drop = FALSE]
if (nrow(v12_end) != 40L || anyNA(v12_end$date) ||
    any(v12_end$vintage != "plus_12_months")) {
  stop("Expected 40 quarterly +12-month-vintage SIA observations for 2015-2024.")
}
for (entry in list(
  c("China", "china_3mma_usd_billions"),
  c("Asia Pacific Other", "asia_pacific_other_3mma_usd_billions"),
  c("Total", "total_3mma_usd_billions")
)) {
  r <- qdata[qdata$source == "SIA_vintage12m" &
               qdata$series == entry[1L] &
               qdata$quarter_end_date <= as.Date("2024-12-01"), , drop = FALSE]
  r <- r[order(r$quarter_end_date), , drop = FALSE]
  if (nrow(r) != 40L ||
      !identical(r$quarter_end_date, v12_end$date) ||
      max(abs(r$quarterly_sales_usd_billions -
                3 * v12_end[[entry[2L]]])) > 1e-7) {
    stop("Stale or inconsistent quarterly data for SIA ", entry[1L],
         ". Re-run approved revised Script 18 locally before Script 19.")
  }
}
message("[CHECK] Revised Script 18 +12-month SIA quarterly data validated")

# Keep a fixed scope for the original WSTS data and the source-specific SIA
# revisions. These labels are from Claudio's Script 18 and must match exactly.
expect_quarters <- function(d, start_year, end_year, name) {
  target <- unlist(lapply(start_year:end_year,
                          function(y) paste0(y, "Q", 1:4)), use.names = FALSE)
  if (!identical(as.character(d$quarter), target)) {
    stop(name, ": expected contiguous unique quarters ", start_year,
         "Q1-", end_year, "Q4. Found ", nrow(d), " rows.")
  }
  actual_ids <- paste0(format(d$quarter_end_date, "%Y"), "Q",
                       (as.integer(format(d$quarter_end_date, "%m")) - 1L) %/% 3L + 1L)
  if (!identical(actual_ids, as.character(d$quarter)) ||
      any(!(as.integer(format(d$quarter_end_date, "%m")) %in%
            c(3L, 6L, 9L, 12L)))) {
    stop(name, ": quarter labels and dates are not aligned.")
  }
}
get_sales <- function(src, name, from, to) {
  d <- qdata[qdata$source == src & qdata$series == name &
               as.integer(substr(qdata$quarter, 1L, 4L)) >= from &
               as.integer(substr(qdata$quarter, 1L, 4L)) <= to, , drop = FALSE]
  d <- d[order(d$quarter_end_date), , drop = FALSE]
  expect_quarters(d, from, to, paste(src, name))
  if (identical(src, "SIA_vintage12m") && to <= 2024L &&
      !all(d$vintage == "plus_12_months")) {
    stop("Expected fixed +12-month revision horizon for ", src, " ", name)
  }
  d
}

# Split series used for relative China measures from SAME-source, SAME-vintage
# China and Asia-Pacific Other. Do not mix WSTS and SIA in a ratio.
relative_from_source <- function(src, from, to, which = "ratio") {
  ch <- get_sales(src, "China", from, to)
  ot <- get_sales(src, "Asia Pacific Other", from, to)
  if (!identical(ch$quarter, ot$quarter)) stop("China and AP Other misaligned")
  cval <- ch$quarterly_sales_usd_billions
  oval <- ot$quarterly_sales_usd_billions
  values <- switch(which,
                   ratio = log(cval / oval),
                   share = cval / (cval + oval),
                   stop("Invalid relative series"))
  list(dates = ch$quarter_end_date, quarters = ch$quarter, values = values,
       transform = "identity")
}
get_series <- function(src, series, from, to, mode = "sales") {
  if (mode %in% c("ratio", "share")) {
    return(relative_from_source(src, from, to, mode))
  }
  d <- get_sales(src, series, from, to)
  list(dates = d$quarter_end_date, quarters = d$quarter,
       values = d$quarterly_sales_usd_billions, transform = "log")
}

# PRIMARY: predeclared three hypotheses. Remaining cases are supporting
# comparisons. Main orders follow Script 18 diagnostics: AR(1) for Worldwide
# and China, AR(0) for China/AP Other log ratio. The other order is sensitivity.
# Holm controls familywise error across exactly these three primary tests.
cases <- list(
  list(id = "world_wsts", label = "WSTS Worldwide", role = "primary",
       source = "WSTS", series = "Worldwide", from = 2010L, to = 2025L,
       mode = "sales", h = 8L),
  list(id = "china_v12", label = "SIA China +12m 2015-2024", role = "primary",
       source = "SIA_vintage12m", series = "China", from = 2015L, to = 2024L,
       mode = "sales", h = 6L),
  list(id = "china_ratio_v12", label = "SIA log(China/AP Other) +12m", role = "primary",
       source = "SIA_vintage12m", series = "relative", from = 2015L,
       to = 2024L, mode = "ratio", h = 6L),
  list(id = "america_wsts", label = "WSTS America", role = "secondary",
       source = "WSTS", series = "America", from = 2010L, to = 2025L,
       mode = "sales", h = 8L),
  list(id = "europe_wsts", label = "WSTS Europe", role = "secondary",
       source = "WSTS", series = "Europe", from = 2010L, to = 2025L,
       mode = "sales", h = 8L),
  list(id = "japan_wsts", label = "WSTS Japan", role = "secondary",
       source = "WSTS", series = "Japan", from = 2010L, to = 2025L,
       mode = "sales", h = 8L),
  list(id = "ap_wsts", label = "WSTS Asia Pacific", role = "secondary",
       source = "WSTS", series = "Asia Pacific", from = 2010L, to = 2025L,
       mode = "sales", h = 8L),
  list(id = "ap_other_v12", label = "SIA AP Other +12m 2015-2024",
       role = "secondary", source = "SIA_vintage12m",
       series = "Asia Pacific Other", from = 2015L, to = 2024L,
       mode = "sales", h = 6L),
  list(id = "china_mixed", label = "SIA China mixed 2015-2024",
       role = "vintage_sensitivity", source = "SIA_processed_mixed_vintage",
       series = "China", from = 2015L, to = 2024L, mode = "sales", h = 6L),
  list(id = "ratio_mixed", label = "SIA ratio mixed 2015-2024",
       role = "vintage_sensitivity", source = "SIA_processed_mixed_vintage",
       series = "relative", from = 2015L, to = 2024L, mode = "ratio", h = 6L),
  list(id = "world_h10", label = "WSTS Worldwide h=10-quarter sensitivity",
       role = "trimming_sensitivity", source = "WSTS", series = "Worldwide",
       from = 2010L, to = 2025L, mode = "sales", h = 10L),
  list(id = "china_v12_h8", label = "SIA China +12m h=8-quarter sensitivity",
       role = "trimming_sensitivity", source = "SIA_vintage12m", series = "China",
       from = 2015L, to = 2024L, mode = "sales", h = 8L),
  list(id = "ratio_v12_h8", label = "SIA ratio +12m h=8-quarter sensitivity",
       role = "trimming_sensitivity", source = "SIA_vintage12m",
       series = "relative", from = 2015L, to = 2024L, mode = "ratio", h = 8L),
  list(id = "china_2025", label = "SIA China +12m plus 2025",
       role = "extension_sensitivity", source = "SIA_vintage12m",
       series = "China", from = 2015L, to = 2025L, mode = "sales", h = 7L),
  list(id = "ratio_2025", label = "SIA ratio +12m plus 2025",
       role = "extension_sensitivity", source = "SIA_vintage12m",
       series = "relative", from = 2015L, to = 2025L, mode = "ratio", h = 7L)
)

# Build quarterly growth/change vector, associated quarters, and its lags.
# A candidate b means last pre-break growth quarter is b, and the post-break
# regime starts at b+1. It does not designate any historical intervention.
prepare_case <- function(case) {
  v <- get_series(case$source, case$series, case$from, case$to, case$mode)
  levels <- if (v$transform == "log") log(v$values) else v$values
  growth <- diff(levels)
  g_dates <- v$dates[-1L]
  if (any(!is.finite(growth)) || length(growth) < 30L) {
    stop("Insufficient or invalid growth observations: ", case$id)
  }
  list(levels = levels, growth = as.numeric(growth),
       level_dates = v$dates, growth_dates = g_dates,
       level_quarters = v$quarters, growth_quarters = v$quarters[-1L],
       seasonal_q = as.integer(substr(v$quarters[-1L], 6L, 6L)))
}

# Regression uses a common lagged-dependent-variable sample for every date.
# Under H0, stable quarterly seasonal intercepts and stable AR coefficients.
make_null_design <- function(g, quarters, p) {
  n <- length(g)
  ix <- (p + 1L):n
  x <- matrix(1, nrow = length(ix), ncol = 1L,
              dimnames = list(NULL, "intercept"))
  for (j in seq_len(p)) {
    x <- cbind(x, g[ix - j])
    colnames(x)[ncol(x)] <- paste0("lag", j)
  }
  for (q in 2:4) {
    x <- cbind(x, as.numeric(quarters[ix] == q))
    colnames(x)[ncol(x)] <- paste0("Q", q)
  }
  list(x = x, y = g[ix], times = ix)
}

# Efficient nested one-dummy F from Frisch-Waugh-Lovell residualization.
# The search statistic is maximal over ALL admissible unknown break positions.
scan_breaks <- function(g, quarters, p, h) {
  m <- make_null_design(g, quarters, p)
  x <- m$x
  fit <- stats::lm.fit(x, m$y)
  if (fit$rank != ncol(x)) stop("Null regression is rank-deficient")
  # Explicit namespace: Script 18 defines qr() as a quarter extractor.
  # This call needs the BASE R QR matrix decomposition, not that helper.
  qr0 <- base::qr(x)
  e <- as.numeric(fit$residuals)
  rss0 <- sum(e^2)
  if (!is.finite(rss0) || rss0 <= .Machine$double.eps) {
    stop("Null model residual variance is zero")
  }
  n <- length(g)
  # At least h usable observations to each side of the proposed break.
  candidates <- seq.int(p + h, n - h)
  if (!length(candidates) || tail(candidates, 1L) < candidates[1L]) {
    stop("Insufficient growth observations for minimum regime size")
  }
  df2 <- nrow(x) - ncol(x) - 1L
  if (df2 < 4L) stop("Insufficient residual degrees of freedom")
  f_values <- numeric(length(candidates))
  effect <- numeric(length(candidates))
  for (i in seq_along(candidates)) {
    step <- as.numeric(m$times > candidates[i])
    # Orthogonalize the break indicator with respect to H0 regressors.
    dr <- step - as.vector(x %*% base::qr.coef(qr0, step))
    norm2 <- sum(dr^2)
    if (norm2 <= 1e-9) stop("Break dummy collinear with baseline")
    beta <- sum(e * dr) / norm2
    improvement <- pmin(pmax(beta^2 * norm2, 0), rss0)
    rss1 <- rss0 - improvement
    f_values[i] <- if (rss1 <= .Machine$double.eps) Inf else
      improvement / (rss1 / df2)
    effect[i] <- beta
  }
  best <- which.max(f_values)
  residual_lb8 <- if (length(e) > 10L) {
    stats::Box.test(e, lag = 8L, type = "Ljung-Box",
                    fitdf = p)$p.value
  } else NA_real_
  # After choosing the strongest candidate, fit the alternative with the
  # selected shift dummy. The long-run multiplier uses these ALTERNATIVE AR
  # coefficients, not the no-break coefficients, and is post-selection only.
  step_best <- as.numeric(m$times > candidates[best])
  fit_alt <- stats::lm.fit(cbind(x, shift = step_best), m$y)
  if (fit_alt$rank != ncol(x) + 1L) stop("Alternative fit is rank-deficient")
  ar_alt <- if (p > 0L) unname(fit_alt$coefficients[2L:(1L + p)]) else numeric(0)
  shift_alt <- unname(tail(fit_alt$coefficients, 1L))
  if (abs(shift_alt - effect[best]) > 1e-7) {
    stop("FWL estimated shift does not match full alternative regression")
  }
  denominator <- 1 - sum(ar_alt)
  multiplier_valid <- ar_stable(ar_alt) &&
    is.finite(denominator) && abs(denominator) > 0.05
  list(
    maxF = f_values[best], best_b = candidates[best],
    shift_coefficient = shift_alt,
    long_run_growth_shift = if (multiplier_valid)
      shift_alt / denominator else NA_real_,
    long_run_denominator = denominator,
    alt_ar_stable = ar_stable(ar_alt),
    alt_ar = ar_alt,
    all_candidates = candidates, allF = f_values,
    intercept = unname(fit$coefficients[1L]),
    ar = if (p > 0L) unname(fit$coefficients[2L:(1L + p)]) else numeric(0),
    seasonal = unname(tail(fit$coefficients, 3L)),
    null_residuals = e, null_lb8_p = residual_lb8,
    n_fit = nrow(x), n_candidates = length(candidates)
  )
}

# The conditional bootstrap is meaningful only for a stable AR process.
ar_stable <- function(phi) {
  # 1 - phi1*z - ... - phi_p*z^p must have no roots inside unit circle.
  if (any(!is.finite(phi))) return(FALSE)
  poly <- c(1, -phi)
  # Strip trailing zeros (all-zero AR coefficients -> white noise).
  while (length(poly) > 1L && abs(tail(poly, 1L)) < 1e-12) {
    poly <- head(poly, -1L)
  }
  if (length(poly) == 1L) return(TRUE)
  all(Mod(polyroot(poly)) > 1.001)
}

simulate_null <- function(observed_g, quarters, fit, p,
                          method = c("iid_residual", "wild_rademacher")) {
  method <- match.arg(method)
  n <- length(observed_g)
  sim <- numeric(n)
  if (p > 0L) sim[seq_len(p)] <- observed_g[seq_len(p)]
  centered <- fit$null_residuals - mean(fit$null_residuals)
  if (method == "iid_residual") {
    eps <- sample(centered, size = n - p, replace = TRUE)
  } else {
    # Preserve observed innovation magnitudes/time positions while randomizing
    # their sign. Sensitivity to nonconstant unconditional volatility; it does
    # NOT solve serial dependence or all forms of conditional heteroskedasticity.
    eps <- centered * sample(c(-1, 1), size = n - p, replace = TRUE)
  }
  for (t in seq.int(p + 1L, n)) {
    seasonal <- if (quarters[t] == 1L) 0 else
      fit$seasonal[quarters[t] - 1L]
    sim[t] <- fit$intercept +
      sum(fit$ar * sim[t - seq_len(p)]) + seasonal + eps[t - p]
  }
  sim
}

# Model order is declared BEFORE testing. The primary China/AP Other ratio
# uses AR(0), and its AR(1) fit is sensitivity. The other two primary series
# use AR(1), with AR(2) sensitivity. Bootstrap methods are distinct.
bootstrap_case <- function(ca, pdata, p, n_rep, case_seed,
                           method = "iid_residual") {
  g <- pdata$growth
  quarters <- pdata$seasonal_q
  obs <- scan_breaks(g, quarters, p, ca$h)
  if (!ar_stable(obs$ar)) {
    warning(ca$id, " AR(", p, ") unstable. Bootstrap p-value not computed.")
    return(list(observed = obs, p = NA_real_, draws = numeric(0),
                status = "unstable_null_ar"))
  }
  set.seed(case_seed)
  draws <- numeric(n_rep)
  for (r in seq_len(n_rep)) {
    simulated <- simulate_null(g, quarters, obs, p, method = method)
    draws[r] <- scan_breaks(simulated, quarters, p, ca$h)$maxF
  }
  # Add-one correction: null p-value never reported as exactly zero.
  p_boot <- (1 + sum(draws >= obs$maxF)) / (n_rep + 1)
  list(observed = obs, p = p_boot, draws = draws, status = "ok")
}

# We deliberately DO NOT rerun segmented-level BIC because Script 17 showed
# how persistent/integrated log levels can produce spurious dates. The focus
# here is inference for a single unknown shift in quarterly growth/relative
# change, with conditional dynamic controls.

message("[RUN] Conditional supF bootstrap; AR orders predeclared, no policy date")
manifest <- list()
results <- list()
boot_rows <- list()
location_rows <- list()
j <- 0L
# Use the Script 18 order for the relative China series as primary.
primary_ar_order <- function(ca) if (ca$mode == "ratio") 0L else 1L
for (i in seq_along(cases)) {
  ca <- cases[[i]]
  pdata <- prepare_case(ca)
  message("  [", i, "/", length(cases), "] ", ca$id, " (", ca$role, ")")
  manifest[[i]] <- data.frame(
    case_id = ca$id, label = ca$label, role = ca$role,
    source = ca$source, mode = ca$mode,
    first_quarter = pdata$level_quarters[1L],
    last_quarter = tail(pdata$level_quarters, 1L),
    n_levels = length(pdata$levels), n_growth = length(pdata$growth),
    min_postlag_segment_observations = ca$h,
    stringsAsFactors = FALSE
  )
  p0 <- primary_ar_order(ca)
  orders <- if (ca$role == "primary") c(p0, p0 + 1L) else p0
  for (p in orders) {
    is_primary <- ca$role == "primary" && p == p0
    # IID residual bootstrap is the baseline for all cases.
    iid <- bootstrap_case(ca, pdata, p, reps,
                          seed + i * 100L + p * 10L + 1L,
                          method = "iid_residual")
    # Wild Rademacher bootstrap is mandatory sensitivity for each predeclared
    # primary statistic, but need not double the secondary run count.
    wild <- if (is_primary) bootstrap_case(
      ca, pdata, p, reps, seed + i * 100L + p * 10L + 2L,
      method = "wild_rademacher") else NULL
    obs <- iid$observed
    if (!is.null(wild) &&
        (obs$best_b != wild$observed$best_b ||
         abs(obs$maxF - wild$observed$maxF) > 1e-9)) {
      stop("Bootstrap procedures unexpectedly disagree about observed supF")
    }
    last_q <- pdata$growth_quarters[obs$best_b]
    first_q <- pdata$growth_quarters[obs$best_b + 1L]
    j <- j + 1L
    results[[j]] <- data.frame(
      case_id = ca$id, label = ca$label, role = ca$role,
      ar_order = p, primary_ar_order = p0, is_primary_spec = is_primary,
      n_growth = length(pdata$growth), n_postlag = obs$n_fit, h = ca$h,
      candidate_dates = obs$n_candidates,
      supF = obs$maxF,
      bootstrap_reps_iid = length(iid$draws),
      bootstrap_p_iid = iid$p, bootstrap_status_iid = iid$status,
      bootstrap_reps_wild = if (is.null(wild)) 0L else length(wild$draws),
      bootstrap_p_wild = if (is.null(wild)) NA_real_ else wild$p,
      bootstrap_status_wild = if (is.null(wild)) "not_run_secondary" else wild$status,
      break_last_pre_quarter = last_q,
      break_first_post_quarter = first_q,
      conditional_shift_estimate = obs$shift_coefficient,
      long_run_shift_log_points_per_quarter = obs$long_run_growth_shift,
      long_run_denominator = obs$long_run_denominator,
      alternative_ar_stable = obs$alt_ar_stable,
      # For sales growth: roughly percentage-points per quarter. For the log
      # China/AP Other ratio: this is a log-relative-growth change, NOT share pp.
      long_run_shift_times_100 = 100 * obs$long_run_growth_shift,
      ar_coefficient_1_null = if (p >= 1L) obs$ar[1L] else NA_real_,
      ar_coefficient_2_null = if (p >= 2L) obs$ar[2L] else NA_real_,
      null_residual_lb8_p = obs$null_lb8_p,
      stringsAsFactors = FALSE
    )
    if (length(iid$draws)) {
      boot_rows[[length(boot_rows) + 1L]] <- data.frame(
        case_id = ca$id, ar_order = p, method = "iid_residual",
        replication = seq_along(iid$draws), null_maxF = iid$draws)
    }
    if (!is.null(wild) && length(wild$draws)) {
      boot_rows[[length(boot_rows) + 1L]] <- data.frame(
        case_id = ca$id, ar_order = p, method = "wild_rademacher",
        replication = seq_along(wild$draws), null_maxF = wild$draws)
    }
    location_rows[[length(location_rows) + 1L]] <- data.frame(
      case_id = ca$id, ar_order = p,
      last_pre_break_quarter = pdata$growth_quarters[obs$all_candidates],
      first_post_break_quarter = pdata$growth_quarters[obs$all_candidates + 1L],
      search_F = obs$allF
    )
  }

}

summary <- do.call(rbind, results)
manifest <- do.call(rbind, manifest)
row.names(summary) <- NULL
# Explicitly require all 3 primary statistics before Holm correction.
# Never silently reclassify an unevaluable primary hypothesis as exploratory.
summary$holm_p_iid_primary <- NA_real_
summary$holm_p_wild_primary <- NA_real_
primary <- which(summary$is_primary_spec)
holm_ready <- length(primary) == 3L &&
  all(is.finite(summary$bootstrap_p_iid[primary])) &&
  all(is.finite(summary$bootstrap_p_wild[primary]))
if (holm_ready) {
  summary$holm_p_iid_primary[primary] <- stats::p.adjust(
    summary$bootstrap_p_iid[primary], method = "holm")
  summary$holm_p_wild_primary[primary] <- stats::p.adjust(
    summary$bootstrap_p_wild[primary], method = "holm")
} else {
  warning("Holm NOT applied: at least one prespecified primary bootstrap ",
          "p-value is missing. All primary conclusions NOT_EVALUABLE.")
}
summary$holm_status <- if (holm_ready) "applied_three_primary" else
  "not_applied_incomplete_primary_family"
summary$diagnostic_note <- ifelse(
  !is.finite(summary$bootstrap_p_iid), "IID bootstrap unavailable",
  ifelse(!is.finite(summary$null_residual_lb8_p) |
           summary$null_residual_lb8_p < alpha,
         "H0 AR residual dependence remains; bootstrap inference fragile",
         "No Ljung-Box(8) rejection at 5%; assumptions still conditional"))
summary$conclusion <- "EXPLORATORY: sensitivity, no multiplicity claim"
for (idx in seq_len(nrow(summary))) {
  row <- summary[idx, ]
  if (row$is_primary_spec) {
    if (!holm_ready) {
      summary$conclusion[idx] <- "NOT_EVALUABLE: primary Holm family incomplete"
    } else if (!is.finite(row$null_residual_lb8_p) ||
               row$null_residual_lb8_p < alpha) {
      summary$conclusion[idx] <- "INCONCLUSIVE: null AR residual autocorrelation"
    } else {
      iid_sig <- row$holm_p_iid_primary <= alpha
      wild_sig <- row$holm_p_wild_primary <= alpha
      summary$conclusion[idx] <- if (iid_sig && wild_sig) {
        "PRIMARY: conditional one-shift evidence robust to both bootstrap methods"
      } else if (xor(iid_sig, wild_sig)) {
        "INCONCLUSIVE: significance depends on bootstrap method"
      } else {
        "PRIMARY: no one-shift evidence at adjusted 5%; low power possible"
      }
    }
  } else if (!is.finite(row$bootstrap_p_iid)) {
    summary$conclusion[idx] <- "NOT_EVALUABLE: sensitivity bootstrap missing"
  } else if (!is.finite(row$null_residual_lb8_p) ||
             row$null_residual_lb8_p < alpha) {
    summary$conclusion[idx] <- "INCONCLUSIVE: null AR residual autocorrelation"
  } else if (row$bootstrap_p_iid <= alpha) {
    summary$conclusion[idx] <- "EXPLORATORY: conditional one-shift candidate"
  } else {
    summary$conclusion[idx] <- "EXPLORATORY: no one-shift evidence (not proof of no effect)"
  }
}

save_table(manifest, "00_case_manifest")
save_table(summary, "01_dynamic_supF_bootstrap_summary")
if (length(boot_rows)) {
  save_table(do.call(rbind, boot_rows), "02_bootstrap_null_supF_draws")
}
save_table(do.call(rbind, location_rows), "03_break_candidate_F_profiles")

# Reproducibility / methodological interpretation. No Git commands invoked.
notes <- c(
  "Semiconductors II - Script 19 revised, Phase 3A methodological reassessment",
  paste("Execution date:", Sys.Date()),
  "Git state not queried, and no repository operations executed.",
  paste("Quarterly input MD5:", unname(tools::md5sum(input_file))),
  paste("+12m vintage input MD5:", unname(tools::md5sum(v12_file))),
  paste("R version:", getRversion()),
  paste("Bootstrap replications per evaluated case/method:", reps),
  paste("Base seed:", seed),
  "",
  "PREDECLARED PRIMARY: WSTS Worldwide (AR1); SIA China +12m 2015-2024 (AR1);",
  "log(China/AP Other) +12m 2015-2024 (AR0), on QUARTERLY CHANGES.",
  "Other orders, WSTS regions, AP Other, trimming, mixed vintages and 2025 are exploratory.",
  "2025 is excluded from the SIA fixed +12m primary window.",
  "Null AR model has stable seasonal quarterly intercepts and stable dynamics.",
  "Two primary bootstraps: centered IID residual and Rademacher wild sign-flip.",
  "Wild sign-flip helps sensitivity to changing volatility, not to serial dependence.",
  "All bootstrap draws re-search every admissible unknown break date.",
  "Primary p-values Holm-adjusted separately for IID and wild across exactly three tests.",
  "Missing primary p-value => NO Holm adjustment and NOT_EVALUABLE, not EXPLORATORY.",
  "Ljung-Box(8) residual rejection => INCONCLUSIVE conditional bootstrap inference.",
  "Long-run shift is delta/(1-sum(phi_j)) using ALTERNATIVE post-selection AR coefficients.",
  "Long-run estimate is DESCRIPTIVE, with neither post-selection CI nor causal interpretation.",
  "Long-run estimate omitted if alternative AR unstable or multiplier denominator <=0.05.",
  "Bootstrap checks only a single permanent conditional-mean shift; multiple/transitory breaks may be missed.",
  "With only 39 quarterly changes in the SIA primary sample, lack of rejection",
  "is NOT evidence of zero effect, nor does it rule out economically meaningful small changes.",
  "No power threshold was established: do NOT claim a specific detectable change size.",
  "No BIC dates in integrated log levels: see Script 17 limitations.",
  "No policy date imposed; no causal impact identified. Phase 3C remains unopened.",
  "",
  "SESSION:", capture.output(sessionInfo())
)
writeLines(notes, file.path(out_dir, "audit19_notes.txt"), useBytes = TRUE)
message("[SAVE] ", file.path(out_dir, "audit19_notes.txt"))
message("[DONE] Script 19 revised: prespecified primary results:")
print(summary[summary$is_primary_spec,
              c("case_id", "ar_order", "supF", "bootstrap_p_iid",
                "holm_p_iid_primary", "bootstrap_p_wild", "holm_p_wild_primary",
                "break_last_pre_quarter", "break_first_post_quarter",
                "long_run_shift_log_points_per_quarter",
                "null_residual_lb8_p", "conclusion")], row.names = FALSE)
message("[DONE] All diagnostics: ", out_dir)
