# Semiconductors II - R/18_quarterly_series_diagnostics.R
# Phase 3A methodological reassessment. NOT Phase 3C.
# No breakpoint estimation, Monte Carlo, policy dates or causal claims.
#
# Execute from repository root:
#   install.packages("tseries")  # if necessary
#   source("R/18_quarterly_series_diagnostics.R", encoding = "UTF-8")
#
# Measurement:
# WSTS quarterly sales = sum of 3 original monthly billings.
# SIA quarterly IMPLIED sales = 3 x the quarter-end trailing 3MMA.
# Do NOT sum the three overlapping 3MMA observations within a quarter.
# SIA PDF values are rounded. Its 2015 data come from retrospective 2016
# "Last Year" releases; 2016-2025 use first-release "Current" figures.

if (!requireNamespace("tseries", quietly = TRUE)) {
  stop("Missing tseries. Install with install.packages('tseries').")
}
inputs <- c(
  wsts = "data/processed/wsts_monthly_2010_2025.csv",
  wsts_3mma = "data/processed/wsts_3mma_2010_2025.csv",
  sia = "data/processed/sia_regional_3mma_2015_2025.csv"
)
if (any(!file.exists(inputs))) {
  stop("Missing input CSV(s): ",
       paste(inputs[!file.exists(inputs)], collapse = "; "),
       ". Run from repository root.")
}
out_dir <- "output/tables/phase3/audit18"
figure_dir <- "output/figures/phase3/audit18"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
save_csv <- function(data, filename, folder = out_dir) {
  path <- file.path(folder, filename)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  message("[SAVE] ", path)
}
read_dataset <- function(path, columns, label) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE)
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(label, ": missing columns: ",
                            paste(missing, collapse = ", "))
  x$date <- as.Date(x$date)
  if (anyNA(x$date)) stop(label, ": invalid dates")
  x
}
yr <- function(d) as.integer(format(d, "%Y"))
mo <- function(d) as.integer(format(d, "%m"))
qr <- function(d) (mo(d) - 1L) %/% 3L + 1L
qid <- function(d) paste0(yr(d), "Q", qr(d))
assert_months <- function(dates, start, end, label) {
  expect <- seq(as.Date(start), as.Date(end), by = "month")
  if (!identical(sort(dates), expect)) {
    stop(label, ": expected exactly ", length(expect),
         " consecutive and unique months")
  }
}
positive <- function(x, label) {
  if (!is.numeric(x) || any(!is.finite(x)) || any(x <= 0)) {
    stop(label, ": nonpositive or nonfinite values")
  }
}
message("[LOAD] Reading WSTS and SIA datasets")
w <- read_dataset(inputs[["wsts"]],
                  c("date", "region", "sales_usd_billions"), "WSTS monthly")
m3 <- read_dataset(inputs[["wsts_3mma"]],
                   c("date", "region", "sales_usd_billions"), "WSTS 3MMA")
sia <- read_dataset(inputs[["sia"]],
                    c("date", "china_3mma_usd_billions",
                      "asia_pacific_other_3mma_usd_billions",
                      "total_3mma_usd_billions", "source_method"), "SIA 3MMA")
regions <- c("America", "Europe", "Japan", "Asia Pacific", "Worldwide")
if (!setequal(unique(w$region), regions) ||
    !setequal(unique(m3$region), regions)) stop("WSTS region set differs")
for (region in regions) {
  assert_months(w$date[w$region == region], "2010-01-01", "2025-12-01",
                paste("WSTS", region))
  assert_months(m3$date[m3$region == region], "2010-01-01", "2025-12-01",
                paste("WSTS 3MMA", region))
}
assert_months(sia$date, "2015-01-01", "2025-12-01", "SIA")
positive(w$sales_usd_billions, "WSTS")
positive(m3$sales_usd_billions, "WSTS 3MMA")
positive(sia$china_3mma_usd_billions, "SIA China")
positive(sia$asia_pacific_other_3mma_usd_billions, "SIA AP Other")
positive(sia$total_3mma_usd_billions, "SIA total")
sia <- sia[order(sia$date), , drop = FALSE]

# 1. Aggregate ORIGINAL WSTS monthly sales, without overlapping windows.
message("[BUILD] WSTS quarterly totals 2010Q1-2025Q4")
rows <- list()
k <- 0L
for (region in regions) {
  d <- w[w$region == region, ]
  d <- d[order(d$date), ]
  groups <- split(seq_len(nrow(d)),
                  factor(qid(d$date), levels = unique(qid(d$date))))
  for (indices in groups) {
    if (length(indices) != 3L) stop("Incomplete WSTS calendar quarter")
    q <- d[indices, ]
    k <- k + 1L
    rows[[k]] <- data.frame(
      region = region, quarter_id = qid(q$date[1L]),
      year = yr(q$date[1L]), quarter = qr(q$date[1L]),
      quarter_end_date = q$date[3L], months = 3L,
      quarterly_sales_usd_billions = sum(q$sales_usd_billions)
    )
  }
}
wq <- do.call(rbind, rows)
row.names(wq) <- NULL
if (nrow(wq) != 320L) stop("Expected 5 regions x 64 WSTS quarters")
save_csv(wq, "wsts_quarterly_2010_2025.csv", folder = "data/processed")

# WSTS independent MA reconstruction: sum of 3 monthly values must match
# 3 times the workbook's quarter-end trailing 3MMA.
m3end <- m3[mo(m3$date) %% 3L == 0L, ]
m3end$quarter_id <- qid(m3end$date)
m3end <- m3end[, c("region", "quarter_id", "sales_usd_billions")]
names(m3end)[3L] <- "quarter_end_3mma_usd_billions"
valid <- merge(wq, m3end, by = c("region", "quarter_id"))
if (nrow(valid) != 320L) stop("Missing WSTS 3MMA quarter-end values")
valid$implied_quarterly_billions <- 3 * valid$quarter_end_3mma_usd_billions
valid$gap_billions <- valid$quarterly_sales_usd_billions -
  valid$implied_quarterly_billions
if (max(abs(valid$gap_billions)) > 1e-5) {
  stop("WSTS quarter-end 3MMA does not reconstruct raw quarterly totals")
}
save_csv(valid, "wsts_quarter_end_3mma_validation.csv")

# Cross-check WSTS Worldwide vs sum of 4 geographic regions.
w4 <- wq[wq$region != "Worldwide", ]
wtotal <- aggregate(w4$quarterly_sales_usd_billions,
                    by = list(quarter_id = w4$quarter_id), FUN = sum)
names(wtotal)[2L] <- "sum_four_regions"
wrld <- wq[wq$region == "Worldwide", ]
wtotal <- merge(wtotal, wrld[, c("quarter_id",
                                "quarterly_sales_usd_billions")],
                by = "quarter_id")
wtotal$gap_billions <- wtotal$quarterly_sales_usd_billions -
  wtotal$sum_four_regions
if (nrow(wtotal) != 64L || max(abs(wtotal$gap_billions)) > 1e-4) {
  stop("Quarterly WSTS regional accounting check failed")
}
save_csv(wtotal, "wsts_quarterly_regional_accounting.csv")

# 2. SIA: quarter-end 3MMA exactly covers its three calendar months
# in a trailing-average definition, but published figures are rounded.
message("[BUILD] SIA implied quarterly totals 2015Q1-2025Q4")
s <- sia[mo(sia$date) %% 3L == 0L, ]
if (nrow(s) != 44L) stop("Expected 44 SIA quarter-end observations")
sq <- data.frame(
  quarter_id = qid(s$date), year = yr(s$date), quarter = qr(s$date),
  quarter_end_date = s$date, vintage_method = s$source_method,
  china_billions = 3 * s$china_3mma_usd_billions,
  ap_other_billions = 3 * s$asia_pacific_other_3mma_usd_billions,
  sia_total_billions = 3 * s$total_3mma_usd_billions
)
sq$ap_billions <- sq$china_billions + sq$ap_other_billions
sq$china_share_ap <- sq$china_billions / sq$ap_billions
sq$log_china_over_ap_other <- log(sq$china_billions / sq$ap_other_billions)
save_csv(sq, "sia_quarterly_implied_2015_2025.csv",
         folder = "data/processed")

# SIA/WSTS Asia-Pacific comparison is NOT an accounting identity.
ap <- wq[wq$region == "Asia Pacific",
         c("quarter_id", "quarterly_sales_usd_billions")]
names(ap)[2L] <- "wsts_ap_billions"
cross <- merge(sq[, c("quarter_id", "year", "vintage_method",
                      "ap_billions")], ap, by = "quarter_id")
if (nrow(cross) != 44L) stop("Missing SIA/WSTS comparison quarters")
cross$gap_billions <- cross$ap_billions - cross$wsts_ap_billions
cross$abs_gap_percent <- 100 * abs(cross$gap_billions) / cross$wsts_ap_billions
save_csv(cross, "sia_wsts_ap_quarterly_comparison.csv")

# Compare independently published SIA Total with sum of 5 published regions.
reg_cols <- c("america_3mma_usd_billions", "europe_3mma_usd_billions",
              "japan_3mma_usd_billions", "china_3mma_usd_billions",
              "asia_pacific_other_3mma_usd_billions")
if (all(reg_cols %in% names(s))) {
  internal <- data.frame(
    quarter_id = qid(s$date),
    published_total = 3 * s$total_3mma_usd_billions,
    sum_five_regions = 3 * rowSums(s[, reg_cols])
  )
  internal$gap_billions <- internal$published_total - internal$sum_five_regions
  save_csv(internal, "sia_quarterly_internal_validation.csv")
}

# 3. Quarterly transformations and time-series diagnostics.
# ADF and PP: H0 unit root. KPSS: H0 stationary around level or trend.
# The tseries ADF/PP deterministic specifications and short quarterly
# samples need caution. Do not infer "definitively I(1)" from these tests.
message("[TEST] ADF / KPSS / PP and quarterly dependence")
series <- list()
add <- function(label, source, kind, dates, vals) {
  series[[label]] <<- list(label = label, source = source,
                          kind = kind, dates = dates, x = as.numeric(vals))
}
for (region in regions) {
  z <- wq[wq$region == region, ]
  z <- z[order(z$quarter_end_date), ]
  add(paste0("WSTS ", region), "WSTS", "sales",
      z$quarter_end_date, z$quarterly_sales_usd_billions)
}
add("SIA China", "SIA", "sales", sq$quarter_end_date, sq$china_billions)
add("SIA AP Other", "SIA", "sales", sq$quarter_end_date, sq$ap_other_billions)
add("SIA AP total", "SIA", "sales", sq$quarter_end_date, sq$ap_billions)
add("SIA China share", "SIA", "share", sq$quarter_end_date, sq$china_share_ap)
add("SIA log China/AP Other", "SIA", "log_ratio",
    sq$quarter_end_date, sq$log_china_over_ap_other)

# Capture p-value table-bound warnings without hiding errors.
run_test <- function(f) {
  warn <- character()
  ans <- tryCatch(
    withCallingHandlers(f(), warning = function(e) {
      warn <<- unique(c(warn, conditionMessage(e)))
      invokeRestart("muffleWarning")
    }),
    error = function(e) e
  )
  if (inherits(ans, "error")) {
    return(list(stat = NA_real_, p = NA_real_,
                note = paste("ERROR:", conditionMessage(ans))))
  }
  list(stat = unname(ans$statistic), p = ans$p.value,
       note = paste(warn, collapse = " | "))
}
stat_rows <- list()
depend_rows <- list()
acf_rows <- list()
demean_rows <- list()
season_rows <- list()
st_i <- 0L
dep_i <- 0L
acf_i <- 0L
dem_i <- 0L
sea_i <- 0L
for (ser in series) {
  v <- ser$x
  lev <- if (ser$kind == "sales") log(v) else v
  n <- length(lev)
  if (n < 40L || any(!is.finite(lev))) stop("Invalid series: ", ser$label)
  level_label <- if (ser$kind == "sales") "log_level" else
    if (ser$kind == "share") "share_level" else "log_ratio_level"
  diff_label <- if (ser$kind == "sales") "log_difference" else "first_difference"
  dif <- diff(lev)
  dif_dates <- ser$dates[-1L]
  pos <- qr(dif_dates)
  pos_mean <- tapply(dif, pos, mean)
  adjusted <- dif - as.numeric(pos_mean[as.character(pos)])
  sea <- lev[5:n] - lev[1:(n - 4L)]
  dem_i <- dem_i + 1L
  demean_rows[[dem_i]] <- data.frame(
    series = ser$label, quarter_id = qid(dif_dates),
    raw_difference = dif, quarter_demeaned_difference = adjusted
  )

  transformations <- list(
    list(name = level_label, x = lev, null = "Trend"),
    list(name = diff_label, x = dif, null = "Level"),
    list(name = "seasonal_difference_4", x = sea, null = "Level")
  )
  for (tr in transformations) {
    z <- as.numeric(tr$x)
    adf_k <- floor((length(z) - 1L)^(1/3))
    adf <- run_test(function() tseries::adf.test(z, k = adf_k,
                                                  alternative = "stationary"))
    kpss <- run_test(function() tseries::kpss.test(
      z, null = tr$null, lshort = TRUE))
    pp <- run_test(function() tseries::pp.test(
      z, lshort = TRUE, alternative = "stationary"))
    st_i <- st_i + 1L
    stat_rows[[st_i]] <- data.frame(
      source = ser$source, series = ser$label,
      transformation = tr$name, n = length(z),
      adf_k = adf_k, adf_stat = adf$stat, adf_p = adf$p,
      adf_note = adf$note,
      kpss_null = tr$null, kpss_stat = kpss$stat,
      kpss_p = kpss$p, kpss_note = kpss$note,
      pp_stat = pp$stat, pp_p = pp$p, pp_note = pp$note
    )
  }

  for (label in c("raw_difference", "quarter_demeaned_difference",
                  "seasonal_difference_4")) {
    z <- switch(label, raw_difference = dif,
                quarter_demeaned_difference = adjusted,
                seasonal_difference_4 = sea)
    ac <- as.numeric(stats::acf(z, lag.max = 8, plot = FALSE)$acf)
    pc <- as.numeric(stats::pacf(z, lag.max = 8, plot = FALSE)$acf)
    lb4 <- stats::Box.test(z, lag = 4, type = "Ljung-Box")
    lb8 <- stats::Box.test(z, lag = 8, type = "Ljung-Box")
    dep_i <- dep_i + 1L
    depend_rows[[dep_i]] <- data.frame(
      source = ser$source, series = ser$label, transformation = label,
      n = length(z), sd = stats::sd(z),
      acf1 = ac[2L], acf2 = ac[3L], acf4 = ac[5L], acf8 = ac[9L],
      pacf1 = pc[1L], pacf2 = pc[2L], pacf4 = pc[4L],
      lb4_stat = unname(lb4$statistic), lb4_p = lb4$p.value,
      lb8_stat = unname(lb8$statistic), lb8_p = lb8$p.value
    )
    for (lag in 1:8) {
      acf_i <- acf_i + 1L
      acf_rows[[acf_i]] <- data.frame(
        series = ser$label, transformation = label,
        lag = lag, acf = ac[lag + 1L], pacf = pc[lag]
      )
    }
  }

  # Descriptive quarter-of-year seasonality, normalized by each year's mean.
  if (ser$kind == "sales") {
    yy <- yr(ser$dates)
    qpos <- qr(ser$dates)
    baseline <- as.numeric(tapply(v, yy, mean)[as.character(yy)])
    ratio <- v / baseline
    for (q in 1:4) {
      sea_i <- sea_i + 1L
      season_rows[[sea_i]] <- data.frame(
        series = ser$label, quarter = q,
        n_years = sum(qpos == q),
        mean_ratio_to_annual_quarter_mean = mean(ratio[qpos == q]),
        sd_ratio = stats::sd(ratio[qpos == q])
      )
    }
  }
}
stationarity <- do.call(rbind, stat_rows)
dependence <- do.call(rbind, depend_rows)
acfs <- do.call(rbind, acf_rows)
save_csv(stationarity, "quarterly_stationarity_tests.csv")
save_csv(dependence, "quarterly_serial_dependence.csv")
save_csv(acfs, "quarterly_acf_pacf.csv")
save_csv(do.call(rbind, demean_rows), "quarterly_demeaned_differences.csv")
save_csv(do.call(rbind, season_rows), "quarterly_seasonality_profile.csv")

summary_rows <- lapply(names(series), function(key) {
  d <- stationarity[stationarity$series == key, ]
  g <- d[d$transformation %in% c("log_level", "share_level",
                                 "log_ratio_level"), ]
  f <- d[d$transformation %in% c("log_difference", "first_difference"), ]
  v <- dependence[dependence$series == key &
                    dependence$transformation == "raw_difference", ]
  data.frame(
    series = key, n_quarters = length(series[[key]]$x),
    adf_level_p = g$adf_p[1], kpss_level_p = g$kpss_p[1],
    pp_level_p = g$pp_p[1],
    adf_diff_p = f$adf_p[1], kpss_diff_p = f$kpss_p[1],
    pp_diff_p = f$pp_p[1],
    acf_diff_lag1 = v$acf1[1], acf_diff_lag4 = v$acf4[1],
    lb4_diff_p = v$lb4_p[1], lb8_diff_p = v$lb8_p[1]
  )
})
summary_table <- do.call(rbind, summary_rows)
save_csv(summary_table, "quarterly_diagnostic_summary.csv")

message("[FIGURES] Generating two descriptive figures")
world <- wq[wq$region == "Worldwide", ]
world <- world[order(world$quarter_end_date), ]
grDevices::png(file.path(figure_dir, "wsts_worldwide_quarterly.png"),
                width = 1100, height = 800, res = 125)
old <- graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
graphics::plot(world$quarter_end_date, world$quarterly_sales_usd_billions,
               type = "l", lwd = 2, xlab = "Quarter",
               ylab = "USD billions",
               main = "WSTS Worldwide: sum of original monthly sales")
graphics::grid()
graphics::plot(world$quarter_end_date[-1L],
               diff(log(world$quarterly_sales_usd_billions)),
               type = "l", lwd = 2, xlab = "Quarter",
               ylab = "Log difference",
               main = "WSTS Worldwide: quarterly log growth")
graphics::abline(h = 0, lty = 3)
graphics::grid()
graphics::par(old)
grDevices::dev.off()

grDevices::png(file.path(figure_dir, "sia_china_vs_ap_other_quarterly.png"),
                width = 1100, height = 800, res = 125)
old <- graphics::par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
graphics::plot(sq$quarter_end_date, sq$china_billions, type = "l",
               col = "#B22222", lwd = 2,
               xlab = "Quarter", ylab = "Implied USD billions",
               main = "SIA: implied non-overlapping quarterly billings")
graphics::lines(sq$quarter_end_date, sq$ap_other_billions,
                col = "#21618C", lwd = 2)
graphics::legend("topleft", c("China", "Asia Pacific Other"),
                 col = c("#B22222", "#21618C"), lty = 1, lwd = 2, bty = "n")
graphics::grid()
graphics::plot(sq$quarter_end_date, 100 * sq$china_share_ap,
               type = "l", col = "#B22222", lwd = 2,
               xlab = "Quarter", ylab = "Percent",
               main = "SIA: China's share within Asia Pacific")
graphics::grid()
graphics::par(old)
grDevices::dev.off()

# Reproducibility and scope notes.
git_commit <- tryCatch(suppressWarnings(
  system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)),
  error = function(e) NA_character_
)
if (length(git_commit) != 1L || !grepl("^[0-9a-f]{40}$", git_commit)) {
  git_commit <- "unknown"
}
md5 <- tools::md5sum(inputs)
notes <- c(
  "Semiconductors II — Script 18 quarterly diagnostics",
  paste("Date:", Sys.Date()), paste("Git commit:", git_commit),
  paste("R:", getRversion()),
  paste("tseries:", as.character(utils::packageVersion("tseries"))),
  paste(names(md5), as.character(md5), sep = " MD5 = "),
  "",
  "WSTS: 2010Q1-2025Q4, 64 quarters, sums of original monthly billings.",
  "SIA: 2015Q1-2025Q4, 44 implied quarters = 3 x quarter-end 3MMA.",
  "SIA PDF 3MMA is rounded, and 2015 uses a retrospective 2016 vintage.",
  "Do not sum monthly overlapping 3MMA values.",
  "WSTS historical and SIA first-release figures are not identical vintages.",
  "Quarter of year demeaning uses the complete sample and is DESCRIPTIVE only.",
  "Unit-root tests may have low power with 44 or 64 quarterly observations.",
  "ADF/PP null is unit root; KPSS null is (trend-)stationarity.",
  "ADF/PP options use tseries conventions; see saved warning columns.",
  "Quarterly seasonal differencing may overdifference: robustness only.",
  "Serial dependence tests here are on observed changes, not model residuals.",
  "Structural breaks can affect stationarity-test conclusions.",
  "No structural break estimation, policy modelling or Monte Carlo in Script 18.",
  "",
  "R session information:"
)
writeLines(c(notes, capture.output(sessionInfo())),
           file.path(out_dir, "audit18_notes.txt"), useBytes = TRUE)
message("[DONE] Script 18 complete. Key quarterly diagnostics:")
print(summary_table)
message("[DONE] Files in ", out_dir)
