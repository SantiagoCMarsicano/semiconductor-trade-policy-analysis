# Semiconductors II
# 09_time_series_diagnostics.R
#
# Purpose:
#   Diagnose the statistical properties of the SIA/WSTS regional
#   3-month moving-average series before structural-break or policy
#   intervention models are estimated.
#
# Reproducibility:
#   Input  : data/processed/sia_regional_3mma_2015_2025.csv
#   Figures: output/figures/3mma_diagnostics/
#   Tables : output/tables/
#
# Statistical conventions:
#   - ADF H0  : unit root / non-stationary.
#   - KPSS H0 : stationary around the specified level or trend.
#   - Ljung-Box H0: no autocorrelation through the selected lag.
#   - Sales series are transformed with first log differences.
#   - China share of Asia-Pacific is transformed with first differences.
#
# Important:
#   These are smoothed monthly observations. Because a 3-month moving
#   average mechanically overlaps adjacent observations, some serial
#   correlation is expected by construction. This script is diagnostic;
#   it does not introduce policy dates or make causal claims.

# -----------------------------------------------------------------------------
# 1. Packages
# -----------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "tibble",
  "purrr",
  "tseries",
  "ggplot2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them first with:\ninstall.packages(c(\"",
      paste(missing_packages, collapse = "\", \""),
      "\"))"
    )
  )
}

library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(tseries)
library(ggplot2)

# -----------------------------------------------------------------------------
# 2. Paths and expected schema
# -----------------------------------------------------------------------------

data_path <- file.path(
  "data",
  "processed",
  "sia_regional_3mma_2015_2025.csv"
)

figures_dir <- file.path(
  "output",
  "figures",
  "3mma_diagnostics"
)

tables_dir <- file.path(
  "output",
  "tables"
)

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Input dataset not found: ",
      data_path,
      "\nRun R/06_build_sia_regional_3mma.R first."
    )
  )
}

required_columns <- c(
  "date",
  "america_3mma_usd_billions",
  "europe_3mma_usd_billions",
  "japan_3mma_usd_billions",
  "china_3mma_usd_billions",
  "asia_pacific_other_3mma_usd_billions",
  "total_3mma_usd_billions",
  "china_share_asia_pacific"
)

# -----------------------------------------------------------------------------
# 3. Read and validate the processed dataset
# -----------------------------------------------------------------------------

data <- read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

missing_columns <- setdiff(required_columns, names(data))

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The processed dataset does not match the current schema. Missing: ",
      paste(missing_columns, collapse = ", "),
      "\nRe-run R/06_build_sia_regional_3mma.R before running this script."
    )
  )
}

expected_dates <- seq(
  as.Date("2015-01-01"),
  as.Date("2025-12-01"),
  by = "month"
)

if (nrow(data) != length(expected_dates)) {
  stop(
    paste0(
      "Expected ", length(expected_dates),
      " monthly observations, found ", nrow(data), "."
    )
  )
}

if (n_distinct(data$date) != length(expected_dates)) {
  stop("The dataset contains duplicate or missing monthly dates.")
}

if (!identical(data$date, expected_dates)) {
  stop("The date sequence is not complete from 2015-01 through 2025-12.")
}

if (anyNA(data[required_columns])) {
  stop("Missing values detected in required diagnostic variables.")
}

sales_columns <- c(
  "america_3mma_usd_billions",
  "europe_3mma_usd_billions",
  "japan_3mma_usd_billions",
  "china_3mma_usd_billions",
  "asia_pacific_other_3mma_usd_billions",
  "total_3mma_usd_billions"
)

if (any(vapply(data[sales_columns], function(x) any(x <= 0), logical(1)))) {
  stop("Sales series must be strictly positive before log transformation.")
}

if (any(data$china_share_asia_pacific < 0 | data$china_share_asia_pacific > 1)) {
  stop("china_share_asia_pacific must be bounded between 0 and 1.")
}

# -----------------------------------------------------------------------------
# 4. Series definitions
# -----------------------------------------------------------------------------

series_map <- c(
  "America" = "america_3mma_usd_billions",
  "Europe" = "europe_3mma_usd_billions",
  "Japan" = "japan_3mma_usd_billions",
  "China" = "china_3mma_usd_billions",
  "Asia Pacific/All Other" = "asia_pacific_other_3mma_usd_billions",
  "Total" = "total_3mma_usd_billions",
  "China share of Asia Pacific" = "china_share_asia_pacific"
)

region_colors <- c(
  "America" = "#1F77B4",
  "China" = "#D62728",
  "Asia Pacific/All Other" = "#FF7F0E",
  "Europe" = "#2CA02C",
  "Japan" = "#9467BD",
  "Total" = "#111111",
  "China share of Asia Pacific" = "#D62728"
)

safe_names <- c(
  "America" = "america",
  "China" = "china",
  "Total" = "total",
  "China share of Asia Pacific" = "china_share_of_asia_pacific"
)

# Remove only deprecated figure names created by older versions of this script.
legacy_files <- file.path(
  figures_dir,
  c("acf_americas.png", "pacf_americas.png")
)

legacy_files <- legacy_files[file.exists(legacy_files)]

if (length(legacy_files) > 0) {
  unlink(legacy_files)
}

# -----------------------------------------------------------------------------
# 5. Safe statistical-test wrappers
# -----------------------------------------------------------------------------

safe_adf <- function(x) {
  x <- x[is.finite(x)]

  result <- tryCatch(
    suppressWarnings(
      adf.test(x, alternative = "stationary")
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

safe_kpss <- function(x, null_type = "Trend") {
  x <- x[is.finite(x)]

  result <- tryCatch(
    suppressWarnings(
      kpss.test(x, null = null_type)
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

safe_ljung_box <- function(x, lag_value) {
  x <- x[is.finite(x)]

  result <- tryCatch(
    Box.test(
      x,
      lag = lag_value,
      type = "Ljung-Box"
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

# -----------------------------------------------------------------------------
# 6. Transform series
# -----------------------------------------------------------------------------

transformed_series <- imap_dfr(
  series_map,
  function(column_name, series_name) {
    x <- data[[column_name]]

    if (series_name == "China share of Asia Pacific") {
      tibble(
        date = data$date,
        series = series_name,
        level = x,
        log_level = NA_real_,
        first_difference = c(NA_real_, diff(x)),
        log_difference = NA_real_
      )
    } else {
      tibble(
        date = data$date,
        series = series_name,
        level = x,
        log_level = log(x),
        first_difference = c(NA_real_, diff(x)),
        log_difference = c(NA_real_, diff(log(x)))
      )
    }
  }
)

# -----------------------------------------------------------------------------
# 7. Stationarity tests
# -----------------------------------------------------------------------------

stationarity_results <- imap_dfr(
  series_map,
  function(column_name, series_name) {
    x <- data[[column_name]]

    level_adf <- safe_adf(x)
    level_kpss <- safe_kpss(x, "Trend")

    if (series_name == "China share of Asia Pacific") {
      transformed_x <- diff(x)
      transformation_name <- "first_difference"
    } else {
      transformed_x <- diff(log(x))
      transformation_name <- "log_difference"
    }

    transformed_adf <- safe_adf(transformed_x)
    transformed_kpss <- safe_kpss(transformed_x, "Level")

    bind_rows(
      tibble(
        series = series_name,
        transformation = "level",
        adf_statistic = level_adf$statistic,
        adf_p_value = level_adf$p_value,
        kpss_statistic = level_kpss$statistic,
        kpss_p_value = level_kpss$p_value
      ),
      tibble(
        series = series_name,
        transformation = transformation_name,
        adf_statistic = transformed_adf$statistic,
        adf_p_value = transformed_adf$p_value,
        kpss_statistic = transformed_kpss$statistic,
        kpss_p_value = transformed_kpss$p_value
      )
    )
  }
)

# -----------------------------------------------------------------------------
# 8. Ljung-Box serial-correlation tests
# -----------------------------------------------------------------------------

serial_correlation_results <- imap_dfr(
  series_map,
  function(column_name, series_name) {
    x <- data[[column_name]]

    if (series_name == "China share of Asia Pacific") {
      transformed_x <- diff(x)
      transformation_name <- "first_difference"
    } else {
      transformed_x <- diff(log(x))
      transformation_name <- "log_difference"
    }

    lb_6 <- safe_ljung_box(transformed_x, 6)
    lb_12 <- safe_ljung_box(transformed_x, 12)
    lb_24 <- safe_ljung_box(transformed_x, 24)

    tibble(
      series = series_name,
      transformation = transformation_name,
      ljung_box_6_statistic = lb_6$statistic,
      ljung_box_6_p_value = lb_6$p_value,
      ljung_box_12_statistic = lb_12$statistic,
      ljung_box_12_p_value = lb_12$p_value,
      ljung_box_24_statistic = lb_24$statistic,
      ljung_box_24_p_value = lb_24$p_value
    )
  }
)

# -----------------------------------------------------------------------------
# 9. ACF / PACF figures
# -----------------------------------------------------------------------------

build_correlation_plot <- function(
  x,
  series_name,
  plot_type = "acf",
  max_lag = 24
) {
  x <- x[is.finite(x)]

  if (plot_type == "acf") {
    correlation_object <- acf(
      x,
      lag.max = max_lag,
      plot = FALSE
    )

    plot_data <- tibble(
      lag = as.numeric(correlation_object$lag),
      correlation = as.numeric(correlation_object$acf)
    ) %>%
      filter(lag > 0)

    plot_title <- paste("ACF:", series_name)
    y_label <- "Autocorrelation"
  } else {
    correlation_object <- pacf(
      x,
      lag.max = max_lag,
      plot = FALSE
    )

    plot_data <- tibble(
      lag = seq_along(correlation_object$acf),
      correlation = as.numeric(correlation_object$acf)
    )

    plot_title <- paste("PACF:", series_name)
    y_label <- "Partial autocorrelation"
  }

  confidence_limit <- 1.96 / sqrt(length(x))

  ggplot(
    plot_data,
    aes(x = lag, y = correlation)
  ) +
    geom_hline(
      yintercept = 0,
      color = "gray50"
    ) +
    geom_hline(
      yintercept = c(-confidence_limit, confidence_limit),
      color = "gray70"
    ) +
    geom_segment(
      aes(xend = lag, y = 0, yend = correlation),
      linewidth = 0.8,
      color = region_colors[[series_name]]
    ) +
    geom_point(
      size = 2,
      color = region_colors[[series_name]]
    ) +
    scale_x_continuous(
      breaks = seq(0, max_lag, by = 3)
    ) +
    labs(
      title = plot_title,
      subtitle = "Transformed smoothed monthly series",
      x = "Lag (months)",
      y = y_label,
      caption = "Gray reference lines show approximate 95% bounds."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
}

diagnostic_series <- c(
  "America",
  "China",
  "Total",
  "China share of Asia Pacific"
)

for (series_name in diagnostic_series) {
  column_name <- series_map[[series_name]]
  x <- data[[column_name]]

  if (series_name == "China share of Asia Pacific") {
    transformed_x <- diff(x)
  } else {
    transformed_x <- diff(log(x))
  }

  safe_name <- safe_names[[series_name]]

  acf_plot <- build_correlation_plot(
    transformed_x,
    series_name,
    "acf",
    24
  )

  pacf_plot <- build_correlation_plot(
    transformed_x,
    series_name,
    "pacf",
    24
  )

  ggsave(
    file.path(figures_dir, paste0("acf_", safe_name, ".png")),
    acf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(figures_dir, paste0("pacf_", safe_name, ".png")),
    pacf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# -----------------------------------------------------------------------------
# 10. Export reproducible tables
# -----------------------------------------------------------------------------

stationarity_path <- file.path(
  tables_dir,
  "time_series_stationarity_tests.csv"
)

serial_path <- file.path(
  tables_dir,
  "time_series_serial_correlation_tests.csv"
)

transformed_path <- file.path(
  tables_dir,
  "time_series_transformed_series.csv"
)

write_csv(stationarity_results, stationarity_path)
write_csv(serial_correlation_results, serial_path)
write_csv(transformed_series, transformed_path)

# -----------------------------------------------------------------------------
# 11. Console audit summary
# -----------------------------------------------------------------------------

cat("\n=========================================\n")
cat("SIA/WSTS 3-MONTH SERIES DIAGNOSTICS\n")
cat("=========================================\n\n")

cat("Input:       ", data_path, "\n", sep = "")
cat("Observations:", nrow(data), "\n")
cat(
  "Period:      ",
  format(min(data$date), "%Y-%m"),
  " to ",
  format(max(data$date), "%Y-%m"),
  "\n\n",
  sep = ""
)

cat("STATIONARITY TESTS\n")
cat("ADF  H0: unit root / non-stationary.\n")
cat("KPSS H0: stationary around the specified level/trend.\n\n")
print(stationarity_results, n = Inf)

cat("\nSERIAL CORRELATION AFTER TRANSFORMATION\n")
cat("Ljung-Box H0: no autocorrelation through the selected lag.\n\n")
print(serial_correlation_results, n = Inf)

cat("\nFiles created:\n")
cat("- ", stationarity_path, "\n", sep = "")
cat("- ", serial_path, "\n", sep = "")
cat("- ", transformed_path, "\n", sep = "")

cat("\nDiagnostic figures:\n")
for (series_name in diagnostic_series) {
  safe_name <- safe_names[[series_name]]
  cat("- ", file.path(figures_dir, paste0("acf_", safe_name, ".png")), "\n", sep = "")
  cat("- ", file.path(figures_dir, paste0("pacf_", safe_name, ".png")), "\n", sep = "")
}

cat("\nInterpretation reminder:\n")
cat(
  paste(
    "The 3-month moving-average construction mechanically overlaps",
    "adjacent months. Significant serial correlation must therefore be",
    "modelled explicitly, but it is not by itself evidence of a policy",
    "effect or of model misspecification.\n"
  )
)
