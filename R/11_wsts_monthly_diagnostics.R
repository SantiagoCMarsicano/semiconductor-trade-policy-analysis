# Semiconductors II
# 11_wsts_monthly_diagnostics.R
#
# Purpose:
#   Econometric diagnostics for the original WSTS monthly series (2010-2025).
#   This evaluates stationarity, serial correlation and temporal dependence
#   before structural-break or intervention models.
#
# Tests:
#   - ADF: H0 = unit root / non-stationary
#   - KPSS: H0 = stationary around level/trend
#   - Phillips-Perron: H0 = unit root / non-stationary
#   - Ljung-Box: H0 = no autocorrelation up to lag k
#
# Important:
#   These are original monthly observations, not 3MMA-smoothed series.

required_packages <- c(
  "dplyr", "readr", "tibble", "purrr",
  "tseries", "ggplot2"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(tseries)
library(ggplot2)

data_path <- file.path(
  "data", "processed",
  "wsts_monthly_2010_2025.csv"
)

figures_dir <- file.path(
  "output", "figures",
  "wsts_monthly_diagnostics"
)

tables_dir <- file.path(
  "output", "tables"
)

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Monthly WSTS dataset not found: ",
      data_path,
      "\nRun R/02_build_wsts_base.R first."
    )
  )
}

data <- read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(
    region,
    date
  )

expected_regions <- c(
  "America",
  "Europe",
  "Japan",
  "Asia Pacific",
  "Worldwide"
)

region_colors <- c(
  "America" = "#1F77B4",
  "Europe" = "#2CA02C",
  "Japan" = "#9467BD",
  "Asia Pacific" = "#FF7F0E",
  "Worldwide" = "#111111"
)

safe_adf <- function(x) {
  x <- x[is.finite(x)]

  result <- tryCatch(
    suppressWarnings(
      adf.test(
        x,
        alternative = "stationary"
      )
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(
      tibble(
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

safe_kpss <- function(
  x,
  null_type = "Trend"
) {
  x <- x[is.finite(x)]

  result <- tryCatch(
    suppressWarnings(
      kpss.test(
        x,
        null = null_type
      )
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(
      tibble(
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

safe_pp <- function(x) {
  x <- x[is.finite(x)]

  result <- tryCatch(
    suppressWarnings(
      pp.test(
        x,
        alternative = "stationary"
      )
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(
      tibble(
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

safe_ljung_box <- function(
  x,
  lag_value
) {
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
    return(
      tibble(
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }

  tibble(
    statistic = unname(result$statistic),
    p_value = result$p.value
  )
}

transformed_data <- data %>%
  group_by(region) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    log_level =
      log(sales_usd_billions),
    log_difference =
      log_level -
      lag(log_level, 1),
    seasonal_log_difference =
      log_level -
      lag(log_level, 12),
    yoy_growth_pct = 100 * (
      sales_usd_billions /
        lag(sales_usd_billions, 12) -
        1
    )
  ) %>%
  ungroup()

stationarity_results <- map_dfr(
  expected_regions,
  function(region_name) {

    region_data <- transformed_data %>%
      filter(
        region == region_name
      )

    test_one <- function(
      x,
      transformation,
      kpss_null
    ) {
      adf_result <- safe_adf(x)
      kpss_result <- safe_kpss(
        x,
        kpss_null
      )
      pp_result <- safe_pp(x)

      tibble(
        region = region_name,
        transformation = transformation,
        adf_statistic =
          adf_result$statistic,
        adf_p_value =
          adf_result$p_value,
        kpss_statistic =
          kpss_result$statistic,
        kpss_p_value =
          kpss_result$p_value,
        pp_statistic =
          pp_result$statistic,
        pp_p_value =
          pp_result$p_value
      )
    }

    bind_rows(
      test_one(
        region_data$sales_usd_billions,
        "level",
        "Trend"
      ),
      test_one(
        region_data$log_difference,
        "log_difference",
        "Level"
      ),
      test_one(
        region_data$seasonal_log_difference,
        "seasonal_log_difference_12",
        "Level"
      )
    )
  }
)

serial_correlation_results <- map_dfr(
  expected_regions,
  function(region_name) {

    x <- transformed_data %>%
      filter(
        region == region_name
      ) %>%
      pull(log_difference)

    lb_1 <- safe_ljung_box(x, 1)
    lb_3 <- safe_ljung_box(x, 3)
    lb_6 <- safe_ljung_box(x, 6)
    lb_12 <- safe_ljung_box(x, 12)
    lb_24 <- safe_ljung_box(x, 24)

    tibble(
      region = region_name,
      transformation = "log_difference",
      ljung_box_1_statistic =
        lb_1$statistic,
      ljung_box_1_p_value =
        lb_1$p_value,
      ljung_box_3_statistic =
        lb_3$statistic,
      ljung_box_3_p_value =
        lb_3$p_value,
      ljung_box_6_statistic =
        lb_6$statistic,
      ljung_box_6_p_value =
        lb_6$p_value,
      ljung_box_12_statistic =
        lb_12$statistic,
      ljung_box_12_p_value =
        lb_12$p_value,
      ljung_box_24_statistic =
        lb_24$statistic,
      ljung_box_24_p_value =
        lb_24$p_value
    )
  }
)

acf_summary <- map_dfr(
  expected_regions,
  function(region_name) {

    x <- transformed_data %>%
      filter(
        region == region_name
      ) %>%
      pull(log_difference)

    x <- x[is.finite(x)]

    acf_object <- acf(
      x,
      lag.max = 24,
      plot = FALSE
    )

    acf_values <- as.numeric(
      acf_object$acf
    )

    tibble(
      region = region_name,
      acf_lag_1 = acf_values[2],
      acf_lag_2 = acf_values[3],
      acf_lag_3 = acf_values[4],
      acf_lag_6 = acf_values[7],
      acf_lag_12 = acf_values[13],
      acf_lag_24 = acf_values[25]
    )
  }
)

build_correlation_plot <- function(
  x,
  region_name,
  plot_type = "acf",
  max_lag = 36
) {
  x <- x[is.finite(x)]

  if (plot_type == "acf") {
    object <- acf(
      x,
      lag.max = max_lag,
      plot = FALSE
    )

    plot_data <- tibble(
      lag = as.numeric(object$lag),
      correlation = as.numeric(object$acf)
    ) %>%
      filter(lag > 0)

    title_text <- paste(
      "ACF:",
      region_name
    )

    y_label <- "Autocorrelation"

  } else {
    object <- pacf(
      x,
      lag.max = max_lag,
      plot = FALSE
    )

    plot_data <- tibble(
      lag = seq_along(object$acf),
      correlation = as.numeric(object$acf)
    )

    title_text <- paste(
      "PACF:",
      region_name
    )

    y_label <- "Partial autocorrelation"
  }

  confidence_limit <-
    1.96 /
    sqrt(length(x))

  ggplot(
    plot_data,
    aes(
      x = lag,
      y = correlation
    )
  ) +
    geom_hline(
      yintercept = 0,
      color = "gray50"
    ) +
    geom_hline(
      yintercept = c(
        -confidence_limit,
        confidence_limit
      ),
      color = "gray75"
    ) +
    geom_segment(
      aes(
        xend = lag,
        y = 0,
        yend = correlation
      ),
      linewidth = 0.75,
      color = region_colors[[region_name]]
    ) +
    geom_point(
      size = 1.7,
      color = region_colors[[region_name]]
    ) +
    scale_x_continuous(
      breaks = seq(
        0,
        max_lag,
        by = 3
      )
    ) +
    labs(
      title = title_text,
      subtitle =
        "Original WSTS monthly data after log differencing",
      x = "Lag (months)",
      y = y_label,
      caption =
        "Lag 12 is especially relevant for possible annual seasonality."
    ) +
    theme_minimal(
      base_size = 12
    ) +
    theme(
      panel.grid.minor =
        element_blank(),
      plot.title =
        element_text(
          face = "bold"
        )
    )
}

for (
  region_name in expected_regions
) {
  x <- transformed_data %>%
    filter(
      region == region_name
    ) %>%
    pull(log_difference)

  safe_name <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(region_name)
  )

  acf_plot <- build_correlation_plot(
    x,
    region_name,
    "acf",
    36
  )

  pacf_plot <- build_correlation_plot(
    x,
    region_name,
    "pacf",
    36
  )

  ggsave(
    file.path(
      figures_dir,
      paste0(
        "acf_",
        safe_name,
        ".png"
      )
    ),
    acf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(
      figures_dir,
      paste0(
        "pacf_",
        safe_name,
        ".png"
      )
    ),
    pacf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )
}

write_csv(
  transformed_data,
  file.path(
    tables_dir,
    "wsts_monthly_transformed_series.csv"
  )
)

write_csv(
  stationarity_results,
  file.path(
    tables_dir,
    "wsts_monthly_stationarity_tests.csv"
  )
)

write_csv(
  serial_correlation_results,
  file.path(
    tables_dir,
    "wsts_monthly_serial_correlation_tests.csv"
  )
)

write_csv(
  acf_summary,
  file.path(
    tables_dir,
    "wsts_monthly_acf_summary.csv"
  )
)

cat("\n==============================\n")
cat("WSTS MONTHLY DIAGNOSTICS COMPLETED\n")
cat("==============================\n\n")

cat(
  "Observations:",
  nrow(data),
  "\n"
)

cat(
  "Unique dates:",
  n_distinct(data$date),
  "\n"
)

cat(
  "Period:",
  format(min(data$date), "%Y-%m"),
  "to",
  format(max(data$date), "%Y-%m"),
  "\n\n"
)

cat("STATIONARITY TESTS\n")
cat("ADF H0: unit root / non-stationary.\n")
cat("KPSS H0: stationary around the selected level/trend.\n")
cat("PP H0: unit root / non-stationary.\n\n")

print(
  stationarity_results,
  n = Inf
)

cat("\nSERIAL CORRELATION AFTER LOG DIFFERENCING\n")
cat(
  "Ljung-Box H0: no autocorrelation up to the selected lag.\n\n"
)

print(
  serial_correlation_results,
  n = Inf
)

cat("\nSELECTED ACF COEFFICIENTS\n\n")

print(
  acf_summary,
  n = Inf
)

cat("\nFiles created in output/tables/.\n")
cat(
  "Diagnostic figures saved to output/figures/wsts_monthly_diagnostics/.\n"
)
