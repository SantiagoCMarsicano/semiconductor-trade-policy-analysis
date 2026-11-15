# Semiconductors II
# 15_china_structural_breaks.R
#
# Phase 3 — Structural breaks and policy intervention
# Phase 3A extension: Endogenous structural-break detection in the SIA/WSTS
# smoothed regional series, with special focus on China.
#
# Data layer:
#   data/processed/sia_regional_3mma_2015_2025.csv
#
# IMPORTANT METHODOLOGICAL CAVEAT
# --------------------------------
# These are MONTHLY observations expressed as a THREE-MONTH MOVING AVERAGE
# (3MMA). Consecutive observations therefore share two of their three underlying
# months. This mechanically smooths volatility and induces serial dependence.
#
# As a result:
#   - break dates should be interpreted as changes in a SMOOTHED trajectory;
#   - exact monthly timing should not be over-interpreted;
#   - HAC inference is especially important;
#   - Ljung-Box rejection is expected to reflect, at least in part, the
#     overlapping-window construction of the 3MMA series.
#
# Sales specifications (China, Asia Pacific/All Other, Total):
#   sales_growth_mean: log_difference ~ 1
#       Detects changes in the mean monthly log change of the smoothed series.
#   sales_log_trend: log_level ~ trend_index
#       Detects changes in the level/trend relationship of the smoothed series.
#
# China-share specifications:
#   share_change: share_difference ~ 1
#       Detects changes in the mean monthly change in China's share of
#       Asia Pacific semiconductor sales.
#   share_trend: share_level ~ trend_index
#       Detects changes in the trajectory of China's Asia Pacific share.
#
# Structural breaks are endogenous statistical dates. No policy dates are
# imposed here, and no causal interpretation is made in this script.

required_packages <- c(
  "dplyr", "readr", "tibble", "purrr",
  "ggplot2", "strucchange", "sandwich"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them before running this script."
    )
  )
}

library(dplyr)
library(readr)
library(tibble)
library(purrr)
library(ggplot2)
library(strucchange)
library(sandwich)

# -----------------------------------------------------------------------------
# 1. Paths and analysis settings
# -----------------------------------------------------------------------------

data_path <- file.path(
  "data", "processed",
  "sia_regional_3mma_2015_2025.csv"
)

tables_dir <- file.path(
  "output", "tables", "phase3"
)

figures_dir <- file.path(
  "output", "figures", "china_structural_breaks"
)

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Primary minimum regime length: 24 monthly 3MMA observations (2 years).
# This deliberately limits over-segmentation in a shorter 2015-2025 sample.
primary_h <- 24L

# Robustness: 15% trimming. With 132 monthly observations this corresponds to
# roughly 20 observations per admissible segment.
robustness_h <- 0.15

# 5% remains the primary inferential level. 10% is reported as marginal
# evidence, while 1% is reported as strong evidence.
alpha_01 <- 0.01
alpha_05 <- 0.05
alpha_10 <- 0.10

sales_series <- c(
  "China",
  "Asia Pacific/All Other",
  "Total"
)

share_series <- "China share of Asia Pacific"

series_colors <- c(
  "China" = "#D62728",
  "Asia Pacific/All Other" = "#FF7F0E",
  "Total" = "#111111",
  "China share of Asia Pacific" = "#D62728"
)

sales_specifications <- c(
  "sales_growth_mean",
  "sales_log_trend"
)

share_specifications <- c(
  "share_change",
  "share_trend"
)

# -----------------------------------------------------------------------------
# 2. Load and validate SIA/WSTS 3MMA data
# -----------------------------------------------------------------------------

if (!file.exists(data_path)) {
  stop(
    paste0(
      "SIA/WSTS 3MMA dataset not found: ", data_path,
      "\nRun R/06_build_sia_regional_3mma.R first."
    )
  )
}

data <- read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

required_columns <- c(
  "date",
  "china_3mma_usd_billions",
  "asia_pacific_other_3mma_usd_billions",
  "total_3mma_usd_billions",
  "china_share_asia_pacific"
)

missing_columns <- setdiff(required_columns, names(data))

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  )
}

expected_dates <- seq(
  as.Date("2015-01-01"),
  as.Date("2025-12-01"),
  by = "month"
)

if (
  nrow(data) != 132L ||
  dplyr::n_distinct(data$date) != 132L ||
  !identical(data$date, expected_dates)
) {
  stop(
    paste0(
      "Unexpected SIA/WSTS 3MMA coverage. ",
      "Expected 132 unique monthly observations from 2015-01 to 2025-12."
    )
  )
}

sales_columns <- c(
  "china_3mma_usd_billions",
  "asia_pacific_other_3mma_usd_billions",
  "total_3mma_usd_billions"
)

if (
  any(!is.finite(as.matrix(data[sales_columns]))) ||
  any(as.matrix(data[sales_columns]) <= 0)
) {
  stop(
    "Core 3MMA sales series contain missing, non-finite or non-positive values."
  )
}

if (
  any(!is.finite(data$china_share_asia_pacific)) ||
  any(data$china_share_asia_pacific <= 0) ||
  any(data$china_share_asia_pacific >= 1)
) {
  stop(
    "china_share_asia_pacific must contain finite proportions strictly between 0 and 1."
  )
}

# Cross-check the derived China share against China / (China + AP/All Other).
calculated_china_share <-
  data$china_3mma_usd_billions /
  (
    data$china_3mma_usd_billions +
      data$asia_pacific_other_3mma_usd_billions
  )

share_validation_gap <- max(
  abs(calculated_china_share - data$china_share_asia_pacific),
  na.rm = TRUE
)

if (share_validation_gap > 1e-10) {
  stop(
    paste0(
      "China share validation failed. Maximum discrepancy: ",
      format(share_validation_gap, scientific = TRUE)
    )
  )
}

# -----------------------------------------------------------------------------
# 3. Build analysis datasets
# -----------------------------------------------------------------------------

sales_long <- bind_rows(
  data %>%
    transmute(
      date,
      series = "China",
      value = china_3mma_usd_billions
    ),
  data %>%
    transmute(
      date,
      series = "Asia Pacific/All Other",
      value = asia_pacific_other_3mma_usd_billions
    ),
  data %>%
    transmute(
      date,
      series = "Total",
      value = total_3mma_usd_billions
    )
) %>%
  mutate(
    series = factor(series, levels = sales_series)
  ) %>%
  group_by(series) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    trend_index = row_number(),
    log_level = log(value),
    log_difference = log_level - lag(log_level)
  ) %>%
  ungroup() %>%
  mutate(series = as.character(series))

share_data <- data %>%
  transmute(
    date,
    series = share_series,
    trend_index = row_number(),
    share_level = china_share_asia_pacific,
    share_difference = share_level - lag(share_level)
  )

# -----------------------------------------------------------------------------
# 4. Helper functions
# -----------------------------------------------------------------------------

safe_name <- function(x) {
  gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(x)
  )
}

extract_break_positions <- function(bp_object) {
  positions <- bp_object$breakpoints
  positions <- positions[is.finite(positions)]
  as.integer(positions)
}

position_to_date <- function(position, date_vector) {
  if (is.na(position) || position < 1 || position > length(date_vector)) {
    return(as.Date(NA))
  }
  date_vector[position]
}

next_regime_date <- function(position, date_vector) {
  next_position <- position + 1L
  if (is.na(position) || next_position > length(date_vector)) {
    return(as.Date(NA))
  }
  date_vector[next_position]
}

add_significance_columns <- function(df) {
  df %>%
    mutate(
      significant_01 = p_value <= alpha_01,
      significant_05 = p_value <= alpha_05,
      significant_10 = p_value <= alpha_10,
      decision_05 = if_else(
        significant_05,
        "Reject structural stability at 5%",
        "Do not reject structural stability at 5%"
      ),
      decision_10 = if_else(
        significant_10,
        "Reject structural stability at 10%",
        "Do not reject structural stability at 10%"
      ),
      evidence_strength = case_when(
        p_value <= alpha_01 ~ "Strong evidence (p <= 0.01)",
        p_value <= alpha_05 ~ "Conventional evidence (0.01 < p <= 0.05)",
        p_value <= alpha_10 ~ "Marginal evidence (0.05 < p <= 0.10)",
        TRUE ~ "No evidence at 10%"
      )
    )
}

build_bic_table <- function(bp_full, series_name, specification_name) {
  bp_summary <- summary(bp_full)
  bic_values <- as.numeric(bp_summary$RSS["BIC", ])
  break_labels <- colnames(bp_summary$RSS)

  if (is.null(break_labels)) {
    number_of_breaks <- seq_along(bic_values) - 1L
  } else {
    number_of_breaks <- suppressWarnings(as.integer(break_labels))
    if (any(is.na(number_of_breaks))) {
      number_of_breaks <- seq_along(bic_values) - 1L
    }
  }

  selected_index <- which.min(bic_values)

  tibble(
    series = series_name,
    specification = specification_name,
    number_of_breaks = number_of_breaks,
    bic = bic_values,
    selected_by_bic = seq_along(bic_values) == selected_index
  )
}

build_break_table <- function(
  bp_selected,
  bp_full,
  series_name,
  specification_name,
  date_vector,
  h_value,
  h_label
) {
  positions <- extract_break_positions(bp_selected)

  if (length(positions) == 0) {
    return(
      tibble(
        series = series_name,
        specification = specification_name,
        h_setting = h_label,
        minimum_segment = as.character(h_value),
        break_number = integer(),
        break_position = integer(),
        break_date_last_regime = as.Date(character()),
        next_regime_start = as.Date(character()),
        ci_lower_position = integer(),
        ci_lower_date = as.Date(character()),
        ci_upper_position = integer(),
        ci_upper_date = as.Date(character())
      )
    )
  }

  ci_matrix <- tryCatch(
    confint(bp_full, breaks = length(positions))$confint,
    error = function(e) NULL
  )

  if (is.null(ci_matrix)) {
    ci_lower <- rep(NA_integer_, length(positions))
    ci_upper <- rep(NA_integer_, length(positions))
  } else {
    ci_lower <- as.integer(ci_matrix[, 1])
    ci_upper <- as.integer(ci_matrix[, 3])
  }

  tibble(
    series = series_name,
    specification = specification_name,
    h_setting = h_label,
    minimum_segment = as.character(h_value),
    break_number = seq_along(positions),
    break_position = positions,
    break_date_last_regime = as.Date(
      map_chr(positions, ~ as.character(position_to_date(.x, date_vector)))
    ),
    next_regime_start = as.Date(
      map_chr(positions, ~ as.character(next_regime_date(.x, date_vector)))
    ),
    ci_lower_position = ci_lower,
    ci_lower_date = as.Date(
      map_chr(ci_lower, ~ as.character(position_to_date(.x, date_vector)))
    ),
    ci_upper_position = ci_upper,
    ci_upper_date = as.Date(
      map_chr(ci_upper, ~ as.character(position_to_date(.x, date_vector)))
    )
  )
}

assign_segments <- function(n_obs, break_positions) {
  if (length(break_positions) == 0) {
    return(rep(1L, n_obs))
  }

  cut(
    seq_len(n_obs),
    breaks = c(0L, break_positions, n_obs),
    labels = FALSE,
    include.lowest = TRUE
  )
}

build_segment_summary <- function(
  model_data,
  break_positions,
  series_name,
  specification_name
) {
  segment_data <- model_data %>%
    mutate(segment = assign_segments(n(), break_positions))

  if (specification_name == "sales_growth_mean") {
    return(
      segment_data %>%
        group_by(segment) %>%
        summarise(
          series = series_name,
          specification = specification_name,
          start_date = min(date),
          end_date = max(date),
          n_months = n(),
          mean_monthly_log_change_3mma = mean(log_difference),
          annualized_smoothed_growth_pct =
            100 * (exp(12 * mean(log_difference)) - 1),
          .groups = "drop"
        ) %>%
        select(
          series, specification, segment,
          start_date, end_date, n_months,
          mean_monthly_log_change_3mma,
          annualized_smoothed_growth_pct
        )
    )
  }

  if (specification_name == "sales_log_trend") {
    return(
      map_dfr(
        split(segment_data, segment_data$segment),
        function(segment_df) {
          fit <- lm(log_level ~ trend_index, data = segment_df)
          slope <- unname(coef(fit)[["trend_index"]])

          tibble(
            series = series_name,
            specification = specification_name,
            segment = unique(segment_df$segment),
            start_date = min(segment_df$date),
            end_date = max(segment_df$date),
            n_months = nrow(segment_df),
            intercept = unname(coef(fit)[["(Intercept)"]]),
            monthly_log_trend_3mma = slope,
            annualized_smoothed_trend_pct =
              100 * (exp(12 * slope) - 1),
            r_squared = summary(fit)$r.squared
          )
        }
      )
    )
  }

  if (specification_name == "share_change") {
    return(
      segment_data %>%
        group_by(segment) %>%
        summarise(
          series = series_name,
          specification = specification_name,
          start_date = min(date),
          end_date = max(date),
          n_months = n(),
          mean_monthly_share_change = mean(share_difference),
          mean_monthly_share_change_pp = 100 * mean(share_difference),
          .groups = "drop"
        ) %>%
        select(
          series, specification, segment,
          start_date, end_date, n_months,
          mean_monthly_share_change,
          mean_monthly_share_change_pp
        )
    )
  }

  if (specification_name == "share_trend") {
    return(
      map_dfr(
        split(segment_data, segment_data$segment),
        function(segment_df) {
          fit <- lm(share_level ~ trend_index, data = segment_df)
          slope <- unname(coef(fit)[["trend_index"]])

          tibble(
            series = series_name,
            specification = specification_name,
            segment = unique(segment_df$segment),
            start_date = min(segment_df$date),
            end_date = max(segment_df$date),
            n_months = nrow(segment_df),
            intercept_share = unname(coef(fit)[["(Intercept)"]]),
            monthly_share_trend = slope,
            monthly_share_trend_pp = 100 * slope,
            annual_share_trend_pp = 1200 * slope,
            r_squared = summary(fit)$r.squared
          )
        }
      )
    )
  }

  stop("Unknown specification: ", specification_name)
}

build_residual_diagnostics <- function(
  model_data,
  break_positions,
  series_name,
  specification_name,
  h_label,
  h_value
) {
  segment_data <- model_data %>%
    mutate(segment = assign_segments(n(), break_positions))

  if (specification_name == "sales_growth_mean") {
    residual_data <- segment_data %>%
      group_by(segment) %>%
      mutate(
        fitted_value = mean(log_difference),
        model_residual = log_difference - fitted_value
      ) %>%
      ungroup()
  } else if (specification_name == "sales_log_trend") {
    residual_data <- map_dfr(
      split(segment_data, segment_data$segment),
      function(segment_df) {
        fit <- lm(log_level ~ trend_index, data = segment_df)
        segment_df %>%
          mutate(
            fitted_value = fitted(fit),
            model_residual = residuals(fit)
          )
      }
    )
  } else if (specification_name == "share_change") {
    residual_data <- segment_data %>%
      group_by(segment) %>%
      mutate(
        fitted_value = mean(share_difference),
        model_residual = share_difference - fitted_value
      ) %>%
      ungroup()
  } else if (specification_name == "share_trend") {
    residual_data <- map_dfr(
      split(segment_data, segment_data$segment),
      function(segment_df) {
        fit <- lm(share_level ~ trend_index, data = segment_df)
        segment_df %>%
          mutate(
            fitted_value = fitted(fit),
            model_residual = residuals(fit)
          )
      }
    )
  } else {
    stop("Unknown specification: ", specification_name)
  }

  residuals_vec <- residual_data$model_residual
  n_resid <- length(residuals_vec)

  lb12_lag <- min(12L, n_resid - 1L)
  lb24_lag <- min(24L, n_resid - 1L)

  lb12 <- Box.test(
    residuals_vec,
    lag = lb12_lag,
    type = "Ljung-Box"
  )

  lb24 <- Box.test(
    residuals_vec,
    lag = lb24_lag,
    type = "Ljung-Box"
  )

  segment_sd <- residual_data %>%
    group_by(segment) %>%
    summarise(
      residual_sd = sd(model_residual),
      .groups = "drop"
    )

  min_sd <- min(segment_sd$residual_sd, na.rm = TRUE)
  max_sd <- max(segment_sd$residual_sd, na.rm = TRUE)

  tibble(
    series = series_name,
    specification = specification_name,
    h_setting = h_label,
    minimum_segment = as.character(h_value),
    selected_breaks = length(break_positions),
    n_residuals = n_resid,
    residual_mean = mean(residuals_vec),
    residual_sd = sd(residuals_vec),
    min_segment_residual_sd = min_sd,
    max_segment_residual_sd = max_sd,
    residual_sd_ratio_max_min = if_else(
      is.finite(min_sd) & min_sd > 0,
      max_sd / min_sd,
      NA_real_
    ),
    ljung_box_lag_12 = lb12_lag,
    ljung_box_statistic_12 = unname(lb12$statistic),
    ljung_box_p_value_12 = lb12$p.value,
    ljung_box_lag_24 = lb24_lag,
    ljung_box_statistic_24 = unname(lb24$statistic),
    ljung_box_p_value_24 = lb24$p.value,
    interpretation_note =
      "3MMA observations overlap; residual serial dependence may be partly mechanical."
  )
}

month_index <- function(x) {
  as.integer(format(x, "%Y")) * 12L + as.integer(format(x, "%m"))
}

build_break_robustness_comparison <- function(primary_breaks, robustness_breaks) {
  if (nrow(primary_breaks) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(primary_breaks)), function(i) {
    row <- primary_breaks[i, ]

    candidates <- robustness_breaks %>%
      filter(
        series == row$series,
        specification == row$specification
      )

    if (nrow(candidates) == 0) {
      return(
        tibble(
          series = row$series,
          specification = row$specification,
          primary_break_number = row$break_number,
          primary_break_date = row$break_date_last_regime,
          nearest_robustness_break_date = as.Date(NA),
          distance_months = NA_integer_,
          robustness_class = "No robustness break selected"
        )
      )
    }

    distances <- abs(
      month_index(candidates$break_date_last_regime) -
        month_index(row$break_date_last_regime)
    )

    j <- which.min(distances)
    distance <- as.integer(distances[j])

    robustness_class <- case_when(
      distance == 0L ~ "Exact match",
      distance <= 3L ~ "Within 3 months",
      distance <= 6L ~ "Within 6 months",
      TRUE ~ "Sensitive (> 6 months)"
    )

    tibble(
      series = row$series,
      specification = row$specification,
      primary_break_number = row$break_number,
      primary_break_date = row$break_date_last_regime,
      nearest_robustness_break_date = candidates$break_date_last_regime[j],
      distance_months = distance,
      robustness_class = robustness_class
    )
  })
}

# -----------------------------------------------------------------------------
# 5. Prepare model-specific data and formulas
# -----------------------------------------------------------------------------

prepare_model <- function(series_name, specification_name) {
  if (specification_name %in% sales_specifications) {
    model_data <- sales_long %>%
      filter(series == series_name) %>%
      arrange(date)

    if (specification_name == "sales_growth_mean") {
      model_data <- model_data %>%
        filter(is.finite(log_difference))

      return(
        list(
          data = model_data,
          formula = log_difference ~ 1,
          y_variable = "log_difference",
          y_label = "Monthly log change of 3MMA sales",
          title_suffix = "mean change in smoothed sales"
        )
      )
    }

    model_data <- model_data %>%
      filter(is.finite(log_level))

    return(
      list(
        data = model_data,
        formula = log_level ~ trend_index,
        y_variable = "log_level",
        y_label = "Log 3MMA sales",
        title_suffix = "smoothed log-level trend"
      )
    )
  }

  if (series_name != share_series) {
    stop("Share specifications can only be used with: ", share_series)
  }

  model_data <- share_data %>%
    arrange(date)

  if (specification_name == "share_change") {
    model_data <- model_data %>%
      filter(is.finite(share_difference))

    return(
      list(
        data = model_data,
        formula = share_difference ~ 1,
        y_variable = "share_difference",
        y_label = "Monthly change in China share",
        title_suffix = "mean change in China share"
      )
    )
  }

  if (specification_name == "share_trend") {
    model_data <- model_data %>%
      filter(is.finite(share_level))

    return(
      list(
        data = model_data,
        formula = share_level ~ trend_index,
        y_variable = "share_level",
        y_label = "China share of Asia Pacific",
        title_suffix = "China share trend"
      )
    )
  }

  stop("Unknown specification: ", specification_name)
}

# -----------------------------------------------------------------------------
# 6. Structural-break estimation function
# -----------------------------------------------------------------------------

run_break_analysis <- function(
  series_name,
  specification_name,
  h_value,
  h_label,
  save_break_plot = FALSE
) {
  model <- prepare_model(series_name, specification_name)
  model_data <- model$data
  model_formula <- model$formula

  # Overall structural-stability tests.
  # HAC is especially important here because 3MMA observations overlap.
  fstats_classical <- Fstats(
    model_formula,
    data = model_data,
    from = h_value
  )

  fstats_hac <- Fstats(
    model_formula,
    data = model_data,
    from = h_value,
    vcov. = sandwich::vcovHAC
  )

  supf_classical <- sctest(
    fstats_classical,
    type = "supF"
  )

  supf_hac <- sctest(
    fstats_hac,
    type = "supF"
  )

  supf_row <- bind_rows(
    tibble(
      series = series_name,
      specification = specification_name,
      h_setting = h_label,
      minimum_segment = as.character(h_value),
      variance_estimator = "classical",
      statistic = unname(supf_classical$statistic),
      p_value = supf_classical$p.value
    ),
    tibble(
      series = series_name,
      specification = specification_name,
      h_setting = h_label,
      minimum_segment = as.character(h_value),
      variance_estimator = "HAC",
      statistic = unname(supf_hac$statistic),
      p_value = supf_hac$p.value
    )
  ) %>%
    add_significance_columns()

  # Bai-Perron multiple-break estimation.
  bp_full <- breakpoints(
    model_formula,
    data = model_data,
    h = h_value
  )

  bp_selected <- breakpoints(bp_full)
  break_positions <- extract_break_positions(bp_selected)

  bic_table <- build_bic_table(
    bp_full,
    series_name,
    specification_name
  ) %>%
    mutate(
      h_setting = h_label,
      minimum_segment = as.character(h_value)
    )

  break_table <- build_break_table(
    bp_selected = bp_selected,
    bp_full = bp_full,
    series_name = series_name,
    specification_name = specification_name,
    date_vector = model_data$date,
    h_value = h_value,
    h_label = h_label
  )

  segment_summary <- build_segment_summary(
    model_data = model_data,
    break_positions = break_positions,
    series_name = series_name,
    specification_name = specification_name
  ) %>%
    mutate(
      h_setting = h_label,
      minimum_segment = as.character(h_value)
    )

  residual_diagnostics <- build_residual_diagnostics(
    model_data = model_data,
    break_positions = break_positions,
    series_name = series_name,
    specification_name = specification_name,
    h_label = h_label,
    h_value = h_value
  )

  if (save_break_plot) {
    plot_data <- model_data %>%
      mutate(segment = assign_segments(n(), break_positions))

    break_dates <- if (length(break_positions) > 0) {
      model_data$date[break_positions]
    } else {
      as.Date(character())
    }

    if (specification_name == "sales_growth_mean") {
      fitted_data <- plot_data %>%
        group_by(segment) %>%
        mutate(segment_fit = mean(log_difference)) %>%
        ungroup()

      break_plot <- ggplot(
        fitted_data,
        aes(x = date, y = log_difference)
      ) +
        geom_line(
          color = series_colors[[series_name]],
          linewidth = 0.5,
          alpha = 0.8
        ) +
        geom_step(
          aes(y = segment_fit),
          color = "black",
          linewidth = 0.9
        )
    } else if (specification_name == "sales_log_trend") {
      fitted_data <- map_dfr(
        split(plot_data, plot_data$segment),
        function(segment_df) {
          fit <- lm(log_level ~ trend_index, data = segment_df)
          segment_df %>%
            mutate(segment_fit = fitted(fit))
        }
      )

      break_plot <- ggplot(
        fitted_data,
        aes(x = date, y = log_level)
      ) +
        geom_line(
          color = series_colors[[series_name]],
          linewidth = 0.6
        ) +
        geom_line(
          aes(y = segment_fit, group = segment),
          color = "black",
          linewidth = 0.9
        )
    } else if (specification_name == "share_change") {
      fitted_data <- plot_data %>%
        group_by(segment) %>%
        mutate(segment_fit = mean(share_difference)) %>%
        ungroup()

      break_plot <- ggplot(
        fitted_data,
        aes(x = date, y = share_difference)
      ) +
        geom_line(
          color = series_colors[[series_name]],
          linewidth = 0.5,
          alpha = 0.8
        ) +
        geom_step(
          aes(y = segment_fit),
          color = "black",
          linewidth = 0.9
        )
    } else {
      fitted_data <- map_dfr(
        split(plot_data, plot_data$segment),
        function(segment_df) {
          fit <- lm(share_level ~ trend_index, data = segment_df)
          segment_df %>%
            mutate(segment_fit = fitted(fit))
        }
      )

      break_plot <- ggplot(
        fitted_data,
        aes(x = date, y = share_level)
      ) +
        geom_line(
          color = series_colors[[series_name]],
          linewidth = 0.6
        ) +
        geom_line(
          aes(y = segment_fit, group = segment),
          color = "black",
          linewidth = 0.9
        )
    }

    if (length(break_dates) > 0) {
      break_plot <- break_plot +
        geom_vline(
          data = tibble(xintercept = break_dates),
          aes(xintercept = xintercept),
          linetype = "dashed",
          color = "gray35",
          linewidth = 0.6
        )
    }

    break_plot <- break_plot +
      labs(
        title = paste("Endogenous structural breaks:", series_name),
        subtitle = paste(
          "Specification:", model$title_suffix,
          "| Minimum regime length:",
          ifelse(
            is.numeric(h_value) && h_value < 1,
            paste0(round(100 * h_value), "% of observations"),
            paste0(h_value, " months")
          )
        ),
        x = NULL,
        y = model$y_label,
        caption = paste(
          "SIA/WSTS monthly 3MMA series. Dashed lines are endogenous statistical break dates;",
          "exact timing may be blurred by the overlapping three-month moving average."
        )
      ) +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
      )

    ggsave(
      file.path(
        figures_dir,
        paste0(
          "breaks_",
          safe_name(series_name),
          "_",
          specification_name,
          ".png"
        )
      ),
      break_plot,
      width = 9,
      height = 5.2,
      dpi = 300
    )
  }

  list(
    supf = supf_row,
    bic = bic_table,
    breaks = break_table,
    segments = segment_summary,
    residual_diagnostics = residual_diagnostics
  )
}

# -----------------------------------------------------------------------------
# 7. Primary estimation: 24-month minimum segments
# -----------------------------------------------------------------------------

primary_sales_results <- map(
  sales_series,
  function(series_name) {
    map(
      sales_specifications,
      function(specification_name) {
        run_break_analysis(
          series_name = series_name,
          specification_name = specification_name,
          h_value = primary_h,
          h_label = "primary_24_months",
          save_break_plot = TRUE
        )
      }
    )
  }
) %>%
  flatten()

primary_share_results <- map(
  share_specifications,
  function(specification_name) {
    run_break_analysis(
      series_name = share_series,
      specification_name = specification_name,
      h_value = primary_h,
      h_label = "primary_24_months",
      save_break_plot = TRUE
    )
  }
)

primary_results <- c(primary_sales_results, primary_share_results)

primary_supf <- map_dfr(primary_results, "supf")
primary_bic <- map_dfr(primary_results, "bic")
primary_breaks <- map_dfr(primary_results, "breaks")
primary_segments <- map_dfr(primary_results, "segments")
primary_residual_diagnostics <- map_dfr(
  primary_results,
  "residual_diagnostics"
)

# -----------------------------------------------------------------------------
# 8. Consolidated BIC figures
# -----------------------------------------------------------------------------

plot_sales_bic <- function(specification_name, output_file, subtitle_text) {
  plot_data <- primary_bic %>%
    filter(specification == specification_name) %>%
    mutate(series = factor(series, levels = sales_series))

  selected_data <- plot_data %>%
    filter(selected_by_bic)

  p <- ggplot(
    plot_data,
    aes(x = number_of_breaks, y = bic, group = 1)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_point(
      data = selected_data,
      aes(fill = series),
      shape = 21,
      color = "black",
      size = 3.6,
      stroke = 0.5,
      show.legend = FALSE
    ) +
    geom_text(
      data = selected_data,
      aes(label = paste0("min BIC: ", number_of_breaks, " breaks")),
      vjust = -1.0,
      size = 3,
      show.legend = FALSE
    ) +
    facet_wrap(~ series, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = series_colors) +
    scale_x_continuous(
      breaks = sort(unique(plot_data$number_of_breaks))
    ) +
    labs(
      title = "BIC by number of structural breaks",
      subtitle = subtitle_text,
      x = "Number of structural breaks",
      y = "BIC",
      caption = paste(
        "SIA/WSTS monthly 3MMA series.",
        "The highlighted point is the minimum-BIC partition."
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold")
    )

  ggsave(
    file.path(figures_dir, output_file),
    p,
    width = 11,
    height = 5.5,
    dpi = 300
  )
}

plot_sales_bic(
  specification_name = "sales_growth_mean",
  output_file = "bic_sales_growth_mean.png",
  subtitle_text = "Mean monthly log change of smoothed sales"
)

plot_sales_bic(
  specification_name = "sales_log_trend",
  output_file = "bic_sales_log_trend.png",
  subtitle_text = "Segmented trend in log smoothed sales"
)

share_bic_plot_data <- primary_bic %>%
  filter(series == share_series) %>%
  mutate(
    specification_label = case_when(
      specification == "share_change" ~ "Change in China share",
      specification == "share_trend" ~ "China share trend",
      TRUE ~ specification
    )
  )

share_bic_selected <- share_bic_plot_data %>%
  filter(selected_by_bic)

share_bic_plot <- ggplot(
  share_bic_plot_data,
  aes(x = number_of_breaks, y = bic, group = 1)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_point(
    data = share_bic_selected,
    fill = series_colors[[share_series]],
    shape = 21,
    color = "black",
    size = 3.6,
    stroke = 0.5
  ) +
  geom_text(
    data = share_bic_selected,
    aes(label = paste0("min BIC: ", number_of_breaks, " breaks")),
    vjust = -1.0,
    size = 3
  ) +
  facet_wrap(~ specification_label, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    breaks = sort(unique(share_bic_plot_data$number_of_breaks))
  ) +
  labs(
    title = "BIC by number of structural breaks",
    subtitle = "China share of Asia Pacific semiconductor sales",
    x = "Number of structural breaks",
    y = "BIC",
    caption = paste(
      "Share = China / (China + Asia Pacific/All Other).",
      "Underlying sales series are monthly 3MMA."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(figures_dir, "bic_china_share.png"),
  share_bic_plot,
  width = 9,
  height = 5.2,
  dpi = 300
)

# -----------------------------------------------------------------------------
# 9. Robustness: 15% minimum segment size
# -----------------------------------------------------------------------------

robustness_sales_results <- map(
  sales_series,
  function(series_name) {
    map(
      sales_specifications,
      function(specification_name) {
        run_break_analysis(
          series_name = series_name,
          specification_name = specification_name,
          h_value = robustness_h,
          h_label = "robustness_15_percent",
          save_break_plot = FALSE
        )
      }
    )
  }
) %>%
  flatten()

robustness_share_results <- map(
  share_specifications,
  function(specification_name) {
    run_break_analysis(
      series_name = share_series,
      specification_name = specification_name,
      h_value = robustness_h,
      h_label = "robustness_15_percent",
      save_break_plot = FALSE
    )
  }
)

robustness_results <- c(
  robustness_sales_results,
  robustness_share_results
)

robustness_supf <- map_dfr(robustness_results, "supf")
robustness_bic <- map_dfr(robustness_results, "bic")
robustness_breaks <- map_dfr(robustness_results, "breaks")
robustness_residual_diagnostics <- map_dfr(
  robustness_results,
  "residual_diagnostics"
)

# -----------------------------------------------------------------------------
# 10. Sensitivity comparison
# -----------------------------------------------------------------------------

summarise_selected_breaks <- function(break_table, supf_table) {
  selected_counts <- break_table %>%
    count(
      series,
      specification,
      h_setting,
      name = "selected_breaks"
    )

  all_models <- supf_table %>%
    select(
      series,
      specification,
      h_setting,
      minimum_segment,
      variance_estimator,
      supf_statistic = statistic,
      supf_p_value = p_value,
      significant_01,
      significant_05,
      significant_10,
      decision_05,
      decision_10,
      evidence_strength
    )

  all_models %>%
    left_join(
      selected_counts,
      by = c(
        "series",
        "specification",
        "h_setting"
      )
    ) %>%
    mutate(
      selected_breaks = coalesce(selected_breaks, 0L)
    )
}

sensitivity_summary <- bind_rows(
  summarise_selected_breaks(primary_breaks, primary_supf),
  summarise_selected_breaks(robustness_breaks, robustness_supf)
)

all_breaks <- bind_rows(
  primary_breaks,
  robustness_breaks
)

all_bic <- bind_rows(
  primary_bic,
  robustness_bic
)

all_residual_diagnostics <- bind_rows(
  primary_residual_diagnostics,
  robustness_residual_diagnostics
)

break_robustness_comparison <- build_break_robustness_comparison(
  primary_breaks = primary_breaks,
  robustness_breaks = robustness_breaks
)

# -----------------------------------------------------------------------------
# 11. Cross-layer comparison with WSTS monthly Phase 3A (if available)
# -----------------------------------------------------------------------------

wsts_break_path <- file.path(
  tables_dir,
  "structural_break_dates.csv"
)

cross_layer_comparison <- tibble()

if (file.exists(wsts_break_path)) {
  wsts_breaks <- read_csv(
    wsts_break_path,
    show_col_types = FALSE
  ) %>%
    mutate(
      break_date_last_regime = as.Date(break_date_last_regime)
    ) %>%
    filter(
      h_setting == "primary_24_months",
      specification == "log_trend",
      region %in% c("Asia Pacific", "Worldwide")
    ) %>%
    select(
      wsts_region = region,
      wsts_break_number = break_number,
      wsts_break_date = break_date_last_regime
    )

  sia_candidates <- primary_breaks %>%
    filter(
      specification %in% c("sales_log_trend", "share_trend")
    ) %>%
    select(
      sia_series = series,
      sia_specification = specification,
      sia_break_number = break_number,
      sia_break_date = break_date_last_regime
    )

  if (nrow(wsts_breaks) > 0 && nrow(sia_candidates) > 0) {
    cross_layer_comparison <- map_dfr(
      seq_len(nrow(sia_candidates)),
      function(i) {
        sia_row <- sia_candidates[i, ]

        map_dfr(
          c("Asia Pacific", "Worldwide"),
          function(wsts_region_name) {
            region_candidates <- wsts_breaks %>%
              filter(wsts_region == wsts_region_name)

            if (nrow(region_candidates) == 0) {
              return(tibble())
            }

            distances <- abs(
              month_index(region_candidates$wsts_break_date) -
                month_index(sia_row$sia_break_date)
            )

            j <- which.min(distances)

            tibble(
              sia_series = sia_row$sia_series,
              sia_specification = sia_row$sia_specification,
              sia_break_number = sia_row$sia_break_number,
              sia_break_date = sia_row$sia_break_date,
              wsts_region = wsts_region_name,
              nearest_wsts_break_date = region_candidates$wsts_break_date[j],
              distance_months = as.integer(distances[j]),
              timing_note = case_when(
                distances[j] == 0 ~ "Same reported month",
                distances[j] <= 3 ~ "Within 3 months",
                distances[j] <= 6 ~ "Within 6 months",
                TRUE ~ "More than 6 months apart"
              )
            )
          }
        )
      }
    )
  }
}

# -----------------------------------------------------------------------------
# 12. Export Phase 3 outputs
# -----------------------------------------------------------------------------

write_csv(
  primary_supf,
  file.path(
    tables_dir,
    "china_structural_break_supf_tests.csv"
  )
)

write_csv(
  all_bic,
  file.path(
    tables_dir,
    "china_structural_break_bic_summary.csv"
  )
)

write_csv(
  all_breaks,
  file.path(
    tables_dir,
    "china_structural_break_dates.csv"
  )
)

write_csv(
  primary_segments,
  file.path(
    tables_dir,
    "china_structural_break_segment_summary.csv"
  )
)

write_csv(
  sensitivity_summary,
  file.path(
    tables_dir,
    "china_structural_break_sensitivity_summary.csv"
  )
)

write_csv(
  all_residual_diagnostics,
  file.path(
    tables_dir,
    "china_structural_break_residual_diagnostics.csv"
  )
)

write_csv(
  break_robustness_comparison,
  file.path(
    tables_dir,
    "china_structural_break_robustness_date_comparison.csv"
  )
)

if (nrow(cross_layer_comparison) > 0) {
  write_csv(
    cross_layer_comparison,
    file.path(
      tables_dir,
      "china_vs_wsts_break_timing_comparison.csv"
    )
  )
}

# -----------------------------------------------------------------------------
# 13. Console summary
# -----------------------------------------------------------------------------

cat("\n==============================================\n")
cat("PHASE 3A — CHINA EXTENSION COMPLETED\n")
cat("==============================================\n\n")

cat("Input:\n")
cat("  ", data_path, "\n\n", sep = "")

cat("Data interpretation:\n")
cat("  Monthly observations expressed as a three-month moving average (3MMA).\n")
cat("  Consecutive observations overlap, so serial dependence is partly mechanical.\n")
cat("  Break dates describe changes in smoothed trajectories, not exact shock months.\n\n")

cat("Primary specification settings:\n")
cat("  Minimum regime length: 24 months\n")
cat("  Sales models: sales_growth_mean and sales_log_trend\n")
cat("  China-share models: share_change and share_trend\n")
cat("  Primary significance level: 5%\n")
cat("  Secondary reporting level: 10% (marginal evidence)\n\n")

cat("Overall structural-stability tests (primary):\n")
print(
  primary_supf %>%
    select(
      series, specification, variance_estimator,
      statistic, p_value,
      significant_05, significant_10,
      evidence_strength
    )
)

cat("\nCases that are significant at 10% but not at 5%:\n")
marginal_cases <- primary_supf %>%
  filter(!significant_05, significant_10) %>%
  select(series, specification, variance_estimator, statistic, p_value)

if (nrow(marginal_cases) == 0) {
  cat("  None.\n")
} else {
  print(marginal_cases)
}

cat("\nSelected endogenous break dates (primary):\n")
if (nrow(primary_breaks) == 0) {
  cat("  No BIC-selected breaks.\n")
} else {
  print(
    primary_breaks %>%
      select(
        series,
        specification,
        break_number,
        break_date_last_regime,
        next_regime_start,
        ci_lower_date,
        ci_upper_date
      )
  )
}

cat("\nPrimary-vs-robustness break-date comparison:\n")
if (nrow(break_robustness_comparison) == 0) {
  cat("  No primary break dates selected.\n")
} else {
  print(break_robustness_comparison)
}

cat("\nResidual diagnostics for primary segmented models:\n")
print(
  primary_residual_diagnostics %>%
    select(
      series, specification, selected_breaks,
      residual_sd_ratio_max_min,
      ljung_box_p_value_12,
      ljung_box_p_value_24
    )
)

cat("\n3MMA residual-diagnostic caveat:\n")
cat("  Ljung-Box rejection must not be read exactly as for raw monthly data.\n")
cat("  Overlapping three-month windows mechanically induce serial dependence.\n")

cat("\nCross-layer timing comparison with WSTS monthly Phase 3A:\n")
if (nrow(cross_layer_comparison) == 0) {
  cat("  Not produced (WSTS Phase 3A break table unavailable or no comparable breaks).\n")
} else {
  print(cross_layer_comparison)
}

cat("\nTables written to:\n")
cat("  ", tables_dir, "\n", sep = "")

cat("\nFigures written to:\n")
cat("  ", figures_dir, "\n", sep = "")
cat("  Consolidated BIC figures:\n")
cat("    bic_sales_growth_mean.png\n")
cat("    bic_sales_log_trend.png\n")
cat("    bic_china_share.png\n")

cat("\nInterpretation rule:\n")
cat("  Breaks are endogenous statistical dates in smoothed series, not causal policy effects.\n")
cat("  Historical/policy chronology will be compared only after these dates are estimated.\n\n")
