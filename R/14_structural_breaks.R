# Semiconductors II
# 14_structural_breaks.R
#
# Phase 3 — Structural breaks and policy intervention
# Part A: Endogenous structural-break detection in original WSTS monthly data.
#
# Purpose:
#   1. Test whether the monthly semiconductor sales series are structurally stable.
#   2. Estimate the number and timing of structural breaks without imposing policy dates.
#   3. Produce reproducible break dates, confidence intervals, segment summaries and figures.
#
# Primary specification:
#   log_difference ~ 1
#   Detects changes in the mean monthly log growth rate.
#
# Complementary specification:
#   log_level ~ trend_index
#   Detects changes in the level/trend relationship.
#
# Important:
#   Structural breaks are statistical changes in the data-generating relationship.
#   They are NOT, by themselves, evidence that trade or technology policy caused the change.
#   Historical/policy dates are deliberately NOT imposed in this script.

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
  "wsts_monthly_2010_2025.csv"
)

tables_dir <- file.path(
  "output", "tables", "phase3"
)

figures_dir <- file.path(
  "output", "figures", "structural_breaks"
)

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

# Remove legacy per-region BIC plots from earlier development runs.
# The final script keeps only two consolidated BIC figures.
legacy_bic_files <- list.files(
  figures_dir,
  pattern = "^bic_(america|europe|japan|asia_pacific|worldwide)_(growth_mean|log_trend)\\.png$",
  full.names = TRUE
)

if (length(legacy_bic_files) > 0) {
  unlink(legacy_bic_files)
}

# Primary minimum regime length: 24 monthly observations (2 years).
# This is deliberately stated in months because the data are monthly.
primary_h <- 24L

# Robustness specification: conventional 15% trimming used by breakpoints().
robustness_h <- 0.15

# The 5% level remains the primary inferential criterion.
# We also report 1% and 10% thresholds explicitly. The 10% level is treated
# as marginal evidence rather than as equivalent to conventional 5% evidence.
alpha_01 <- 0.01
alpha_05 <- 0.05
alpha_10 <- 0.10

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

# -----------------------------------------------------------------------------
# 2. Load and validate monthly WSTS data
# -----------------------------------------------------------------------------

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Monthly WSTS dataset not found: ", data_path,
      "\nRun R/02_build_wsts_base.R first."
    )
  )
}

data <- read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  mutate(date = as.Date(date)) %>%
  arrange(region, date)

required_columns <- c(
  "date",
  "region",
  "sales_usd_billions"
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

missing_regions <- setdiff(expected_regions, unique(data$region))

if (length(missing_regions) > 0) {
  stop(
    paste0(
      "Missing expected regions: ",
      paste(missing_regions, collapse = ", ")
    )
  )
}

if (any(!is.finite(data$sales_usd_billions)) ||
    any(data$sales_usd_billions <= 0)) {
  stop(
    "sales_usd_billions contains missing, non-finite or non-positive values."
  )
}

coverage_check <- data %>%
  filter(region %in% expected_regions) %>%
  group_by(region) %>%
  summarise(
    n = n(),
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  )

if (any(coverage_check$n != 192L) ||
    any(coverage_check$start_date != as.Date("2010-01-01")) ||
    any(coverage_check$end_date != as.Date("2025-12-01"))) {
  stop(
    "Unexpected WSTS monthly coverage. Expected 192 months from 2010-01 to 2025-12 for every region."
  )
}

# -----------------------------------------------------------------------------
# 3. Transformations
# -----------------------------------------------------------------------------

analysis_data <- data %>%
  filter(region %in% expected_regions) %>%
  group_by(region) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    trend_index = row_number(),
    log_level = log(sales_usd_billions),
    log_difference = log_level - lag(log_level)
  ) %>%
  ungroup()

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

# Build a BIC table from a breakpointsfull object.
# summary.breakpointsfull stores the BIC for every admissible partition
# (0, 1, 2, ... breaks). Using the full BIC path is essential for the
# diagnostic figures; BIC(bp_full) does not provide that sequence.
build_bic_table <- function(bp_full, region_name, specification_name) {
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
    region = region_name,
    specification = specification_name,
    number_of_breaks = number_of_breaks,
    bic = bic_values,
    selected_by_bic = seq_along(bic_values) == selected_index
  )
}

# Build dated breakpoints and confidence intervals.
build_break_table <- function(
  bp_selected,
  bp_full,
  region_name,
  specification_name,
  date_vector,
  h_value,
  h_label
) {
  positions <- extract_break_positions(bp_selected)

  if (length(positions) == 0) {
    return(
      tibble(
        region = region_name,
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
    region = region_name,
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
  region_name,
  specification_name
) {
  segment_data <- model_data %>%
    mutate(
      segment = assign_segments(n(), break_positions)
    )

  if (specification_name == "growth_mean") {
    return(
      segment_data %>%
        group_by(segment) %>%
        summarise(
          region = region_name,
          specification = specification_name,
          start_date = min(date),
          end_date = max(date),
          n_months = n(),
          mean_monthly_log_growth = mean(log_difference),
          annualized_growth_pct = 100 * (exp(12 * mean(log_difference)) - 1),
          .groups = "drop"
        ) %>%
        select(
          region, specification, segment,
          start_date, end_date, n_months,
          mean_monthly_log_growth,
          annualized_growth_pct
        )
    )
  }

  map_dfr(
    split(segment_data, segment_data$segment),
    function(segment_df) {
      fit <- lm(log_level ~ trend_index, data = segment_df)
      slope <- unname(coef(fit)[["trend_index"]])

      tibble(
        region = region_name,
        specification = specification_name,
        segment = unique(segment_df$segment),
        start_date = min(segment_df$date),
        end_date = max(segment_df$date),
        n_months = nrow(segment_df),
        intercept = unname(coef(fit)[["(Intercept)"]]),
        monthly_log_trend = slope,
        annualized_trend_pct = 100 * (exp(12 * slope) - 1),
        r_squared = summary(fit)$r.squared
      )
    }
  )
}

# Residual diagnostics for the BIC-selected segmented model.
# These diagnostics do not "prove" a model is correctly specified. They tell us
# whether substantial serial dependence or large dispersion differences remain
# after allowing for the estimated structural breaks.
build_residual_diagnostics <- function(
  model_data,
  break_positions,
  region_name,
  specification_name,
  h_label,
  h_value
) {
  segment_data <- model_data %>%
    mutate(segment = assign_segments(n(), break_positions))

  if (specification_name == "growth_mean") {
    residual_data <- segment_data %>%
      group_by(segment) %>%
      mutate(
        fitted_value = mean(log_difference),
        model_residual = log_difference - fitted_value
      ) %>%
      ungroup()
  } else {
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
    region = region_name,
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
    ljung_box_p_value_24 = lb24$p.value
  )
}

# Compare each primary break with the nearest break under the 15% trimming rule.
# This is descriptive sensitivity evidence, not a new hypothesis test.
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
        region == row$region,
        specification == row$specification
      )

    if (nrow(candidates) == 0) {
      return(
        tibble(
          region = row$region,
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
      region = row$region,
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
# 5. Structural-break estimation function
# -----------------------------------------------------------------------------

run_break_analysis <- function(
  region_name,
  specification_name,
  h_value,
  h_label,
  save_break_plot = FALSE
) {
  region_data <- analysis_data %>%
    filter(region == region_name) %>%
    arrange(date)

  if (specification_name == "growth_mean") {
    model_data <- region_data %>%
      filter(is.finite(log_difference))

    model_formula <- log_difference ~ 1
    y_label <- "Monthly log growth"
    title_suffix <- "mean growth"
  } else if (specification_name == "log_trend") {
    model_data <- region_data %>%
      filter(is.finite(log_level))

    model_formula <- log_level ~ trend_index
    y_label <- "Log sales"
    title_suffix <- "log-level trend"
  } else {
    stop("Unknown specification: ", specification_name)
  }

  # Overall structural-stability tests.
  # We report both the classical supF test and a HAC-robust version because
  # Phase 2 documented substantial serial correlation in monthly growth.
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
      region = region_name,
      specification = specification_name,
      h_setting = h_label,
      minimum_segment = as.character(h_value),
      variance_estimator = "classical",
      statistic = unname(supf_classical$statistic),
      p_value = supf_classical$p.value
    ),
    tibble(
      region = region_name,
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
    region_name,
    specification_name
  ) %>%
    mutate(
      h_setting = h_label,
      minimum_segment = as.character(h_value)
    )

  break_table <- build_break_table(
    bp_selected = bp_selected,
    bp_full = bp_full,
    region_name = region_name,
    specification_name = specification_name,
    date_vector = model_data$date,
    h_value = h_value,
    h_label = h_label
  )

  segment_summary <- build_segment_summary(
    model_data = model_data,
    break_positions = break_positions,
    region_name = region_name,
    specification_name = specification_name
  ) %>%
    mutate(
      h_setting = h_label,
      minimum_segment = as.character(h_value)
    )

  residual_diagnostics <- build_residual_diagnostics(
    model_data = model_data,
    break_positions = break_positions,
    region_name = region_name,
    specification_name = specification_name,
    h_label = h_label,
    h_value = h_value
  )

  if (save_break_plot) {
    plot_data <- model_data %>%
      mutate(
        segment = assign_segments(n(), break_positions)
      )

    break_dates <- if (length(break_positions) > 0) {
      model_data$date[break_positions]
    } else {
      as.Date(character())
    }

    if (specification_name == "growth_mean") {
      fitted_data <- plot_data %>%
        group_by(segment) %>%
        mutate(segment_fit = mean(log_difference)) %>%
        ungroup()

      break_plot <- ggplot(
        fitted_data,
        aes(x = date, y = log_difference)
      ) +
        geom_line(
          color = region_colors[[region_name]],
          linewidth = 0.45,
          alpha = 0.75
        ) +
        geom_step(
          aes(y = segment_fit),
          color = "black",
          linewidth = 0.9
        )
    } else {
      fitted_data <- plot_data %>%
        group_by(segment) %>%
        group_modify(
          ~ {
            segment_fit <- lm(
              log_level ~ trend_index,
              data = .x
            )

            .x %>%
              mutate(
                segment_fit = fitted(segment_fit)
              )
          }
        ) %>%
        ungroup()

      break_plot <- ggplot(
        fitted_data,
        aes(x = date, y = log_level)
      ) +
        geom_line(
          color = region_colors[[region_name]],
          linewidth = 0.55
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
        title = paste("Endogenous structural breaks:", region_name),
        subtitle = paste(
          "Specification:", title_suffix,
          "| Minimum regime length:",
          ifelse(is.numeric(h_value) && h_value < 1,
                 paste0(round(100 * h_value), "% of observations"),
                 paste0(h_value, " months"))
        ),
        x = NULL,
        y = y_label,
        caption = "Dashed lines are statistically estimated break dates; no policy dates are imposed."
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
          safe_name(region_name),
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
# 6. Primary estimation: 24-month minimum segments
# -----------------------------------------------------------------------------

specifications <- c(
  "growth_mean",
  "log_trend"
)

primary_results <- map(
  expected_regions,
  function(region_name) {
    map(
      specifications,
      function(specification_name) {
        run_break_analysis(
          region_name = region_name,
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

primary_supf <- map_dfr(primary_results, "supf")
primary_bic <- map_dfr(primary_results, "bic")
primary_breaks <- map_dfr(primary_results, "breaks")
primary_segments <- map_dfr(primary_results, "segments")
primary_residual_diagnostics <- map_dfr(primary_results, "residual_diagnostics")

# -----------------------------------------------------------------------------
# 6A. Consolidated BIC figures
# -----------------------------------------------------------------------------

# BIC is a model-selection diagnostic rather than a substantive time-series
# result. Two consolidated figures are easier to compare than one file per
# region. Each panel shows the complete BIC path for one region and highlights
# the minimum-BIC partition.
plot_consolidated_bic <- function(bic_data, specification_name, output_file) {
  plot_data <- bic_data %>%
    filter(specification == specification_name) %>%
    mutate(region = factor(region, levels = expected_regions))

  selected_data <- plot_data %>%
    filter(selected_by_bic)

  subtitle_text <- if (specification_name == "growth_mean") {
    "Mean monthly log-growth specification"
  } else {
    "Log-level segmented-trend specification"
  }

  p <- ggplot(
    plot_data,
    aes(x = number_of_breaks, y = bic, group = 1)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_point(
      data = selected_data,
      aes(fill = region),
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
    facet_wrap(~ region, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = region_colors) +
    scale_x_continuous(
      breaks = sort(unique(plot_data$number_of_breaks))
    ) +
    labs(
      title = "BIC by number of structural breaks",
      subtitle = subtitle_text,
      x = "Number of structural breaks",
      y = "BIC",
      caption = "The highlighted point is the minimum-BIC partition for each region."
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
    height = 7,
    dpi = 300
  )
}

plot_consolidated_bic(
  primary_bic,
  specification_name = "growth_mean",
  output_file = "bic_growth_mean.png"
)

plot_consolidated_bic(
  primary_bic,
  specification_name = "log_trend",
  output_file = "bic_log_trend.png"
)

# -----------------------------------------------------------------------------
# 7. Robustness: 15% minimum segment size
# -----------------------------------------------------------------------------

robustness_results <- map(
  expected_regions,
  function(region_name) {
    map(
      specifications,
      function(specification_name) {
        run_break_analysis(
          region_name = region_name,
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

robustness_supf <- map_dfr(robustness_results, "supf")
robustness_bic <- map_dfr(robustness_results, "bic")
robustness_breaks <- map_dfr(robustness_results, "breaks")
robustness_residual_diagnostics <- map_dfr(robustness_results, "residual_diagnostics")

# -----------------------------------------------------------------------------
# 8. Sensitivity comparison
# -----------------------------------------------------------------------------

summarise_selected_breaks <- function(break_table, supf_table) {
  selected_counts <- break_table %>%
    count(
      region,
      specification,
      h_setting,
      name = "selected_breaks"
    )

  all_models <- supf_table %>%
    select(
      region,
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
        "region",
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
# 9. Export Phase 3 outputs
# -----------------------------------------------------------------------------

write_csv(
  primary_supf,
  file.path(
    tables_dir,
    "structural_break_supf_tests.csv"
  )
)

write_csv(
  all_bic,
  file.path(
    tables_dir,
    "structural_break_bic_summary.csv"
  )
)

write_csv(
  all_breaks,
  file.path(
    tables_dir,
    "structural_break_dates.csv"
  )
)

write_csv(
  primary_segments,
  file.path(
    tables_dir,
    "structural_break_segment_summary.csv"
  )
)

write_csv(
  sensitivity_summary,
  file.path(
    tables_dir,
    "structural_break_sensitivity_summary.csv"
  )
)

write_csv(
  all_residual_diagnostics,
  file.path(
    tables_dir,
    "structural_break_residual_diagnostics.csv"
  )
)

write_csv(
  break_robustness_comparison,
  file.path(
    tables_dir,
    "structural_break_robustness_date_comparison.csv"
  )
)

# -----------------------------------------------------------------------------
# 10. Console summary
# -----------------------------------------------------------------------------

cat("\n========================================\n")
cat("PHASE 3A — STRUCTURAL BREAKS COMPLETED\n")
cat("========================================\n\n")

cat("Input:\n")
cat("  ", data_path, "\n\n", sep = "")

cat("Primary specification:\n")
cat("  Minimum regime length: 24 months\n")
cat("  Models: growth_mean and log_trend\n")
cat("  Primary significance level: 5%\n")
cat("  Secondary reporting level: 10% (marginal evidence)\n\n")

cat("Overall structural-stability tests (primary):\n")
print(
  primary_supf %>%
    select(
      region, specification, variance_estimator,
      statistic, p_value, significant_05, significant_10, evidence_strength
    )
)

cat("\nCases that are significant at 10% but not at 5%:\n")
marginal_cases <- primary_supf %>%
  filter(!significant_05, significant_10) %>%
  select(region, specification, variance_estimator, statistic, p_value)

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
        region,
        specification,
        break_number,
        break_date_last_regime,
        next_regime_start,
        ci_lower_date,
        ci_upper_date
      )
  )
}

cat("\nSensitivity summary:\n")
print(sensitivity_summary)

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
      region, specification, selected_breaks,
      residual_sd_ratio_max_min,
      ljung_box_p_value_12, ljung_box_p_value_24
    )
)

cat("\nTables written to:\n")
cat("  ", tables_dir, "\n", sep = "")

cat("\nFigures written to:\n")
cat("  ", figures_dir, "\n", sep = "")
cat("  Consolidated BIC figures: bic_growth_mean.png, bic_log_trend.png\n")

cat("\nInterpretation rule:\n")
cat("  Breaks are endogenous statistical dates, not causal policy effects.\n")
cat("  Policy chronology will be compared only after these dates are estimated.\n\n")
