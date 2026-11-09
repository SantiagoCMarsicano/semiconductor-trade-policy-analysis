# Semiconductors II
# 12_compare_monthly_vs_3mma.R
#
# Purpose:
#   Quantify how much WSTS 3MMA smoothing changes volatility and
#   serial dependence relative to the original monthly data.
#
# This is a robustness / measurement comparison, not a causal model.
#
# Common series:
#   America, Europe, Japan, Asia Pacific, Worldwide
#
# Common comparison period:
#   2015-01 to 2025-12
#
# Outputs:
#   output/figures/monthly_vs_3mma/*.png
#   output/tables/monthly_vs_3mma_*.csv
#
# Visual convention:
#   The smoothed 3-month moving-average series is drawn as a
#   high-contrast dashed red line for readability.

required_packages <- c(
  "dplyr", "readr", "tidyr",
  "purrr", "tibble", "ggplot2"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", "),
      ".\nInstall them before running this script."
    )
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)

monthly_path <- file.path(
  "data", "processed",
  "wsts_monthly_2010_2025.csv"
)

mma3_path <- file.path(
  "data", "processed",
  "wsts_3mma_2010_2025.csv"
)

figures_dir <- file.path(
  "output", "figures",
  "monthly_vs_3mma"
)

tables_dir <- file.path(
  "output", "tables"
)

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(monthly_path)) {
  stop(
    paste0(
      "File not found: ",
      monthly_path
    )
  )
}

if (!file.exists(mma3_path)) {
  stop(
    paste0(
      "File not found: ",
      mma3_path
    )
  )
}

comparison_start <- as.Date(
  "2015-01-01"
)

comparison_end <- as.Date(
  "2025-12-01"
)

regions <- c(
  "America",
  "Europe",
  "Japan",
  "Asia Pacific",
  "Worldwide"
)

monthly <- read_csv(
  monthly_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    dataset = "Monthly"
  ) %>%
  filter(
    date >= comparison_start,
    date <= comparison_end,
    region %in% regions
  )

mma3 <- read_csv(
  mma3_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    dataset = "3MMA"
  ) %>%
  filter(
    date >= comparison_start,
    date <= comparison_end,
    region %in% regions
  )

combined <- bind_rows(
  monthly,
  mma3
) %>%
  arrange(
    dataset,
    region,
    date
  ) %>%
  group_by(
    dataset,
    region
  ) %>%
  mutate(
    log_sales =
      log(sales_usd_billions),
    log_difference =
      log_sales -
      lag(log_sales, 1)
  ) %>%
  ungroup()

safe_acf_value <- function(
  x,
  lag_value
) {
  x <- x[is.finite(x)]

  object <- acf(
    x,
    lag.max = lag_value,
    plot = FALSE
  )

  as.numeric(
    object$acf
  )[lag_value + 1]
}

safe_ljung_box <- function(
  x,
  lag_value
) {
  x <- x[is.finite(x)]

  result <- Box.test(
    x,
    lag = lag_value,
    type = "Ljung-Box"
  )

  tibble(
    statistic = unname(
      result$statistic
    ),
    p_value = result$p.value
  )
}

comparison_summary <- combined %>%
  group_by(
    dataset,
    region
  ) %>%
  group_modify(
    ~ {
      x <- .x$log_difference
      x <- x[is.finite(x)]

      lb_6 <- safe_ljung_box(
        x,
        6
      )

      lb_12 <- safe_ljung_box(
        x,
        12
      )

      tibble(
        observations =
          length(x),
        mean_log_difference =
          mean(x),
        sd_log_difference =
          sd(x),
        acf_lag_1 =
          safe_acf_value(x, 1),
        acf_lag_2 =
          safe_acf_value(x, 2),
        acf_lag_3 =
          safe_acf_value(x, 3),
        acf_lag_12 =
          safe_acf_value(x, 12),
        ljung_box_6_statistic =
          lb_6$statistic,
        ljung_box_6_p_value =
          lb_6$p_value,
        ljung_box_12_statistic =
          lb_12$statistic,
        ljung_box_12_p_value =
          lb_12$p_value
      )
    }
  ) %>%
  ungroup()

comparison_wide <- comparison_summary %>%
  select(
    dataset,
    region,
    sd_log_difference,
    acf_lag_1,
    acf_lag_2,
    acf_lag_3,
    acf_lag_12,
    ljung_box_12_statistic,
    ljung_box_12_p_value
  ) %>%
  pivot_wider(
    names_from = dataset,
    values_from = c(
      sd_log_difference,
      acf_lag_1,
      acf_lag_2,
      acf_lag_3,
      acf_lag_12,
      ljung_box_12_statistic,
      ljung_box_12_p_value
    ),
    names_sep = "_"
  ) %>%
  mutate(
    volatility_ratio_3MMA_to_Monthly =
      sd_log_difference_3MMA /
      sd_log_difference_Monthly,

    acf1_change_3MMA_minus_Monthly =
      acf_lag_1_3MMA -
      acf_lag_1_Monthly,

    acf2_change_3MMA_minus_Monthly =
      acf_lag_2_3MMA -
      acf_lag_2_Monthly,

    acf3_change_3MMA_minus_Monthly =
      acf_lag_3_3MMA -
      acf_lag_3_Monthly
  )

level_comparison <- monthly %>%
  select(
    date,
    region,
    monthly_sales =
      sales_usd_billions
  ) %>%
  inner_join(
    mma3 %>%
      select(
        date,
        region,
        mma3_sales =
          sales_usd_billions
      ),
    by = c(
      "date",
      "region"
    )
  )

level_correlations <- level_comparison %>%
  group_by(region) %>%
  summarise(
    correlation_monthly_3mma =
      cor(
        monthly_sales,
        mma3_sales,
        use = "complete.obs"
      ),
    mean_absolute_difference_usd_billions =
      mean(
        abs(
          monthly_sales -
            mma3_sales
        )
      ),
    .groups = "drop"
  )

write_csv(
  comparison_summary,
  file.path(
    tables_dir,
    "monthly_vs_3mma_diagnostic_summary.csv"
  )
)

write_csv(
  comparison_wide,
  file.path(
    tables_dir,
    "monthly_vs_3mma_effect_of_smoothing.csv"
  )
)

write_csv(
  level_correlations,
  file.path(
    tables_dir,
    "monthly_vs_3mma_level_correlations.csv"
  )
)

region_colors <- c(
  "America" = "#1F77B4",
  "Europe" = "#2CA02C",
  "Japan" = "#9467BD",
  "Asia Pacific" = "#FF7F0E",
  "Worldwide" = "#111111"
)

monthly_line_color <- function(region_name) {
  region_colors[[region_name]]
}

mma3_line_color <- "#D62728"

plot_regions <- c(
  "Worldwide",
  "America",
  "Asia Pacific"
)

for (
  region_name in plot_regions
) {
  plot_data <- combined %>%
    filter(
      region == region_name
    )

  safe_name <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(region_name)
  )

  level_plot <- ggplot() +
    geom_line(
      data = plot_data %>% filter(dataset == "Monthly"),
      aes(
        x = date,
        y = sales_usd_billions
      ),
      linewidth = 0.9,
      color = monthly_line_color(region_name),
      linetype = "solid"
    ) +
    geom_line(
      data = plot_data %>% filter(dataset == "3MMA"),
      aes(
        x = date,
        y = sales_usd_billions
      ),
      linewidth = 0.95,
      color = mma3_line_color,
      linetype = "longdash"
    ) +
    labs(
      title = paste(
        region_name,
        "- Monthly vs 3MMA"
      ),
      subtitle =
        "Monthly = solid regional color | 3MMA = dashed red",
      x = NULL,
      y = "USD billions",
      caption = "3MMA is highlighted in dashed red for visibility."
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

  ggsave(
    file.path(
      figures_dir,
      paste0(
        "levels_",
        safe_name,
        "_monthly_vs_3mma.png"
      )
    ),
    level_plot,
    width = 10,
    height = 5.5,
    dpi = 300
  )

  growth_plot <- ggplot() +
    geom_hline(
      yintercept = 0,
      color = "gray55"
    ) +
    geom_line(
      data = plot_data %>%
        filter(
          dataset == "Monthly",
          is.finite(log_difference)
        ),
      aes(
        x = date,
        y = 100 * log_difference
      ),
      linewidth = 0.85,
      color = monthly_line_color(region_name),
      linetype = "solid"
    ) +
    geom_line(
      data = plot_data %>%
        filter(
          dataset == "3MMA",
          is.finite(log_difference)
        ),
      aes(
        x = date,
        y = 100 * log_difference
      ),
      linewidth = 0.95,
      color = mma3_line_color,
      linetype = "longdash"
    ) +
    labs(
      title = paste(
        region_name,
        "- monthly log changes"
      ),
      subtitle =
        "Monthly = solid regional color | 3MMA = dashed red",
      x = NULL,
      y = "Approx. monthly change (%)",
      caption = "3MMA is highlighted in dashed red for visibility."
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

  ggsave(
    file.path(
      figures_dir,
      paste0(
        "growth_",
        safe_name,
        "_monthly_vs_3mma.png"
      )
    ),
    growth_plot,
    width = 10,
    height = 5.5,
    dpi = 300
  )
}

cat("\n==============================\n")
cat("MONTHLY VS 3MMA COMPARISON COMPLETED\n")
cat("==============================\n\n")

cat(
  "Comparison period:",
  format(comparison_start, "%Y-%m"),
  "to",
  format(comparison_end, "%Y-%m"),
  "\n\n"
)

cat("DIAGNOSTIC SUMMARY\n\n")
print(
  comparison_summary,
  n = Inf
)

cat("\nEFFECT OF 3MMA SMOOTHING\n\n")
print(
  comparison_wide,
  n = Inf
)

cat("\nLEVEL CORRELATIONS\n\n")
print(
  level_correlations,
  n = Inf
)

cat("\nFiles created in output/tables/.\n")
cat("Figures saved to output/figures/monthly_vs_3mma/.\n")
