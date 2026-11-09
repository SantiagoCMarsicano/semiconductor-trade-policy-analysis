# Semiconductors II
# 10_wsts_monthly_eda.R
#
# Purpose:
#   Exploratory analysis of the original WSTS Monthly Data series
#   for 2010-2025, without 3MMA smoothing and without policy variables.
#
# Input:
#   data/processed/wsts_monthly_2010_2025.csv
#
# Outputs:
#   output/tables/phase2/wsts_monthly_*.csv
#   output/figures/wsts_monthly_eda/*.png

required_packages <- c(
  "dplyr", "readr", "tidyr", "ggplot2",
  "scales", "lubridate", "tibble"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(tibble)

data_path <- file.path(
  "data", "processed",
  "wsts_monthly_2010_2025.csv"
)

figures_dir <- file.path(
  "output", "figures",
  "wsts_monthly_eda"
)

tables_dir <- file.path(
  "output", "tables", "phase2"
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

expected_dates <- 16 * 12
expected_rows <- expected_dates * length(expected_regions)

if (
  nrow(data) != expected_rows ||
  n_distinct(data$date) != expected_dates ||
  !all(expected_regions %in% unique(data$region))
) {
  stop(
    paste0(
      "Unexpected WSTS monthly structure. Expected ",
      expected_rows,
      " rows and ",
      expected_dates,
      " unique dates."
    )
  )
}

region_colors <- c(
  "America" = "#1F77B4",
  "Europe" = "#2CA02C",
  "Japan" = "#9467BD",
  "Asia Pacific" = "#FF7F0E",
  "Worldwide" = "#111111"
)

region_order <- c(
  "America",
  "Asia Pacific",
  "Europe",
  "Japan",
  "Worldwide"
)

data <- data %>%
  mutate(
    region = factor(
      region,
      levels = region_order
    )
  )

theme_semiconductors <- theme_minimal(
  base_size = 12
) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35")
  )

analysis_data <- data %>%
  group_by(region) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    log_sales = log(
      sales_usd_billions
    ),
    mom_growth_pct = 100 * (
      sales_usd_billions /
        lag(sales_usd_billions, 1) -
        1
    ),
    yoy_growth_pct = 100 * (
      sales_usd_billions /
        lag(sales_usd_billions, 12) -
        1
    ),
    log_difference =
      log_sales -
      lag(log_sales, 1)
  ) %>%
  ungroup()

regional_share_data <- data %>%
  filter(region != "Worldwide") %>%
  select(
    date,
    region,
    sales_usd_billions
  ) %>%
  left_join(
    data %>%
      filter(region == "Worldwide") %>%
      select(
        date,
        worldwide_sales = sales_usd_billions
      ),
    by = "date"
  ) %>%
  mutate(
    share_worldwide = 100 *
      sales_usd_billions /
      worldwide_sales
  )

series_summary <- analysis_data %>%
  group_by(region) %>%
  summarise(
    observations = n(),
    start_date = min(date),
    end_date = max(date),
    start_sales_usd_billions =
      first(sales_usd_billions),
    end_sales_usd_billions =
      last(sales_usd_billions),
    absolute_change_usd_billions =
      end_sales_usd_billions -
      start_sales_usd_billions,
    total_growth_pct = 100 * (
      end_sales_usd_billions /
        start_sales_usd_billions -
        1
    ),
    mean_monthly_sales_usd_billions =
      mean(sales_usd_billions),
    sd_monthly_sales_usd_billions =
      sd(sales_usd_billions),
    mean_mom_growth_pct =
      mean(mom_growth_pct, na.rm = TRUE),
    sd_mom_growth_pct =
      sd(mom_growth_pct, na.rm = TRUE),
    mean_yoy_growth_pct =
      mean(yoy_growth_pct, na.rm = TRUE),
    .groups = "drop"
  )

yearly_summary <- analysis_data %>%
  mutate(
    year = year(date)
  ) %>%
  group_by(
    region,
    year
  ) %>%
  summarise(
    annual_mean_monthly_sales_usd_billions =
      mean(sales_usd_billions),
    annual_total_monthly_sales_usd_billions =
      sum(sales_usd_billions),
    mean_yoy_growth_pct =
      mean(yoy_growth_pct, na.rm = TRUE),
    .groups = "drop"
  )

seasonality_data <- analysis_data %>%
  mutate(
    calendar_year = year(date),
    calendar_month = month(
      date,
      label = TRUE,
      abbr = TRUE
    )
  ) %>%
  group_by(
    region,
    calendar_year
  ) %>%
  mutate(
    yearly_mean =
      mean(sales_usd_billions),
    seasonal_ratio =
      sales_usd_billions /
      yearly_mean
  ) %>%
  ungroup()

seasonal_profile <- seasonality_data %>%
  group_by(
    region,
    calendar_month
  ) %>%
  summarise(
    mean_seasonal_ratio =
      mean(seasonal_ratio),
    median_seasonal_ratio =
      median(seasonal_ratio),
    sd_seasonal_ratio =
      sd(seasonal_ratio),
    .groups = "drop"
  )

write_csv(
  analysis_data,
  file.path(
    tables_dir,
    "wsts_monthly_analysis_data.csv"
  )
)

write_csv(
  series_summary,
  file.path(
    tables_dir,
    "wsts_monthly_series_summary.csv"
  )
)

write_csv(
  yearly_summary,
  file.path(
    tables_dir,
    "wsts_monthly_yearly_summary.csv"
  )
)

write_csv(
  regional_share_data,
  file.path(
    tables_dir,
    "wsts_monthly_regional_shares.csv"
  )
)

write_csv(
  seasonal_profile,
  file.path(
    tables_dir,
    "wsts_monthly_seasonal_profile.csv"
  )
)

plot_worldwide <- analysis_data %>%
  filter(region == "Worldwide") %>%
  ggplot(
    aes(
      x = date,
      y = sales_usd_billions
    )
  ) +
  geom_line(
    linewidth = 0.9,
    color = region_colors[["Worldwide"]]
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Worldwide semiconductor sales",
    subtitle =
      "Original WSTS monthly data, 2010-2025",
    x = NULL,
    y = "USD billions",
    caption =
      "Source: WSTS Historical Billings. Monthly Data."
  ) +
  theme_semiconductors

ggsave(
  file.path(
    figures_dir,
    "01_worldwide_monthly_sales.png"
  ),
  plot_worldwide,
  width = 10,
  height = 5.5,
  dpi = 300
)

plot_regions <- analysis_data %>%
  filter(region != "Worldwide") %>%
  ggplot(
    aes(
      x = date,
      y = sales_usd_billions,
      color = region
    )
  ) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = region_colors
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Semiconductor sales by WSTS region",
    subtitle =
      "Original monthly observations, without 3MMA smoothing",
    x = NULL,
    y = "USD billions",
    color = NULL,
    caption =
      "Asia Pacific includes China in the WSTS historical regional classification."
  ) +
  theme_semiconductors

ggsave(
  file.path(
    figures_dir,
    "02_regional_monthly_sales.png"
  ),
  plot_regions,
  width = 10,
  height = 5.8,
  dpi = 300
)

plot_yoy <- analysis_data %>%
  # Figure 4 focuses on regional dynamics; the aggregate Worldwide
  # series is intentionally excluded to avoid visual saturation.
  filter(
    !is.na(yoy_growth_pct),
    region != "Worldwide"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = yoy_growth_pct,
      color = region
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "gray55"
  ) +
  geom_line(linewidth = 0.75) +
  scale_color_manual(
    values = region_colors
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Year-over-year semiconductor sales growth",
    subtitle =
      "Comparison with the same calendar month one year earlier",
    x = NULL,
    y = "YoY growth (%)",
    color = NULL,
    caption =
      "Worldwide aggregate omitted to improve regional comparison."
  ) +
  theme_semiconductors

ggsave(
  file.path(
    figures_dir,
    "03_yoy_growth.png"
  ),
  plot_yoy,
  width = 10,
  height = 6,
  dpi = 300
)

plot_shares <- regional_share_data %>%
  ggplot(
    aes(
      x = date,
      y = share_worldwide,
      color = region
    )
  ) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(
    values = region_colors
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Regional share of worldwide semiconductor sales",
    subtitle =
      "Original WSTS monthly data",
    x = NULL,
    y = "Share of Worldwide (%)",
    color = NULL
  ) +
  theme_semiconductors

ggsave(
  file.path(
    figures_dir,
    "04_regional_shares.png"
  ),
  plot_shares,
  width = 10,
  height = 5.8,
  dpi = 300
)

plot_seasonality <- seasonal_profile %>%
  # Figure 5 compares regional seasonal profiles only; Worldwide is
  # retained in the underlying table but omitted from the figure.
  filter(region != "Worldwide") %>%
  ggplot(
    aes(
      x = calendar_month,
      y = mean_seasonal_ratio,
      group = region,
      color = region
    )
  ) +
  geom_hline(
    yintercept = 1,
    color = "gray55"
  ) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(
    values = region_colors
  ) +
  labs(
    title = "Descriptive monthly seasonal profile",
    subtitle =
      "Monthly sales relative to each region's annual mean",
    x = NULL,
    y = "Seasonal ratio",
    color = NULL,
    caption =
      "1.00 = equal to the annual monthly average. Worldwide aggregate omitted from the figure."
  ) +
  theme_semiconductors

ggsave(
  file.path(
    figures_dir,
    "05_seasonal_profile.png"
  ),
  plot_seasonality,
  width = 10,
  height = 5.8,
  dpi = 300
)

cat("\n==============================\n")
cat("WSTS MONTHLY EDA COMPLETED\n")
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

cat("SERIES SUMMARY\n\n")
print(series_summary, n = Inf)

cat("\nTables saved to output/tables/phase2/.\n")
cat("Figures saved to output/figures/wsts_monthly_eda/.\n")
cat(
  "\nNo policy dates or causal claims are introduced in this script.\n"
)
