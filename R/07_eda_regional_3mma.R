# Semiconductors II
# 07_eda_regional_3mma.R
#
# Purpose:
#   - Perform exploratory time-series analysis of the main
#     SIA/WSTS regional smoothed dataset.
#   - Inspect levels, regional shares, China within Asia Pacific,
#     and year-over-year growth.
#   - Use a fixed regional color palette across every figure.
#
# Outputs:
#   output/figures/3mma_eda/*.png
#   output/tables/phase2/eda_*.csv
#
# Important:
#   - All data series use solid lines.
#   - No policy dummies are introduced here.
#   - No causal claims are made here.
#   - "America" is the project label for the WSTS source region "Americas";
#     it is not the United States alone.

required_packages <- c(
  "dplyr", "tidyr", "readr", "lubridate",
  "ggplot2", "scales", "tibble"
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
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(tibble)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

data_path <- file.path(
  "data", "processed",
  "sia_regional_3mma_2015_2025.csv"
)

figures_dir <- file.path(
  "output", "figures", "3mma_eda"
)

tables_dir <- file.path(
  "output", "tables", "phase2"
)

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Main dataset not found: ",
      data_path,
      "\nRun R/06_build_sia_regional_3mma.R first."
    )
  )
}

# ------------------------------------------------------------
# Fixed visual identity
# ------------------------------------------------------------

region_colors <- c(
  "America" = "#1F77B4",                # blue
  "China" = "#D62728",                   # red
  "Asia Pacific/All Other" = "#FF7F0E", # orange
  "Europe" = "#2CA02C",                  # green
  "Japan" = "#9467BD",                   # purple
  "Total" = "#111111"                    # black
)

region_order <- c(
  "America",
  "China",
  "Asia Pacific/All Other",
  "Europe",
  "Japan"
)

series_order <- c(
  region_order,
  "Total"
)

# ------------------------------------------------------------
# Read main dataset
# ------------------------------------------------------------

data <- read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    month_label = month(date, label = TRUE, abbr = TRUE),
    trend = row_number()
  ) %>%
  arrange(date)

# ------------------------------------------------------------
# Basic validation
# ------------------------------------------------------------

expected_dates <- seq(
  as.Date("2015-01-01"),
  as.Date("2025-12-01"),
  by = "month"
)

if (
  nrow(data) != 132 ||
  n_distinct(data$date) != 132 ||
  !all(expected_dates %in% data$date)
) {
  stop(
    paste0(
      "Unexpected time coverage. ",
      "Expected 132 unique monthly observations from 2015-01 to 2025-12."
    )
  )
}

regional_columns <- c(
  "america_3mma_usd_billions",
  "europe_3mma_usd_billions",
  "japan_3mma_usd_billions",
  "china_3mma_usd_billions",
  "asia_pacific_other_3mma_usd_billions",
  "total_3mma_usd_billions"
)

if (any(is.na(data[regional_columns]))) {
  stop("Missing values detected in core regional series.")
}

# ------------------------------------------------------------
# Year-over-year growth rates
# ------------------------------------------------------------

data_eda <- data %>%
  mutate(
    america_yoy =
      (america_3mma_usd_billions /
         lag(america_3mma_usd_billions, 12) - 1) * 100,

    europe_yoy =
      (europe_3mma_usd_billions /
         lag(europe_3mma_usd_billions, 12) - 1) * 100,

    japan_yoy =
      (japan_3mma_usd_billions /
         lag(japan_3mma_usd_billions, 12) - 1) * 100,

    china_yoy =
      (china_3mma_usd_billions /
         lag(china_3mma_usd_billions, 12) - 1) * 100,

    asia_pacific_other_yoy =
      (asia_pacific_other_3mma_usd_billions /
         lag(asia_pacific_other_3mma_usd_billions, 12) - 1) * 100,

    total_yoy =
      (total_3mma_usd_billions /
         lag(total_3mma_usd_billions, 12) - 1) * 100
  )

# ------------------------------------------------------------
# Long-format regional levels
# ------------------------------------------------------------

regional_levels_long <- data_eda %>%
  select(
    date,
    america_3mma_usd_billions,
    europe_3mma_usd_billions,
    japan_3mma_usd_billions,
    china_3mma_usd_billions,
    asia_pacific_other_3mma_usd_billions
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "region",
    values_to = "sales_usd_billions"
  ) %>%
  mutate(
    region = recode(
      region,
      america_3mma_usd_billions = "America",
      europe_3mma_usd_billions = "Europe",
      japan_3mma_usd_billions = "Japan",
      china_3mma_usd_billions = "China",
      asia_pacific_other_3mma_usd_billions = "Asia Pacific/All Other"
    ),
    region = factor(region, levels = region_order)
  )

# ------------------------------------------------------------
# Long-format regional shares
# ------------------------------------------------------------

regional_shares_long <- data_eda %>%
  select(
    date,
    america_share_total,
    europe_share_total,
    japan_share_total,
    china_share_total,
    asia_pacific_other_share_total
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "region",
    values_to = "share"
  ) %>%
  mutate(
    region = recode(
      region,
      america_share_total = "America",
      europe_share_total = "Europe",
      japan_share_total = "Japan",
      china_share_total = "China",
      asia_pacific_other_share_total = "Asia Pacific/All Other"
    ),
    region = factor(region, levels = region_order)
  )

# ------------------------------------------------------------
# Long-format YoY growth
# ------------------------------------------------------------

regional_yoy_long <- data_eda %>%
  select(
    date,
    america_yoy,
    europe_yoy,
    japan_yoy,
    china_yoy,
    asia_pacific_other_yoy,
    total_yoy
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = "series",
    values_to = "yoy_growth"
  ) %>%
  mutate(
    series = recode(
      series,
      america_yoy = "America",
      europe_yoy = "Europe",
      japan_yoy = "Japan",
      china_yoy = "China",
      asia_pacific_other_yoy = "Asia Pacific/All Other",
      total_yoy = "Total"
    ),
    series = factor(series, levels = series_order)
  )

# ------------------------------------------------------------
# Summary tables
# ------------------------------------------------------------

start_end_summary <- tibble(
  region = c(
    "America",
    "Europe",
    "Japan",
    "China",
    "Asia Pacific/All Other",
    "Total"
  ),
  start_value = c(
    first(data_eda$america_3mma_usd_billions),
    first(data_eda$europe_3mma_usd_billions),
    first(data_eda$japan_3mma_usd_billions),
    first(data_eda$china_3mma_usd_billions),
    first(data_eda$asia_pacific_other_3mma_usd_billions),
    first(data_eda$total_3mma_usd_billions)
  ),
  end_value = c(
    last(data_eda$america_3mma_usd_billions),
    last(data_eda$europe_3mma_usd_billions),
    last(data_eda$japan_3mma_usd_billions),
    last(data_eda$china_3mma_usd_billions),
    last(data_eda$asia_pacific_other_3mma_usd_billions),
    last(data_eda$total_3mma_usd_billions)
  )
) %>%
  mutate(
    absolute_change = end_value - start_value,
    percent_change = (end_value / start_value - 1) * 100
  )

share_start_end <- tibble(
  region = c(
    "America",
    "Europe",
    "Japan",
    "China",
    "Asia Pacific/All Other"
  ),
  start_share = c(
    first(data_eda$america_share_total),
    first(data_eda$europe_share_total),
    first(data_eda$japan_share_total),
    first(data_eda$china_share_total),
    first(data_eda$asia_pacific_other_share_total)
  ),
  end_share = c(
    last(data_eda$america_share_total),
    last(data_eda$europe_share_total),
    last(data_eda$japan_share_total),
    last(data_eda$china_share_total),
    last(data_eda$asia_pacific_other_share_total)
  )
) %>%
  mutate(
    share_change_pp = (end_share - start_share) * 100
  )

china_ap_summary <- data_eda %>%
  summarise(
    start_china_share_ap = first(china_share_asia_pacific),
    max_china_share_ap = max(china_share_asia_pacific, na.rm = TRUE),
    max_china_share_ap_date =
      date[which.max(china_share_asia_pacific)],
    end_china_share_ap = last(china_share_asia_pacific)
  )

# ------------------------------------------------------------
# Shared theme
# ------------------------------------------------------------

project_theme <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# ------------------------------------------------------------
# Figure 1: Total market
# ------------------------------------------------------------

p_total <- ggplot(
  data_eda,
  aes(x = date, y = total_3mma_usd_billions)
) +
  geom_line(
    linewidth = 1.1,
    color = region_colors["Total"]
  ) +
  labs(
    title = "Global semiconductor sales",
    subtitle = "Monthly 3-month moving average, 2015-2025",
    x = NULL,
    y = "USD billions",
    caption = "Source: SIA/WSTS monthly market reports."
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1)
  ) +
  project_theme

# ------------------------------------------------------------
# Figure 2: Regional levels
# ------------------------------------------------------------

p_regions <- ggplot(
  regional_levels_long,
  aes(
    x = date,
    y = sales_usd_billions,
    color = region
  )
) +
  geom_line(linewidth = 1.05) +
  labs(
    title = "Regional semiconductor sales",
    subtitle = "Monthly 3-month moving average by WSTS market region",
    x = NULL,
    y = "USD billions",
    color = "Region",
    caption = "Source: SIA/WSTS monthly market reports."
  ) +
  scale_color_manual(
    values = region_colors,
    breaks = region_order,
    drop = FALSE
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  project_theme

# ------------------------------------------------------------
# Figure 3: Regional shares
# ------------------------------------------------------------

p_shares <- ggplot(
  regional_shares_long,
  aes(
    x = date,
    y = share,
    color = region
  )
) +
  geom_line(linewidth = 1.05) +
  labs(
    title = "Regional shares of global semiconductor sales",
    subtitle = "Share of total monthly 3MMA sales",
    x = NULL,
    y = "Share of global sales",
    color = "Region",
    caption = "Source: SIA/WSTS monthly market reports."
  ) +
  scale_color_manual(
    values = region_colors,
    breaks = region_order,
    drop = FALSE
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  project_theme

# ------------------------------------------------------------
# Figure 4: China within Asia Pacific
# ------------------------------------------------------------

p_china_ap <- ggplot(
  data_eda,
  aes(
    x = date,
    y = china_share_asia_pacific
  )
) +
  geom_line(
    linewidth = 1.15,
    color = region_colors["China"]
  ) +
  geom_hline(
    yintercept = 0.5,
    linewidth = 0.6,
    color = "gray55"
  ) +
  labs(
    title = "China share of Asia Pacific semiconductor sales",
    subtitle = "China / (China + Asia Pacific/All Other)",
    x = NULL,
    y = "China share of Asia Pacific",
    caption = "Gray line = 50%. Source: SIA/WSTS monthly market reports."
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  project_theme +
  theme(
    legend.position = "none"
  )

# ------------------------------------------------------------
# Figure 5: Year-over-year growth
# ------------------------------------------------------------

p_yoy <- regional_yoy_long %>%
  filter(!is.na(yoy_growth)) %>%
  ggplot(
    aes(
      x = date,
      y = yoy_growth,
      color = series
    )
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.6,
    color = "gray55"
  ) +
  geom_line(linewidth = 0.95) +
  labs(
    title = "Year-over-year growth in semiconductor sales",
    subtitle = "Growth of monthly 3MMA values relative to the same month one year earlier",
    x = NULL,
    y = "Year-over-year growth (%)",
    color = "Series",
    caption = "Source: SIA/WSTS monthly market reports."
  ) +
  scale_color_manual(
    values = region_colors,
    breaks = series_order,
    drop = FALSE
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  project_theme

# ------------------------------------------------------------
# Save figures
# ------------------------------------------------------------

ggsave(
  file.path(figures_dir, "01_total_market_3mma.png"),
  p_total,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "02_regional_sales_3mma.png"),
  p_regions,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "03_regional_shares_3mma.png"),
  p_shares,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "04_china_share_asia_pacific.png"),
  p_china_ap,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(figures_dir, "05_regional_yoy_growth.png"),
  p_yoy,
  width = 11,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# Save analytical tables
# ------------------------------------------------------------

write_csv(
  data_eda,
  file.path(tables_dir, "eda_regional_monthly_metrics.csv")
)

write_csv(
  start_end_summary,
  file.path(tables_dir, "eda_start_end_sales_summary.csv")
)

write_csv(
  share_start_end,
  file.path(tables_dir, "eda_start_end_share_summary.csv")
)

write_csv(
  china_ap_summary,
  file.path(tables_dir, "eda_china_ap_share_summary.csv")
)

# ------------------------------------------------------------
# Console report
# ------------------------------------------------------------

cat("\n==============================\n")
cat("EXPLORATORY ANALYSIS V2 COMPLETED\n")
cat("==============================\n\n")

cat("Observations:", nrow(data_eda), "\n")
cat(
  "Period:",
  format(min(data_eda$date), "%Y-%m"),
  "to",
  format(max(data_eda$date), "%Y-%m"),
  "\n\n"
)

cat(
  "Color convention:\n",
  "- America: blue\n",
  "- China: red\n",
  "- Asia Pacific/All Other: orange\n",
  "- Europe: green\n",
  "- Japan: purple\n",
  "- Total: black\n\n",
  sep = ""
)

cat("Sales change, start to end:\n")
print(start_end_summary, n = Inf)

cat("\nRegional share change, start to end:\n")
print(share_start_end, n = Inf)

cat("\nChina share within Asia Pacific:\n")
print(china_ap_summary)

cat("\nFigures saved to:\n")
cat("- ", figures_dir, "\n", sep = "")

cat("\nTables saved to:\n")
cat("- ", tables_dir, "\n", sep = "")
