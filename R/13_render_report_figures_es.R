# Semiconductors II
# 13_render_report_figures_es.R
#
# Purpose:
#   Regenerate the complete figure set used so far in Spanish for the report.
#   No data construction or econometric result is changed here: this script
#   only recreates presentation-ready figures from the processed datasets.
#
# Inputs:
#   data/processed/sia_regional_3mma_2015_2025.csv
#   data/processed/wsts_monthly_2010_2025.csv
#   data/processed/wsts_3mma_2010_2025.csv
#
# Output root:
#   output/figures/informe_es/
#
# Subfolders:
#   3mma_eda/
#   3mma_diagnostics/
#   wsts_monthly_eda/
#   wsts_monthly_diagnostics/
#   monthly_vs_3mma/

required_packages <- c(
  "dplyr", "readr", "tidyr", "ggplot2",
  "lubridate", "tibble", "purrr", "scales"
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
library(lubridate)
library(tibble)
library(purrr)
library(scales)

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

sia_path <- file.path(
  "data", "processed",
  "sia_regional_3mma_2015_2025.csv"
)

monthly_path <- file.path(
  "data", "processed",
  "wsts_monthly_2010_2025.csv"
)

wsts_3mma_path <- file.path(
  "data", "processed",
  "wsts_3mma_2010_2025.csv"
)

output_root <- file.path(
  "output", "figures", "informe_es"
)

folders <- list(
  eda_3mma = file.path(output_root, "3mma_eda"),
  diag_3mma = file.path(output_root, "3mma_diagnostics"),
  eda_monthly = file.path(output_root, "wsts_monthly_eda"),
  diag_monthly = file.path(output_root, "wsts_monthly_diagnostics"),
  compare = file.path(output_root, "monthly_vs_3mma")
)

walk(
  folders,
  ~ dir.create(
    .x,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

required_files <- c(
  sia_path,
  monthly_path,
  wsts_3mma_path
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing processed file(s):\n- ",
      paste(missing_files, collapse = "\n- ")
    )
  )
}

# ------------------------------------------------------------
# 2. Visual identity and Spanish labels
# ------------------------------------------------------------

colors <- c(
  "America" = "#1F77B4",
  "América" = "#1F77B4",
  "Europa" = "#2CA02C",
  "Japón" = "#9467BD",
  "China" = "#D62728",
  "Asia-Pacífico" = "#FF7F0E",
  "Asia-Pacífico / Otros" = "#FF7F0E",
  "Total" = "#111111",
  "Mundial" = "#111111"
)

region_labels_wsts <- c(
  "America" = "América",
  "Europe" = "Europa",
  "Japan" = "Japón",
  "Asia Pacific" = "Asia-Pacífico",
  "Worldwide" = "Mundial"
)

month_labels_es <- c(
  "Ene", "Feb", "Mar", "Abr", "May", "Jun",
  "Jul", "Ago", "Set", "Oct", "Nov", "Dic"
)

theme_report <- theme_minimal(
  base_size = 12
) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray35"),
    plot.caption = element_text(
      hjust = 1,
      color = "gray30"
    )
  )

# ------------------------------------------------------------
# 3. Helpers
# ------------------------------------------------------------

safe_name <- function(x) {
  x <- iconv(
    x,
    from = "UTF-8",
    to = "ASCII//TRANSLIT"
  )
  x <- tolower(x)
  gsub("[^a-z0-9]+", "_", x)
}

build_acf_pacf_plot <- function(
  x,
  series_label,
  type = c("acf", "pacf"),
  subtitle,
  line_color,
  max_lag = 24
) {
  type <- match.arg(type)
  x <- x[is.finite(x)]

  if (length(x) < 10) {
    stop(
      paste(
        "Not enough observations for",
        series_label
      )
    )
  }

  confidence_limit <- 1.96 / sqrt(length(x))

  if (type == "acf") {
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
      "ACF —",
      series_label
    )

    y_label <- "Autocorrelación"
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
      "PACF —",
      series_label
    )

    y_label <- "Autocorrelación parcial"
  }

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
      color = line_color
    ) +
    geom_point(
      size = 1.7,
      color = line_color
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
      subtitle = subtitle,
      x = "Rezago (meses)",
      y = y_label,
      caption =
        "Las líneas grises indican límites aproximados de significancia al 95 %."
    ) +
    theme_report +
    theme(
      legend.position = "none"
    )
}

# ------------------------------------------------------------
# 4. Read processed datasets
# ------------------------------------------------------------

sia <- read_csv(
  sia_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(date)

monthly <- read_csv(
  monthly_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(
    region,
    date
  )

wsts_3mma <- read_csv(
  wsts_3mma_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(
    region,
    date
  )

# ============================================================
# PART A — 3MMA EDA IN SPANISH
# ============================================================

# ------------------------------------------------------------
# A1. Total market
# ------------------------------------------------------------

p_3mma_total <- ggplot(
  sia,
  aes(
    x = date,
    y = total_3mma_usd_billions
  )
) +
  geom_line(
    linewidth = 0.9,
    color = colors[["Total"]]
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Ventas mundiales de semiconductores",
    subtitle =
      "Promedio móvil mensual de 3 meses (3MMA), 2015–2025",
    x = NULL,
    y = "Miles de millones de USD",
    caption =
      "Fuente: informes mensuales de mercado SIA/WSTS."
  ) +
  theme_report +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "none"
  )

ggsave(
  file.path(
    folders$eda_3mma,
    "01_ventas_mundiales_3mma.png"
  ),
  p_3mma_total,
  width = 10,
  height = 5.8,
  dpi = 300
)

# ------------------------------------------------------------
# A2. Regional sales
# ------------------------------------------------------------

sia_sales_long <- sia %>%
  select(
    date,
    América = america_3mma_usd_billions,
    Europa = europe_3mma_usd_billions,
    Japón = japan_3mma_usd_billions,
    China = china_3mma_usd_billions,
    `Asia-Pacífico / Otros` =
      asia_pacific_other_3mma_usd_billions
  ) %>%
  pivot_longer(
    -date,
    names_to = "region_es",
    values_to = "sales"
  )

p_3mma_regions <- ggplot(
  sia_sales_long,
  aes(
    x = date,
    y = sales,
    color = region_es
  )
) +
  geom_line(
    linewidth = 0.85
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Ventas de semiconductores por región",
    subtitle =
      "Promedio móvil mensual de 3 meses (3MMA), 2015–2025",
    x = NULL,
    y = "Miles de millones de USD",
    color = NULL,
    caption =
      "Fuente: informes mensuales de mercado SIA/WSTS."
  ) +
  theme_report +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    folders$eda_3mma,
    "02_ventas_regionales_3mma.png"
  ),
  p_3mma_regions,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# A3. Regional shares
# ------------------------------------------------------------

sia_shares_long <- sia %>%
  transmute(
    date,
    América = 100 * america_share_total,
    Europa = 100 * europe_share_total,
    Japón = 100 * japan_share_total,
    China = 100 * china_share_total,
    `Asia-Pacífico / Otros` =
      100 * asia_pacific_other_share_total
  ) %>%
  pivot_longer(
    -date,
    names_to = "region_es",
    values_to = "share"
  )

p_3mma_shares <- ggplot(
  sia_shares_long,
  aes(
    x = date,
    y = share,
    color = region_es
  )
) +
  geom_line(
    linewidth = 0.85
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Participación regional en las ventas mundiales de semiconductores",
    subtitle =
      "Participación sobre el total mundial a partir de series 3MMA",
    x = NULL,
    y = "Participación mundial (%)",
    color = NULL,
    caption =
      "Fuente: informes mensuales de mercado SIA/WSTS."
  ) +
  theme_report +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    folders$eda_3mma,
    "03_participaciones_regionales_3mma.png"
  ),
  p_3mma_shares,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# A4. China share of Asia-Pacific
# ------------------------------------------------------------

p_china_share_ap <- ggplot(
  sia,
  aes(
    x = date,
    y = 100 * china_share_asia_pacific
  )
) +
  geom_line(
    linewidth = 0.9,
    color = colors[["China"]]
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Participación de China dentro de Asia-Pacífico",
    subtitle =
      "China / (China + Asia-Pacífico / Otros), series 3MMA",
    x = NULL,
    y = "Participación (%)",
    caption =
      "Fuente: informes mensuales de mercado SIA/WSTS."
  ) +
  theme_report +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "none"
  )

ggsave(
  file.path(
    folders$eda_3mma,
    "04_participacion_china_asia_pacifico.png"
  ),
  p_china_share_ap,
  width = 10,
  height = 5.8,
  dpi = 300
)

# ------------------------------------------------------------
# A5. Regional YoY growth
# ------------------------------------------------------------

sia_yoy <- sia_sales_long %>%
  group_by(region_es) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    yoy_growth = 100 * (
      sales / lag(sales, 12) - 1
    )
  ) %>%
  ungroup()

p_3mma_yoy <- sia_yoy %>%
  filter(
    is.finite(yoy_growth)
  ) %>%
  ggplot(
    aes(
      x = date,
      y = yoy_growth,
      color = region_es
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "gray55"
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    title = "Crecimiento interanual de las ventas de semiconductores",
    subtitle =
      "Comparación con el mismo mes del año anterior, series 3MMA",
    x = NULL,
    y = "Crecimiento interanual (%)",
    color = NULL,
    caption =
      "Fuente: informes mensuales de mercado SIA/WSTS."
  ) +
  theme_report +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    folders$eda_3mma,
    "05_crecimiento_interanual_regional.png"
  ),
  p_3mma_yoy,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# PART B — 3MMA DIAGNOSTICS IN SPANISH
# ============================================================

sia_diag <- sia %>%
  transmute(
    date,
    América =
      america_3mma_usd_billions,
    China =
      china_3mma_usd_billions,
    Total =
      total_3mma_usd_billions,
    `Participación de China en Asia-Pacífico` =
      china_share_asia_pacific
  )

sia_diag_long <- sia_diag %>%
  pivot_longer(
    -date,
    names_to = "series",
    values_to = "value"
  ) %>%
  group_by(series) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    transformed = case_when(
      series ==
        "Participación de China en Asia-Pacífico" ~
        value - lag(value, 1),
      TRUE ~
        log(value) - lag(log(value), 1)
    )
  ) %>%
  ungroup()

series_3mma <- c(
  "América",
  "China",
  "Participación de China en Asia-Pacífico",
  "Total"
)

color_3mma <- c(
  "América" = colors[["America"]],
  "China" = colors[["China"]],
  "Participación de China en Asia-Pacífico" = colors[["China"]],
  "Total" = colors[["Total"]]
)

for (series_name in series_3mma) {
  x <- sia_diag_long %>%
    filter(
      series == series_name
    ) %>%
    pull(transformed)

  transformation_text <- if (
    series_name ==
      "Participación de China en Asia-Pacífico"
  ) {
    "Primera diferencia de la participación; datos SIA/WSTS 3MMA"
  } else {
    "Primera diferencia logarítmica; datos SIA/WSTS 3MMA"
  }

  acf_plot <- build_acf_pacf_plot(
    x = x,
    series_label = series_name,
    type = "acf",
    subtitle = transformation_text,
    line_color = color_3mma[[series_name]],
    max_lag = 24
  )

  pacf_plot <- build_acf_pacf_plot(
    x = x,
    series_label = series_name,
    type = "pacf",
    subtitle = transformation_text,
    line_color = color_3mma[[series_name]],
    max_lag = 24
  )

  file_stub <- safe_name(series_name)

  ggsave(
    file.path(
      folders$diag_3mma,
      paste0("acf_", file_stub, ".png")
    ),
    acf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(
      folders$diag_3mma,
      paste0("pacf_", file_stub, ".png")
    ),
    pacf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# ============================================================
# PART C — MONTHLY WSTS EDA IN SPANISH
# ============================================================

monthly_es <- monthly %>%
  mutate(
    region_es = recode(
      region,
      !!!region_labels_wsts
    )
  ) %>%
  group_by(region_es) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    log_sales = log(sales_usd_billions),
    mom_growth_pct = 100 * (
      sales_usd_billions /
        lag(sales_usd_billions, 1) - 1
    ),
    yoy_growth_pct = 100 * (
      sales_usd_billions /
        lag(sales_usd_billions, 12) - 1
    ),
    log_difference =
      log_sales - lag(log_sales, 1)
  ) %>%
  ungroup()

p_monthly_world <- monthly_es %>%
  filter(
    region_es == "Mundial"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = sales_usd_billions
    )
  ) +
  geom_line(
    linewidth = 0.9,
    color = colors[["Mundial"]]
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Ventas mundiales de semiconductores",
    subtitle =
      "Datos mensuales originales de WSTS, 2010–2025",
    x = NULL,
    y = "Miles de millones de USD",
    caption =
      "Fuente: WSTS Historical Billings, Monthly Data."
  ) +
  theme_report +
  theme(
    legend.position = "none"
  )

ggsave(
  file.path(
    folders$eda_monthly,
    "01_ventas_mundiales_mensuales.png"
  ),
  p_monthly_world,
  width = 10,
  height = 5.5,
  dpi = 300
)

p_monthly_regions <- monthly_es %>%
  filter(
    region_es != "Mundial"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = sales_usd_billions,
      color = region_es
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Ventas de semiconductores por región WSTS",
    subtitle =
      "Observaciones mensuales originales, sin suavizado 3MMA",
    x = NULL,
    y = "Miles de millones de USD",
    color = NULL,
    caption =
      "Asia-Pacífico incluye China en la clasificación regional histórica de WSTS."
  ) +
  theme_report

ggsave(
  file.path(
    folders$eda_monthly,
    "02_ventas_regionales_mensuales.png"
  ),
  p_monthly_regions,
  width = 10,
  height = 5.8,
  dpi = 300
)

p_monthly_yoy <- monthly_es %>%
  # Figura 4: comparación exclusivamente regional. Se excluye el
  # agregado Mundial para evitar saturación visual.
  filter(
    is.finite(yoy_growth_pct),
    region_es != "Mundial"
  ) %>%
  ggplot(
    aes(
      x = date,
      y = yoy_growth_pct,
      color = region_es
    )
  ) +
  geom_hline(
    yintercept = 0,
    color = "gray55"
  ) +
  geom_line(
    linewidth = 0.75
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Crecimiento interanual de las ventas de semiconductores",
    subtitle =
      "Comparación con el mismo mes calendario del año anterior",
    x = NULL,
    y = "Crecimiento interanual (%)",
    color = NULL,
    caption =
      "Se omite el agregado Mundial para facilitar la comparación regional."
  ) +
  theme_report

ggsave(
  file.path(
    folders$eda_monthly,
    "03_crecimiento_interanual.png"
  ),
  p_monthly_yoy,
  width = 10,
  height = 6,
  dpi = 300
)

regional_share_monthly <- monthly_es %>%
  filter(
    region_es != "Mundial"
  ) %>%
  select(
    date,
    region_es,
    sales_usd_billions
  ) %>%
  left_join(
    monthly_es %>%
      filter(
        region_es == "Mundial"
      ) %>%
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

p_monthly_shares <- ggplot(
  regional_share_monthly,
  aes(
    x = date,
    y = share_worldwide,
    color = region_es
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  scale_color_manual(
    values = colors
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Participación regional en las ventas mundiales de semiconductores",
    subtitle =
      "Datos mensuales originales de WSTS",
    x = NULL,
    y = "Participación mundial (%)",
    color = NULL
  ) +
  theme_report

ggsave(
  file.path(
    folders$eda_monthly,
    "04_participaciones_regionales.png"
  ),
  p_monthly_shares,
  width = 10,
  height = 5.8,
  dpi = 300
)

seasonal_profile <- monthly_es %>%
  mutate(
    calendar_year = year(date),
    month_number = month(date),
    month_es = factor(
      month_labels_es[month_number],
      levels = month_labels_es
    )
  ) %>%
  group_by(
    region_es,
    calendar_year
  ) %>%
  mutate(
    yearly_mean = mean(sales_usd_billions),
    seasonal_ratio =
      sales_usd_billions /
      yearly_mean
  ) %>%
  ungroup() %>%
  group_by(
    region_es,
    month_es
  ) %>%
  summarise(
    mean_seasonal_ratio =
      mean(seasonal_ratio),
    .groups = "drop"
  )

p_seasonality <- seasonal_profile %>%
  # Figura 5: se mantienen sólo las regiones; Mundial permanece en
  # los datos de base, pero no se representa en este gráfico.
  filter(region_es != "Mundial") %>%
  ggplot(
  aes(
    x = month_es,
    y = mean_seasonal_ratio,
    group = region_es,
    color = region_es
  )
) +
  geom_hline(
    yintercept = 1,
    color = "gray55"
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
  ) +
  scale_color_manual(
    values = colors
  ) +
  labs(
    title = "Perfil estacional mensual descriptivo",
    subtitle =
      "Ventas mensuales en relación con el promedio anual de cada región",
    x = NULL,
    y = "Índice estacional",
    color = NULL,
    caption =
      "1,00 = igual al promedio mensual del año. Se omite el agregado Mundial."
  ) +
  theme_report

ggsave(
  file.path(
    folders$eda_monthly,
    "05_perfil_estacional_mensual.png"
  ),
  p_seasonality,
  width = 10,
  height = 5.8,
  dpi = 300
)

# ============================================================
# PART D — MONTHLY DIAGNOSTICS IN SPANISH
# ============================================================

monthly_diag <- monthly_es %>%
  group_by(region_es) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    transformed =
      log(sales_usd_billions) -
      lag(log(sales_usd_billions), 1)
  ) %>%
  ungroup()

monthly_diag_regions <- c(
  "América",
  "Europa",
  "Japón",
  "Asia-Pacífico",
  "Mundial"
)

for (region_name in monthly_diag_regions) {
  x <- monthly_diag %>%
    filter(
      region_es == region_name
    ) %>%
    pull(transformed)

  acf_plot <- build_acf_pacf_plot(
    x = x,
    series_label = region_name,
    type = "acf",
    subtitle =
      "Datos mensuales originales de WSTS tras primera diferencia logarítmica",
    line_color = colors[[region_name]],
    max_lag = 36
  )

  pacf_plot <- build_acf_pacf_plot(
    x = x,
    series_label = region_name,
    type = "pacf",
    subtitle =
      "Datos mensuales originales de WSTS tras primera diferencia logarítmica",
    line_color = colors[[region_name]],
    max_lag = 36
  )

  file_stub <- safe_name(region_name)

  ggsave(
    file.path(
      folders$diag_monthly,
      paste0("acf_", file_stub, ".png")
    ),
    acf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )

  ggsave(
    file.path(
      folders$diag_monthly,
      paste0("pacf_", file_stub, ".png")
    ),
    pacf_plot,
    width = 8,
    height = 5,
    dpi = 300
  )
}

# ============================================================
# PART E — MONTHLY VS 3MMA IN SPANISH
# ============================================================

comparison_start <- as.Date("2015-01-01")
comparison_end <- as.Date("2025-12-01")

compare_regions <- c(
  "America",
  "Asia Pacific",
  "Worldwide"
)

monthly_compare <- monthly %>%
  filter(
    date >= comparison_start,
    date <= comparison_end,
    region %in% compare_regions
  ) %>%
  mutate(
    dataset = "Mensual",
    region_es = recode(
      region,
      !!!region_labels_wsts
    )
  )

mma3_compare <- wsts_3mma %>%
  filter(
    date >= comparison_start,
    date <= comparison_end,
    region %in% compare_regions
  ) %>%
  mutate(
    dataset = "3MMA",
    region_es = recode(
      region,
      !!!region_labels_wsts
    )
  )

compare_combined <- bind_rows(
  monthly_compare,
  mma3_compare
) %>%
  arrange(
    dataset,
    region_es,
    date
  ) %>%
  group_by(
    dataset,
    region_es
  ) %>%
  mutate(
    log_sales = log(sales_usd_billions),
    log_difference =
      log_sales - lag(log_sales, 1)
  ) %>%
  ungroup()

mma3_highlight <- "#D62728"

for (region_name in c(
  "Mundial",
  "América",
  "Asia-Pacífico"
)) {
  plot_data <- compare_combined %>%
    filter(
      region_es == region_name
    )

  file_stub <- safe_name(region_name)

  p_levels <- ggplot() +
    geom_line(
      data = plot_data %>%
        filter(dataset == "Mensual"),
      aes(
        x = date,
        y = sales_usd_billions
      ),
      linewidth = 0.9,
      color = colors[[region_name]],
      linetype = "solid"
    ) +
    geom_line(
      data = plot_data %>%
        filter(dataset == "3MMA"),
      aes(
        x = date,
        y = sales_usd_billions
      ),
      linewidth = 0.95,
      color = mma3_highlight,
      linetype = "longdash"
    ) +
    labs(
      title = paste(
        region_name,
        "— serie mensual vs. 3MMA"
      ),
      subtitle =
        "Mensual = línea sólida en color regional | 3MMA = línea discontinua roja",
      x = NULL,
      y = "Miles de millones de USD",
      caption =
        "La serie 3MMA se resalta en rojo para facilitar la comparación visual."
    ) +
    theme_report +
    theme(
      legend.position = "none"
    )

  ggsave(
    file.path(
      folders$compare,
      paste0(
        "niveles_",
        file_stub,
        "_mensual_vs_3mma.png"
      )
    ),
    p_levels,
    width = 10,
    height = 5.5,
    dpi = 300
  )

  p_growth <- ggplot() +
    geom_hline(
      yintercept = 0,
      color = "gray55"
    ) +
    geom_line(
      data = plot_data %>%
        filter(
          dataset == "Mensual",
          is.finite(log_difference)
        ),
      aes(
        x = date,
        y = 100 * log_difference
      ),
      linewidth = 0.85,
      color = colors[[region_name]],
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
      color = mma3_highlight,
      linetype = "longdash"
    ) +
    labs(
      title = paste(
        region_name,
        "— variación mensual: serie mensual vs. 3MMA"
      ),
      subtitle =
        "Mensual = línea sólida en color regional | 3MMA = línea discontinua roja",
      x = NULL,
      y = "Variación mensual aproximada (%)",
      caption =
        "Variación aproximada mediante primera diferencia logarítmica."
    ) +
    theme_report +
    theme(
      legend.position = "none"
    )

  ggsave(
    file.path(
      folders$compare,
      paste0(
        "crecimiento_",
        file_stub,
        "_mensual_vs_3mma.png"
      )
    ),
    p_growth,
    width = 10,
    height = 5.5,
    dpi = 300
  )
}

# ------------------------------------------------------------
# Final console report
# ------------------------------------------------------------

cat("\n========================================\n")
cat("FIGURAS EN ESPAÑOL PARA INFORME COMPLETADAS\n")
cat("========================================\n\n")

cat("Carpeta raíz:\n")
cat("- output/figures/informe_es/\n\n")

cat("Subcarpetas creadas:\n")
cat("- 3mma_eda/\n")
cat("- 3mma_diagnostics/\n")
cat("- wsts_monthly_eda/\n")
cat("- wsts_monthly_diagnostics/\n")
cat("- monthly_vs_3mma/\n\n")

cat("Los datos y cálculos no fueron modificados; sólo la presentación gráfica.\n")
