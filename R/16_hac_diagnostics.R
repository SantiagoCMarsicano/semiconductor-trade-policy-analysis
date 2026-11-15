# Semiconductors II
# 16_hac_diagnostics.R
#
# Phase 3A robustness diagnostic:
# Investigate unusually large HAC supF statistics in the SIA/WSTS 3MMA
# China and China-share trend models.
#
# This script DOES NOT estimate new structural break dates and DOES NOT impose
# policy dates. Its purpose is numerical/inferential validation of the HAC
# covariance estimator used in R/15_china_structural_breaks.R.
#
# Questions:
#   1. Are the default vcovHAC covariance matrices nearly singular?
#   2. Are the very large HAC supF statistics specific to one HAC estimator?
#   3. Does the structural-instability conclusion survive alternative,
#      reasonable HAC estimators?
#
# Models checked:
#   China:                  log(3MMA sales) ~ trend
#   China share of AP:      share ~ trend
#
# HAC variants:
#   - vcovHAC default (Andrews weights)
#   - Newey-West, lag 2   (minimum natural check for overlapping 3MMA windows)
#   - Newey-West, lag 6
#   - Newey-West, lag 12
#   - Quadratic Spectral kernel with VAR(1) prewhitening
#
# Interpretation:
#   The exact magnitude of a supF statistic is less important than whether
#   the inferential conclusion is stable across reasonable covariance choices.

required_packages <- c(
  "dplyr", "readr", "tibble", "purrr",
  "strucchange", "sandwich"
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
library(strucchange)
library(sandwich)

# -----------------------------------------------------------------------------
# 1. Paths and settings
# -----------------------------------------------------------------------------

data_path <- file.path(
  "data", "processed",
  "sia_regional_3mma_2015_2025.csv"
)

tables_dir <- file.path(
  "output", "tables", "phase3"
)

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

primary_h <- 24L

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Dataset not found: ", data_path,
      "\nRun the SIA/WSTS 3MMA construction scripts first."
    )
  )
}

# -----------------------------------------------------------------------------
# 2. Load data and construct the two models that produced extreme HAC statistics
# -----------------------------------------------------------------------------

data <- read_csv(
  data_path,
  show_col_types = FALSE
) %>%
  mutate(
    date = as.Date(date),
    trend_index = row_number(),
    china_log_level = log(china_3mma_usd_billions),
    china_share_level = china_share_asia_pacific
  ) %>%
  arrange(date)

if (
  nrow(data) != 132L ||
  dplyr::n_distinct(data$date) != 132L
) {
  stop("Expected 132 monthly observations from 2015-01 through 2025-12.")
}

model_specs <- list(
  list(
    series = "China",
    specification = "sales_log_trend",
    formula = china_log_level ~ trend_index
  ),
  list(
    series = "China share of Asia Pacific",
    specification = "share_trend",
    formula = china_share_level ~ trend_index
  )
)

# -----------------------------------------------------------------------------
# 3. Covariance estimators
# -----------------------------------------------------------------------------

# Current estimator used in script 15.
vcov_hac_default <- function(x, ...) {
  sandwich::vcovHAC(x)
}

# The 3MMA construction mechanically overlaps at lags 1 and 2.
vcov_nw2 <- function(x, ...) {
  sandwich::NeweyWest(
    x,
    lag = 2,
    prewhite = FALSE,
    adjust = TRUE
  )
}

# Broader short-run dependence check.
vcov_nw6 <- function(x, ...) {
  sandwich::NeweyWest(
    x,
    lag = 6,
    prewhite = FALSE,
    adjust = TRUE
  )
}

# Annual monthly dependence check.
vcov_nw12 <- function(x, ...) {
  sandwich::NeweyWest(
    x,
    lag = 12,
    prewhite = FALSE,
    adjust = TRUE
  )
}

# Alternative kernel HAC, close to the type used in strucchange documentation.
vcov_qs_pw1 <- function(x, ...) {
  sandwich::kernHAC(
    x,
    kernel = "Quadratic Spectral",
    prewhite = 1,
    adjust = TRUE
  )
}

vcov_estimators <- list(
  classical = stats::vcov,
  HAC_default_Andrews = vcov_hac_default,
  NeweyWest_lag2 = vcov_nw2,
  NeweyWest_lag6 = vcov_nw6,
  NeweyWest_lag12 = vcov_nw12,
  QS_prewhite1 = vcov_qs_pw1
)

# -----------------------------------------------------------------------------
# 4. Helpers
# -----------------------------------------------------------------------------

safe_condition_number <- function(matrix_object) {
  tryCatch(
    kappa(matrix_object, exact = TRUE),
    error = function(e) NA_real_
  )
}

safe_eigenvalues <- function(matrix_object) {
  tryCatch(
    eigen(
      matrix_object,
      symmetric = TRUE,
      only.values = TRUE
    )$values,
    error = function(e) rep(NA_real_, nrow(matrix_object))
  )
}

covariance_diagnostics <- function(
  fit,
  vcov_function,
  estimator_name,
  series_name,
  specification_name
) {
  covariance_matrix <- tryCatch(
    vcov_function(fit),
    error = function(e) NULL
  )

  if (is.null(covariance_matrix)) {
    return(
      tibble(
        series = series_name,
        specification = specification_name,
        variance_estimator = estimator_name,
        covariance_ok = FALSE,
        min_eigenvalue = NA_real_,
        max_eigenvalue = NA_real_,
        raw_condition_number = NA_real_,
        correlation_condition_number = NA_real_,
        min_standard_error = NA_real_,
        max_standard_error = NA_real_,
        determinant = NA_real_
      )
    )
  }

  eigenvalues <- safe_eigenvalues(covariance_matrix)

  correlation_matrix <- tryCatch(
    cov2cor(covariance_matrix),
    error = function(e) NULL
  )

  standard_errors <- sqrt(pmax(diag(covariance_matrix), 0))

  tibble(
    series = series_name,
    specification = specification_name,
    variance_estimator = estimator_name,
    covariance_ok = all(is.finite(covariance_matrix)),
    min_eigenvalue = min(eigenvalues, na.rm = TRUE),
    max_eigenvalue = max(eigenvalues, na.rm = TRUE),
    raw_condition_number = safe_condition_number(covariance_matrix),
    correlation_condition_number = if (is.null(correlation_matrix)) {
      NA_real_
    } else {
      safe_condition_number(correlation_matrix)
    },
    min_standard_error = min(standard_errors, na.rm = TRUE),
    max_standard_error = max(standard_errors, na.rm = TRUE),
    determinant = tryCatch(
      det(covariance_matrix),
      error = function(e) NA_real_
    )
  )
}

supf_diagnostics <- function(
  formula_object,
  model_data,
  vcov_function,
  estimator_name,
  series_name,
  specification_name
) {
  fstats_object <- tryCatch(
    Fstats(
      formula_object,
      data = model_data,
      from = primary_h,
      vcov. = vcov_function
    ),
    error = function(e) NULL
  )

  if (is.null(fstats_object)) {
    return(
      tibble(
        series = series_name,
        specification = specification_name,
        variance_estimator = estimator_name,
        supf_statistic = NA_real_,
        supf_p_value = NA_real_,
        maximum_pointwise_f = NA_real_,
        reject_05 = NA,
        reject_10 = NA
      )
    )
  }

  supf_test <- tryCatch(
    sctest(
      fstats_object,
      type = "supF"
    ),
    error = function(e) NULL
  )

  if (is.null(supf_test)) {
    return(
      tibble(
        series = series_name,
        specification = specification_name,
        variance_estimator = estimator_name,
        supf_statistic = NA_real_,
        supf_p_value = NA_real_,
        maximum_pointwise_f = max(
          as.numeric(fstats_object$Fstats),
          na.rm = TRUE
        ),
        reject_05 = NA,
        reject_10 = NA
      )
    )
  }

  tibble(
    series = series_name,
    specification = specification_name,
    variance_estimator = estimator_name,
    supf_statistic = unname(supf_test$statistic),
    supf_p_value = supf_test$p.value,
    maximum_pointwise_f = max(
      as.numeric(fstats_object$Fstats),
      na.rm = TRUE
    ),
    reject_05 = supf_test$p.value <= 0.05,
    reject_10 = supf_test$p.value <= 0.10
  )
}

# -----------------------------------------------------------------------------
# 5. Run diagnostics
# -----------------------------------------------------------------------------

covariance_results <- map_dfr(
  model_specs,
  function(spec) {
    fit <- lm(
      spec$formula,
      data = data
    )

    map_dfr(
      names(vcov_estimators),
      function(estimator_name) {
        covariance_diagnostics(
          fit = fit,
          vcov_function = vcov_estimators[[estimator_name]],
          estimator_name = estimator_name,
          series_name = spec$series,
          specification_name = spec$specification
        )
      }
    )
  }
)

supf_results <- map_dfr(
  model_specs,
  function(spec) {
    map_dfr(
      names(vcov_estimators),
      function(estimator_name) {
        supf_diagnostics(
          formula_object = spec$formula,
          model_data = data,
          vcov_function = vcov_estimators[[estimator_name]],
          estimator_name = estimator_name,
          series_name = spec$series,
          specification_name = spec$specification
        )
      }
    )
  }
)

# Automatic bandwidths are useful context for understanding the default HAC.
bandwidth_results <- map_dfr(
  model_specs,
  function(spec) {
    fit <- lm(
      spec$formula,
      data = data
    )

    tibble(
      series = spec$series,
      specification = spec$specification,
      andrews_bandwidth = tryCatch(
        sandwich::bwAndrews(fit),
        error = function(e) NA_real_
      ),
      newey_west_bandwidth = tryCatch(
        sandwich::bwNeweyWest(fit),
        error = function(e) NA_real_
      )
    )
  }
)

# Classification deliberately focuses on the correlation-form condition number,
# because raw covariance matrices combine coefficients with very different units.
covariance_results <- covariance_results %>%
  mutate(
    numerical_flag = case_when(
      !covariance_ok ~ "Covariance computation failed",
      !is.finite(correlation_condition_number) ~
        "Correlation condition number unavailable",
      correlation_condition_number >= 1e8 ~
        "Severe near-singularity",
      correlation_condition_number >= 1e4 ~
        "Potential numerical instability",
      TRUE ~
        "No strong near-singularity signal"
    )
  )

# Inferential stability across the alternative HAC estimators (excluding classical).
inferential_stability <- supf_results %>%
  filter(variance_estimator != "classical") %>%
  group_by(series, specification) %>%
  summarise(
    hac_estimators_checked = n(),
    all_reject_05 = all(reject_05, na.rm = TRUE),
    all_reject_10 = all(reject_10, na.rm = TRUE),
    minimum_hac_statistic = min(supf_statistic, na.rm = TRUE),
    maximum_hac_statistic = max(supf_statistic, na.rm = TRUE),
    minimum_hac_p_value = min(supf_p_value, na.rm = TRUE),
    maximum_hac_p_value = max(supf_p_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    conclusion = case_when(
      all_reject_05 ~
        "Structural-instability conclusion is robust across HAC estimators at 5%",
      all_reject_10 ~
        "Conclusion is robust only at 10% across HAC estimators",
      TRUE ~
        "Structural-instability conclusion depends on HAC specification"
    )
  )

# -----------------------------------------------------------------------------
# 6. Write outputs
# -----------------------------------------------------------------------------

write_csv(
  covariance_results,
  file.path(
    tables_dir,
    "china_hac_covariance_diagnostics.csv"
  )
)

write_csv(
  supf_results,
  file.path(
    tables_dir,
    "china_hac_supf_sensitivity.csv"
  )
)

write_csv(
  bandwidth_results,
  file.path(
    tables_dir,
    "china_hac_bandwidth_diagnostics.csv"
  )
)

write_csv(
  inferential_stability,
  file.path(
    tables_dir,
    "china_hac_inferential_stability.csv"
  )
)

# -----------------------------------------------------------------------------
# 7. Console summary
# -----------------------------------------------------------------------------

cat("\n")
cat("==========================================\n")
cat("PHASE 3A — HAC NUMERICAL CHECK COMPLETED\n")
cat("==========================================\n\n")

cat("Models checked:\n")
cat("  China: log(3MMA sales) ~ trend\n")
cat("  China share of Asia Pacific: share ~ trend\n\n")

cat("Why this check exists:\n")
cat("  Script 15 produced unusually large default-HAC supF statistics.\n")
cat("  This script tests whether that reflects near-singular covariance matrices\n")
cat("  or sensitivity to the particular HAC estimator.\n\n")

cat("Automatic HAC bandwidth diagnostics:\n")
print(bandwidth_results, n = Inf)

cat("\nCovariance numerical diagnostics:\n")
print(
  covariance_results %>%
    select(
      series,
      specification,
      variance_estimator,
      correlation_condition_number,
      min_eigenvalue,
      numerical_flag
    ),
  n = Inf
)

cat("\nSupF sensitivity across covariance estimators:\n")
print(
  supf_results %>%
    select(
      series,
      specification,
      variance_estimator,
      supf_statistic,
      supf_p_value,
      reject_05,
      reject_10
    ),
  n = Inf
)

cat("\nInferential stability summary:\n")
print(inferential_stability, n = Inf)

cat("\nInterpretation guide:\n")
cat("  1. A huge default-HAC statistic is NOT by itself evidence of a problem.\n")
cat("  2. Severe correlation condition numbers suggest covariance near-singularity.\n")
cat("  3. If Newey-West lag 2/6/12 and QS-prewhitened HAC also reject at 5%,\n")
cat("     the substantive instability conclusion is robust even if magnitudes differ.\n")
cat("  4. If alternative HAC estimators do not reject, the default-HAC result should\n")
cat("     not be used as primary inferential evidence.\n")
cat("  5. This diagnostic does not change Bai-Perron break dates by itself.\n\n")

cat("Tables written to:\n")
cat("  output/tables/phase3/china_hac_covariance_diagnostics.csv\n")
cat("  output/tables/phase3/china_hac_supf_sensitivity.csv\n")
cat("  output/tables/phase3/china_hac_bandwidth_diagnostics.csv\n")
cat("  output/tables/phase3/china_hac_inferential_stability.csv\n")
