# ============================================================
# Finance Workflow Automation Agent - Phase 1 Prototype
# Course: BAIM 640
# Language: R
# Purpose: Load cleaned finance datasets, flag exceptions,
#          generate summary outputs, and export results.
# ============================================================

# ---------------------------
# 1. Install / Load Packages
# ---------------------------
required_packages <- c("readr", "dplyr", "tidyr", "lubridate", "ggplot2", "stringr", "scales")

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(scales)

options(scipen = 999)
rm(list = ls())

# ---------------------------
# 2. Paths
# ---------------------------
# Update this path if your folder is somewhere else.
base_dir <- "C:/Users/udayr/Downloads"
data_dir <- file.path(base_dir, "cleaned_finance_datasets")
output_dir <- file.path(base_dir, "finance_agent_outputs")

if (!dir.exists(data_dir)) {
  # fallback: try current working directory directly
  data_dir <- getwd()
}

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cashflow_file   <- file.path(data_dir, "cashflow_combined_ready.csv")
budget_file     <- file.path(data_dir, "finance_budget_actual_long_cleaned.csv")
accounting_file <- file.path(data_dir, "accounting_data_cleaned_ready.csv")

# ---------------------------
# 3. Utility Functions
# ---------------------------
validate_columns <- function(df, required_cols, df_name = "dataframe") {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required columns in ", df_name, ": ",
                paste(missing_cols, collapse = ", ")))
  }
  invisible(TRUE)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) stop(paste("File not found:", path))
  read_csv(path, show_col_types = FALSE)
}

# ---------------------------
# 4. Load Datasets
# ---------------------------
load_datasets <- function() {
  cashflow <- safe_read_csv(cashflow_file)
  budget <- safe_read_csv(budget_file)
  accounting <- safe_read_csv(accounting_file)

  validate_columns(
    cashflow,
    c("record_id", "date", "month", "flow_type", "category", "amount"),
    "cashflow_combined_ready.csv"
  )

  validate_columns(
    budget,
    c("month", "scenario", "cost_category", "amount"),
    "finance_budget_actual_long_cleaned.csv"
  )

  validate_columns(
    accounting,
    c("transaction_id", "date", "transaction_amount", "processing_time_seconds",
      "accuracy_score", "missing_data_indicator"),
    "accounting_data_cleaned_ready.csv"
  )

  cashflow$date <- as.Date(cashflow$date)
  budget$month <- as.Date(budget$month)
  accounting$date <- as.Date(accounting$date)

  list(cashflow = cashflow, budget = budget, accounting = accounting)
}

# ---------------------------
# 5. Expense Threshold Logic
# ---------------------------
build_expense_thresholds <- function(cashflow) {
  expense_data <- cashflow %>%
    filter(flow_type == "expense") %>%
    group_by(category) %>%
    summarise(
      avg_amount = mean(amount, na.rm = TRUE),
      sd_amount = sd(amount, na.rm = TRUE),
      q75 = quantile(amount, 0.75, na.rm = TRUE),
      q90 = quantile(amount, 0.90, na.rm = TRUE),
      max_amount = max(amount, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      sd_amount = ifelse(is.na(sd_amount), 0, sd_amount),
      threshold_amount = pmax(avg_amount + 2 * sd_amount, q90)
    )

  expense_data
}

flag_unusual_expenses <- function(cashflow, thresholds) {
  flagged <- cashflow %>%
    filter(flow_type == "expense") %>%
    left_join(thresholds %>% select(category, threshold_amount, avg_amount, q90),
              by = "category") %>%
    mutate(
      flagged_unusual = ifelse(amount > threshold_amount, TRUE, FALSE),
      severity = case_when(
        amount > (threshold_amount * 1.5) ~ "High",
        amount > threshold_amount ~ "Medium",
        TRUE ~ "Normal"
      )
    ) %>%
    filter(flagged_unusual) %>%
    arrange(desc(amount))

  flagged
}

# ---------------------------
# 6. Budget vs Actual Logic
# ---------------------------
calculate_budget_variance <- function(budget) {
  budget_variance <- budget %>%
    mutate(month = as.Date(month)) %>%
    group_by(month, cost_category, scenario) %>%
    summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = scenario, values_from = amount, values_fill = 0) %>%
    mutate(
      variance = Actual - Budget,
      variance_pct = ifelse(Budget == 0, NA, variance / Budget),
      budget_status = case_when(
        variance > 0 ~ "Over Budget",
        variance < 0 ~ "Under Budget",
        TRUE ~ "On Budget"
      )
    ) %>%
    arrange(desc(variance))

  budget_variance
}

# ---------------------------
# 7. Accounting Risk Logic
# ---------------------------
flag_accounting_risk <- function(accounting) {
  proc_time_cutoff <- quantile(accounting$processing_time_seconds, 0.90, na.rm = TRUE)
  amount_cutoff <- quantile(accounting$transaction_amount, 0.95, na.rm = TRUE)

  flagged <- accounting %>%
    mutate(
      risk_reason = case_when(
        missing_data_indicator == TRUE ~ "Missing Data",
        accuracy_score < 0.90 ~ "Low Accuracy Score",
        processing_time_seconds > proc_time_cutoff ~ "Long Processing Time",
        transaction_amount > amount_cutoff ~ "Very High Transaction Amount",
        TRUE ~ NA_character_
      ),
      flagged_risk = !is.na(risk_reason)
    ) %>%
    filter(flagged_risk) %>%
    arrange(risk_reason, desc(transaction_amount))

  flagged
}

# ---------------------------
# 8. Summary Metrics
# ---------------------------
create_summary_metrics <- function(cashflow, budget_variance, unusual_expenses, accounting_risk) {
  total_income <- cashflow %>%
    filter(flow_type == "income") %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    pull(total)

  total_expense <- cashflow %>%
    filter(flow_type == "expense") %>%
    summarise(total = sum(amount, na.rm = TRUE)) %>%
    pull(total)

  net_cashflow <- total_income - total_expense
  over_budget_count <- sum(budget_variance$budget_status == "Over Budget", na.rm = TRUE)

  tibble(
    metric = c(
      "Total Income",
      "Total Expense",
      "Net Cash Flow",
      "Flagged Unusual Expenses",
      "Over Budget Categories",
      "Flagged Accounting Risk Records"
    ),
    value = c(
      round(total_income, 2),
      round(total_expense, 2),
      round(net_cashflow, 2),
      nrow(unusual_expenses),
      over_budget_count,
      nrow(accounting_risk)
    )
  )
}

# ---------------------------
# 9. Charts
# ---------------------------
plot_monthly_cashflow <- function(cashflow) {
  chart_data <- cashflow %>%
    group_by(month, flow_type) %>%
    summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop")

  p <- ggplot(chart_data, aes(x = month, y = total_amount, fill = flow_type)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Monthly Cash Flow by Type",
      x = "Month",
      y = "Amount",
      fill = "Flow Type"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(output_dir, "monthly_cashflow.png"),
    plot = p, width = 9, height = 5, dpi = 300
  )
  p
}

plot_budget_variance <- function(budget_variance) {
  top_variance <- budget_variance %>%
    arrange(desc(abs(variance))) %>%
    slice_head(n = 15)

  p <- ggplot(top_variance, aes(x = reorder(cost_category, variance), y = variance)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(
      title = "Top Budget Variances",
      x = "Cost Category",
      y = "Variance (Actual - Budget)"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(output_dir, "budget_variance.png"),
    plot = p, width = 9, height = 6, dpi = 300
  )
  p
}

# ---------------------------
# 10. Export Outputs
# ---------------------------
export_outputs <- function(summary_metrics, unusual_expenses, budget_variance, accounting_risk) {
  write_csv(summary_metrics, file.path(output_dir, "summary_metrics.csv"))
  write_csv(unusual_expenses, file.path(output_dir, "flagged_unusual_expenses.csv"))
  write_csv(budget_variance, file.path(output_dir, "budget_variance_results.csv"))
  write_csv(accounting_risk, file.path(output_dir, "flagged_accounting_risk.csv"))
}

# ---------------------------
# 11. Main Workflow
# ---------------------------
run_finance_agent <- function() {
  cat("Loading datasets...\n")
  data_list <- load_datasets()

  cashflow <- data_list$cashflow
  budget <- data_list$budget
  accounting <- data_list$accounting

  cat("Building thresholds...\n")
  thresholds <- build_expense_thresholds(cashflow)

  cat("Flagging unusual expenses...\n")
  unusual_expenses <- flag_unusual_expenses(cashflow, thresholds)

  cat("Calculating budget variance...\n")
  budget_variance <- calculate_budget_variance(budget)

  cat("Flagging accounting risk...\n")
  accounting_risk <- flag_accounting_risk(accounting)

  cat("Creating summary metrics...\n")
  summary_metrics <- create_summary_metrics(
    cashflow, budget_variance, unusual_expenses, accounting_risk
  )

  cat("Exporting outputs...\n")
  export_outputs(summary_metrics, unusual_expenses, budget_variance, accounting_risk)

  cat("Creating charts...\n")
  plot_monthly_cashflow(cashflow)
  plot_budget_variance(budget_variance)

  cat("\n===== FINANCE AGENT SUMMARY =====\n")
  print(summary_metrics)

  cat("\nTop flagged unusual expenses:\n")
  print(head(unusual_expenses, 10))

  cat("\nTop over-budget categories:\n")
  print(head(budget_variance %>% filter(budget_status == "Over Budget"), 10))

  cat("\nTop accounting risk records:\n")
  print(head(accounting_risk, 10))

  invisible(list(
    summary_metrics = summary_metrics,
    unusual_expenses = unusual_expenses,
    budget_variance = budget_variance,
    accounting_risk = accounting_risk
  ))
}

# ---------------------------
# 12. Run the Agent
# ---------------------------
results <- run_finance_agent()

