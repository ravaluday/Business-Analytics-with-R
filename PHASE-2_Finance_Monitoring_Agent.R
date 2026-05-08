############################################################
# BAIM 640 - Phase 2
# Enhanced Finance Monitoring Agent Backend
############################################################

rm(list = ls())

required_packages <- c(
  "readr", "dplyr", "tidyr", "lubridate",
  "stringr", "ggplot2", "scales", "tibble"
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(scales)
library(tibble)

############################################################
# 1. PATH SETUP
############################################################

base_dir <- "C:/Users/udayr/Downloads"

data_dir <- file.path(base_dir, "cleaned_finance_datasets")
output_dir <- file.path(base_dir, "phase2_outputs")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cashflow_file <- file.path(data_dir, "cashflow_combined_ready.csv")
budget_file <- file.path(data_dir, "finance_budget_actual_long_cleaned.csv")
accounting_file <- file.path(data_dir, "accounting_data_cleaned_ready.csv")
############################################################
# 2. SAFE READ FUNCTION
############################################################

safe_read_csv <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  read_csv(file_path, show_col_types = FALSE)
}

############################################################
# 3. SIMULATED ERP FEED
############################################################

simulate_erp_feed <- function() {
  cat("Simulated ERP feed started...\n")
  cat("Reading finance files as simulated SAP/ERP system inputs...\n")
  
  list(
    cashflow = safe_read_csv(cashflow_file),
    budget = safe_read_csv(budget_file),
    accounting = safe_read_csv(accounting_file)
  )
}

############################################################
# 4. DATA VALIDATION
############################################################

validate_agent_inputs <- function(data) {
  if (nrow(data$cashflow) == 0) stop("Cashflow dataset is empty.")
  if (nrow(data$budget) == 0) stop("Budget dataset is empty.")
  if (nrow(data$accounting) == 0) stop("Accounting dataset is empty.")
  
  cat("Input validation completed successfully.\n")
}

############################################################
# 5. PREPARE DATA
############################################################

prepare_agent_data <- function(data) {
  
  cashflow <- data$cashflow %>%
    mutate(
      date = as.Date(date),
      month = format(date, "%Y-%m"),
      flow_type = str_to_lower(as.character(flow_type)),
      category = str_to_lower(as.character(category)),
      amount = as.numeric(amount)
    )
  
  budget <- data$budget %>%
    mutate(
      month = as.character(month),
      scenario = str_to_lower(as.character(scenario)),
      cost_category = str_to_lower(as.character(cost_category)),
      amount = as.numeric(amount)
    )
  
  accounting <- data$accounting %>%
    mutate(
      transaction_id = as.numeric(transaction_id),
      transaction_amount = as.numeric(transaction_amount),
      accuracy_score = as.numeric(accuracy_score),
      processing_time_seconds = as.numeric(processing_time_seconds),
      missing_data_indicator = as.numeric(missing_data_indicator)
    )
  
  list(
    cashflow = cashflow,
    budget = budget,
    accounting = accounting
  )
}

############################################################
# 6. EXPENSE RISK ENGINE
############################################################

flag_unusual_expenses <- function(cashflow) {
  
  expense_data <- cashflow %>%
    filter(flow_type == "expense") %>%
    group_by(category) %>%
    mutate(
      category_avg = mean(amount, na.rm = TRUE),
      category_sd = sd(amount, na.rm = TRUE),
      category_sd = ifelse(is.na(category_sd), 0, category_sd),
      threshold = category_avg + (2 * category_sd),
      risk_score = ifelse(threshold > 0, amount / threshold, 0),
      risk_level = case_when(
        amount > threshold * 1.50 ~ "High Risk",
        amount > threshold ~ "Medium Risk",
        TRUE ~ "Normal"
      ),
      flag_reason = case_when(
        risk_level == "High Risk" ~ "Expense is significantly higher than normal category pattern.",
        risk_level == "Medium Risk" ~ "Expense is above expected category threshold.",
        TRUE ~ "Expense appears normal."
      ),
      recommendation = case_when(
        risk_level == "High Risk" ~ "Request receipt, manager approval, and finance review.",
        risk_level == "Medium Risk" ~ "Review transaction details before approval.",
        TRUE ~ "No action required."
      )
    ) %>%
    ungroup() %>%
    filter(risk_level != "Normal")
  
  expense_data
}

############################################################
# 7. BUDGET VARIANCE ENGINE
############################################################

calculate_budget_variance <- function(budget) {
  
  budget_clean <- budget %>%
    group_by(month, cost_category, scenario) %>%
    summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")
  
  budget_wide <- budget_clean %>%
    pivot_wider(
      names_from = scenario,
      values_from = amount,
      values_fill = 0
    )
  
  if (!all(c("actual", "budget") %in% names(budget_wide))) {
    stop("Budget data must contain scenario values: actual and budget.")
  }
  
  budget_results <- budget_wide %>%
    mutate(
      variance = actual - budget,
      variance_percent = ifelse(budget != 0, variance / budget, NA),
      risk_level = case_when(
        variance_percent > 0.20 ~ "High Risk",
        variance_percent > 0.10 ~ "Medium Risk",
        variance > 0 ~ "Low Risk",
        TRUE ~ "Normal"
      ),
      flag_reason = case_when(
        risk_level == "High Risk" ~ "Actual spending exceeds budget by more than 20%.",
        risk_level == "Medium Risk" ~ "Actual spending exceeds budget by more than 10%.",
        risk_level == "Low Risk" ~ "Actual spending is slightly above budget.",
        TRUE ~ "Spending is within budget."
      ),
      recommendation = case_when(
        risk_level == "High Risk" ~ "Immediate finance review required. Investigate cost drivers.",
        risk_level == "Medium Risk" ~ "Monitor spending and review upcoming expenses.",
        risk_level == "Low Risk" ~ "Track category closely in the next reporting cycle.",
        TRUE ~ "No action required."
      )
    )
  
  budget_results
}

############################################################
# 8. ACCOUNTING RISK ENGINE
############################################################

flag_accounting_risk <- function(accounting) {
  
  processing_threshold <- quantile(
    accounting$processing_time_seconds,
    probs = 0.90,
    na.rm = TRUE
  )
  
  accounting_risk <- accounting %>%
    mutate(
      missing_data_risk = ifelse(missing_data_indicator == 1, 1, 0),
      accuracy_risk = ifelse(accuracy_score < 0.85, 1, 0),
      processing_risk = ifelse(processing_time_seconds > processing_threshold, 1, 0),
      total_risk_score = missing_data_risk + accuracy_risk + processing_risk,
      risk_level = case_when(
        total_risk_score >= 3 ~ "High Risk",
        total_risk_score == 2 ~ "Medium Risk",
        total_risk_score == 1 ~ "Low Risk",
        TRUE ~ "Normal"
      ),
      flag_reason = case_when(
        risk_level == "High Risk" ~ "Record has multiple quality or processing concerns.",
        risk_level == "Medium Risk" ~ "Record has more than one risk indicator.",
        risk_level == "Low Risk" ~ "Record has one risk indicator.",
        TRUE ~ "No accounting risk detected."
      ),
      recommendation = case_when(
        risk_level == "High Risk" ~ "Send to accounting review queue immediately.",
        risk_level == "Medium Risk" ~ "Review before financial close.",
        risk_level == "Low Risk" ~ "Monitor and verify if needed.",
        TRUE ~ "No action required."
      )
    ) %>%
    filter(risk_level != "Normal")
  
  accounting_risk
}

############################################################
# 9. SUMMARY METRICS
############################################################

create_summary_metrics <- function(cashflow, budget_results, accounting, expense_flags, accounting_risk) {
  
  summary_metrics <- tibble(
    metric = c(
      "Total Cashflow Records Reviewed",
      "Total Expense Flags",
      "Total Budget Categories Reviewed",
      "Budget Items Above Plan",
      "Accounting Records Reviewed",
      "Accounting Risk Records",
      "High Risk Expense Flags",
      "High Risk Budget Flags",
      "High Risk Accounting Flags"
    ),
    value = c(
      nrow(cashflow),
      nrow(expense_flags),
      nrow(budget_results),
      sum(budget_results$risk_level != "Normal", na.rm = TRUE),
      nrow(accounting),
      nrow(accounting_risk),
      sum(expense_flags$risk_level == "High Risk", na.rm = TRUE),
      sum(budget_results$risk_level == "High Risk", na.rm = TRUE),
      sum(accounting_risk$risk_level == "High Risk", na.rm = TRUE)
    )
  )
  
  summary_metrics
}

############################################################
# 10. CHARTS
############################################################

create_agent_charts <- function(cashflow, budget_results) {
  
  monthly_cashflow <- cashflow %>%
    group_by(month, flow_type) %>%
    summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop")
  
  p1 <- ggplot(monthly_cashflow, aes(x = month, y = total_amount, fill = flow_type)) +
    geom_col(position = "dodge") +
    labs(
      title = "Monthly Cash Flow by Type",
      x = "Month",
      y = "Amount",
      fill = "Flow Type"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    filename = file.path(output_dir, "monthly_cashflow.png"),
    plot = p1,
    width = 10,
    height = 6
  )
  
  top_variance <- budget_results %>%
    arrange(desc(abs(variance))) %>%
    slice_head(n = 10)
  
  p2 <- ggplot(top_variance, aes(x = reorder(cost_category, variance), y = variance)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Top Budget Variances",
      x = "Cost Category",
      y = "Variance (Actual - Budget)"
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path(output_dir, "budget_variance.png"),
    plot = p2,
    width = 10,
    height = 6
  )
}

############################################################
# 11. EXECUTIVE SUMMARY REPORT
############################################################

create_executive_summary <- function(summary_metrics, budget_results) {
  
  top_budget_issue <- budget_results %>%
    filter(variance > 0) %>%
    arrange(desc(variance)) %>%
    slice(1)
  
  if (nrow(top_budget_issue) == 0) {
    top_category <- "No over-budget category detected"
    top_variance <- 0
    top_risk <- "Normal"
  } else {
    top_category <- top_budget_issue$cost_category
    top_variance <- round(top_budget_issue$variance, 2)
    top_risk <- top_budget_issue$risk_level
  }
  
  summary_text <- paste0(
    "Finance Monitoring Agent - Phase 2 Executive Summary\n",
    "Generated on: ", Sys.time(), "\n\n",
    "The enhanced Finance Monitoring Agent reviewed financial records across cash flow, budget, and accounting datasets. ",
    "The agent identified unusual expenses, budget variance concerns, and accounting risk records using rule-based decision logic.\n\n",
    
    "Key Results:\n",
    "- Total cashflow records reviewed: ", summary_metrics$value[summary_metrics$metric == "Total Cashflow Records Reviewed"], "\n",
    "- Total unusual expense flags: ", summary_metrics$value[summary_metrics$metric == "Total Expense Flags"], "\n",
    "- Budget items above plan: ", summary_metrics$value[summary_metrics$metric == "Budget Items Above Plan"], "\n",
    "- Accounting risk records: ", summary_metrics$value[summary_metrics$metric == "Accounting Risk Records"], "\n\n",
    
    "Highest Over-Budget Category:\n",
    "- Cost Category: ", top_category, "\n",
    "- Variance: ", top_variance, "\n",
    "- Risk Level: ", top_risk, "\n\n",
    
    "Recommended Management Action:\n",
    "Finance users should review high-risk expense flags, investigate over-budget categories, and prioritize accounting records with missing data, low accuracy scores, or high processing times.\n"
  )
  
  writeLines(summary_text, file.path(output_dir, "finance_agent_summary.txt"))
}

############################################################
# 12. AUDIT LOG
############################################################

create_audit_log <- function(summary_metrics) {
  
  audit_entry <- tibble(
    run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    total_cashflow_records = summary_metrics$value[summary_metrics$metric == "Total Cashflow Records Reviewed"],
    total_expense_flags = summary_metrics$value[summary_metrics$metric == "Total Expense Flags"],
    total_budget_items_above_plan = summary_metrics$value[summary_metrics$metric == "Budget Items Above Plan"],
    total_accounting_risk_records = summary_metrics$value[summary_metrics$metric == "Accounting Risk Records"],
    output_folder = output_dir
  )
  
  audit_file <- file.path(output_dir, "agent_audit_log.csv")
  
  if (file.exists(audit_file)) {
    old_log <- read_csv(audit_file, show_col_types = FALSE) %>%
      mutate(run_timestamp = as.character(run_timestamp))
    
    audit_entry <- audit_entry %>%
      mutate(run_timestamp = as.character(run_timestamp))
    
    new_log <- bind_rows(old_log, audit_entry)
    write_csv(new_log, audit_file)
  } else {
    write_csv(audit_entry, audit_file)
  }
}

############################################################
# 13. EXPORT OUTPUTS
############################################################

export_agent_outputs <- function(expense_flags, budget_results, accounting_risk, summary_metrics) {
  
  write_csv(expense_flags, file.path(output_dir, "flagged_unusual_expenses_phase2.csv"))
  write_csv(budget_results, file.path(output_dir, "budget_variance_results_phase2.csv"))
  write_csv(accounting_risk, file.path(output_dir, "flagged_accounting_risk_phase2.csv"))
  write_csv(summary_metrics, file.path(output_dir, "summary_metrics_phase2.csv"))
  
  cat("All Phase 2 outputs exported successfully.\n")
}

############################################################
# 14. MAIN AGENT RUNNER
############################################################

run_finance_monitoring_agent <- function() {
  
  cat("--------------------------------------------------\n")
  cat("Finance Monitoring Agent Started\n")
  cat("--------------------------------------------------\n")
  
  data <- simulate_erp_feed()
  validate_agent_inputs(data)
  
  prepared_data <- prepare_agent_data(data)
  
  expense_flags <- flag_unusual_expenses(prepared_data$cashflow)
  budget_results <- calculate_budget_variance(prepared_data$budget)
  accounting_risk <- flag_accounting_risk(prepared_data$accounting)
  
  summary_metrics <- create_summary_metrics(
    prepared_data$cashflow,
    budget_results,
    prepared_data$accounting,
    expense_flags,
    accounting_risk
  )
  
  export_agent_outputs(
    expense_flags,
    budget_results,
    accounting_risk,
    summary_metrics
  )
  
  create_agent_charts(prepared_data$cashflow, budget_results)
  create_executive_summary(summary_metrics, budget_results)
  create_audit_log(summary_metrics)
  
  cat("--------------------------------------------------\n")
  cat("Finance Monitoring Agent Completed Successfully\n")
  cat("Outputs saved to:", output_dir, "\n")
  cat("--------------------------------------------------\n")
  
  return(summary_metrics)
}

############################################################
# 15. RUN AGENT
############################################################

agent_results <- run_finance_monitoring_agent()
print(agent_results)