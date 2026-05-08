############################################################
# BAIM 640 - Phase 3
# Finance Monitoring Agent Shiny Interface
# Error-Fixed Version
############################################################

library(shiny)
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(tibble)

############################################################
# SOURCE PHASE 2 BACKEND
############################################################

source("PHASE-2_Finance_Monitoring_Agent1.R")

############################################################
# FILE PATHS
############################################################

summary_file <- file.path(output_dir, "summary_metrics_phase2.csv")
expense_file <- file.path(output_dir, "flagged_unusual_expenses_phase2.csv")
budget_results_file <- file.path(output_dir, "budget_variance_results_phase2.csv")
risk_file <- file.path(output_dir, "flagged_accounting_risk_phase2.csv")
summary_text_file <- file.path(output_dir, "finance_agent_summary.txt")
cashflow_chart <- file.path(output_dir, "monthly_cashflow.png")
budget_chart <- file.path(output_dir, "budget_variance.png")
audit_file <- file.path(output_dir, "agent_audit_log.csv")

cashflow_data_file <- file.path(data_dir, "cashflow_combined_ready.csv")
budget_data_file <- file.path(data_dir, "finance_budget_actual_long_cleaned.csv")
accounting_data_file <- file.path(data_dir, "accounting_data_cleaned_ready.csv")

############################################################
# UI
############################################################

ui <- fluidPage(
  
  titlePanel("Finance Monitoring Agent"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Agent Control Panel"),
      p("This interface allows a non-technical user to run the finance agent, add new records, and review outputs."),
      
      actionButton("run_agent", "Run Finance Agent", class = "btn-primary"),
      br(), br(),
      
      h5("Access Plan"),
      p("Intended users: finance analysts, finance managers, and operations teams."),
      p("Guardrail: Flagged records are recommendations and should be reviewed before final action."),
      
      br(),
      downloadButton("download_summary", "Download Summary Metrics"),
      br(), br(),
      downloadButton("download_expenses", "Download Expense Flags"),
      br(), br(),
      downloadButton("download_budget", "Download Budget Results"),
      br(), br(),
      downloadButton("download_risk", "Download Accounting Risk")
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Overview",
          h3("AI-Powered Budget & Expense Monitoring Agent"),
          p("This agent reviews simulated enterprise finance data and identifies unusual expenses, budget variances, and accounting risk records."),
          verbatimTextOutput("agent_status"),
          h4("Executive Summary"),
          verbatimTextOutput("executive_summary")
        ),
        
        tabPanel(
          "Summary Metrics",
          h3("Summary Metrics"),
          DTOutput("summary_table")
        ),
        
        tabPanel(
          "Unusual Expenses",
          h3("Flagged Unusual Expenses"),
          DTOutput("expense_table")
        ),
        
        tabPanel(
          "Budget Variance",
          h3("Budget Variance Results"),
          DTOutput("budget_table")
        ),
        
        tabPanel(
          "Accounting Risk",
          h3("Flagged Accounting Risk Records"),
          DTOutput("risk_table")
        ),
        
        tabPanel(
          "Add / Update Records",
          h3("Add New Finance Records"),
          p("Use these forms to add new records into the simulated finance datasets. After each submission, the agent reruns automatically."),
          
          tabsetPanel(
            
            tabPanel(
              "Cashflow Record",
              br(),
              fluidRow(
                column(4, dateInput("cf_date", "Date", value = Sys.Date())),
                column(4, selectInput("cf_flow_type", "Flow Type", choices = c("income", "expense"))),
                column(4, textInput("cf_category", "Category", value = "travel"))
              ),
              fluidRow(
                column(4, textInput("cf_account", "Account", value = "company_card")),
                column(4, numericInput("cf_amount", "Amount", value = 100, min = 0)),
                column(4, textInput("cf_currency", "Currency", value = "USD"))
              ),
              fluidRow(
                column(12, textInput("cf_tags", "Description / Tags", value = "new finance record"))
              ),
              actionButton("submit_cashflow", "Submit Cashflow Record", class = "btn-success"),
              br(), br(),
              verbatimTextOutput("cashflow_status")
            ),
            
            tabPanel(
              "Budget / Actual Record",
              br(),
              fluidRow(
                column(4, textInput("ba_month", "Month", value = format(Sys.Date(), "%Y-%m"))),
                column(4, selectInput("ba_scenario", "Scenario", choices = c("budget", "actual"))),
                column(4, textInput("ba_category", "Cost Category", value = "marketing_costs"))
              ),
              fluidRow(
                column(4, numericInput("ba_amount", "Amount", value = 1000, min = 0))
              ),
              actionButton("submit_budget", "Submit Budget/Actual Record", class = "btn-success"),
              br(), br(),
              verbatimTextOutput("budget_status")
            ),
            
            tabPanel(
              "Accounting Record",
              br(),
              fluidRow(
                column(4, dateInput("acc_date", "Date", value = Sys.Date())),
                column(4, textInput("acc_account_type", "Account Type", value = "operating_expense")),
                column(4, numericInput("acc_transaction_amount", "Transaction Amount", value = 500, min = 0))
              ),
              fluidRow(
                column(4, numericInput("acc_accuracy_score", "Accuracy Score", value = 0.95, min = 0, max = 1)),
                column(4, numericInput("acc_processing_time", "Processing Time Seconds", value = 30, min = 0)),
                column(4, selectInput("acc_missing", "Missing Data Indicator", choices = c(0, 1)))
              ),
              actionButton("submit_accounting", "Submit Accounting Record", class = "btn-success"),
              br(), br(),
              verbatimTextOutput("accounting_status")
            )
          )
        ),
        
        tabPanel(
          "Charts",
          h3("Monthly Cash Flow"),
          imageOutput("cashflow_plot"),
          h3("Top Budget Variances"),
          imageOutput("budget_plot")
        ),
        
        tabPanel(
          "Audit Log",
          h3("Agent Audit Log"),
          DTOutput("audit_table")
        )
      )
    )
  )
)

############################################################
# SERVER
############################################################

server <- function(input, output, session) {
  
  agent_ran <- reactiveVal(FALSE)
  refresh_counter <- reactiveVal(0)
  last_run_time <- reactiveVal(NULL)
  
  refresh_outputs <- function() {
    refresh_counter(refresh_counter() + 1)
    last_run_time(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    agent_ran(TRUE)
  }
  
  ##########################################################
  # RUN AGENT BUTTON
  ##########################################################
  
  observeEvent(input$run_agent, {
    
    output$agent_status <- renderText({
      "Running Finance Agent... Please wait."
    })
    
    tryCatch({
      run_finance_monitoring_agent()
      refresh_outputs()
      
      output$agent_status <- renderText({
        paste(
          "Finance Agent Status: Completed Successfully",
          "\nOutputs refreshed from:", output_dir,
          "\nLast run time:", last_run_time()
        )
      })
      
    }, error = function(e) {
      output$agent_status <- renderText({
        paste("Error running agent:", e$message)
      })
    })
  })
  
  ##########################################################
  # STATUS
  ##########################################################
  
  output$agent_status <- renderText({
    if (agent_ran()) {
      paste(
        "Finance Agent Status: Completed Successfully",
        "\nOutputs refreshed from:", output_dir,
        "\nLast run time:", last_run_time()
      )
    } else {
      "Click 'Run Finance Agent' to view or refresh the latest finance monitoring outputs."
    }
  })
  
  ##########################################################
  # EXECUTIVE SUMMARY
  ##########################################################
  
  output$executive_summary <- renderText({
    refresh_counter()
    if (file.exists(summary_text_file)) {
      paste(readLines(summary_text_file), collapse = "\n")
    } else {
      "Executive summary file not found. Please run the Finance Agent first."
    }
  })
  
  ##########################################################
  # TABLE OUTPUTS
  ##########################################################
  
  output$summary_table <- renderDT({
    refresh_counter()
    req(file.exists(summary_file))
    datatable(read_csv(summary_file, show_col_types = FALSE), options = list(pageLength = 10))
  })
  
  output$expense_table <- renderDT({
    refresh_counter()
    req(file.exists(expense_file))
    datatable(read_csv(expense_file, show_col_types = FALSE), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$budget_table <- renderDT({
    refresh_counter()
    req(file.exists(budget_results_file))
    datatable(read_csv(budget_results_file, show_col_types = FALSE), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$risk_table <- renderDT({
    refresh_counter()
    req(file.exists(risk_file))
    datatable(read_csv(risk_file, show_col_types = FALSE), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$audit_table <- renderDT({
    refresh_counter()
    req(file.exists(audit_file))
    datatable(read_csv(audit_file, show_col_types = FALSE), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  ##########################################################
  # CHART OUTPUTS
  ##########################################################
  
  output$cashflow_plot <- renderImage({
    refresh_counter()
    req(file.exists(cashflow_chart))
    list(src = cashflow_chart, contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)
  
  output$budget_plot <- renderImage({
    refresh_counter()
    req(file.exists(budget_chart))
    list(src = budget_chart, contentType = "image/png", width = "100%")
  }, deleteFile = FALSE)
  
  ##########################################################
  # ADD CASHFLOW RECORD
  ##########################################################
  
  observeEvent(input$submit_cashflow, {
    
    tryCatch({
      
      cashflow_data <- read_csv(cashflow_data_file, show_col_types = FALSE)
      
      cashflow_data <- cashflow_data %>%
        mutate(
          record_id = as.character(record_id),
          date_time = as.character(date_time),
          date = as.character(date),
          month = as.character(month),
          flow_type = as.character(flow_type),
          category = as.character(category),
          account = as.character(account),
          amount = as.numeric(amount),
          currency = as.character(currency),
          tags = as.character(tags)
        )
      
      new_cashflow <- tibble(
        record_id = as.character(max(as.numeric(cashflow_data$record_id), na.rm = TRUE) + 1),
        date_time = as.character(Sys.time()),
        date = as.character(input$cf_date),
        month = as.character(format(as.Date(input$cf_date), "%Y-%m")),
        flow_type = as.character(input$cf_flow_type),
        category = as.character(str_to_lower(input$cf_category)),
        account = as.character(input$cf_account),
        amount = as.numeric(input$cf_amount),
        currency = as.character(input$cf_currency),
        tags = as.character(input$cf_tags)
      )
      
      cashflow_updated <- bind_rows(cashflow_data, new_cashflow)
      write_csv(cashflow_updated, cashflow_data_file)
      
      run_finance_monitoring_agent()
      refresh_outputs()
      
      output$cashflow_status <- renderText({
        paste(
          "Cashflow record added successfully.",
          "\nFlow type:", input$cf_flow_type,
          "\nCategory:", input$cf_category,
          "\nAmount:", input$cf_amount,
          "\nDataset updated and agent reran successfully.",
          "\nTime:", last_run_time()
        )
      })
      
    }, error = function(e) {
      output$cashflow_status <- renderText({
        paste("Error adding cashflow record:", e$message)
      })
    })
  })
  
  ##########################################################
  # ADD BUDGET / ACTUAL RECORD
  ##########################################################
  
  observeEvent(input$submit_budget, {
    
    tryCatch({
      
      budget_data <- read_csv(budget_data_file, show_col_types = FALSE)
      
      budget_data <- budget_data %>%
        mutate(
          month = as.character(month),
          scenario = as.character(scenario),
          cost_category = as.character(cost_category),
          amount = as.numeric(amount)
        )
      
      new_budget <- tibble(
        month = as.character(input$ba_month),
        scenario = as.character(input$ba_scenario),
        cost_category = as.character(str_to_lower(input$ba_category)),
        amount = as.numeric(input$ba_amount)
      )
      
      budget_updated <- bind_rows(budget_data, new_budget)
      write_csv(budget_updated, budget_data_file)
      
      run_finance_monitoring_agent()
      refresh_outputs()
      
      output$budget_status <- renderText({
        paste(
          "Budget/Actual record added successfully.",
          "\nScenario:", input$ba_scenario,
          "\nCategory:", input$ba_category,
          "\nAmount:", input$ba_amount,
          "\nDataset updated and agent reran successfully.",
          "\nTime:", last_run_time()
        )
      })
      
    }, error = function(e) {
      output$budget_status <- renderText({
        paste("Error adding budget/actual record:", e$message)
      })
    })
  })
  
  ##########################################################
  # ADD ACCOUNTING RECORD
  ##########################################################
  
  observeEvent(input$submit_accounting, {
    
    tryCatch({
      
      accounting_data <- read_csv(accounting_data_file, show_col_types = FALSE)
      
      accounting_data <- accounting_data %>%
        mutate(
          transaction_id = as.character(transaction_id),
          date = as.character(date),
          account_type = as.character(account_type),
          transaction_amount = as.numeric(transaction_amount),
          cash_flow = as.numeric(cash_flow),
          net_income = as.numeric(net_income),
          revenue = as.numeric(revenue),
          expenditure = as.numeric(expenditure),
          profit_margin = as.numeric(profit_margin),
          debt_to_equity_ratio = as.numeric(debt_to_equity_ratio),
          operating_expenses = as.numeric(operating_expenses),
          gross_profit = as.numeric(gross_profit),
          transaction_volume = as.numeric(transaction_volume),
          processing_time_seconds = as.numeric(processing_time_seconds),
          accuracy_score = as.numeric(accuracy_score),
          missing_data_indicator = as.numeric(missing_data_indicator),
          normalized_transaction_amount = as.numeric(normalized_transaction_amount),
          transaction_outcome = as.character(transaction_outcome)
        )
      
      new_accounting <- tibble(
        transaction_id = as.character(max(as.numeric(accounting_data$transaction_id), na.rm = TRUE) + 1),
        date = as.character(input$acc_date),
        account_type = as.character(input$acc_account_type),
        transaction_amount = as.numeric(input$acc_transaction_amount),
        cash_flow = as.numeric(input$acc_transaction_amount),
        net_income = NA_real_,
        revenue = NA_real_,
        expenditure = as.numeric(input$acc_transaction_amount),
        profit_margin = NA_real_,
        debt_to_equity_ratio = NA_real_,
        operating_expenses = as.numeric(input$acc_transaction_amount),
        gross_profit = NA_real_,
        transaction_volume = as.numeric(1),
        processing_time_seconds = as.numeric(input$acc_processing_time),
        accuracy_score = as.numeric(input$acc_accuracy_score),
        missing_data_indicator = as.numeric(input$acc_missing),
        normalized_transaction_amount = NA_real_,
        transaction_outcome = as.character("new_user_entry")
      )
      
      accounting_updated <- bind_rows(accounting_data, new_accounting)
      write_csv(accounting_updated, accounting_data_file)
      
      run_finance_monitoring_agent()
      refresh_outputs()
      
      output$accounting_status <- renderText({
        paste(
          "Accounting record added successfully.",
          "\nTransaction amount:", input$acc_transaction_amount,
          "\nAccuracy score:", input$acc_accuracy_score,
          "\nDataset updated and agent reran successfully.",
          "\nTime:", last_run_time()
        )
      })
      
    }, error = function(e) {
      output$accounting_status <- renderText({
        paste("Error adding accounting record:", e$message)
      })
    })
  })
  
  ##########################################################
  # DOWNLOAD HANDLERS
  ##########################################################
  
  output$download_summary <- downloadHandler(
    filename = function() "summary_metrics_phase2.csv",
    content = function(file) file.copy(summary_file, file)
  )
  
  output$download_expenses <- downloadHandler(
    filename = function() "flagged_unusual_expenses_phase2.csv",
    content = function(file) file.copy(expense_file, file)
  )
  
  output$download_budget <- downloadHandler(
    filename = function() "budget_variance_results_phase2.csv",
    content = function(file) file.copy(budget_results_file, file)
  )
  
  output$download_risk <- downloadHandler(
    filename = function() "flagged_accounting_risk_phase2.csv",
    content = function(file) file.copy(risk_file, file)
  )
}

############################################################
# RUN APP
############################################################

shinyApp(ui = ui, server = server)