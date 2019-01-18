library(shiny)
library(shinythemes)
library(tidyverse)
source("source.R")

x <- list(list(name = "Loan 1", balance = 13390.79, int = 0.0445, min_pay = 275.54),
          list(name = "Loan 2", balance = 2303.40, int = 0.0535, min_pay = 38.04),
          list(name = "Loan 3", balance = 3520.73, int = 0.0315, min_pay = 54.32),
          list(name = "Loan 4", balance = 940.62, int = 0.0655, min_pay = 31.09),
          list(name = "Loan 5", balance = 3427.62, int = 0.0655, min_pay = 95.80))

word_num <- function(word, i){
  sprintf("%s%s", word, i)
}

server <- function(input, output){
  
  counter <- reactiveValues(n = 0)
  
  observeEvent(input$add_loan, {
    
    counter$n <- counter$n + 1
    i <- counter$n
    
    if(input$fillin & counter$n <= length(x)){
      value_list <- list(name = x[[i]]$name,
                         balance = x[[i]]$balance,
                         min_pay = x[[i]]$min_pay,
                         int = x[[i]]$int)
    } else {
      value_list <- list(name = "", balance = NA, min_pay = NA, int = NA)
    }
    
    insertUI(
      selector = "#add_loan", 
      where = "beforeBegin",
      ui = tags$div(id = sprintf("loan_%s", i),
                    h4(sprintf("Loan #%s", i)),
                    textInput(word_num("loan", i), label = "Name", value = value_list$name),
                    numericInput(word_num("bal", i), label = "Remaining balance", value = value_list$bal, min = 0, step = 20),
                    numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = value_list$min_pay, min = 10, step = 5),
                    numericInput(word_num("int", i), label = "Interest Rate", value = value_list$int, min = 0, max = 1, step = 0.01),
                    br()) 
    )
  })
  
  observeEvent(input$minus_loan, {
    counter$n <- counter$n - 1
    if(counter$n < 0) counter$n <- 0
    
    i <- counter$n
    
    remove_loan <- sprintf("#loan_%s", i+1)
    removeUI(selector = remove_loan)
  })

  observeEvent(input$fillin, { 
    
    #if(input$fillin == FALSE){
      
    for(i in 1:counter$n){
      removeUI(selector = sprintf("#loan_%s", i))
    }
    
    if(input$fillin == FALSE){
      if(counter$n > 0){
        for(i in 1:counter$n){
          insertUI(
            selector = "#add_loan",
            where = "beforeBegin",
            ui = tags$div(id = sprintf("loan_%s", i),
                          h4(sprintf("Loan #%s", i)),
                          textInput(word_num("loan", i), label = "Name", value = ""),
                          numericInput(word_num("bal", i), label = "Remaining balance", value = NA, min = 0, step = 20),
                          numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = NA, min = 10, step = 5),
                          numericInput(word_num("int", i), label = "Interest Rate", value = NA, min = 0, max = 1, step = 0.01),
                          br())
          )
        }
      }
    }
    
    if(input$fillin == TRUE & counter$n >=1){

      for(i in 1:counter$n){
        insertUI(
          selector = "#add_loan",
          where = "beforeBegin",
          ui = tags$div(id = sprintf("loan_%s", i),
                        h4(sprintf("Loan #%s", i)),
                        textInput(word_num("loan", i), label = "Name", value = x[[i]]$name),
                        numericInput(word_num("bal", i), label = "Remaining balance", value = x[[i]]$balance, min = 0, step = 20),
                        numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = x[[i]]$min_pay, min = 10, step = 5),
                        numericInput(word_num("int", i), label = "Interest Rate", value = x[[i]]$int, min = 0, max = 1, step = 0.01),
                        br())
        )
      }
    }
  })
  
  loan_list <- eventReactive(input$submit, {
    loans <- purrr::map(1:(counter$n), function(num){
  
      list(name = input[[word_num("loan", num)]],
           balance = input[[word_num("bal", num)]],
           int = input[[word_num("int", num)]],
           min_pay = input[[word_num("min_pay", num)]])
    })
  
    any_miss <- any(unlist(lapply(loans, function(sub) any(is.na(sub)))))
  
    validate(
      need(!any_miss, "Please fill in missing inputs")
    )
  
    loans
  })
  
  options_plot_data <- eventReactive(input$submit, {
    #get_payoff_options(loan_list())
    
    loan_list <- loan_list()
    
    withProgress(message = 'Calculating options\r\n', value = 0, {
      
      min_pay <- lapply(loan_list, function(sub_list){
        sub_list[["min_pay"]]
      }) %>% unlist %>% sum
      
      start_50 <- min_pay - min_pay %% 50 + 50
      
      if(abs(min_pay - start_50) < 20) start_50 <- start_50 + 50
      
      mo_pay_try <- c(NA, min_pay, seq(start_50, ceiling(start_50+500), by = 50))
      
      n <- length(mo_pay_try)
      
      summ_sched_df <- data.frame()
      
      for(mo_pay in mo_pay_try){
        
        mo_pay_display <- ifelse(is.na(mo_pay), min_pay, mo_pay)
        incProgress(1/n, detail = sprintf("$%0.2f/month", mo_pay_display))
        
        sched_df <- conduct_schedule_analysis(loan_info_list = loan_list, 
                                              max_mo_pay = mo_pay, 
                                              int_tiebreak = "higher balance")
        
        summ <- sched_df %>%
          summarize(`Months to pay off` = max(month),
                    `Total interest to be paid` = sum(int_pay)) %>%
          mutate(mo_pay = mo_pay)
        
        summ_sched_df <- bind_rows(summ_sched_df, summ)
      }
      
    })
    
    summ_sched_df
    
  })
  
  output$options_plot <- renderPlot({
    
    plot_payoff_options(options_plot_data())
    
  })
  
  mo_pay_choice <- reactive({
    
    data <- options_plot_data()
    
    min_dat <- data %>%
      filter(is.na(mo_pay))
    
    click <- input$options_click
    
    if((click$panelvar1 == "Months to pay off" & click$y > min_dat$`Months to pay off`) |
       (click$panelvar1 == "Total interest to be paid" & click$y > min_dat$`Total interest to be paid`)){
      x_val <- NA  
      
    } else {
      x_val <- plyr::round_any(click$x, 50)
      
    }
    
    if(!is.na(x_val) & x_val < data$mo_pay[2]) x_val <- data$mo_pay[2]
    
    x_val
  })
  
  sched <- reactive({
    
    req(input$options_click)
    
    x_val <- mo_pay_choice()
    
    conduct_schedule_analysis(loan_list(), max_mo_pay = x_val)
    
  })
  
  values <- reactiveValues(test = NULL)
  
  observeEvent(loan_list(), {
    if(!is.null(loan_list())) {
      values$test <- loan_list()
      removeUI(selector = "#heading1")
    } 
  })
  
  heading1_str <- eventReactive(input$options_click, {
    "Payment plan"
  })
  
  output$heading1 <- renderText({
    heading1_str()
  })
  
  output$choice_info <- renderTable({
    req(input$options_click)
    
    data <- options_plot_data()
    
    click_val <- mo_pay_choice()
    
    values <- data %>% 
      filter(mo_pay %in% c(NA, click_val)) %>%
      mutate(`Total interest to be paid` = sprintf("$%0.2f", `Total interest to be paid`),
             `Months to pay off` = sprintf("%0.0f", `Months to pay off`),
             mo_pay = sprintf("$%0.2f", mo_pay)) %>%
      rename(`Monthly payment` = mo_pay) %>%
      select(`Monthly payment`, `Months to pay off`, `Total interest to be paid`) 
    
    values[values$`Monthly payment` == "$NA", "Monthly payment"] = "Minimum"
    
    values
    
  })
  
  output$heading2 <- eventReactive(input$options_click, {
    if(!is.na(mo_pay_choice())) "Action items"
    else ""
  })
  
  output$heading3 <- eventReactive(input$options_click, {
    "Payments by month under payment plan"
  })
  
  output$heading4 <- eventReactive(input$options_click, {
    "Total balance across loans over time"
  })
  
  output$schedule_plot <- renderPlot({
    plot_mo_payments(payment_sched = sched())
  })
  
  output$balance_plot <- renderPlot({
    plot_balance_over_time(payment_sched = sched())
  })
  
  output$action_items <- renderText({
    req(input$options_click)
    
    loan_overpay <- sched() %>% 
      filter(month == 1) %>% 
      filter(payment > min_pay) 
    
    name <- loan_overpay$name
    total <- loan_overpay$payment
    amount_overpay <- total - loan_overpay$min_pay
    
    sprintf("Call loan servicer for loan '%s' and raise monthly payments 
            from $%0.2f to $%0.2f", 
            name, total - amount_overpay, total)
  })
  
}


ui <- fluidPage(theme = shinytheme("cosmo"),
  
  titlePanel("Loan Repayment Calculator"),
  br(),

  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      tags$head(
        tags$style(type="text/css", "label.control-label, .form-group.shiny-bound-input{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
      ),
      
      checkboxInput("fillin", "Fill in example loan figures?", value = TRUE),
      actionButton("add_loan", label = "", icon = icon("plus-square")),
      actionButton("minus_loan", label = "", icon = icon("minus-square")),
      h5(""),
      actionButton("submit", "Show me my options")
    ),
    
    mainPanel(
      
      plotOutput(outputId = "options_plot", click = "options_click"),
      
      h4(textOutput("heading1")),
      tableOutput(outputId = "choice_info"),
      
      h4(textOutput("heading2")),
      textOutput("action_items"),
      br(),
      
      h4(textOutput("heading3")), 
      plotOutput(outputId = "schedule_plot"),
      
      h4(textOutput("heading4")), 
      plotOutput(outputId = "balance_plot")
    
    )
  )
)

shinyApp(
  ui = ui,
  server = server
)

