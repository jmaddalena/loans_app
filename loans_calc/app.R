library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
source("source.R")

x <- list(list(name = "Commonbond", balance = 12916.83, int = 4.45, min_pay = 275.54),
          list(name = "Navient 1", balance = 2243.34, int = 5.35, min_pay = 38.04),
          list(name = "Navient 2", balance = 3426.51, int = 3.15, min_pay = 54.32),
          list(name = "Navient 3", balance = 933.43, int = 6.55, min_pay = 31.09),
          list(name = "Navient 4", balance = 3217.86, int = 6.55, min_pay = 95.80))

# x <- list(list(name = "loan 1", balance = 5000, int = 4.5, min_pay = 93.22),
#            list(name = "loan 2", balance = 8000, int = 3.5, min_pay = 145.53),
#           list(name = "loan 3", balance = 10000, int = 4.5, min_pay = 158.74))


word_num <- function(word, i){
  sprintf("%s%s", word, i)
}

server <- function(input, output, session){
  
  num_loans <- reactive(input$num_loans)
  
  prev_loans <- reactiveValues(n = 0)
  
  observeEvent(input$submit, {
    updateTabsetPanel(session, 
                      inputId = "intabset",
                      selected = "Overview"
    )
  })
  
  observeEvent(c(num_loans(), input$submit), {
    
    removeUI(selector = "#inserted_ui")
  
    acc_list <- purrr::map(1:input$num_loans, function(i){
      
      # if new loan and within length of x
      if(i > prev_loans$n & i <= length(x)){
        value_list <- list(name = x[[i]]$name,
                           balance = x[[i]]$balance,
                           min_pay = x[[i]]$min_pay,
                           int = x[[i]]$int) 
      # if new loan but beyond length of x
      } else if(i > prev_loans$n){
        value_list <- list(name = NA,
                           balance = NA,
                           min_pay = NA,
                           int = NA)
      # if not new loan
      } else {
        name <- loan_list()[[i]]$name
        balance <- loan_list()[[i]]$balance
        min_pay <- loan_list()[[i]]$min_pay
        int <- loan_list()[[i]]$int
    
        value_list <- list(name = ifelse(is.null(name), NA, name),
                           balance = ifelse(is.null(balance), NA, balance),
                           min_pay =  ifelse(is.null(min_pay), NA, min_pay),
                           int = ifelse(is.null(int), NA, int))
      }
      
      coll <- ifelse(i == 1, FALSE, TRUE)
      
      accordionItem(
        id = word_num("acc_", i),
        title = word_num("LOAN ", i),
        collapsed = TRUE,
        textInput(word_num("loan", i), label = "Name", value = value_list$name),
        numericInput(word_num("bal", i), label = "Remaining balance ($)", value = value_list$bal, min = 0, step = 20),
        numericInput(word_num("min_pay", i), label = "Minimum monthly payment ($)", value = value_list$min_pay, min = 10, step = 5),
        numericInput(word_num("int", i), label = "Interest Rate (%)", value = value_list$int, min = 0, max = 1, step = 0.01)
      )
    })
    
    insertUI(
      selector = "#num_loans",
      where = "afterEnd",
      ui =  tags$div(id = "inserted_ui",
        helpText("Expand each loan to input information"),
        do.call(accordion, acc_list)
      )
    )
    
    prev_loans$n <- input$num_loans
    
  })
  
  # observeEvent(input$submit, {
  #   removeUI("#slider")
  #   removeUI("#one_bar_plot")
  #   removeUI("#action_head")
  #   removeUI("#action_items")
  # })
  
  loans_inputs <- reactive({
    
    purrr::map(1:num_loans(), function(i){
        purrr::map(c("loan", "bal", "min_pay", "int"), function(str){
          input[[word_num(str, i)]]
        })
    })
  })
  
  loan_list <- eventReactive(loans_inputs(), {

    loan_list <- purrr::map(1:num_loans(), function(num){
      
      list(name = input[[word_num("loan", num)]],
           balance = input[[word_num("bal", num)]],
           int = input[[word_num("int", num)]],
           min_pay = input[[word_num("min_pay", num)]])
    })
    
  })
  
  sched_data <- eventReactive(input$submit, {
    
    loan_list <- loan_list()
    
    is_miss <- unlist(lapply(loan_list, function(sub)
      any(is.na(sub))
    ))
    
    which_miss <- which(is_miss)
    
    validate(
      need(!any(is_miss), sprintf("Please fill in missing inputs (Loans %s)", paste(which_miss, collapse = ", ")))
    )
    
    withProgress(message = 'Calculating options\r\n', value = 0, {
      
      min_pay <- lapply(loan_list, function(sub_list){
        sub_list[["min_pay"]]
      }) %>% unlist %>% sum
      
      start_50 <- min_pay - min_pay %% 50 + 50
      
      if(abs(min_pay - start_50) < 20) start_50 <- start_50 + 50
      
      mo_pay_try <- c(NA, min_pay, seq(start_50, ceiling(start_50+500), by = 50))
      
      n <- length(mo_pay_try)
      
      all_sched_df <- data.frame()
      
      factor_levels <- c()      
      
      for(mo_pay in mo_pay_try){
        
        lab <- ifelse(is.na(mo_pay), "Minimum", sprintf("$%0.2f", mo_pay))
        factor_levels <- c(factor_levels, lab)
        
        mo_pay_display <- ifelse(is.na(mo_pay), min_pay, mo_pay)
        incProgress(1/n, detail = sprintf("$%0.2f/month", mo_pay_display))
        
        sched_df <- conduct_schedule_analysis(loan_info_list = loan_list, 
                                              max_mo_pay = mo_pay, 
                                              int_tiebreak = "lower balance") %>%
          mutate(mo_pay = mo_pay,
                 mo_pay_lab = lab)
        
        all_sched_df <- bind_rows(all_sched_df, sched_df)
      }
      
    })
    
    all_sched_df$mo_pay_lab <- factor(all_sched_df$mo_pay_lab, levels = factor_levels)
    
    all_sched_df
    
  })
  
  options_plot_data <- reactive({
    
    sched_data() %>%
      group_by(mo_pay) %>%
      summarize(`Months to pay off` = max(month),
                `Total interest to be paid` = sum(int_pay)) %>%
      mutate(mo_pay = mo_pay) %>%  
      arrange(!is.na(mo_pay), mo_pay)
  })
  
  output$options_plot <- renderPlot({
    
    plot_payoff_options_v2(options_plot_data())
    
  })
  
  # output$all_bar_plots <- renderPlot({
  #   plot_bar_all_options(sched_data())
  # })
  
  same_term <- reactive({
    
    unique_vals <- unique(sched_data()$mo_pay)
    
    max_mo_vals <- sched_data() %>%
      filter(mo_pay %in% unique_vals[1:2]) %>%
      group_by(mo_pay) %>%
      summarize(max_mo = max(month)) 
    
    if(length(unique(max_mo_vals$max_mo)) == 1) TRUE
    else FALSE
  })
  
  sched_mo_pays <- reactive({
    unique(sched_data()$mo_pay_lab)  
  })
  
  observeEvent(sched_data(), {
    
    sched_opt <- sched_mo_pays()
    
    if(same_term()) sched_opt <- sched_opt[-2]
    
    output$slider <- renderUI(
         sliderTextInput("slider", label = h3("Select monthly payment"), 
                          choices = sched_opt,
                         grid = TRUE)
    )
  })
  
  output$one_bar_plot <- renderPlot({
    req(input$slider)
    plot_bar_one_option(sched_data(), mo_pay_use = input$slider)
  })
  
  output$action_head <- renderText({
    req(input$slider)
    if(! input$slider %in% sched_mo_pays()[1:2]) "Action items"
    else ""
  })
  
  output$action_items <- renderText({
    req(input$slider)
  
    pay_slider <- sched_data() %>% 
      filter(mo_pay_lab == input$slider)
    
    loan_overpay <- pay_slider %>%
      filter(month == 1)
    
    loan_overpay <- loan_overpay %>% filter(abs(payment - min_pay) > 1E-10) 
    
    if(nrow(loan_overpay) == 0){
      ""
    } else {
      name <- loan_overpay$name
      total <- loan_overpay$payment
      amount_overpay <- total - loan_overpay$min_pay
      
      sprintf("Call loan servicer for '%s' and raise monthly payments 
              from $%0.2f to $%0.2f.", 
              name, total - amount_overpay, total)
    } 
  })
}

ui <- fluidPage(theme = shinytheme("cosmo"),
                
  includeCSS("styles.css"),
  
  titlePanel("Pay off your loans faster!"),
  helpText("Repaying loans is stressful, but it helps to have a good plan. Let’s see how much time and money you can save by paying more than the minimum required amount each month."),
  helpText(HTML('<p style= font-size: 4pt">NOTE: If any of your loans have variable interest rates, the calculations in this app are only an approximation.</p>')),
  br(),

  sidebarLayout(
    
    sidebarPanel(
      numericInput("num_loans", "How many loans do you have?", value = 1, min = 1, max = 8, step = 1),
      actionButton("submit", "Crunch numbers")
    ),
    
    mainPanel(
      
      tabsetPanel(id = "intabset",
        tabPanel("Overview", 
                 br(), 
                 plotOutput(outputId = "options_plot")),
        tabPanel("Payment Plans", 
                 uiOutput("slider"),
                 plotOutput("one_bar_plot"),
                 br(),
                 h3(textOutput("action_head")),
                 textOutput("action_items")
        )
      )
      
    )
  )
)

shinyApp(
  ui = ui,
  server = server
)

