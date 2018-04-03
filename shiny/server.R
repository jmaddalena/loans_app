library(tidyverse)
source("~/Documents/student_loans/shiny/source.R")

x <- list(salliemae = list(name = "Sallie Mae", bal = 14707.01, int = 0.04375,
                           min_pay = 207.97, fixed = F, months_left = NA),
          navient1 = list(name = "Navient 1", bal = 2575.84, int = 0.0535,
                          min_pay = 38.04, fixed = T, months_left = 85),
          navient2 = list(name = "Navient 2", bal = 3965.99, int = 0.0315,
                          min_pay = 54.32, fixed = T, months_left = 85),
          navient3 = list(name = "Navient 3", bal = 2010.55, int = 0.0655,
                          min_pay = 31.09, fixed = T, months_left = 85),
          navient4 = list(name = "Navient 4", bal = 4448.61, int = 0.0655,
                          min_pay = 95.80, fixed = T, months_left = 85))

word_num <- function(word, i){
  sprintf("%s%s", word, i)
}

loans <- length(x)

server <- function(input, output) {

  observeEvent(input$submit, {
    print("Processing options")
  })

  counter <- reactiveValues(n = 1)

  #Track the number of input boxes previously
  prevcount <- reactiveValues(n = 1)

  observeEvent(input$add_loan, {
    counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1
  })

  output$counter <- renderPrint(print(counter$n))
  
  more_loans <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      
      if(input$fillin){
        lapply(seq_len(n), function(i) {
          list(h4(sprintf("Loan #%s", i)),
                textInput(word_num("loan", i), label = "Name", value = x[[i]]$name),
                numericInput(word_num("bal", i), label = "Remaining balance", value = x[[i]]$bal, min = 0, step = 20),
                numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = x[[i]]$min_pay, min = 10, step = 5),
                numericInput(word_num("int", i), label = "Interest Rate", value = x[[i]]$int, min = 0, max = 1, step = 0.01))
        })
      } else {
        lapply(seq_len(n), function(i) {
          list(h4(sprintf("Loan #%s", i)),
               textInput(word_num("loan", i), label = "Name", value = NA),
               numericInput(word_num("bal", i), label = "Remaining balance", value = NA, min = 0, step = 20),
               numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = NA, min = 10, step = 5),
               numericInput(word_num("int", i), label = "Interest Rate", value = NA, min = 0, max = 1, step = 0.01))
        })
      }
    }
  
  })
  
  output$more_loans_ui <- renderUI({ more_loans() })

  loan_list <- eventReactive(input$submit, {
    purrr::map(1:(counter$n), function(num){
      
      list(name = input[[word_num("loan", num)]], 
           bal = input[[word_num("bal", num)]],
           int = input[[word_num("int", num)]],
           min_pay = input[[word_num("min_pay", num)]])
    })
  })
  
  options_plot_data <- eventReactive(input$submit, {
    get_payoff_options(loan_list())
  })
  
  output$options_plot <- renderPlot({
    
    print(options_plot_data()[1:5,])
    plot_payoff_options(options_plot_data())
  })
  
  sched <- reactive({
    
    req(input$options_click)
    
    data <- options_plot_data()
    min_pay <- data
    
    x_val <- plyr::round_any(input$options_click$x, 50)
    if(x_val < data$mo_pay[2]) x_val <- data$mo_pay[2]
    
    sched <- conduct_schedule_analysis(loan_list(), max_mo_pay = x_val)
    
  })
  
  output$choice_info <- renderTable({
    req(input$options_click)
    
   sched <- sched()

    data_frame(`Monthly payment` = sprintf("$%0.2f", plyr::round_any(input$options_click$x, 50)),
               `Months to pay off` = max(sched$month_count),
               `Total interest to pay` = sprintf("$%0.2f", sum(sched$int_pay)))
  })

  output$schedule_plot <- renderPlot({
    plot_mo_payments(payment_sched = sched())
  })
  
}


# shinyApp(ui, server)