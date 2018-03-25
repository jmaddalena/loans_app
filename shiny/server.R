# 
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

loans <- length(loan_info_list)


server <- function(input, output) {

  observeEvent(input$submit, {
    print("Processing options")
  })

  counter <- reactiveValues(n = 0)

  #Track the number of input boxes previously
  prevcount <- reactiveValues(n = 0)

  observeEvent(input$add_loan, {
    counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1
  })

  output$counter <- renderPrint(print(counter$n))
  
  more_loans <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      lapply(seq_len(n), function(i) {
        list(textInput(word_num("loan", i), label = "Name", value = x[[i+1]]$name),
              numericInput(word_num("bal", i), label = "Remaining balance", value = x[[i+1]]$bal, min = 0, step = 20),
              numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = x[[i+1]]$min_pay, min = 10, step = 5),
              numericInput(word_num("int", i), label = "Interest Rate", value = x[[i+1]]$int, min = 0, max = 1, step = 0.01))
      })
    }
  
  })
  
  output$more_loans_ui <- renderUI({ more_loans() })

  # loan_list <- eventReactive(input$submit, {
  #   
  #   loan_list <- NULL
  #   
  #   loan_list$loan1 <- list(name = input$loan1, bal = input$bal1,
  #                           int = input$int1, min_pay = input$min_pay1)
  #   
  #   loan_list$loan2 <- list(name = input$loan2, bal = input$bal2,
  #                           int = input$int2, min_pay = input$min_pay2)
  # 
  #   loan_list
  # })  
  
  observeEvent(input$submit, print(names(input)))
               
  
  loan_list <- eventReactive(input$submit, {
    purrr::map(1:loans, function(num){
      print(num)
      
      list(name = input[[word_num("loan", num)]], 
           bal = input[[word_num("bal", num)]],
           int = input[[word_num("int", num)]],
           min_pay = input[[word_num("min_pay", num)]])
    })
  })
  
  print(loan_list)
  
  options_plot_data <- eventReactive(input$submit, {
    get_payoff_options(loan_list())
  })

  output$options_plot <- renderPlot({
    plot_payoff_options(options_plot_data())
  })
  
  output$schedule_plot <- renderPlot({

    data <- options_plot_data()
    x_val <- plyr::round_any(input$options_click$x, 100)
    
    sched <- conduct_schedule_analysis(loan_list(), max_mo_pay = x_val)
  
    plot_mo_payments(payment_sched = sched)
    
  })
  
  
}


# shinyApp(ui, server)