library(tidyverse)
source("~/Documents/student_loans/shiny/source.R")

x <- list(list(name = "Sample loan 1", balance = 2000, int = 0.065, min_pay = 22.71),
          list(name = "Sample loan 2", balance = 8000, int = 0.05, min_pay = 84.86),
          list(name = "Sample loan 3", balance = 1000, int = 0.03, min_pay = 17.97),
          list(name = "Sample loan 4", balance = 4000, int = 0.025, min_pay = 37.71))
          
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
    if(input$fillin) counter$n <- min(length(x), counter$n + 1)
    else counter$n <- counter$n + 1
    prevcount$n <- counter$n - 1
  })
  
  observeEvent(input$minus_loan, {
    counter$n <- counter$n - 1
    prevcount$n <- counter$n + 1
  })

  more_loans <- reactive({
    
    n <- counter$n
    
    if (n > 0) {
      
      if(input$fillin){
        lapply(seq_len(n), function(i) {
          list(h4(sprintf("Loan #%s", i)),
                textInput(word_num("loan", i), label = "Name", value = x[[i]]$name),
                numericInput(word_num("bal", i), label = "Remaining balance", value = x[[i]]$bal, min = 0, step = 20),
                numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = x[[i]]$min_pay, min = 10, step = 5),
                numericInput(word_num("int", i), label = "Interest Rate", value = x[[i]]$int, min = 0, max = 1, step = 0.01),
                br())
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
        incProgress(1/n, detail = sprintf("Monthly payment: $%0.2f", mo_pay_display))
        
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
    
  heading1_str <- eventReactive(input$submit, {
    "Select a point on either plot to see payment plan:"
  })
  
  output$heading1 <- renderText({
    heading1_str()
  })
  
  heading2_str <- eventReactive(input$options_click, {
    "Payment plan"
  })
  
  output$heading2 <- renderText({
    heading2_str()
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
  
  output$heading3 <- eventReactive(input$options_click, {
    "Action items"
  })

  output$heading4 <- eventReactive(input$options_click, {
    "Payments by month under payment plan"
  })
  
  output$schedule_plot <- renderPlot({
    plot_mo_payments(payment_sched = sched())
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

# shinyApp(ui, server)