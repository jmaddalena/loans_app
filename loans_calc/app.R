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
  
  num_loans <- reactive(input$num_loans)
  
  prev_loans <- reactiveValues(n = 0)
  
  observeEvent(num_loans(), {
    
    print("observing")
    
    print(loan_list())
    
    if(prev_loans$n != input$num_loans){
      removeUI(selector = "#inserted_ui")
    
      acc_list <- purrr::map(1:input$num_loans, function(i){
      
        if(i > prev_loans$n & i <= length(x)){
            value_list <- list(name = x[[i]]$name,
                               balance = x[[i]]$balance,
                               min_pay = x[[i]]$min_pay,
                               int = x[[i]]$int) 
        } else {
          value_list <- list(name = loan_list()[[i]]$name,
                             balance = loan_list()[[i]]$balance,
                             min_pay = loan_list()[[i]]$min_pay,
                             int = loan_list()[[i]]$int) 
        }
        
        accordionItem(
          id = word_num("acc_", i),
          title = word_num("LOAN ", i),
          collapsed = TRUE,
          textInput(word_num("loan", i), label = "Name", value = value_list$name),
          numericInput(word_num("bal", i), label = "Remaining balance", value = value_list$bal, min = 0, step = 20),
          numericInput(word_num("min_pay", i), label = "Minimum monthly payment", value = value_list$min_pay, min = 10, step = 5),
          numericInput(word_num("int", i), label = "Interest Rate", value = value_list$int, min = 0, max = 1, step = 0.01)
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
    }
    
    prev_loans$n <- input$num_loans
    
  })
  
  loans_inputs <- reactive({
    purrr::map(1:num_loans(), function(i){
        purrr::map(c("loan", "bal", "min_pay", "int"), function(str){
          input[[word_num(str, i)]]
        })
    })
  })
  
  loan_list <- eventReactive(loans_inputs(), {
    print("reacting")
    
    loans <- purrr::map(1:num_loans(), function(num){
      list(name = input[[word_num("loan", num)]],
           balance = input[[word_num("bal", num)]],
           int = input[[word_num("int", num)]],
           min_pay = input[[word_num("min_pay", num)]])
    })
  
    any_miss <- any(
      unlist(
        lapply(loans, function(sub) 
          any(is.na(sub))
        )
      )
    )
    
    validate(
      need(!any_miss, "Please fill in missing inputs")
    )
  
    loans
  })
  
  options_plot_data <- eventReactive(input$submit, {
    
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
    
    curr_pay <- options_plot_data()$mo_pay[2]
    values[values$`Monthly payment` == "$NA", "Monthly payment"] = 
      sprintf("Minimum (currently $%0.2f)", curr_pay)
    
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
    withProgress(message = '', value = 1, {
      plot_mo_payments(payment_sched = sched())
    })
  })
  
  output$balance_plot <- renderPlot({
    withProgress(message = '', value = 1, {
      plot_balance_over_time(payment_sched = sched())
    })
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
                
  includeCSS("styles.css"),
  
  titlePanel("Pay off your loans faster!"),
  helpText("Repaying loans is stressful, but it helps to have a good plan. Let’s see how much time and money you can save by paying more than the minimum required amount each month."),
  br(),

  sidebarLayout(
    
    sidebarPanel(

      numericInput("num_loans", "How many loans do you have?", value = 1, min = 1, max = 12, step = 1),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      
      textOutput(outputId = "temp"),      
      
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

