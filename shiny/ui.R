
ui <- fluidPage(
  
  # App title ----
  titlePanel("Title"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    sidebarPanel(    
    
      # for(num in loans){ 
      #   h3(sprintf("Loan %s", loan)),
      # 
      #   word_num <- function(word, i){
      #     sprintf("%s%s", word, i)
      #   }
      #   
      #   textInput(word_num("loan", num), label = "Name", value = word_num("loan", num)),
      #   numericInput(word_num("bal", num), label = "Remaining balance", value = x[[num]]$bal, min = 0, step = 20),
      #   numericInput(word_num("min_pay", num), label = "Minimum monthly payment", value = x[[num]]$min_pay, min = 10, step = 5),
      #   numericInput(word_num("int", num), label = "Interest Rate", value = 0.06, min = x[[num]]$int, max = 1, step = 0.01),
     # }
    
      # h4("Loan #1"),
      # textInput("loan1", label = "Name", value = "Sallie Mae"),
      # numericInput("bal1", label = "Remaining balance", value = 14707.01, min = 0, step = 20),
      # numericInput("min_pay1", label = "Minimum monthly payment", value = 207.97, min = 10, step = 5),
      # numericInput("int1", label = "Interest Rate", value = 0.06, min = 0.04375, max = 1, step = 0.01),
      # 
      uiOutput("more_loans_ui"),

      actionButton("add_loan", label = "", icon = icon("plus-square")),
      textOutput("counter"),
      
      # h3("Loan #2"),
      # 
      # textInput("loan2", label = "Name", value = "loan2"),
      # numericInput("bal2", label = "Remaining balance", value = 10000, min = 0, step = 20),
      # numericInput("mo_pay2", label = "Minimum monthly payment", value = 150, min = 10, step = 5),
      # numericInput("int2", label = "Interest Rate", value = 0.07, min = 0, max = 1, step = 0.01),
      
      checkboxInput("fillin", "Fill in example numbers?", value = FALSE),
      
      actionButton("submit", "Show me my options")
      
    ),
    
    # conditionalPanel(
    #   checkboxInput("smooth", "Smooth"),
    #   conditionalPanel(
    #     condition = "input.smooth == true",
    #     selectInput("smoothMethod", "Method",
    #                 list("lm", "glm", "gam", "loess", "rlm"))
    #   )
    # 
    # )
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      h4("Choose a point on plot to see payment plan"),
      plotOutput(outputId = "options_plot", click = "options_click"),
     # verbatimTextOutput("click_info")
      
      tableOutput(outputId = "choice_info"),
      
      h4("Payment Plan"),
      plotOutput(outputId = "schedule_plot")
    )
  )
)