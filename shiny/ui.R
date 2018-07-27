
ui <- fluidPage(
  
  titlePanel("Student Loan Repayment Calculator"),
  br(),
  
  sidebarLayout(

    sidebarPanel(
      
      width = 3,
      
      tags$head(
        tags$style(type="text/css", "label.control-label, .form-group.shiny-bound-input{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
      ),

      checkboxInput("fillin", "Fill in example loan figures?", value = TRUE),
      uiOutput("more_loans_ui"),
      actionButton("add_loan", label = "", icon = icon("plus-square")),
      actionButton("minus_loan", label = "", icon = icon("minus-square")),
      actionButton("submit", "Show me my options")
      
    ),
    
    mainPanel(
      
      h5(textOutput("heading1")),
      plotOutput(outputId = "options_plot", click = "options_click"),
      
      h4(textOutput("heading2")),
      tableOutput(outputId = "choice_info"),
      
      h4(textOutput("heading3")),
      textOutput("action_items"),
     
      h4(textOutput("heading4")), 
      plotOutput(outputId = "schedule_plot")

    )
  )
)